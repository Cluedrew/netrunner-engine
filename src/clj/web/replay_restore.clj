(ns web.replay-restore
  (:require
   [clojure.data.json :refer [read-json]]
   [game.core.say :refer [system-msg]]
   [game.core.finding :refer [find-card]]
   [game.core.moving :refer [move]]
   [game.replay :as replay]))

(defn replay-deps [game]
  {:app-state (atom {})
   :game-state (atom {})
   :last-state (atom {})
   :replay-status (atom {:autoplay false :speed 1600})
   :replay-timeline (atom [])
   :replay-side (atom :spectator)
   :load-notes (fn [] nil)
   :get-remote-annotations (fn [_] nil)})

(defn check-for-correct-ids [game replay-state]
  (let [state (:state game)]
    (when (not= (get-in @state [:corp :identity :normalizedtitle])
                (get-in @replay-state [:corp :identity :normalizedtitle]))
      (throw (Exception. "Selected Corp ID does not match replay.")))
    (when (not= (get-in @state [:runner :identity :normalizedtitle])
                (get-in @replay-state [:runner :identity :normalizedtitle]))
      (throw (Exception. "Selected Runner ID does not match replay.")))))

(defn move-all-cards-to-decks [game]
  (doseq [side [:corp :runner]]
    (let [state (:state game)]
      (doseq [card (get-in @state [side :hand])]
        (move state side card :deck {:suppress-event true :force true})))))

(defn restore-card [game side target-card path]
  (let [state (:state game)
        card (find-card (:title target-card) (get-in @state [side :deck]))]
    (when-not card (throw (Exception. (str "Card " (:title target-card) " not found in deck. Check whether you selected the correct decklists."))))
    (move state side card path {:suppress-event true :force true})))

(defn restore-cards-at-path [game replay-state side path cid-map]
  (doseq [target-card (get-in @replay-state (cons side path))]
    (let [new-card (restore-card game side target-card path)]
      (swap! cid-map assoc (:cid target-card) (:cid new-card)))))

(def zones {:runner [:hand :deck :discard :scored :rfg :play-area :current]
            :corp [:hand :deck :discard :scored :rfg :play-area :current]})

(defn restore-basic-zones [game replay-state cid-map]
  (doseq [side [:corp :runner]
          zone (side zones)]
    (restore-cards-at-path game replay-state side [zone] cid-map)))

(defn restore-installed-zones [game replay-state cid-map]
  (doseq [server (keys (get-in @replay-state [:corp :servers]))]
    (restore-cards-at-path game replay-state :corp [:servers server :content] cid-map)
    (restore-cards-at-path game replay-state :corp [:servers server :ices] cid-map))
  (doseq [rig-zone [:program :hardware :resource :facedown]]
    (restore-cards-at-path game replay-state :runner [:rig rig-zone] cid-map)))

(defn setup-state-from-replay [game replay-deps]
  (let [replay-state (:game-state replay-deps)
        cid-map (atom {})]
    (check-for-correct-ids game replay-state)
    (move-all-cards-to-decks game)
    (restore-basic-zones game replay-state cid-map)
    (restore-installed-zones game replay-state cid-map)))

(defn handle-replay-state
  [game {:keys [replay]} replay-timestamp]
  (when replay
    (let [history (read-json replay true)
          replay-state (replay-deps game)]
      (reset! (:game-state replay-state) (replay/replay-init-state-from-history history (:gameid game)))
      (replay/populate-replay-timeline! replay-state @(:game-state replay-state))
      (replay/replay-jump-to! replay-state replay-timestamp)
      (setup-state-from-replay game replay-state)
      (system-msg (:state game) :public "[!] Replay restored")
      game)))
