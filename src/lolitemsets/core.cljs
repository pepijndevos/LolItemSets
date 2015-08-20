(ns ^:figwheel-always lolitemsets.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cljs.core.async :refer [put! chan <!]]
    [reagent.core :as reagent :refer [atom]]
    [lolitemsets.algo :as algo]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"
                          :props [algo/build-dps]
                          :recommended []}))

(go
  (let [champs (:data (<! (algo/champ-chan)))
        items (:data (<! (algo/item-chan)))]
    (swap! app-state assoc :items items :champs champs)))

(defn champion-select []
  [:div.form-group
    [:label "Pick a Champion"]
    [:select  {:on-change (fn [event]
                            (swap! app-state
                              #(assoc %1 :champ (get-in %1 [:champs %2]))
                              (keyword (-> event .-target .-value))))}
     (for [[id ch] (:champs @app-state)]
       [:option {:key id :value id} (:name ch)])]])

(defn champion-image []
  [:img {:src (str "http://ddragon.leagueoflegends.com/cdn/img/champion/loading/"
                   (:id (:champ @app-state))
                   "_0.jpg")}])

(defn recommend []
  (let [{:keys [items champ props]} @app-state
        ch (algo/recommend (vals items) champ props)]
    (go-loop []
      (when-let [build (<! ch)]
        (swap! app-state assoc :recommended build)
        (recur)))))

(defn item-recommendation []
    [:ul (for [item (:recommended @app-state)]
           [:li (:name item)])])

(defn hello-world []
  [:div
    [:h1 (:text @app-state)]
    [champion-select]
    [champion-image]
    [item-recommendation]])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

