(ns ^:figwheel-always lolitemsets.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cljs.core.async :refer [put! chan <!]]
    [reagent.core :as reagent :refer [atom]]
    [lolitemsets.algo :as algo]
    [lolitemsets.data :as data]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app (atom {:text "Hello world!"
                    :props #{}
                    :recommended []
                    :champ-level 18
                    :num-items 6}))

(go
  (let [champs (<! (data/champ-chan))
        items (<! (data/item-chan))]
    (swap! app assoc :items items :champs champs)))

(defn champion-select []
  [:label "Pick a Champion"
    [:select  {:field :list
               :on-change (fn [event]
                            (swap! app
                              #(assoc %1 :champ (get-in %1 [:champs %2]))
                              (keyword (-> event .-target .-value))))}
     (for [[id ch] (sort-by (comp :name val) (:champs @app))]
       [:option {:key id :value id} (:name ch)])]])

(defn champion-image []
  [:img {:src (data/champ-img-square-url (get-in @app [:champ :id] "Aatrox"))}])

(defn recommend []
  (let [{:keys [items num-items champ champ-level props]} @app
        ch (algo/recommend items num-items champ champ-level props)]
    (go-loop []
      (when-let [build (<! ch)]
        (swap! app assoc :recommended build)
        (recur)))))

(defn item-recommendation []
    [:ul (for [item (:recommended @app)]
           [:li {:key (:name item)} (:name item)])])

(defn objective-checkbox [name objective]
  (letfn [(toggle [event]
            (swap! app update-in [:props]
                   (if (-> event .-target .-checked) conj disj)
                   objective))]
  [:label
   name
   [:input {:type :checkbox :on-change toggle}]]))

(defn number-selector [label key max min]
  [:label label
    [:input {:type "number"
             :field :numeric
             :max max :min min
             :value (key @app)
             :on-change #(swap! app assoc key (int (-> % .-target .-value)))}]])

(defn needlesly-large-button []
   [:button {:on-click recommend} "Recommend"])

(defn hello-world []
  [:div
    [:h1 (:text @app)]
    [:div
      [champion-select]]
    [champion-image]
    [:div
      [objective-checkbox "Attack damaeg per second" algo/build-dps]
      [objective-checkbox "Life Steal per second" algo/build-lsps]
      [objective-checkbox "Ability power" (algo/item-wrapper algo/ability-power)]
      [objective-checkbox "Effective Health (AP)" algo/build-hp-ap]
      [objective-checkbox "Effective Health (AD)" algo/build-hp-ad]]
    [number-selector "Champion level" :champ-level 18 1]
    [number-selector "Number of items" :num-items 6 1]
    [needlesly-large-button]
    [item-recommendation]])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app to force rerendering depending on
  ;; your application
  ;; (swap! app update-in [:__figwheel_counter] inc)
)

