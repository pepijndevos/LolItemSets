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

(defn champion-image []
  [:img.img-rounded {:src (data/champ-img-square-url (get-in @app [:champ :id] "Aatrox"))
                     :width 64
                     :height 64}])

(defn champion-select []
  [:div.media
   [:div.media-left (champion-image)]
   [:div.media-body
    [:h4.media-heading "Make a build for:"]
    [:select {:field :list
              :on-change (fn [event]
                           (swap! app
                                  #(assoc %1 :champ (get-in %1 [:champs %2]))
                                  (keyword (-> event .-target .-value))))}
     (for [[id ch] (sort-by (comp :name val) (:champs @app))]
       [:option {:key id :value id} (:name ch)])]]])

(defn recommend []
  (let [{:keys [items num-items champ champ-level props]} @app
        ch (algo/recommend items num-items champ champ-level props)]
    (go-loop []
      (when-let [build (<! ch)]
        (swap! app assoc :recommended build)
        (recur)))))

(defn item-component [item]
  [:div.media
   [:div.media-left [:img.img-rounded {:src
                                       (if item
                                         (data/item-img-url (get-in item [:image :full]))
                                         "/questionmark.png")
                                       :width 32 :height 32}]]
   [:div.media-body
    [:h4.media-heading (if item
                         (:name item)
                         "???")]
    [:div {:dangerouslySetInnerHTML {:__html(:description item)}}]]])

(defn item-recommendation []
  [:ul (for [[i item] (-> (:recommended @app)
                          (concat (repeat nil))
                          (->> (take 6)
                               (map vector (range))))]
         [:div.well.well-sm {:key (str (:name item) (hash item) i)}
          (item-component item)])])

(defn objective-checkbox [name objective]
  (let [id (str (gensym))
        toggle (fn [event]
            (swap! app update-in [:props]
                   (if (-> event .-target .-checked) conj disj)
                   objective))]
    [:div.row
     [:div.col-xs-1 [:input {:type :checkbox :on-change toggle :id id}]]
     [:label {:for id} name]]))

(defn number-selector [label key max min]
  [:div.row
   [:div.col-xs-3 label]
   [:div.col-xs-9
    [:input {:type "number"
             :field :numeric
             :max max :min min
             :value (key @app)
             :on-change #(swap! app assoc key (int (-> % .-target .-value)))}]]])

(defn needlessly-large-button []
  [:button.btn.btn-info {:on-click recommend} "Generate build"])

(defn app-component []
  [:div.container
   [:div.container.col-sm-6
    [:h1 (:text @app)]
    [champion-select] [:br]
    [objective-checkbox "Attack damage per second" algo/build-dps]
    [objective-checkbox "Life Steal per second" algo/build-lsps]
    [objective-checkbox "Ability power" (algo/item-wrapper algo/ability-power)]
    [objective-checkbox "Effective Health (AP)" algo/build-hp-ap]
    [objective-checkbox "Effective Health (AD)" algo/build-hp-ad] [:br]
    [number-selector "Champion level" :champ-level 18 1]
    [number-selector "Number of items" :num-items 6 1] [:br]
    [needlessly-large-button] [:br]]
   [:div.container.col-sm-6 [item-recommendation]]])

(reagent/render-component [app-component]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  (swap! app assoc :props #{}))
