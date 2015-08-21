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

(defonce app (atom {:text "LoL Set Forge"
                    :props #{}
                    :recommended []
                    :champ-level 18
                    :num-items 6
                    :itemset {:title "Untitled"
                              :type "custom"
                              :map "any"
                              :mode "any"
                              :blocks []}}))

(go
  (let [champs (<! (data/champ-chan))
        items (<! (data/item-chan))]
    (swap! app assoc :items items :champs champs :champ (val (first champs)))))

(defn recommend []
  (let [{:keys [items num-items champ champ-level props]} @app
        ch (algo/recommend items num-items champ champ-level props)]
    (go-loop []
      (when-let [build (<! ch)]
        (swap! app assoc :recommended build)
        (recur)))))

(defn add-block []
  (swap! app update-in [:itemset :blocks] conj
    {:type "Untitled"
     :items (for [item (:recommended @app)]
              {:id (str (:id item)) :count 1})}))

(defn champion-image []
  [:img.img-rounded {:src (data/champ-img-square-url (get-in @app [:champ :id] "Heimerdinger"))
                     :width 64
                     :height 64}])

(defn champion-select []
  [:div.media
   [:div.media-left (champion-image)]
   [:div.media-body
    [:h4.media-heading "Make a build for:"]
    [:select {:value (:id (:champ @app))
              :on-change (fn [event]
                           (swap! app
                                  #(assoc %1 :champ (get-in %1 [:champs %2]))
                                  (keyword (-> event .-target .-value))))}
     (for [[id ch] (sort-by (comp :name val) (:champs @app))]
       [:option {:key id :value id} (:name ch)])]]])

(defn item-image [item]
  [:img.img-rounded {:src (data/item-img-url (get-in item [:image :full]))
                     :width 64 :height 64}])

(defn item-component [item]
  [:div.media.well.well-sm {:key (:id item)}
   [:div.media-left [item-image item]]
   [:div.media-body
    [:h4.media-heading (:name item)]
    [:div {:dangerouslySetInnerHTML {:__html(:description item)}}]]])

(defn item-recommendation []
  [:ul.media-list (for [item (:recommended @app)]
          (item-component item))])

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
             :max max :min min
             :value (key @app)
             :on-change #(swap! app assoc key (int (-> % .-target .-value)))}]]])

(defn build-stats []
  (let [{:keys [recommended champ champ-level]} @app]
    [:div.panel.panel-primary
     [:div.panel-heading "Build statistics"]
     [:table.table
      [:tr [:th "Stat"] [:th "Value"]]
      [:tr [:td "Attack damage per second"] [:td (int (algo/build-dps champ champ-level recommended))]]
      [:tr [:td "Attack damage"] [:td (int (algo/attack-damage champ champ-level recommended))]]
      [:tr [:td "Critical strike chance"] [:td (int (* 100 (algo/critical-strike champ champ-level recommended))) "%"]]
      [:tr [:td "Attack speed"] [:td (.toFixed (algo/attack-speed champ champ-level recommended) 2)]]
      [:tr [:td "Life steal per second"] [:td (int (algo/build-lsps champ champ-level recommended))]]
      [:tr [:td "Life steal"] [:td (int (* 100 (algo/life-steal recommended))) "%"]]
      [:tr [:td "Ability power"] [:td (algo/ability-power recommended)]]
      [:tr [:td "Effcetive health (AD)"] [:td (int (algo/build-hp-ad champ champ-level recommended))]]
      [:tr [:td "Effcetive health (AP)"] [:td (int (algo/build-hp-ap champ champ-level recommended))]]
      [:tr [:td "HP"] [:td (int (algo/health champ champ-level recommended))]]
      [:tr [:td "Armor"] [:td (int (algo/armor champ champ-level recommended))]]
      [:tr [:td "Magic resist"] [:td (int (algo/magic-resist champ champ-level recommended))]]
      ]]))

(defn item-block [id block items]
  (let [ids (set (map #(int (:id %)) (:items block)))
       check #(contains? ids (:id %))
       items (filter check items)]
   [:div.panel.panel-default {:key id}
    [:div.panel-heading
     [:input.form-control
      {:type "text"
       :on-change #(swap! app assoc-in [:itemset :blocks id :type] (-> % .-target .-value))
       :value (:type block)}]]
      [:div.panel-body
     (for [item items]
       [:span {:key (:id item)}
        (item-image item)])]]))

(defn item-set []
  (let [state @app
        itemset (:itemset state)
        items (:items state)
        blocks (map vector (range) (:blocks itemset))]
    [:div.panel.panel-primary
      [:div.panel-heading
       [:input.form-control
        {:type "text"
         :on-change #(swap! app assoc-in [:itemset :title] (-> % .-target .-value))
         :value (:title itemset)}]]
      [:div.panel-body
       (for [[id block] blocks]
         [item-block id block items])]]))

(defn needlessly-large-button []
  (let [blob (js/Blob. #js[(.stringify js/JSON (clj->js (:itemset @app)))]
                       #js{:type "application/json"})
        url (.createObjectURL js/URL blob)]
    [:a.btn.btn-default.btn-xl {:href url :download "build.json"} "Download"]))

(defn mirage-button []
  [:button.btn.btn-default.btn-xl {:on-click add-block} "Add to set"])

(defn button-of-command []
  [:button.btn.btn-primary.btn-xl {:on-click recommend} "Generate build"])

(defn app-component []
  [:div.container
   [:div.row
    [:h1.col-sm-12 (:text @app)]]
   [:div.row
     [:div.col-sm-6
      [champion-select] [:br]
      [objective-checkbox "Attack damage per second" algo/build-dps]
      [objective-checkbox "Life Steal per second" algo/build-lsps]
      [objective-checkbox "Ability power" (algo/item-wrapper algo/ability-power)]
      [objective-checkbox "Effective Health (AP)" algo/build-hp-ap]
      [objective-checkbox "Effective Health (AD)" algo/build-hp-ad] [:br]
      [number-selector "Champion level" :champ-level 18 1]
      [number-selector "Number of items" :num-items 6 1] [:br]
      [:div.btn-group
        [button-of-command] ; generate
        [mirage-button] ; add
        [needlessly-large-button]] ; download
      [:br][:br] ; ugly
      [item-set]]
     [:div.col-sm-6
      [build-stats]
      [item-recommendation]]]])

(reagent/render-component [app-component]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  (swap! app assoc :props #{}))
