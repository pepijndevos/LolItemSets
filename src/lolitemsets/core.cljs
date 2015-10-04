(ns ^:figwheel-always lolitemsets.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cljs.core.async :refer [put! chan <!]]
    [reagent.core :as reagent :refer [atom]]
    [lolitemsets.algo :as algo]
    [lolitemsets.overlay :as overlay]
    [lolitemsets.data :as data]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app (atom {:text "LoL Set Forge"
                    :props {}
                    :recommended []
                    :champ nil
                    :champ-level 18
                    :num-items 6
                    :generating false
                    :trolling false
                    :itemset {:title "Untitled"
                              :type "custom"
                              :map "any"
                              :mode "any"
                              :blocks [{:type "Starting items"
                                        :items [{:id "1001" :count 1}
                                                {:id "1054" :count 1}
                                                {:id "1055" :count 1}
                                                {:id "1056" :count 1}
                                                {:id "2003" :count 1}
                                                {:id "2004" :count 1}
                                                {:id "3340" :count 1}]}]}}))

(defonce items (atom nil))
(defonce champs (atom nil))

(defn init []
  (go
    (let [champ-chan (data/champ-chan)
          item-chan (data/item-chan)
          champ-data (<! champ-chan)]
      (reset! champs champ-data)
      (swap! app assoc :champ (val (first champ-data)))
      (let [item-data (<! item-chan)]
        (reset! items item-data)))))

(defonce do-it (init))

(defn recommend []
  (let [{:keys [num-items champ champ-level props]} @app
        items @items
        ch (algo/recommend (vals items) num-items champ champ-level (vals props))]
    (go-loop []
      (when-let [build (<! ch)]
        (swap! app assoc :recommended build)
        (recur)))))

(defn add-block []
  (swap! app update-in [:itemset :blocks] conj
    {:type "Untitled"
     :items (for [item (:recommended @app)]
              {:id (str (:id item)) :count 1})}))

(defn set-url []
  (let [blob (js/Blob. #js[(.stringify js/JSON (clj->js (:itemset @app)))]
                       #js{:type "application/json"})]
    (.createObjectURL js/URL blob)))

(defn champion-image [champ-id]
  [:img.img-rounded {:src (data/champ-img-square-url champ-id)
                     :width 64
                     :height 64}])

(defn champion-select [champ-id]
  [:div.media
   [:div.media-left [champion-image champ-id]]
   [:div.media-body
    [:h4.media-heading "Make a build for:"]
    [:select {:value champ-id
              :on-change (fn [event]
                           (let [champ-name (keyword (-> event .-target .-value))]
                             (swap! app assoc :champ (@champs champ-name))))}
     (for [[id ch] (sort-by (comp :name val) @champs)]
       [:option {:key id :value id} (:name ch)])]]])

(defn item-select [idx item]
  [:select {:value (:id item)
            :on-change (fn [event]
                         (swap! app
                                #(assoc-in %
                                   [:recommended idx]
                                   (get-in % [:items %2]))
                                (int (-> event .-target .-value))))}
   (for [[id {name :name}] (sort-by (comp :name val) @items)]
     [:option {:key id :value id} name])])

(defn item-image [item]
  [:span {:style {:padding-right "5px"}}
   [:img.img-rounded
    {:src (if item
            (data/item-img-url (get-in item [:image :full]))
            "questionmark.png")
     :width 64 :height 64}]])

(defn item-component [idx item]
  [:div.media.well.well-sm {:key (:id item)}
   [:div.media-left [item-image item]]
   [:div.media-body
    [:h4.media-heading
     [item-select idx item]
     [:span
      [:img {:src (data/gold-url)}]
      (:total (:gold item))]]
    [:div {:dangerouslySetInnerHTML {:__html (:description item)}}]]])

(defn item-recommendation [recommended]
  [:div.media-list
   (map item-component
        (range)
        recommended)])

(defn objective-checkbox [props name objective]
  (let [id (str (gensym))
        toggle (fn [event]
            (swap! app update-in [:props]
                   (if (-> event .-target .-checked)
                     #(assoc % name objective)
                     #(dissoc % name))))]
    [:div.row
     [:div.col-xs-1 [:input {:type :checkbox
                             :checked (contains? props name)
                             :on-change toggle :id id}]]
     [:label {:for id} name]]))

(defn number-selector [value label key max min]
  [:div.row
   [:div.col-xs-3 label]
   [:div.col-xs-9
    [:input {:type "number"
             :max max :min min
             :value value
             :on-change #(swap! app assoc key (int (-> % .-target .-value)))}]]])

(def stats
  "A vector of maps
  :name - The user friendly name of the stat
  :calc - A function that takes a champion, champion level, and item
  set, and returns the raw value of that stat.
  :pretty - A function that takes the raw value of a stat and returns
  a user friendly value (i.e. adds a % sign if appropriate). Optional,
  default is the \"int\" function.
  :optimizable - Whether a checkbox exists for this stat.
  :troll - Whether the Troll button may optimize for this stat."
  [{:name "Attack damage (AD)"
    :calc algo/attack-damage
    :optimizable true}
   {:name "Critical strike chance"
    :calc algo/critical-strike
    :pretty (fn [n] (str (int (* 100 n)) "%"))}
   {:name "Attack speed"
    :calc algo/attack-speed
    :pretty (fn [n] (.toFixed n 2))}
   {:name "Physical damage per second (DPS)"
    :calc algo/build-dps
    :optimizable true
    :troll true}

   {:name "Life steal"
    :calc (algo/item-wrapper algo/life-steal)
    :pretty (fn [n] (str (int (* 100 n)) "%"))}
   {:name "Life stolen per second"
    :calc algo/build-lsps
    :optimizable true}

   {:name "Ability power (AP)"
    :calc (algo/item-wrapper algo/ability-power)
    :optimizable true
    :troll true}

   {:name "Health"
    :calc algo/health}
   {:name "Armor"
    :calc algo/armor}
   {:name "Magic resist"
    :calc algo/magic-resist}
   {:name "Effective health (physical)"
    :calc algo/build-hp-ad
    :optimizable true}
   {:name "Effective health (magic)"
    :calc algo/build-hp-ap
    :optimizable true}

   {:name "Mana"
    :calc algo/mana
    :optimizable true
    :troll true}
   {:name "Mana regen over 5 seconds"
    :calc algo/mana-regen
    :optimizable true}

   {:name "Movement speed"
    :calc algo/move-speed
    :optimizable true
    :troll true}])

(defn build-stats [recommended champ champ-level props]
  [:div.panel.panel-primary
   [:div.panel-heading "Build statistics"]
   (into [:table.table
          [:tr [:th "Stat"] [:th "Value"]]]
         (for [{:keys [name calc pretty] :as stat} stats
               :let [stat-optimized? (contains? props name)]]
           [(if stat-optimized?
              :tr.info
              :tr)
            [:td (cond-> name
                   stat-optimized? (as-> el [:b el]))]
            [:td (cond-> (calc champ champ-level recommended)
                   (not pretty) int
                   pretty pretty
                   stat-optimized? (as-> el [:b el]))]]))])

(defn item-block [id block]
  (let [items (map #(get @items (int (:id %))) (:items block))]
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

(defn item-set [itemset]
  (let [blocks (map vector (range) (:blocks itemset))]
    [:div.panel.panel-primary
     [:div.panel-heading
      [:input.form-control
       {:type "text"
        :on-change #(swap! app assoc-in [:itemset :title] (-> % .-target .-value))
        :value (:title itemset)}]]
     [:div.panel-body
      (for [[id block] blocks]
        (item-block id block))]]))

(def troll-objectives
  (into {} (for [{:keys [name calc troll] :as stat} stats
                 :when troll]
             [name calc])))

(defn troll []
  (swap! app #(assoc % :champ (rand-nth (vals (:champs %)))
                       :props (conj {} (rand-nth (seq troll-objectives)))))
  (recommend))

(defn dorans-button [trolling]
  [:a.btn.btn-danger.btn-lg
   {:on-click (fn [] (go (swap! app assoc :trolling true)
                         (<! (troll))
                         (swap! app assoc :trolling false)))
    :class (if trolling
             "disabled"
             nil)}
   [:span.glyphicon.glyphicon-fire]
   (if trolling
     " YOLO..."
     " Troll")])

(defn needlessly-large-button [itemset-title]
  [:a.btn.btn-default.btn-lg {:href (set-url)
                              :download (str itemset-title ".json")}
   [:span.glyphicon.glyphicon-save] " Download"])

(defn mirage-button []
  [:a.btn.btn-default.btn-lg {:on-click add-block}
   [:span.glyphicon.glyphicon-plus-sign] " Add to set"])

(defn button-of-command [generating]
  [:a.btn.btn-primary.btn-lg
   {:on-click (fn [] (go (swap! app assoc :generating true)
                         (<! (recommend))
                         (swap! app assoc :generating false)))
    :class (if generating
             "disabled"
             nil)}
   [:span.glyphicon.glyphicon-equalizer]
   (if generating
     " Generating..."
     " Generate")])

(defn app-component []
  (let [state @app]
    [:div.container
     [:div.row
      [:h1.col-sm-6 (:text state)]
      [:div.col-sm-6
       [:br]
       [:a {:href "https://github.com/pepijndevos/LolItemSets#usage"}
        [:span.glyphicon.glyphicon-question-sign] " About"]]]
     [:div.row
      [:div.col-sm-6
       [champion-select (:id (:champ state))]
       [:h4 "Objectives "
        [:a.glyphicon.glyphicon-question-sign
         {:href "https://github.com/pepijndevos/LolItemSets#objectives"}]]
       (into [:div] (for [{:keys [name calc optimizable] :as stat} stats
                          :when optimizable]
                      [objective-checkbox (:props state) name calc]))
       [number-selector (:champ-level state) "Champion level" :champ-level 18 1]
       [number-selector (:champ-level state) "Number of items" :num-items 6 1] [:br]
       [:div.btn-group.btn-group-justified.visible-lg-block
        [button-of-command (:generating state)] ; generate
        [dorans-button (:trolling state)] ; troll
        [mirage-button]              ; add
        [needlessly-large-button (:title (:itemset state))]] ; download
       [:div.btn-group.btn-group-justified.hidden-lg
        [button-of-command (:generating state)]
        [dorans-button (:trolling state)]]
       [:div.btn-group.btn-group-justified.hidden-lg
        [mirage-button]
        [needlessly-large-button (:title (:itemset state))]]
       [:p [:br]
        "Save inside your League of Legends folder in "
        [:code
         "Config/Champions/" 
         (:id (:champ state))
         "/Recommended/" 
         (:title (:itemset state))
         ".json"]]
       [item-set (:itemset state) (:blocks state)]]
      [:div.col-sm-6
       [build-stats (:recommended state) (:champ state) (:champ-level state) (:props state)]
       [item-recommendation (:recommended state)]]]]))

(reagent/render-component [app-component]
                          (. js/document (getElementById "app")))


(defn on-js-reload [])
