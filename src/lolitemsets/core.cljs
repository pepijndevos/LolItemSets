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
                    :champ-level 18
                    :num-items 6
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

(defonce init (go
                (let [champ-chan (data/champ-chan)
                      item-chan (data/item-chan)
                      champs (<! champ-chan)
                      items (<! item-chan)]
                  (swap! app assoc :items items :champs champs :champ (val (first champs))))))

(defn recommend []
  (let [{:keys [items num-items champ champ-level props]} @app
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

(def troll-objectives {
"Attack damage per second" algo/build-dps
"Ability power" (algo/item-wrapper algo/ability-power)
"Mana" algo/mana
"Movement speed" algo/move-speed
"Effective Health (AP)" algo/build-hp-ap
"Effective Health (AD)" algo/build-hp-ad})

(defn troll []
  (swap! app #(assoc % :champ (rand-nth (vals (:champs %)))
                       :props (conj {} (rand-nth (seq troll-objectives)))))
  (go
    (<! (recommend))
    (add-block)
    (set! (.-location js/window) (set-url))))

(defn champion-image []
  [:img.img-rounded {:src (data/champ-img-square-url (get-in @app [:champ :id] "Heimerdinger"))
                     :width 64
                     :height 64}])

(defn champion-select []
  [:div.media
   [:div.media-left [champion-image]]
   [:div.media-body
    [:h4.media-heading "Make a build for:"]
    [:select {:value (:id (:champ @app))
                   :on-change (fn [event]
                                (swap! app
                                       #(assoc %1 :champ (get-in %1 [:champs %2]))
                                       (keyword (-> event .-target .-value))))}
        (for [[id ch] (sort-by (comp :name val) (:champs @app))]
          [:option {:key id :value id} (:name ch)])]]])

(defn item-select [idx item]
  [:select {:value (:id item)
            :on-change (fn [event]
                         (swap! app
                                #(assoc-in %
                                   [:recommended idx]
                                   (get-in % [:items %2]))
                                (int (-> event .-target .-value))))}
   (for [[id {name :name}] (sort-by (comp :name val) (:items @app))]
     [:option {:key id :value id} name])])

(defn item-image [item]
  [:img.img-rounded {:src (if item
                            (data/item-img-url (get-in item [:image :full]))
                            "questionmark.png")
                     :width 64 :height 64}])

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

(defn item-recommendation []
  [:div.media-list
   (map item-component
        (range)
        (:recommended @app))])

(defn objective-checkbox [name objective]
  (let [id (str (gensym))
        toggle (fn [event]
            (swap! app update-in [:props]
                   (if (-> event .-target .-checked)
                     #(assoc % name objective)
                     #(dissoc % name))))]
    [:div.row
     [:div.col-xs-1 [:input {:type :checkbox
                             :checked (contains? (:props @app) name) 
                             :on-change toggle :id id}]]
     [:label {:for id} name]]))

(defn number-selector [label key max min]
  [:div.row
   [:div.col-xs-3 label]
   [:div.col-xs-9
    [:input {:type "number"
             :max max :min min
             :value (key @app)
             :on-change #(swap! app assoc key (int (-> % .-target .-value)))}]]])

(def stats
  "A vector of maps
  :name - The user friendly name of the stat
  :calc - A function that takes a champion, champion level, and item
  set, and returns the raw value of that stat.
  :pretty - A function that takes the raw value of a stat and returns
  a user friendly value (i.e. adds a % sign if appropriate). Optional,
  default is the \"int\" function."
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
    :optimizable true}

   {:name "Life steal"
    :calc (algo/item-wrapper algo/life-steal)
    :pretty (fn [n] (str (int (* 100 n)) "%"))}
   {:name "Life stolen per second"
    :calc algo/build-lsps
    :optimizable true}

   {:name "Ability power (AP)"
    :calc (algo/item-wrapper algo/ability-power)
    :optimizable true}

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
    :optimizable true}
   {:name "Mana regen over 5 seconds"
    :calc algo/mana-regen
    :optimizable true}])

(defn build-stats []
  (let [{:keys [recommended champ champ-level]} @app]
    [:div.panel.panel-primary
     [:div.panel-heading "Build statistics"]
     (into [:table.table
            [:tr [:th "Stat"] [:th "Value"]]]
           (for [{:keys [name calc pretty] :as stat} stats
                 :let [stat-optimized? (contains? (:props @app) name)]]
             [(if stat-optimized?
                :tr.info
                :tr)
              [:td (cond-> name
                     stat-optimized? (as-> el [:b el]))]
              [:td (cond-> (calc champ champ-level recommended)
                     (not pretty) int
                     pretty pretty
                     stat-optimized? (as-> el [:b el]))]]))]))

(defn item-block [id block all-items]
  (let [items (map #(get all-items (int (:id %))) (:items block))]
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
         (item-block id block items))]]))

(defn dorans-button []
  [:a.btn.btn-danger.btn-xl {:on-click troll} "Troll"])

(defn needlessly-large-button []
  [:a.btn.btn-default.btn-xl {:href (set-url) :download "build.json"} "Download"])

(defn mirage-button []
  [:a.btn.btn-default.btn-xl {:on-click add-block} "Add to set"])

(defn button-of-command []
  [:a.btn.btn-primary.btn-xl {:on-click recommend} "Generate build"])

(defn app-component []
  [:div.container
   [:div.row
    [:h1.col-sm-6 (:text @app)]
    [:div.col-sm-6
      [:br]
      [:a {:href "https://github.com/pepijndevos/LolItemSets#usage"}
       [:span.glyphicon.glyphicon-question-sign] " About"]]]
   [:div.row
     [:div.col-sm-6
      [champion-select]
      [:h4 "Objectives "
       [:a.glyphicon.glyphicon-question-sign
        {:href "https://github.com/pepijndevos/LolItemSets#objectives"}]]
      (into [:div] (for [{:keys [name calc optimizable] :as stat} stats
                         :when optimizable]
                     [objective-checkbox name calc]))
      [number-selector "Champion level" :champ-level 18 1]
      [number-selector "Number of items" :num-items 6 1] [:br]
      [:div.btn-group.btn-group-justified
        [button-of-command] ; generate
        [dorans-button] ; troll
        [mirage-button] ; add
        [needlessly-large-button]] ; download
      [:p 
       "Save inside your League of Legends folder in "
       [:code
        "Config/Champions/" 
        (:id (:champ @app))
        "/Recommended/" 
        (:title (:itemset @app))
        ".json"]]
      [item-set]]
     [:div.col-sm-6
      [build-stats]
      [item-recommendation]]]])

(reagent/render-component [app-component]
                          (. js/document (getElementById "app")))


(defn on-js-reload [])
