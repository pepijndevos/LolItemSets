(ns lolitemsets.data
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [cljs.core.async :refer [put! chan <! close!]]
    [clojure.browser.net :as net]
    [clojure.browser.event :as event]))

(defn item-data-url []
  "http://ddragon.leagueoflegends.com/cdn/5.2.1/data/en_US/item.json")

(defn champ-data-url []
  "http://ddragon.leagueoflegends.com/cdn/5.2.1/data/en_US/champion.json")

(defn champ-img-square-url
  [champion-name]
  (str "http://ddragon.leagueoflegends.com/cdn/5.2.1/img/champion/"
       champion-name
       ".png"))

(defn champ-img-loading-url
  [champion-name & [skin-number]]
  (str "http://ddragon.leagueoflegends.com/cdn/img/champion/loading/"
       champion-name "_" (or skin-number 0)
       ".jpg"))

(defn champ-img-splash-url
  [champion-name & [skin-number]]
  (str "http://ddragon.leagueoflegends.com/cdn/img/champion/splash/"
       champion-name "_" (or skin-number 0)
       ".jpg"))

(defn item-img-url
  [item-image-ref]  ; items conveniently come with strings of its filename, e.g. "1001.png"
  (str "http://ddragon.leagueoflegends.com/cdn/5.2.1/img/item/"
       item-image-ref))

(defn resource [url]
  (let [xhr (net/xhr-connection)
        state (chan)]
    (event/listen xhr :complete
      #(put! state (js->clj
                       (.getResponseJson (.-target %))
                       :keywordize-keys true)))
    (net/transmit xhr url)
    state))

(defn item-chan []
  (go
    (let [items (:data (<! (resource (item-data-url))))]
      (->> items
        (map (fn [[id item]] (assoc item :id (int (name id)))))
        (filter
          (fn [item]
            (and
              (not (seq (:into item)))
              (:1 (:maps item) true) ; Summoner's Rift only.
              (:purchasable item true))))))))

(defn champ-chan []
  (go (:data (<! (resource (champ-data-url))))))
