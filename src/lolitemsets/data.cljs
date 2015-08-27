(ns lolitemsets.data
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [lolitemsets.overlay :refer [item-overlay]]
    [cljs.core.async :refer [put! chan <! close!]]
    [cljs-http.client :as http]))

(defn item-data-url []
  "http://ddragon.leagueoflegends.com/cdn/5.16.1/data/en_US/item.json")

(defn champ-data-url []
  "http://ddragon.leagueoflegends.com/cdn/5.16.1/data/en_US/champion.json")

(defn gold-url []
  "http://ddragon.leagueoflegends.com/cdn/5.2.1/img/ui/gold.png")

(defn champ-img-square-url
  [champion-name]
  (str "http://ddragon.leagueoflegends.com/cdn/5.16.1/img/champion/"
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
  (str "http://ddragon.leagueoflegends.com/cdn/5.16.1/img/item/"
       item-image-ref))

(defn resource [url]
  (cljs-http.client/get url {:with-credentials? false
                             :channel (chan 1 (map :body))}))

(defn item-chan []
  (go
    (let [items (:data (<! (resource (item-data-url))))]
      (->> items
        (map (fn [[id item]] [(int (name id)) (assoc item :id (int (name id)))]))
        (map (fn [[id item]] [id (merge-with merge item (get item-overlay (:id item)))]))
        (filter
          (fn [[id item]]
            (and
              ;(not (seq (:into item)))
              ;(> (:depth item) 1)
              (:1 (:maps item) true) ; Summoner's Rift only.
              (:purchasable (:gold item) true)
              (not (:requiredChampion item)))))
        (into {})))))

(defn champ-chan []
  (go (:data (<! (resource (champ-data-url))))))
