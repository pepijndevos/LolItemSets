(ns lolitemsets.data
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [cljs.core.async :refer [put! chan <! close!]]
    [cljs-http.client :as http]))

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
  (cljs-http.client/get url {:with-credentials? false
                             :channel (chan 1 (map :body))}))

(def item-overlay
  {3504 {:stats {:PercentMPRegenMod 1}} ;Ardent Censer
   3174 {:stats {:PercentMPRegenMod 1}} ;Athene's Unholy Grail
   3040 {:stats {:PercentMPRegenMod 0.5}} ;Seraph's Embrace
   3043 {:stats {:PercentMPRegenMod 0.25}} ;Muramana
   3092 {:stats {:PercentMPRegenMod 0.5}} ;Frost Queen's Claim
   3222 {:stats {:PercentMPRegenMod 1}} ;Mikael's Crucible
   3165 {:stats {:PercentMPRegenMod 1}} ;Morellonomicon
   3069 {:stats {:PercentMPRegenMod 1}} ;Talisman of Ascension

   3027 {:stats {:FlatHPPoolMod 500.0 :FlatMPPoolMod 800.0 :FlatMagicDamageMod 80.0}} ;Rod of Ages - stacked

   3072 {:stats {:PercentLifeStealMod 0.2}} ;The Bloodthirster

   3190 {:stats {:FlatSpellBlockMod 35}} ;Locket of the Iron Solari
   3060 {:stats {:FlatSpellBlockMod 35}} ;Banner of Command
   })

(defn item-chan []
  (go
    (let [items (:data (<! (resource (item-data-url))))]
      (->> items
        (map (fn [[id item]] (assoc item :id (int (name id)))))
        (map (fn [item] (merge-with merge item (get item-overlay (:id item)))))
        (filter
          (fn [item]
            (and
              (not (seq (:into item)))
              (:1 (:maps item) true) ; Summoner's Rift only.
              ;(:purchasable (:gold item) true)
              (not (:requiredChampion item)))))))))

(defn champ-chan []
  (go (:data (<! (resource (champ-data-url))))))
