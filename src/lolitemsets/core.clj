(ns lolitemsets.core
  (:require
    [cheshire.core :as json]
    [clojure.java.io :as io])
  (:gen-class))

(defn resource [fname]
  (let [stream (io/reader (io/resource fname))
        data (json/parse-stream stream true)]
    (->> data :data vals)))

(def items (filter #(empty? (:into %))
  (resource "item.json")))
(def champs (resource "champion.json"))

(def max-level 18)
(def max-items 5) ; + boots

(def greaves (nth champs 5)) ; test

(defn dps [ad as crit]
  (let [as (min as 2.5)
        crit (min crit 1)]
    (+ (* ad 2 crit as) (* ad as))))

(defn max-champ-stat [champ base-name lvl-name]
  (let [stats (:stats champ)
        base (get stats base-name)
        level (get stats lvl-name)]
    (+ base (* max-level level))))

(defn max-champ-as [champ]
  (let [stats (:stats champ)
        offset (:attackspeedoffset stats)
        level (:attackspeedperlevel stats)]
    (+ (/ 0.625 (+ 1 offset)) (* max-level level 0.01))))

(defn champ-dps [champ ad as crit]
  (let [base-ad (max-champ-stat champ :attackdamage :attackdamageperlevel)
        base-crit (/ (max-champ-stat champ :crit :critperlevel) 100) ; always 0?
        base-as (max-champ-as champ)]
    (dps
      (+ base-ad ad)
      (+ base-as as)
      (+ base-crit crit))))

(defn champ-armor [champ armor]
  (let [base-armor (max-champ-stat champ :armor :armorperlevel)]
    (+ base-armor armor)))

(defn champ-ad [champ ad]
  (let [base-ad (max-champ-stat champ :attackdamage :attackdamageperlevel)]
    (+ base-ad ad)))

(defn rand-build []
  (vec (take max-items (repeatedly #(rand-nth items)))))

(defn swap-item [build]
  (assoc build (rand-int max-items) (rand-nth items)))

(defn item-ad [item] (get-in item [:stats :FlatPhysicalDamageMod] 0))
(defn item-crit [item] (get-in item [:stats :FlatCritChanceMod] 0))
(defn item-as [item] (get-in item [:stats :PercentAttackSpeedMod] 0))
(defn item-armor [item] (get-in item [:stats :FlatArmorMod] 0))
(defn item-ls [item] (get-in item [:stats :PercentLifeStealMod] 0))

(defn build-prop [prop build]
  (apply + (map prop build)))

(defn build-dps [champ build]
  (champ-dps
    champ
    (build-prop item-ad build)
    (build-prop item-as build)
    (build-prop item-crit build)))

(defn build-armor [champ build]
  (champ-armor champ (build-prop item-armor build)))

(defn build-lsps [champ build] ; life steal per second
  (* (build-dps champ build)
     (build-prop item-ls build)))

(defn weighted-sum [& fn-weight]
  (let [fns (take-nth 2 fn-weight)
        weights (take-nth 2 (rest fn-weight))
        f1 (apply juxt fns)
        f2 #(map * % weights)
        f3 #(apply + %)]
    (comp f3 f2 f1)))

(defn steps [S0 energy perturb T dT]
  (letfn [(step [[S E T]]
            (let [S2     (perturb S)
                  E2     (energy S2)
                  [S E]  (if (and E2 (> (Math/exp (/  (- E2 E) T)) (rand)))
                               [S2 E2] [S E])]
              [S E (* T (- 1.0 dT))]))]
    (iterate step [S0 (energy S0) T])))

(defn list-champs []
  (dorun (map-indexed #(println %1 (:name %2)) champs)))

(defn print-results [champ [build & _]]
  (println "Items:" (map :name build))
  (println "DPS:" (build-dps champ build))
  (println "life steal:" (* 100 (build-prop item-ls build)))
  (println "armor:" (build-armor champ build))
  (println "attack damage:" (champ-ad champ (build-prop item-ad build)))
  (println "attack speed:" (+ (max-champ-as champ) (build-prop item-as build)))
  (println "crit:" (* 100 (build-prop item-crit build))))

(defn -main
  "I don't do a whole lot ... yet."
  [champ dps lsps armor]
  (let [champ (nth champs (Integer. champ))
        dps-weight (Float. dps)
        lsps-weight (Float. lsps)
        armor-weight (Float. armor)
        energy (weighted-sum build-dps dps-weight
                             build-lsps lsps-weight
                             build-armor armor-weight)]
    (print-results champ
      (nth (steps (rand-build)
                  (partial energy champ)
                  swap-item 1000 0.0001)
           50000))))
