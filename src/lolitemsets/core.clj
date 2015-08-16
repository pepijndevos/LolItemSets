(ns lolitemsets.core
  (:require
    [clojure.tools.cli :refer [parse-opts]]
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

(defn champ-hp-ad [champ armor hp]
  (let [base-armor (max-champ-stat champ :armor :armorperlevel)
        base-hp    (max-champ-stat champ :hp :hpperlevel)]
    (+ (+ base-hp hp)
       (* (+ base-hp hp)
          (/ (+ base-armor armor)
             100)))))

(defn champ-hp-ap [champ mr hp]
  (let [base-mr (max-champ-stat champ :spellblock :spellblockperlevel)
        base-hp (max-champ-stat champ :hp :hpperlevel)]
    (+ (+ base-hp hp)
       (* (+ base-hp hp)
          (/ (+ base-mr mr)
             100)))))

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
(defn item-mr [item] (get-in item [:stats :FlatSpellBlockMod] 0))
(defn item-hp [item] (get-in item [:stats :FlatHPPoolMod] 0))
(defn item-ls [item] (get-in item [:stats :PercentLifeStealMod] 0))

(defn build-prop [prop build]
  (apply + (map prop build)))

(defn build-dps [champ build]
  (champ-dps
    champ
    (build-prop item-ad build)
    (build-prop item-as build)
    (build-prop item-crit build)))

(defn build-hp-ad [champ build] ; effecive health
  (champ-hp-ad
    champ
    (build-prop item-armor build)
    (build-prop item-hp build)))

(defn build-hp-ap [champ build] ; effecive health
  (champ-hp-ap
    champ
    (build-prop item-mr build)
    (build-prop item-hp build)))

(defn build-lsps [champ build] ; life steal per second
  (* (build-dps champ build)
     (build-prop item-ls build)))

(defn weighted-sum [& fn-weight]
  (let [fns (take-nth 2 fn-weight)
        weights (take-nth 2 (rest fn-weight))
        f1 (apply juxt fns)
        f2 (fn [s] (apply + (map #(Math/sqrt %) (map * s weights))))]
    (comp f2 f1)))

; Multiobjective Simulated Annealing: A Comparative Study to Evolutionary Algorithms
; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.74.2194&rep=rep1&type=pdf
(defn steps [S0 energy perturb T dT]
  (letfn [(dominates [i j] (every? identity (map #(<= (% i) (% j)) energy)))
          (step [[S T]]
            (let [S2  (perturb S)
                  Sn  (if (or (dominates S S2) (> (Math/exp (/ -10 T)) (rand))) S2 S)]
              [Sn (* T (- 1.0 dT))]))]
    (iterate step [S0 T])))

(defn list-champs []
  (dorun (map-indexed #(println %1 (:name %2)) champs)))

(defn print-results [champ build]
  (println "Items:" (map :name build))
  (println "AD/s:" (build-dps champ build))
  (println "life steal/s:" (build-lsps champ build))
  (println "Effective life (Armor):" (build-hp-ad champ build))
  (println "Effective life (MR):" (build-hp-ap champ build))
  (println "life steal:" (* 100 (build-prop item-ls build)))
  (println "armor:" (champ-armor champ (build-prop item-armor build)))
  (println "attack damage:" (champ-ad champ (build-prop item-ad build)))
  (println "attack speed:" (+ (max-champ-as champ) (build-prop item-as build)))
  (println "crit:" (* 100 (build-prop item-crit build))))

(defn run [s]
  (ffirst (drop-while (fn [[_ t]] (> t 0.1)) s)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [opts (parse-opts args
               [[nil "--attack-damage"]
                [nil "--ability-power"]
                [nil "--life-steal"]
                [nil "--armor"]
                [nil "--magic-resist"]
                [nil "--list"]])
        chid (Integer. (or (first (:arguments opts)) 0))
        champ (nth champs chid)
        energy (map
                 #(partial % champ)
                 (remove nil?
                   [(when (:attack-damage (:options opts)) build-dps)
                    (when (:life-steal    (:options opts)) build-lsps)
                    (when (:armor         (:options opts)) build-hp-ad)
                    (when (:magic-resist  (:options opts)) build-hp-ap)
                    ]))]
    (if (:list (:options opts))
      (list-champs)
      (print-results champ
        (run (steps (rand-build) energy swap-item 10 0.001))))))
