(ns lolitemsets.algo
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [cljs.core.async :refer [put! chan <! close!]]
    [clojure.browser.net :as net]
    [clojure.browser.event :as event]))


(defn resource [fname]
  (let [xhr (net/xhr-connection)
        state (chan)]
    (event/listen xhr :complete
      #(put! state (js->clj
                       (.getResponseJson (.-target %))
                       :keywordize-keys true)))
    (net/transmit xhr (str "http://ddragon.leagueoflegends.com/cdn/5.2.1/data/en_US/" fname))
    state))

(defn item-chan []
  (go
    (let [items (vals (:data (<! (resource "item.json"))))]
      (filter
        (fn [item]
          (and
            (not (seq (:into item)))
            (:1 (:maps item) true) ; Summoner's Rift only.
            (:purchasable item true)))
        items))))

(defn champ-chan []
  (go (:data (<! (resource "champion.json")))))

(def max-level 18)
(def max-items 5) ; + boots

(defn dps [ad as crit]
  (let [as (min as 2.5)
        crit (min crit 1)]
    (+ (* ad 2 crit as) (* ad as))))

; Ideally we would use the actual skill of a champ
; typical poke skill:
; 100 mana, 250 + 70% AP
; AP for mana per minute
(defn poke [ap mpm]
  (let [apm (/ (+ 250 (* 0.7 ap)) 100)]
    (* apm mpm)))

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

(defn champ-poke [champ ap mpregen]
  (let [;base-mana (max-champ-stat champ :mp :mpperlevel)
        base-mpregen (max-champ-stat champ :mpregen :mpregenperlevel) ; per 5 seconds
        mpm (* (+ base-mpregen mpregen) 12)]
    (poke ap mpm)))

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

(defn rand-build [items]
  (vec (take max-items (repeatedly #(rand-nth items)))))

(defn swap-item [items build]
  (let [item (rand-nth items)]
    (if (some #{item} build)
      (recur items build)
      (assoc build (rand-int max-items) item))))

(defn item-ad [item] (get-in item [:stats :FlatPhysicalDamageMod] 0))
(defn item-crit [item] (get-in item [:stats :FlatCritChanceMod] 0))
(defn item-as [item] (get-in item [:stats :PercentAttackSpeedMod] 0))
(defn item-armor [item] (get-in item [:stats :FlatArmorMod] 0))
(defn item-mr [item] (get-in item [:stats :FlatSpellBlockMod] 0))
(defn item-hp [item] (get-in item [:stats :FlatHPPoolMod] 0))
(defn item-ls [item] (get-in item [:stats :PercentLifeStealMod] 0))
(defn item-ap [item] (get-in item [:stats :FlatMagicDamageMod] 0))
(defn item-mpregen [item] (get-in item [:stats :FlatMPRegenMod] 0))
(defn item-mpregen% [item] (get-in item [:stats :FlatMPRegenMod] 0))

(defn build-prop [prop build]
  (apply + (map prop build)))

(defn build-dps [champ build] ; AD per second
  (champ-dps
    champ
    (build-prop item-ad build)
    (build-prop item-as build)
    (build-prop item-crit build)))

(defn build-poke [champ build] ; AP output per minute
  (champ-poke
    champ
    (build-prop item-ap build)
    (build-prop item-mpregen build)))

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

; Multiobjective Simulated Annealing: A Comparative Study to Evolutionary Algorithms
; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.74.2194&rep=rep1&type=pdf
(defn steps [S0 energy perturb T dT]
  (letfn [(dominates [i j] (every? identity (map #(<= (% i) (% j)) energy)))
          (step [[S T]]
            (let [S2  (perturb S)
                  Sn  (if (or (dominates S S2) (> (Math/exp (/ -2 T)) (rand))) S2 S)]
              [Sn (* T (- 1.0 dT))]))]
    (iterate step [S0 T])))

(defn print-results [champ build]
  (println "Items:" (map :name build))
  (println "AD/s:" (build-dps champ build))
  (println "AP/m:" (build-poke champ build))
  (println "life steal/s:" (build-lsps champ build))
  (println "Effective life (Armor):" (build-hp-ad champ build))
  (println "Effective life (MR):" (build-hp-ap champ build))
  (println "life steal:" (* 100 (build-prop item-ls build)))
  (println "armor:" (champ-armor champ (build-prop item-armor build)))
  (println "attack damage:" (champ-ad champ (build-prop item-ad build)))
  (println "attack speed:" (+ (max-champ-as champ) (build-prop item-as build)))
  (println "crit:" (* 100 (build-prop item-crit build))))

(defn run
 ([s] (run (chan) s))
 ([ch [[v t] & other]]
  (put! ch v)
  (if (< t 0.01)
    (close! ch)
    (.setTimeout js/window run 0 ch (drop 100 other)))
  ch))

(defn recommend [items champ props]
  (let [energy (map #(partial % champ) props)]
    (run (steps (rand-build items)
                energy
                (partial swap-item items)
                10 0.001))))
