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

(defn rand-build [n items]
  (vec (take n (repeatedly #(rand-nth items)))))

(defn swap-item [items build]
  (let [item (rand-nth items)]
    (if (some #{item} build)
      (recur items build)
      (assoc build (rand-int (count build)) item))))

(defn items-stat [prop build]
  (apply + (map #(get-in % [:stats prop] 0) build)))

(defn champ-stat [champ level base-name lvl-name]
  (let [stats (:stats champ)
        base (get stats base-name 0)
        per-level (get stats lvl-name 0)]
    (+ base (* level per-level))))

(defn build-stat [base-name lvl-name item-flat item-percent champ level build]
  (* (+ (champ-stat champ level base-name lvl-name)
        (items-stat item-flat build))
     (+ 1 (items-stat item-percent build))))

(def attack-damage (partial build-stat :attackdamage :attackdamageperlevel :FlatPhysicalDamageMod :PercentPhysicalDamageMod))
(def critical-strike (partial build-stat :crit :critperlevel :FlatCritChanceMod :PercentCritChanceMod))
(def armor (partial build-stat :armor :armorperlevel :FlatArmorMod :PercentArmorMod))
(def magic-resist (partial build-stat :spellblock :spellblockperlevel :FlatSpellBlockMod :PercentSpellBlockMod))
(def health (partial build-stat :hp :hpperlevel :FlatHPPoolMod :PercentHPPoolMod))

(defn item-wrapper [stat]
  (fn [champ level build] (stat build)))

(def life-steal (partial items-stat :PercentLifeStealMod))
(def ability-power (partial items-stat :FlatMagicDamageMod))

; attack speed is a little weird
; 0.625
; -----
; 1+ofs
(defn champ-as [champ level]
  (let [stats (:stats champ)
        offset (:attackspeedoffset stats)
        per-level (:attackspeedperlevel stats)]
    (+ (/ 0.625 (+ 1 offset)) (* level per-level 0.01))))

(defn attack-speed [champ level build]
  (* (+ (champ-as champ level)
        (items-stat :FlatAttackSpeedMod build))
     (+ 1 (items-stat :PercentAttackSpeedMod build))))

(defn dps [ad as crit]
  (let [as (min as 2.5)
        crit (min crit 1)]
    (+ (* ad 2 crit as) (* ad as))))

(defn build-dps [champ level build] ; AD per second
  (dps
    (attack-damage champ level build)
    (attack-speed champ level build)
    (critical-strike champ level build)))

(comment
; Ideally we would use the actual skill of a champ
; typical poke skill:
; 100 mana, 250 + 70% AP
; AP for mana per minute
(defn poke [ap mpm]
  (let [apm (/ (+ 250 (* 0.7 ap)) 100)]
    (* apm mpm)))

(defn champ-poke [champ ap mpregen]
  (let [;base-mana (max-champ-stat champ :mp :mpperlevel)
        base-mpregen (max-champ-stat champ :mpregen :mpregenperlevel) ; per 5 seconds
        mpm (* (+ base-mpregen mpregen) 12)]
    (poke ap mpm)))

(defn build-poke [champ build] ; AP output per minute
  (champ-poke
    champ
    (build-prop item-ap build)
    (build-prop item-mpregen build)))
)

(defn effective-health [defence hp]
  (+ hp
     (* hp
        (/ defence
           100))))

(defn build-hp-ad [champ level build]
  (effective-health
    (armor champ level build)
    (health champ level build)))

(defn build-hp-ap [champ level build]
  (effective-health
    (magic-resist champ level build)
    (health champ level build)))

(defn build-lsps [champ level build] ; life steal per second
  (* (build-dps champ level build)
     (life-steal build)))

; Multiobjective Simulated Annealing: A Comparative Study to Evolutionary Algorithms
; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.74.2194&rep=rep1&type=pdf
(defn steps [S0 energy perturb T dT]
  (letfn [(dominates [i j] (every? identity (map #(<= (% i) (% j)) energy)))
          (step [[S T]]
            (let [S2  (perturb S)
                  Sn  (if (or (dominates S S2) (> (Math/exp (/ -2 T)) (rand))) S2 S)]
              [Sn (* T (- 1.0 dT))]))]
    (iterate step [S0 T])))

(defn run
 ([s] (run (chan) s))
 ([ch [[v t] & other]]
  (put! ch v)
  (if (< t 0.01)
    (close! ch)
    (.setTimeout js/window run 0 ch (drop 500 other)))
  ch))

(defn recommend [items champ props]
  (let [energy (map #(partial % champ 18) props)]
    (run (steps (rand-build 5 items)
                energy
                (partial swap-item items)
                10 0.001))))
