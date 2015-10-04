(ns lolitemsets.algo
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [cljs.core.async :refer [put! chan <! close!]]))

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

(defmulti build-stat (fn build-stat* [base-name lvl-name item-flat item-percent champ level build] [(:id champ) base-name]))

(defmethod build-stat :default [base-name lvl-name item-flat item-percent champ level build]
  (* (+ (champ-stat champ level base-name lvl-name)
        (items-stat item-flat build))
     (+ 1 (items-stat item-percent build))))

(def build-stat* (get-method build-stat :default))

(def attack-damage (partial build-stat :attackdamage :attackdamageperlevel :FlatPhysicalDamageMod :PercentPhysicalDamageMod))
(def critical-strike (partial build-stat :crit :critperlevel :FlatCritChanceMod :PercentCritChanceMod))
(def armor (partial build-stat :armor :armorperlevel :FlatArmorMod :PercentArmorMod))
(def magic-resist (partial build-stat :spellblock :spellblockperlevel :FlatSpellBlockMod :PercentSpellBlockMod))
(def health (partial build-stat :hp :hpperlevel :FlatHPPoolMod :PercentHPPoolMod))
(def mana (partial build-stat :mp :mpperlevel :FlatMPPoolMod :PercentMPPoolMod))
(def mana-regen (partial build-stat :mpregen :mpregenperlevel :FlatMPRegenMod :PercentMPRegenMod))
; Slightly wrong. Percentages are multiplicative.
(def move-speed* (partial build-stat :movespeed nil :FlatMovementSpeedMod :PercentMovementSpeedMod))

(defn item-wrapper [stat]
  (fn [champ level build] (stat build)))

(def life-steal (partial items-stat :PercentLifeStealMod))
(def ability-power (partial items-stat :FlatMagicDamageMod))

(defn one-boot [build]
  (let [split (juxt filter remove)
        [boots other] (split #(some #{"Boots"} (:tags %)) build)]
    (if (seq boots)
      (cons (first boots) other)
      build)))

(defn move-speed [champ level build]
  (move-speed* champ level (one-boot build)))

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

; This produces terrible results
; We should do gold *efficiency*
(defn cost [build]
  (- (apply + (map #(get-in % [:gold :total] 0) build))))

(defn dps [ad as crit]
  (let [as (min as 2.5)
        crit (min crit 1)]
    (+ (* ad 2 crit as) (* ad as))))

(defn build-dps [champ level build] ; AD per second
  (dps
    (attack-damage champ level build)
    (attack-speed champ level build)
    (critical-strike champ level build)))

; mana plus mana regen over session-lenght
; session-regen being the average time between recalls
; (and thus mana refills)
(defn disposable-mana [session-lenght champ level build]
  (+ (mana champ level build)
     (* (mana-regen champ level build)
        12 session-lenght)))

; Basic abilities cannot be ranked higher than
; half the level of the champion (rounded up)
(defn ability-level [level]
  (min 5 (.ceil js/Math (/ level 2))))

; Champion-specific poke damage per minute
(defmulti poke (fn [champ level build] (:id champ)))

(defmethod poke :default [champ level build] 0)

; Champion-specific burst damage
(defmulti burst (fn [champ level build] (:id champ)))

(defmethod burst :default [champ level build] 0)

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
    (.setTimeout js/window run 0 ch (drop 100 other)))
  ch))

(defn recommend [items num-items champ level props]
  (let [energy (map #(partial % champ level) props)]
    (run (steps (rand-build num-items items)
                energy
                (partial swap-item items)
                10 0.001))))
