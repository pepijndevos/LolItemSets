(ns lolitemsets.overlay
  (:require
    [lolitemsets.algo :as algo]))

(def item-overlay
  {3504 {:stats {:PercentMPRegenMod 1}} ;Ardent Censer
   3174 {:stats {:PercentMPRegenMod 1}} ;Athene's Unholy Grail
   3092 {:stats {:PercentMPRegenMod 0.5}} ;Frost Queen's Claim
   3222 {:stats {:PercentMPRegenMod 1}} ;Mikael's Crucible
   3165 {:stats {:PercentMPRegenMod 1}} ;Morellonomicon
   3069 {:stats {:PercentMPRegenMod 1 :FlatMovementSpeedMod 20}} ;Talisman of Ascension

   3003 {:stats {:PercentMPRegenMod 0.5 :FlatMPPoolMod 1000}} ;Archangel's Staff - stacked
   3004 {:stats {:PercentMPRegenMod 0.25 :FlatMPPoolMod 1000}} ;Manamune - stacked
   3027 {:stats {:FlatHPPoolMod 500.0 :FlatMPPoolMod 800.0 :FlatMagicDamageMod 80.0}} ;Rod of Ages - stacked

   3072 {:stats {:PercentLifeStealMod 0.2}} ;The Bloodthirster

   3190 {:stats {:FlatSpellBlockMod 35}} ;Locket of the Iron Solari
   3060 {:stats {:FlatSpellBlockMod 35}} ;Banner of Command

   ; Missing maps data from recent item data
   3452 {:maps {:12 false, :1 false, :10 false}},
   3073 {:maps {:12 false, :1 false, :10 false}},
   3181 {:maps {:1 false}},
   3454 {:maps {:12 false, :1 false, :10 false}},
   3084 {:maps {:12 false, :1 false}},
   2047 {:maps {:1 false}},
   1063 {:maps {:12 false, :1 false, :10 false}},
   1062 {:maps {:12 false, :1 false, :10 false}},
   3170 {:maps {:12 false, :1 false}},
   3043 {:maps {:12 false, :1 false, :10 false}},
   3029 {:maps {:12 false, :1 false, :10 false}},
   3420 {:maps {:1 false}},
   3048 {:maps {:12 false, :1 false, :10 false}},
   3007 {:maps {:12 false, :1 false, :10 false}},
   3187 {:maps {:1 false}},
   3422 {:maps {:1 false}},
   3104 {:maps {:12 false, :1 false}},
   3421 {:maps {:1 false}},
   1075 {:maps {:1 false, :10 false}},
   3137 {:maps {:12 false, :1 false}},
   3451 {:maps {:12 false, :1 false, :10 false}},
   2051 {:maps {:1 false, :10 false}},
   1076 {:maps {:1 false, :10 false}},
   3450 {:maps {:12 false, :1 false, :10 false}},
   3184 {:maps {:1 false, :10 false}},
   3185 {:maps {:1 false}},
   1074 {:maps {:1 false, :10 false}},
   3418 {:maps {:1 false}},
   3417 {:maps {:1 false}},
   3419 {:maps {:1 false}},
   3180 {:maps {:12 false, :1 false, :10 false}},
   3460 {:maps {:12 false, :1 false, :10 false}},
   3290 {:maps {:12 false, :1 false}},
   3345 {:maps {:12 false, :1 false, :10 false}},
   3159 {:maps {:12 false, :1 false}},
   3090 {:maps {:12 false, :1 false}},
   3112 {:maps {:1 false, :10 false}},
   3188 {:maps {:12 false, :1 false, :10 false}},
   3122 {:maps {:1 false}},
   3008 {:maps {:12 false, :1 false, :10 false}},
   3453 {:maps {:12 false, :1 false, :10 false}}
   })

; Singed gains 25% mana as health
(defmethod algo/build-stat ["Singed" :hp] [base-name lvl-name item-flat item-percent champ level build]
  (+ (algo/build-stat* base-name lvl-name item-flat item-percent champ level build)
     (* 0.25 (algo/mana champ level build))))

; magic damage / second: 22 / 34 / 46 / 58 / 70 (+ 30% AP)
; mana per second: 13
; assuming 5 min laning sessions
(defmethod algo/poke "Singed" [champ level build]
  (let [mana-per-second 13
        session-lenght 5
        session-mana-cost (* session-lenght 60 mana-per-second)
        mana (algo/disposable-mana session-lenght champ level build)
        base-damage [0 22 34 57 58 70]
        damage (+ (nth base-damage (algo/ability-level level))
                  (* (algo/ability-power build) 0.3))]
    (* (min 1 (/ mana session-mana-cost))
       damage
       60)))

; Yasuo has double crit chance TODO: and 10% reduced crit damage
(defmethod algo/build-stat ["Yasuo" :crit] [base-name lvl-name item-flat item-percent champ level build]
  (* 2 (algo/build-stat* base-name lvl-name item-flat item-percent champ level build)))


