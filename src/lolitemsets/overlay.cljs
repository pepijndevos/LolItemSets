(ns lolitemsets.overlay
  (:require
    [lolitemsets.algo :refer [build-stat build-stat* mana]]))

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

; Singed gains 25% mana as health
(defmethod build-stat ["Singed" :hp] [base-name lvl-name item-flat item-percent champ level build]
  (+ (build-stat* base-name lvl-name item-flat item-percent champ level build)
     (* 0.25 (mana champ level build))))
