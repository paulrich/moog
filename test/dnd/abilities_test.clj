(ns dnd.abilities-test
  (:use midje.sweet
        dnd.abilities))

(fact (hit-probability {:strength 3}) => -3)
(fact (hit-probability {:strength 4}) => -2)
(fact (hit-probability {:strength 7 :exceptional-str 77}) => -1)
(fact (hit-probability {:strength 18 :exceptional-str 92}) => 2)

(fact (damage-adjustment {:strength 18}) => 2)
(fact (damage-adjustment {:strength 18 :exceptional-str 100}) => 6)

(fact (weight-allowance {:strength 14}) => 200)

(fact (open-doors {:strength 18}) => 3)

(fact (open-locked-door {:strength 18}) => 0)

(fact (bend-bars {:strength 18}) => 16)
(fact (bend-bars {:strength 12}) => 4)

(fact (languages {:intelligence 17}) => 6)

(fact (know-spell? {:intelligence 14}) => 55)

(fact (minimum-spells {:intelligence 17}) => 8)

(fact (maximum-spells {:intelligence 18}) => 18)
(fact (parse-restrictions [[5 :only :magic-user]
                           [12 :assassin :paladin]] 4) => #{:magic-user})

(fact (max-spell-level-cleric {:wisdom 17}) => 6)

(fact (max-spell-level-magic_user {:intelligence 9}) => 4)
