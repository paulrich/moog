(ns dnd.character_test
  (:refer-clojure :exclude (class))
  (:use midje.sweet
        (dnd character abilities classes)))

(fact (->> {:strength 5} (merge (zipmap abilities (repeat 10))) valid-classes) => #{:magic-user})

(fact (minimum-abilities :ranger) => {strength 13 intelligence 13 wisdom 14 dexterity 6 constitution 14 charisma 6})

(fact (character (strength 17)) => (contains {strength 17}))

;; (fact (sort-ability-requirements :ranger) =>
;;       (contains (contains [charisma 6] [dexterity 6] :in-any-order) (contains [strength 13] [intelligence 13] :in-any-order)
;;                 (contains [wisdom 14] [constitution 14] :in-any-order)))

(fact (character (strength 17) (intelligence 17) (wisdom 17) (charisma 17) (constitution 17)) =>
      (contains {errors (contains :class :dexterity :in-any-order)}))

(fact (-> {intelligence 9 class nil} validate-class validate-abilities) => (contains {errors (contains #{strength class})}))

(fact (-> abilities (zipmap (repeat 9)) (validate-class)) => (contains {:possible-classes #{:cleric :fighter :magic-user :thief}}))

(fact (character (class :paladin)) =>
      (contains {:minimum-abilities {strength 12 intelligence 9 wisdom 13 dexterity 6 constitution 9 charisma 17}}))

(fact (character (class :paladin)(strength 12) (intelligence 9) (wisdom 13) (dexterity 6) (constitution 9) (charisma 16))
      => (contains {errors :invalid-class :minimum-abilities anything}))

(fact (character (class :paladin) (strength 12) (intelligence 9) (wisdom 13) (dexterity 6) (constitution 9) (charisma 17))
      =not=> (contains {:minimum-abilities anything}))
