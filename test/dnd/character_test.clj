(ns dnd.character_test
  (:refer-clojure :exclude (class))
  (:use midje.sweet
        (dnd character abilities classes races)))

(fact (->> {:strength 5} (merge (zipmap abilities (repeat 10))) valid-classes) => #{:magic-user})

(fact (minimum-abilities [:ranger]) => {strength 13 intelligence 13 wisdom 14 dexterity 6 constitution 14 charisma 6})

(fact (character (strength 17)) => (contains {strength 17}))

;; (fact (sort-ability-requirements :ranger) =>
;;       (contains (contains [charisma 6] [dexterity 6] :in-any-order) (contains [strength 13] [intelligence 13] :in-any-order)
;;                 (contains [wisdom 14] [constitution 14] :in-any-order)))

(fact (character (strength 17) (intelligence 17) (wisdom 17) (charisma 17) (constitution 17)) =>
      (contains {errors (contains :class :dexterity :in-any-order :gaps-ok)}))

(fact (-> {intelligence 9 class nil} validate-class validate-abilities) => (contains {errors (contains #{strength class})}))

(fact (-> abilities (zipmap (repeat 9)) (validate-class)) => (contains {:possible-classes #{:cleric :fighter :magic-user :thief}}))

(fact (character (class :paladin)) =>
      (contains {:minimum-abilities {strength 12 intelligence 9 wisdom 13 dexterity 6 constitution 9 charisma 17}}))

(fact (character (class :paladin)(strength 12) (intelligence 9) (wisdom 13) (dexterity 6) (constitution 9) (charisma 16))
      => (contains {errors (contains unqualified-for-class) :minimum-abilities anything}))

(fact (let [char
            (character (class :paladin) (strength 12) (intelligence 9) (wisdom 13) (dexterity 6) (constitution 9) (charisma 17))]
        char =not=> (contains {:minimum-abilities anything})
        char => (contains {errors (contains missing-race)})))

(fact (let [char
            (character (class thief) (race half-orc)
                       (intelligence 11) (dexterity 17) (constitution 6) (wisdom 6) (strength 12) (charisma 7))]
        char =not=> (contains {errors anything})
        char =not=> (contains {minimum-abilities anything})
        (max-level char (first (char class))) => 7
        (char strength) => 13
        (char class) => #{thief}))

(defn make-char [p-race p-abilities & classes]
  (->>
   (zipmap abilities p-abilities)
   (concat (list 'character [race p-race] (list* class classes)))
   eval))

(fact (-> (make-char human (repeat 18) paladin) class) => [paladin])
(fact (-> (make-char elf (repeat 17) fighter thief) class) => #{fighter thief})
(fact (-> (make-char elf (repeat 18) paladin)) => (contains {errors (contains invalid-race-for-class)}))