(ns dnd.character_test
  (:use midje.sweet
        (dnd character abilities)))

(def joe (zipmap abilities (repeat 10)))

(fact (->> {:strength 5} (merge joe) character valid-classes) => #{:magic-user})

(fact (minimum-abilities :ranger) => {strength 13 intelligence 13 wisdom 14 dexterity 6 constitution 14 charisma 6})
