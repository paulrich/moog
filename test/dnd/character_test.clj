(ns dnd.character_test
  (:use midje.sweet
        dnd.character))

(def joe (zipmap dnd.abilities/abilities (repeat 10)))

(fact (->> {:strength 5} (merge joe) character character-class) => #{:magic-user})
