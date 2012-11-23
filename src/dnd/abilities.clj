(ns dnd.attributes
  (:use dnd.dice))

(def attributes [:str
                 :int 
                 :wis 
                 :dex 
                 :con 
                 :cha])

(defn roll-attribute [] #(roll 3 d 6))

(defn assign-attributes [dice-roll]
  (reduce #(assoc %1 %2 (dice-roll)) {} attributes))

(defn method-1 []
  (let [roll #(apply + (rest (sort (roll-dice 4 d 6))))]
    (repeatedly (count attributes) roll)))

(defn method-2 []
  (let [number-of-rolls (* 2 (count attributes))
        dice-rolls (repeatedly number-of-rolls roll-attribute)]
    (drop 6 (sort dice-rolls))))

(defn method-3 []
  (let [roll #(apply max (repeatedly 6 roll-attribute))]
    (assign-attributes roll)))

(defn method-4 []
  (let [create-character #(assign-attributes roll-attribute)]
    (repeatedly 12 create-character)))
