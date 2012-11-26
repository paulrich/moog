(ns dnd.abilities
  (:use dnd.dice)
  (:require [clojure.set :refer [union]])
  )

(def abilities [:str
                 :int 
                 :wis 
                 :dex 
                 :con 
                 :cha])

(defn roll-ability [] #(roll 3 d 6))

(defn assign-abilities [dice-roll]
  (reduce #(assoc %1 %2 (dice-roll)) {} abilities))

(defn method-1 []
  (let [roll #(apply + (rest (sort (roll-dice 4 d 6))))]
    (repeatedly (count abilities) roll)))

(defn method-2 []
  (let [number-of-rolls (* 2 (count abilities))
        dice-rolls (repeatedly number-of-rolls roll-ability)]
    (drop 6 (sort dice-rolls))))

(defn method-3 []
  (let [roll #(apply max (repeatedly 6 roll-ability))]
    (assign-abilities roll)))

(defn method-4 []
  (let [create-character #(assign-abilities roll-ability)]
    (repeatedly 12 create-character)))

(def classes
  [:fighter
   :assassin
   :paladin
   :ranger
   :monk
   :magic-user
   :serf
   :thief
   ])

(defn unrestricted-classes [limits]
  (reduce (fn [unrestricted [limit & classes]]
            (apply disj unrestricted (if (= :only (first classes))
                                       (rest classes) classes)))
          (set classes) limits))

(defn parse-restrictions [[[lower-limit _ solo-class] & limits] value]
  (let [unrestricted (unrestricted-classes limits)]
    (if (<= value lower-limit) #{solo-class}
        (reduce
         (fn [valid-classes [_ & classes]]
           (if (= :only (first classes))
             (set (rest classes))
             (apply conj valid-classes classes)))
         unrestricted
         (take-while #(> value (first %)) limits)))))

(defn drink-potion [mod-attr effect duration char]
  (fn [attr]
    (cond
      (= attr mod-attr) (effect (char attr))
      (= attr :eot) (if (= duration 1) char (drink-potion mod-attr effect (dec duration) char))
      :hi! (char attr))))

   ; bad keys: k u 9 0 c maybe f maybe j 5 is a little weird 6 for sure
