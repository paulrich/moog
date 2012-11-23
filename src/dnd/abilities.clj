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

(defmacro table-1a [[[key param]] pred & body]
  `(fn [~param] (condp ~pred (or (~key ~param) ~param) ~@body)))

(defmacro table-2 [[[key1 param1] [key2 param2]] [col-pred & col-keys] row-pred & table-body]
  `(fn [~param1 ~param2]
     (let [row-data# (condp ~row-pred (~key1 ~param1) ~@table-body)
           expr# (~key2 ~param2)]
       (row-data# ((zipmap '~col-keys (range))
                   (first (filter #(~col-pred % expr#) '~col-keys)))))))

(defmacro table-2a [[[key1 param1] [key2 param2]] [col-pred & col-keys] row-pred & table-body]
  `(fn [~param1] (condp ~row-pred (~key1 ~param1) ~@table-body)))

(defmacro table-2b [[kp1 [key2 param2]] [col-pred & col-keys] row-pred & table-body]
  `(table-1a (~kp1) ~row-pred ~@table-body))

(defn lte [x y]
  (let [lt (fnil > -1 Integer/MAX_VALUE)]
   (or (= x y) (lt x y))))

                                        ;lte nil 3  false
            ;lte 3 nil  false (this one might not matter; depends on order of rows in table)
                                        ;lte nil nil true
                                        ;lte 3 3 true
                                        ;lte 4 3 true

(defmacro table-a [[& params] row-key pred & body]
  (let [param-list (vec (repeatedly (count params) gensym))]
    `(fn ~param-list (condp #(~pred %2 %1) (apply ~key ~param-list) ~@body))))

   ; bad keys: k u 9 0 c maybe f maybe j 5 is a little weird
