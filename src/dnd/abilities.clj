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

(def args (list [ 5 :only :magic-user]
       [ 9 :fighter]
       [ 12 :assassin :paladin]
       [ 13 :ranger]
       [ 15 :monk]
       [ 18 :only :fighter]))

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

(defmacro add-self [& attributes]
  (let [defs (map (fn [attr-name]
                    `(defn ~attr-name [char# amt#] (assoc char# (keyword '~attr-name) amt#)))
                  attributes)]
    `(do ~@defs)))

(defmacro table [& [ability & table-data :as args]]
  (let [ability (if (symbol? ability) (keyword ability))
        body (table-resolve (if ability table-data args))]
    `(fn [char#]
       (condp (table-pred char#)
           (table-expr ~ability char#) ~@body))))

(defn table-expr [ability val]
  (if ability (val ability) val))

(defn table-pred [char]
  (fn [& [table-expr char-val :as args]]
    (cond (fn? table-expr) (table-expr char-val)
          (symbol? table-expr) (or (table-expr char) ((keyword table-expr) char))
          :else ((if (every? number? args) <= =) char-val table-expr))))

(defn table-resolve [body]
  (map #(if (and (symbol? %) (not (resolve %))) `(quote ~%) %) body))

(def bend-bars (table str
        7 0
        9 1
        11 2
        13 4
        15 7
        16 10
        17 13
        18 16
        exceptional :>> (table
                            50 20
                            75 25
                            90 30
                            99 35
                            100 40)
        :no-value
        ))

(table
 [spell-lvl]
 [lvl 1  2 3 4 5 6 7] 
 1   1
 2   2
 3  2 1
 4 3 2
 5 3 3 1 


 (table  [(func1 param1) (func2 param2) (func3 param1)] ) => (fn [param1 param2])
 (table [(func1 literal)]) => (fn) #_ the function is only parameterized if the params are symbols

num-funcs = axes in table
num-params = arity of resulting function

apply args to funcs => yields lookup value

(table ... body)
body is
- 1d table (pred expr value expr value default)
2d table (1d-table #_ header: key to which column - yields the expr to match the 2nd axis against
                   pred
             row => (expr val val val val))
 )

(table #_languages [(:int character)] <=
       7 0
       9 1
       11 2
       13 3
       15 4
       16 5
       17 6
       18 7)

(table #_ [(level character) (level spell)]
       (= 1 2 3 4 5 6 7 8)
       <=
       (1 1 nil nil...) ; maybe just (1 1)
       (2 2 1 1)
       ...)
way to get whole row? varargs... if one arg, match it and return matching row
threading macro (->) to apply each arg to values! yay

(defmacro table-1 [[[key param]] pred & body]
  `(fn [~param] (condp ~pred (~key ~param) ~@body)))

; with non-map params
(defmacro table-1a [[[key param]] pred & body]
  `(fn [~param] (condp ~pred (or (~key ~param) ~param) ~@body)))

(defmacro table-2 [[[key1 param1] [key2 param2]] [col-pred & col-keys] row-pred & table-body]
  `(fn [~param1 ~param2]
     (let [row-data# (condp ~row-pred (~key1 ~param1) ~@table-body)
           expr# (~key2 ~param2)]
       (row-data# ((zipmap '~col-keys (range))
                   (first (filter #(~col-pred % expr#) '~col-keys)))))))

; returns row
(defmacro table-2a [[[key1 param1] [key2 param2]] [col-pred & col-keys] row-pred & table-body]
  `(fn [~param1] (condp ~row-pred (~key1 ~param1) ~@table-body)))

; calls other macro
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

                                        ; table-2 collapse args if using same param?
                                        ; table-n try to make one macro to rules them all

(defmacro table-a [[& params] row-key pred & body]
  (let [param-list (vec (repeatedly (count params) gensym))]
    `(fn ~param-list (condp #(~pred %2 %1) (apply ~key ~param-list) ~@body))))

(table strength-classes ...)

(character key val key val (strength 10))
inserts itself
inserts bend-bars
if class, validates, else tampers with possible-classes

(ability strength [implicit self] <ability>-classes bend-bars open-door etc)

; bad keys: k u 9 0 c maybe f maybe j 5 is a little weird