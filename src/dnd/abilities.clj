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

(defmacro table [& table-def]
  (let [{:keys [params body]} (apply parse-table {} table-def)]
    `(fn ~(vec (vals params)) ~body)))

(defn parse-table
  ([metadata [pred [entity key] & table-data]]
     (let [{:keys [params body]} (reduce process-subtables metadata table-data)
           params (merge {entity (gensym)} params)
           param (params entity)]
       {:params
        :body (
               `(condp ~pred (if (ifn? ~param) (~param ~key) ~param) ~@table-body))}))
  
  ([metadata [pred [entity key] col-data] table-rows]
     (let [{:keys [params body]} (parse-table metadata table-rows)
           params (merge {entity (gensym)} params)
           param (params entity)]
       {:params params
        :body `(let [expr# (if (ifn? ~param) (~param ~key) ~param)
                     column# (first (filter #(~pred % expr#) ~col-data))
                     index# ((zipmap ~col-data (range)) column#)]
                 (if (integer? index#) (~body index#) ~body))})))

(defn process-subtables [acc table-form]
  (if (and (seq? table-form) (= 'table (first table-form)))
    (apply parse-table acc (rest table-form))
    (merge-with conj-dl acc {:body table-form})))

3-d syntax
(table (lte (char int) 0 (table ...) 1 (table ...) 2 (table ...)))

(defn comp-maybe [f1 f2]
  (let [chain (comp f2 f1)]
    (fn [& args]
      (apply chain args))))

(defn chainable [f n]
  (fn [& args]
    (let [arg (nth args n)]
      (f arg))))

(defn drink-potion [mod-attr effect duration char]
  (fn [attr]
    (cond
      (= attr mod-attr) (effect (char attr))
      (= attr :eot) (if (= duration 1) char (drink-potion mod-attr effect (dec duration) char))
      :hi! (char attr))))

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

   ; bad keys: k u 9 0 c maybe f maybe j 5 is a little weird 6 for sure
