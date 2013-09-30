(ns dnd.character
  (:use [util tables common]
        [dnd classes abilities races]
        clojure.set)
  (:refer-clojure :exclude (class)))

(defkeywords errors missing-race class unqualified-for-class invalid-race-for-class unqualified-for-race)

(defn- missing-abilities [character]
  (seq (filter #(not (contains? character %)) abilities)))

(defn validate-abilities [character]
  (let [missing (missing-abilities character)]
    (if missing
      (merge-with concat character {errors missing})
      character)))

(defn abilities-for-race [race])

(defn validate-race [character]
  (let [character-class (character class)
        class-coll #(assoc % class (if character-class [character-class]))]
    (if-let [race (character race)]
      (->
       (if (or (coll? character-class) (nil? character-class))
         character
         (class-coll character))
       (update-in [class] (if (= race human) vec set)))
      (class-coll (merge-with concat character {errors [missing-race]})))))

(defn valid-classes [character]
  (let [possible-classes (fn [[_ restriction]]
                           (restriction character))]
    (apply intersection (map possible-classes class-restrictions))))

(defn minimum-abilities [character]
  (let [classes (character class)]
   (->> class-restrictions
        (map (fn [[ability func]]
               [ability
                (first-match #(every? (func {ability %}) classes) (range 18))]))
        (into {}))))

(defn validate-class [character]
  (let [c (character class)
        minimum-abilities (minimum-abilities character)
        valid-classes (valid-classes character)]
    (cond
     (not c) (merge-with concat character {errors [class] :possible-classes valid-classes})
     (missing-abilities character) (merge character {:minimum-abilities minimum-abilities})
     (not-every? valid-classes c) (merge-with conj character {errors unqualified-for-class :minimum-abilities minimum-abilities})
     :default character)))

(defn add-attribute
  ([character key value]
     (assoc character key value))
  ([character key value & more]
     (assoc character key (list* value more))))

(defmacro character [& attributes]
  (concat `(-> {})
   (map #(cons `add-attribute %) attributes)
   `(validate-abilities validate-race validate-class)))

;; (defn sort-ability-requirements [class]
;;   (let [minimums (minimum-abilities class)]
;;     (vals (apply sorted-map (map-to-seq (group-by val minimums))))))
