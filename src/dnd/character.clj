(ns dnd.character
  (:use [util tables common]
        dnd.abilities
        clojure.set)
  (:refer-clojure :exclude (class)))

(def class :class)
(def errors :errors)

(defn validate-abilities [character]
  (if-let [missing (filter #(not (contains? character %)) abilities)]
    (merge-with concat character {errors missing})
    character))

(defn validate-race [character] character)

(defn valid-classes [character]
  (let [possible-classes (fn [[_ restriction]]
                           (restriction character))]
    (apply intersection (map possible-classes class-restrictions))))

(defn validate-class [character]
  (if (not (character class))
    (merge-with concat character {errors [class] :possible-classes (valid-classes character)})
    character))

(defmacro character [& attributes]
  (concat `(-> {})
   (map #(cons `assoc %) attributes)
   `(validate-abilities validate-race validate-class)))

(defn minimum-abilities [class]
  (reduce conj {} (map (fn [[ability restriction]]
          [ability (some (predentity #((restriction {ability %}) class)) (range 18))])
                       class-restrictions)))

(defn sort-ability-requirements [class]
  (let [minimums (minimum-abilities class)]
    (vals (apply sorted-map (map-to-seq (group-by val minimums))))))
