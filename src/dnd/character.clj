(ns dnd.character
  (:use [util tables common]
        [dnd classes abilities]
        clojure.set)
  (:refer-clojure :exclude (class)))

(def errors :errors)

(defn- missing-abilities [character]
  (seq (filter #(not (contains? character %)) abilities)))

(defn validate-abilities [character]
  (let [missing (missing-abilities character)]
    (if missing
      (merge-with concat character {errors missing})
      character)))

(defn validate-race [character] character)

(defn valid-classes [character]
  (let [possible-classes (fn [[_ restriction]]
                           (restriction character))]
    (apply intersection (map possible-classes class-restrictions))))

(defn minimum-abilities [class]
  (reduce conj {} (map (fn [[ability restriction]]
                         [ability (first-match #((restriction {ability %}) class) (range 18))])
                       class-restrictions)))

(defn validate-class [character]
  (let [c (character class)
        minimum-abilities (minimum-abilities c)
        valid-classes (valid-classes character)]
    (cond
     (not c) (merge-with concat character {errors [class] :possible-classes valid-classes})
     (missing-abilities character) (merge character {:minimum-abilities minimum-abilities})
     (not (valid-classes c)) (merge-with conj character {errors :invalid-class :minimum-abilities minimum-abilities})
     :default character)))

(defmacro character [& attributes]
  (concat `(-> {})
   (map #(cons `assoc %) attributes)
   `(validate-abilities validate-race validate-class)))

;; (defn sort-ability-requirements [class]
;;   (let [minimums (minimum-abilities class)]
;;     (vals (apply sorted-map (map-to-seq (group-by val minimums))))))
