(ns dnd.character
  (:use [util tables common]
        dnd.abilities
        clojure.set)
  (:refer-clojure :exclude (class)))

(defn character [abilities]
  (fn self [attribute]
    (cond
     (#{:eot} attribute) self
     (fn? attribute) (attribute abilities)
     :else (abilities attribute))))

(defn valid-classes [character]
  (let [possible-classes (fn [[_ restriction]]
                           (restriction character))]
    (apply intersection (map possible-classes class-restrictions))))

(defn minimum-abilities [class]
  (reduce conj {} (map (fn [[ability restriction]]
          [ability (some (predentity #((restriction {ability %}) class)) (range 18))])
                       class-restrictions)))

(defn sort-ability-requirements [class]
  (let [minimums (minimum-abilities class)
        minimum (apply min (vals minimums))]
    (keys (filter (fn [[_ value]] (= minimum value)) minimums))
    (vals (apply sorted-map (map-to-seq (group-by val minimums))))))
