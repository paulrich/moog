(ns dnd.character
  (:use util.tables
        dnd.abilities
        clojure.set))

(defn character [abilities]
  (fn self [attribute]
    (cond
     (#{:eot} attribute) self
     (fn? attribute) (attribute abilities)
     :else (abilities attribute))))

(defn character-class [character]
  (if-let [char-class (character :class)]
    char-class
    (let [possible-classes (fn [[_ restriction]]
                             (restriction character))]
      (apply intersection (map possible-classes class-restrictions)))))
