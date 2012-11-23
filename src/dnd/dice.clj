(ns dnd.dice)

(defn random
  ([sides] (inc (rand-int sides)))
  ([from to] (let [augend (dec from)
                   sides (- to augend)]
               (+ augend (random sides)))))

(defn d [sides] #(random sides))

(defn roll-dice [number dice-fn sides]
  (repeatedly number (dice-fn sides)))

(defn roll [number dice-fn sides]
  (apply + (roll-dice number dice-fn sides)))
