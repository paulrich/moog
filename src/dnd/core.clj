(ns dnd.core
  (:require [serializable.fn :as s]))

(defmethod print-dup clojure.lang.PersistentQueue [q w]
  (print-method `(into clojure.lang.PersistentQueue/EMPTY '~(seq q)) w))

(defmethod print-method clojure.lang.PersistentQueue [q w]
  (print-method '<- w) (print-method (seq q) w) (print-method '-< w))

(def templates {})

(defn xp-for-level [level]
  (case (dec level)
    0 0
    1
    250 2
    950 3
    2250 4
    4750 5
    9500 6
    16000 7
    25000 8
    38000 9
    56000 10
    77000 11
    96000 12
    120000 13
    150000 14
    190000))

(defn modifier [score] (-> score (quot 2) (- 5)))

(defn average-roll [d] (-> d (/ 2) inc))

(defn proficiency [char]
  (-> char identity (+ 5) (quot 4)))

(defn level [character]
  (if-let [l (::levels character)]
    (apply + l) 1))

(defn hp
  "Pass the character (map) and this function returns the hit points.
  Returns the value of ::hp, if it is mapped; otherwise, it is calculated using average rolls"
  [{:keys [::hp ::classes ::levels ::hit-die ::constitution] :as character}]
  (if hp hp
      (let [first-class (first classes)
            class-hit-die (zipmap classes hit-die)
            class-levels (update-in (zipmap classes levels) [first-class] dec)]
        (apply + (* (modifier constitution) (level character))
               (class-hit-die first-class)
               (map #(* (average-roll (class-hit-die %)) (class-levels %)) classes)))))

(defn ac [character]
  (if-let [ac (::ac character)]
    ac
    10)) ;; todo

(defprotocol Templated
  (apply-template [template transform]))

(extend-protocol Templated
  nil
  (apply-template [_ transforms] transforms)
  java.util.Collection
  (apply-template [coll transform]
    (reduce #(apply-template %2 %1) transform coll))
  clojure.lang.Named
  (apply-template [template-key transform]
    {:pre [(template-key templates)]}
    (apply-template (templates template-key) transform))
  clojure.lang.Fn
  (apply-template [template-fn transform] 
    (template-fn transform)))

(defmacro deftemplate
  "The name will be the name of the template
  The rest should be like a defn, taking a character (map) as its single parameter, and returning the modified version of it.
  By default, this function goes into the \":main\" priority.
  Alternate syntax is instead of writing it as a function, just define it as a map, which will be merged-with conj:
  (defmacro foo :main #(do-somthing-with-character %) :persistent #(something-important %)})"
  [name & [args & body :as merge-map]]
  (let [missing-keys (zipmap (take-nth 2 merge-map) (repeat ()))
        transforms (gensym "transforms")]
   `(alter-var-root #'templates assoc ~(apply keyword (map str [*ns* name]))
                    (fn [~transforms]
                      (merge-with conj
                                  ~@(if (keyword? args)
                                      [`(merge ~missing-keys ~transforms) (apply hash-map merge-map)]
                                      [transforms {:main `(fn ~args ~@body)}]))))))

(deftemplate dwarf
  :main
  (fn [character] {:pre [(::constitution character)]}
    (-> character
        (update-in [::constitution] inc)
        (update-in [::proficiencies] conj ::battleaxe ::handaxe ::throwing-hammer ::warhammer)
        (update-in [::languages] conj ::common ::dwarvish)
        (update-in [::advantage] conj ::poison [::history :only ::stonework-origin])
        (update-in [::resistance] conj ::poison)
        (assoc ::size ::medium ::speed 25 ::darkvision 60)))
  :persistent
  #(if (-> % ::lost)
     (assoc % ::lost (-> % ::underground not))
     %))

(deftemplate hill-dwarf [character]
  {:pre [(::strength character)]}
  (-> character
      (update-in [::strength] inc)
      (assoc ::hp (+ (hp character) (level character)))))
;; todo: hill-dwarf increased regeneration while resting

(deftemplate mountain-dwarf [character]
  {:pre [(::wisdom character)]}
  (-> character
      (update-in [::wisdom] inc)
      (update-in [::proficiencies] conj ::light-armor ::medium-armor)
      (assoc ::ac (let [ac (ac character)]
                    (if (some #{::heavy-armor ::medium-armor} (::equipped character))
                      (inc ac) ac)))))
;; todo change check for ::heavy... etc to a fn, since they might have ::equipped [::plate]

(deftemplate barbarian [character]
  (-> character
      (update-in [::proficiencies] conj ::light-armor ::medium-armor ::shields
                 ::simple-weapons ::martial-weapons [::mounts :only ::land])
      (update-in [::saving-throws] conj ::strength ::constitution)
      (update-in [::hit-die] conj 12)))
;; todo: skill choice: athletics, intimidation, survival

(deftemplate bard [character]
  (update-in character [::hit-die] conj 6))
;; todo: finish bard, do more templates

(defn process-condition-modifiers [condition-modifiers transform]
  (reduce
   (fn [new-transforms {:keys [func priority]}]
     (update-in new-transforms [(if priority priority :main)] conj func))
   transform condition-modifiers))

(defn character
  "character is a map describing a character's attributes
  condition-modifiers are optional temporary conditions, such as injuries or spell effects.
  Their form is a map, with keys such as :func (a fn applied to character), :priority (one of :main, :persistent, etc)
  :duration, and any other keys the condition needs, i.e. damage-type"
  [character & condition-modifiers]
  (let [defaults {::proficiencies #{} ::saving-throws #{} ::hit-die []}
        prioritize #(reverse (mapcat % [:main :persistent :override]))
        transform
        (->> {:main []}
             (apply-template (::classes character))
             (apply-template (::race character))
             (apply-template (character ::subrace))
             (process-condition-modifiers condition-modifiers)
             prioritize
             (apply comp))]
    (transform (merge defaults character))))
