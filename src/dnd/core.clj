(ns dnd.core
  (:require [util [common :refer :all] [tables :refer [table]]]))

(defprotocol Signalable
  (dispatch [_ signal]))

(extend-protocol Signalable
  nil
  (dispatch [_ _] nil)
  Object
  (dispatch [this _] this))

(defn modify
  "The first arg can be the first effect of an effect-attribute pair, or the keyword :duration -
   a temporary effect can be specified by passing the keyword :duration as the effect and
    a Signalable as the following arg: when the Signalable returns nil, the effect ends; otherwise
    its return value replaces the previous Signalable.
   Further effect-attribute pairs can be specified in the variadic args.

   Examples: (modify character inc :con) (modify character dec :str inc :int)
     (modify character :duration <expiration> (partial + 2) :str)"
  ([character duration-or-first-effect & more]
     (let [[duration & mods] (into more
                                (if (not= duration-or-first-effect :duration)
                                  [duration-or-first-effect nil]))
           affected-attrs (zipmap (take-nth 2 (rest mods)) (take-nth 2 mods))
           re-wrap (fn ([new-character]
                          (apply modify new-character mods))
                       ([new-character new-duration]
                          (apply modify new-character :duration new-duration mods)))]
      (reify
        clojure.lang.IFn
        (invoke [this arg]
          (get this arg))
        clojure.lang.Associative
        (valAt [this key]
          (let [val (key character)]
            (try (if-let [affect (affected-attrs key)] (affect val) val)
                 (catch Exception e val))))
        (valAt [this key default]
          (if-let [val (get this key)]
            val (-> this (assoc key default) (get key))))
        (assoc [_ key val]
          (re-wrap (assoc character key val)))
        Signalable
        (dispatch [_ signal]
          (let [new-character (dispatch character signal)]
            (if (= :unwrap signal)
              new-character
              (if-let [new-duration (dispatch duration signal)]
                (re-wrap new-character new-duration)
                new-character))))))))

(defmulti update (fn [map key val]
                   [(type (key map)) (type val)]))
(defmethod update :default [map key val] (assoc map key val))
(defmethod update [clojure.lang.Sequential clojure.lang.Sequential]
  [map key val] (update-in map [key] into val))
(defmethod update [clojure.lang.Sequential java.lang.Object]
  [map key val] (update-in map [key] conj val))
(defmethod update [java.lang.Object clojure.lang.Sequential]
  [map key val] (update-in map [key] #(vec (cons %1 %2)) val))
(defmethod update [java.lang.Object java.lang.Object]
  [map key val] (update-in map [key] vector val))

(defn symbol-to-keyword [form] (if (symbol? form) (keyword form) form))

(defn until-with-state [state {:keys [interested-in contiguous total] :as spec}]
  (reify Signalable
    (dispatch [this signal]
      (if-let [interesting (find-first = signal interested-in)]
        (let [new-state (as-> state <>
                              (conj <> interesting)
                              (if (> (count <>) (count contiguous)) (pop <>) <>))
              new-total (remove-first interesting total)]
          (if-not (or (= new-state contiguous) (and (seq total) (empty? new-total)))
            (until-with-state new-state (assoc spec :total (remove-first interesting total)))))
        this))))

(defn until [& specs]
  (until-with-state clojure.lang.PersistentQueue/EMPTY specs))

(defmacro deftemplate
  "Takes a name, an arg vector, some transforms, and optional keyword :assoc followed by attrs
   The arg vector will make the base object available if needed (one arg, deconstructable)
   Will assoc the attrs to the base object as pairs, with symbols being turned into keywords
    The remaining transforms will have the resulting map threaded through them with ->
    If the second arg is not a vector, there will be no assoc pairs"
  [name args & desc]
  (let [[transforms attrs] (split-with (partial not= :assoc) desc)
        assoc-pairs (->> (next attrs)
                         (clojure.walk/prewalk symbol-to-keyword)
                         (partition 2)
                         (map (partial cons `update)))]
    `(defn ~name [char#]
       (let [~@args char#]
         (-> char# ~@assoc-pairs ~@transforms)))))

(defn report [key valid-options]
  (binding [*out* *err*]
    (println "Required property" (str "'" (name key) "'") "is missing or invalid")
    (if (seq valid-options) (apply println "Valid options:" (map name valid-options)))))

(defn apply-template [desc template-key & valid-values]
  (let [{template-name template-key} desc
        values (if (seq valid-values) valid-values [template-name])]
    (if-let [template
             (some-> (some #{template-name} values)
                     name symbol resolve)]
      (template desc)
      (do (report template-key valid-values) desc))))

(deftemplate dwarf [_]
  (modify inc :con)
  (apply-template :subrace :hill-dwarf :mountain-dwarf)
  :assoc size medium speed 25
  properties [darkvision never-lost-underground]
  advantages [[saves poison] [check history :only stonework]]
  proficiencies [battleaxe handaxe throwinghammer warhammer]
  languages [common dwarvish] resistance poison)

(deftemplate hill-dwarf [{:keys [level]}]
  (modify inc :str #(+ level %) :hp)
  ;; when roll a hit-die, gain 1 extra hp per hit-die
  )

(deftemplate mountain-dwarf [_]
  (modify inc :wis)
  ; while wearing medium or heavy, +1 ac
  :assoc proficiencies [:light-armor :medium-armor])

(deftemplate barbarian [_]
  :assoc proficiencies [light-armor medium-armor shields simple-weapons martial-weapons]
  :hd 12)

(deftemplate bard [_]
  :assoc :hd 6)

(deftemplate cleric [_]
  :assoc :hd 8)

(defn rage [{:keys [class] :as toon}]
  (if (= class :barbarian)
    (modify toon :duration (until :interested-in [:eot :attack :take-damage]
                                       :total (repeat 10 :eot) :contiguous [:eot :eot])
            (partial + 10) :hp)))

(defn modifier [score] (-> score (quot 2) (- 5)))

(defn average-roll [d] (-> d (/ 2) inc))

(defn hp [{:keys [hp hd level con] :as char}]
  (let [con-mod (modifier con)]
    (get char :hp
         (-> (+ con-mod (average-roll hd)) (* (dec level)) (+ hd con-mod)))))

(defn level [{:keys [xp]}]
 (dec
  ((table (> % xp)
              0 1
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
         190000 15) xp)))

(defn proficiency [char]
  (-> char level (+ 5) (quot 4)))

(defmacro character [& desc]
  `(->
    ~(->> desc (map symbol-to-keyword) (apply hash-map))
    (apply-template :race)
    (apply-template :class)))

;; (character name? player?
;;  str 10
;;  int 10
;;  wis 12
;;  dex 15
;;  con 9
;;  cha 3
;;  race elf
;;  class {:multiclass [fighter thief]}
;;  level 3
;;  level {:fighter 2 :thief 2}
;;  background ?
;;  attributes {:personality ""
;;              :traits "s-m-t"
;;              :name ?
;;              :alignment #{:chaotic :good}}
;;  equipment ?
;;  :other [hp hd ac init melee-mod ranged-mod proficiency]
;;  :meta? {player "Name" ability-method #{:dice :array :point-spend}})

;; (character
;;  (ability-scores 10 11 12 13 14 18 :point-spend)
;;  (abilties cha int wis dex con str))
