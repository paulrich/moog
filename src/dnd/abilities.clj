(ns dnd.abilities
  (:use dnd.dice
        util.tables))

(defmacro ^:private ability [& ability-list]
  (let [do-me (map #(list 'def % (keyword %)) ability-list)]
    (concat (cons 'do do-me)
     (list `(def ~'abilities [~@ability-list])))))

(ability strength intelligence wisdom dexterity constitution charisma)

(defn roll-ability [] (roll 3 d 6))

(defn- assign-abilities [dice-roll]
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
   :thief
   :cleric
   :druid
   :illusionist
   ])

(defn- unrestricted-classes [limits]
  (reduce (fn [unrestricted [limit & classes]]
            (apply disj unrestricted (if (= :only (first classes))
                                       (rest classes) classes)))
          (set classes) limits))

(defn parse-restrictions [value [[lower-limit _ solo-class] & limits]]
  (let [unrestricted (unrestricted-classes limits)]
    (if (<= value lower-limit) #{solo-class}
        (reduce
         (fn [valid-classes [_ & classes]]
           (if (= :only (first classes))
             (set (rest classes))
             (apply conj valid-classes classes)))
         unrestricted
         (take-while #(>= value (first %)) limits)))))

(defmacro ^:private restrict-classes [& restriction-defs]
  (let [create-fn (fn [[ability & restrictions]]
                    `{~ability (fn [character#]
                         ((fnil ~'parse-restrictions 0)
                          (character# ~ability)
                          ~(vec restrictions)))})]
    (apply merge (map create-fn restriction-defs))))

;; return map of attribute => function[character] mappings
(def class-restrictions
  (restrict-classes
   [strength               
    [5 :only :magic-user]
    [9 :fighter]
    [12 :assassin :paladin]
    [13 :ranger]
    [15 :monk]
    [19 :only :fighter]]

   [intelligence 
    [5 :only :fighter]
    [9 :paladin :magic-user]
    [11 :assassin]
    [13 :ranger]
    [15 :illusionist]]

   [wisdom 
    [5 :only :thief]
    [9 :cleric]
    [12 :druid]
    [13 :paladin]
    [14 :ranger]
    [15 :monk]]

   [dexterity 
    [5 :only :cleric]
    [6 :magic-user]
    [9 :thief]
    [12 :assassin]
    [15 :monk]
    [16 :illusionist]]

   [constitution 
    [5 :only :illusionist]
    [7 :fighter]
    [9 :paladin]
    [11 :monk]
    [14 :ranger]]

   [charisma 
    [5 :only :assassin]
    [15 :druid]
    [17 :paladin]]))

(defn drink-potion [mod-attr effect duration char]
  (fn [attr]
    (cond
      (= attr mod-attr) (effect (char attr))
      (= attr :eot) (if (= duration 1) char
                        (drink-potion mod-attr effect (dec duration)
                                      (if-let [new-char (char :eot)] new-char char)))
      :hi! (char attr))))

(table hit-probability "bonus to melee roll"
       (>= character :strength)
       3 -3
       5 -2
       7 -1
       16 0
       18 (table (gte character :exceptional-str)
                 50 1
                 99 2
                 100 3))

(table damage-adjustment "bonus; melee combat only"
       (>= character :strength)
       5 -1
       15 0
       17 1
       18 (table (gte character :exceptional-str)
                 nil 2
                 75 3
                 90 4
                 99 5
                 100 6))

(table weight-allowance "gold pieces over maximum"
       (>= character :strength)
       3 -350
       5 -250
       7 -150
       11 0
       13 100
       15 200
       16 300
       17 500
       18 (table (gte character :exceptional-str)
                 nil 750
                 50 1000
                 75 1250
                 90 1500
                 99 2000
                 100 3000))

(table open-doors "Roll this number or less on a d6 for a heavy or stuck door.
   Successive attempts are allowed but take time and cause noise"
       (>= character :strength)
       7 1
       15 2
       17 3
       18 (table (gte character :exceptional-str)
                 50 3
                 99 4
                 100 5))

(table open-locked-door "roll d6 (less), one attempt only; includes locked, barred, wizard-locked or magically held"
       (gte character :exceptional-str)
       90 0
       99 1
       100 2)

(table bend-bars "percent chance; also lift gates; can try each once, total of two (when applicable)"
       (gte character :strength)
       7 0
       9 1
       11 2
       13 4
       15 7
       16 10
       17 13
       18 (table (gte character :exceptional-str)
                 nil 16
                 50 20
                 75 25
                 90 30
                 99 35
                 100 40))

(table languages "possible number of additional languages known"
       (>= character :intelligence)
       7 0
       9 1
       11 2
       13 3
       15 4
       16 5
       17 6
       18 7)

(table know-spell? "chance for character to know any given spell"
       (>= character :intelligence)
       8 0
       9 35
       12 45
       14 55
       16 65
       17 75
       18 85
       19 95)

(table minimum-spells "minimum number of spells known per spell level"
       (>= character :intelligence)
       8 0
       9 4
       12 5
       14 6
       16 7
       17 8
       18 9
       19 10)

(table maximum-spells "the maxiumum number of spells that can be known"
       (>= character :intelligence)
       8 0
       9 6
       12 7
       14 9
       16 11
       17 14
       18 18
       :all) ; all spells can be known at 19 int

(table max-spell-level
       (= character :class)
       :magic-user (let [int (character :intelligence)]
                     (cond
                      (<= int 8) 0
                      (= int 9) 4
                      :else (-> int (/ 2) Math/floor clojure.core/int (min 9))))
       :cleric (let [wis (character :wisdom)]
                 (cond
                  (< wis 17) 5
                  (= wis 17) 6
                  :else 7))
       "You can't cast spells")

(table will-save-adjustment
       (>= character :wisdom)
       3 -3
       4 -2
       7 -1
       14 0
       15 1
       16 2
       17 3
       18 4)


   ; bad keys: k u 9 0 c maybe f maybe j 5 is a little weird 6 for sure
