(ns dnd.races
  (:use [clojure.core.logic :only [defrel facts run* fresh everyg membero]]
        [util (tables :only [table]) common]
        [dnd classes abilities]))

(define-keyword-list races elf halfling half-elf half-orc human dwarf gnome)
(define-keyword-list subraces hairfeet stout tallfellow)
(defkeywords race subrace)

(defrel multiclass race classes)

(defn- map-classes [race]
  #(map (partial vector race) %))

(facts multiclass
       (concat ((map-classes
                 half-elf) [[cleric fighter]
                            [cleric fighter magic-user]
                            [cleric ranger]
                            [cleric magic-user]
                            [fighter magic-user]
                            [fighter magic-user thief]
                            [magic-user thief]
                            [druid]])
               ((map-classes
                 half-orc) [[fighter cleric]
                            [cleric thief]
                            [cleric assassin]
                            [fighter assassin]])
               ((map-classes
                 human) (map vector classes))
               ((map-classes
                 elf) [[fighter magic-user]
                       [fighter magic-user thief]
                       [magic-user thief]])
               ((map-classes
                 gnome) [[fighter illusionist]
                         [illusionist thief]])))

(def ^:private races [dwarf elf gnome half-elf half-orc])
(defn- make-multiclass [& classes] #(map (fn [race] [race classes]) %))
(->> races
     (cons halfling)
     ((make-multiclass fighter thief))
     (facts multiclass))
(->> races
     ((make-multiclass assassin))
     (facts multiclass))

(defn valid-races [& classes]
  (set (run* [valid-race]
         (fresh [valid-classes]
                (multiclass valid-race valid-classes)
                (everyg #(membero % valid-classes) classes)))))

(defn more-classes [race & classes]
  (-> (run* [multiclasses]
             (multiclass race multiclasses)
             (everyg #(membero % multiclasses) classes))
      flatten
      set
      (clojure.set/difference classes)))

(def u :unlimited)
(def ^:private gte (fnil >= -1 18))
(table max-level "Max level (including NPC-only combinations) from AD&D PHB p.14
For combinations that are valid for PCs, use valid-races and more-classes"
      (= class) [thief assassin cleric druid fighter paladin ranger magic-user illusionist monk]
 (= (character race))
       human    [u     14       u      14    u       u       u      u          u           17]
       dwarf    [u     9        8      nil   (table (gte % (character strength)) 16 7 17 8 9)]
       elf      [u     10       7      nil   (table \" 16 5 17 6 7)
                                                     nil     nil    (table (gte % (character intelligence)) 16 9 17 10 11)]
       gnome    [u     8        7      nil   (table (gte % (character strength)) 17 5 6)
                                                     nil     nil    nil        (table (gte % (character intelligence)) [16 17 99]
                                                                                      (gte % (character dexterity)) 16 [5  5  5]
                                                                                                                    17 [5  6  7]
                                                                                                                    99 [5  7  7])]
       half-elf [u     11       5      14    (table (gte % (character strength)) 16 6 17 7 8)
                                                     nil     (table \" 16 6 17 7 8)
                                                                    (table (gte % (character intelligence)) 16 6 17 7 8)]
       halfling [u     nil      nil    6     (table (contains? % (character subrace)) [#{hairfeet nil} #{stout} #{tallfellow}]
                                                    (gte % (character strength)) 16   [4 4 4]
                                                                                 17   [4 4 5]
                                                                                 18   [4 5 6])]
       half-orc [(table (gte % (character dexterity)) 16 6 17 7 8)
                       14       4      nil   10])
