(ns dnd.races-test
  (:use [dnd races classes abilities]
        midje.sweet))

(fact (valid-races :cleric :fighter) => (just #{half-elf half-orc}))

(fact (valid-races cleric) => (contains #{half-elf half-orc human}))

(fact (max-level {race human} illusionist) => u
      (max-level {race elf intelligence 17} magic-user) => 10
      (max-level {race elf} magic-user) => 11
      (max-level {race dwarf} cleric) => 8
      (max-level {race half-elf strength 17} ranger) => 7
      (max-level {race halfling strength 18 subrace stout} fighter) => 5
      (max-level {race halfling strength 18 subrace tallfellow} fighter) => 6
      (max-level {race halfling strength 17} fighter) => 4
      (max-level {race half-orc dexterity 14} thief) => 6
      (max-level {race gnome} illusionist) => 7
      (max-level {race gnome dexterity 17 intelligence 17} illusionist) => 6
      (max-level {race gnome dexterity 17} illusionist) => 7
      (max-level {race gnome dexterity 15} illusionist) => 5
      (max-level {race gnome intelligence 17} illusionist) => 7
      (max-level {race gnome intelligence 15} illusionist) => 5)

