(ns util.tables-test
  (:use midje.sweet
        util.tables))

(declare wimp average-joe jock)

(def test-str (table (= (character :str)) 0 3 1 4 2 5))

(fact (test-str wimp) => 3
      (provided (wimp :str) => 0))

(fact
 (test-str average-joe) => 4
 (provided (average-joe :str) => 1))

(fact
 (test-str jock) => 5
 (provided (jock :str) => 2))

(def table-2-d (table (= (character1 :str)) [1 2 3] (= % (character2 :level)) 1 [4 5 6] 2 [7 8 9]))

(fact (table-2-d {:level 2}{:str 1}) => 7)

(fact (table-2-d {:level 1} {:str 2}) => 5)

(fact "combine the args"
      ((table (= (character :str)) [1 2 3] (= (character :level)) 1 [4 5 6] 2 [7 8 9])
       {:str 3 :level 1}) => 6)

(fact "3-axis table"
      ((table (= (char1 :class)) :fig (table (= (char2 :level)) [1] (= (monster :h-d)) 2 [3]))
       {:class :fig} {:h-d 2} {:level 1}) => 3)

(fact "1-arg 3-axis table"
      ((table (= (monster :class)) :fig (table (= (monster :level)) [1] (= (monster :h-d)) 2 [3]))
       {:class :fig :h-d 2 :level 1}) => 3)

(fact "2-arg 3-axis table"
      ((table (= (monster :class)) :fig (table (= (monster :level)) [1] (= (char :h-d)) 2 [3]))
       {:class :fig :level 1} {:h-d 2}) => 3)

(fact "inverted 3-axis table"
      ((table (= (monster :level)) [1] (= (char :level)) 2 [(table (= (monster :str)) 3 4)])
       {:level 2} {:level 1 :str 3}) => 4)

(fact "short rows default to nil"
      ((table (= (monster :level)) [1 2 3] (= (char :level)) 4 [5]) {:level 4} {:level 2}) => nil)
