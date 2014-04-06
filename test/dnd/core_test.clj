(ns dnd.core-test
  (:require [clojure.test :refer :all]
            [dnd.core :refer :all]))

(dorun (map #(eval `(def ~% ~(keyword "dnd.core" (str %))))
      '(levels classes barbarian bard constitution underground lost)))

(deftest creation
  (testing "Dwarf"
    (let [attrs {constitution 10 :dnd.core/race :dnd.core/dwarf}
          toon (character attrs)]
      (is (= (select-keys toon [constitution lost])
             {constitution 11}) "basic")
      (is (= (-> (character attrs {:func #(update-in % [constitution] * 2)})
                 (select-keys [constitution]))
             {constitution 22}) "potioned")
      (is (= (-> (character attrs {:func #(assoc % lost true underground true)})
                 (select-keys [lost]))
             {lost false}) "is never lost (underground)")
      (is (= (-> (character attrs {:func #(assoc % lost true)})
                 (select-keys [lost]))
             {lost true}) "can be lost above ground")
      (is (= (-> attrs (assoc underground true) (character {:func #(assoc % lost true) :priority :override})
                 (select-keys [lost]))
             {lost true}) "Cast confuse-dwarf...")
      (is (= (-> attrs (assoc levels [2 1]) (assoc classes [bard barbarian]) character hp) 17) "hp calculation")
      )))
