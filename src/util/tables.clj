(ns util.tables
  (:use clojure.walk util.common))

(def ^{:private true :dynamic true} entities)
(def ^{:private true :dynamic true} params)
(def ^{:private true :dynamic true} last-lookup)

(defn- add-param-if-absent [entity]
         (if-let [param (get params entity)]
           param
           (let [param (gensym)]
             (set! entities (conj entities entity))
             (set! params (merge params {entity param}))
             param)))

(defn- parse-entity-key [ek-param pred]
  (let [ek (first ek-param)
        [entity-key param-first] (if (= '% ek)
                             [(second ek-param) true]
                             [ek false])
        [entity key] (if (coll? entity-key) entity-key [entity-key nil])
        clause #(let [entity (add-param-if-absent entity)]
                  (if (nil? key) `~entity `(~entity ~key)))]
    [(fn [] (if param-first [pred (clause)] [`#(~pred %2 %1) (clause)]))
     (fn [] (if param-first `#(~pred % ~(clause)) `#(~pred ~(clause) %)))]))

(def ^:private raw-table
  (letfn
      [(parse-subtables [element]
         (cond
          (and (seq? element) (= 'table (first element))) (table-parse (rest element))
          (vector? element) (vec (map parse-subtables element))
          :else element))
       
       (table-parse [[lookup & body]]
         (let [[pred & ek-param] (if (= \" lookup)
                                   last-lookup
                                   (set! last-lookup lookup))
               [row-clause col-clause] (parse-entity-key ek-param pred)]
           (if (vector? (first body))
             (let [col-data (first body)]
               `((zipmap ~col-data ~(table-parse (rest body)))
                 (find-first ~(col-clause) ~col-data)))
             `(condp ~@(row-clause) ~@(doall (map parse-subtables body))))))]
    
    (fn [table-data]
      (let [body (table-parse table-data)]
        [(vec (map params entities)) (postwalk-replace params body)]))))

(defmacro table [& table-def]
  (binding [entities [] params {} last-lookup nil]
    (let [[form table-data] (split-with-n 2 #(or (symbol? %) (string? %)) table-def)
          name (first form)
          body (raw-table table-data)]
      (if name
        `(do (defn ~@form ~@body)
             (alter-meta! (var ~name) merge {:arglists '(~entities)})
             ~name)
        `(fn ~@body)))))

(defn gte [x y]
  (or (= x y) ((fnil > -1 -1) x y)))
