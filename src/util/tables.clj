(ns util.tables)

(def ^{:private true :dynamic true} entities)
(def ^{:private true :dynamic true} params)

(def ^:private raw-table
  (letfn
      [(add-param-if-absent [entity]
         (if-let [param (params entity)]
           param
           (let [param (gensym)]
             (set! entities (conj entities entity))
             (set! params (merge params {entity param}))
             param)))
       
       (parse-subtables [body element]
         (conj body
               (cond
                (and (seq? element) (= 'table (first element))) (table-parse (rest element))
                (vector? element) (reduce parse-subtables [] element)
                :else element)))
       
       (table-parse [[[pred entity key] & body]]
         (if (vector? (first body))
           (let [col-data (first body)]
             `((zipmap ~col-data ~(table-parse (rest body)))
               (some #(if (~pred (~(add-param-if-absent entity) ~key) %) %) ~col-data)))
           `(condp ~pred (~(add-param-if-absent entity) ~key) ~@(seq (reduce parse-subtables [] body)))))]
    
    (fn [table-data]
      (let [body (table-parse table-data)]
        [(vec (map params entities)) body]))))

(defmacro table [& table-def]
  (binding [entities [] params {}]
    (let [[form table-data] (split-with (complement seq?) table-def)
          name (first form)
          body (raw-table table-data)]
      (if name
        `(do (defn ~@form ~@body)
             (alter-meta! (var ~name) merge {:arglists '~entities})
             ~name)
        `(fn ~@body)))))

(defn gte [x y]
  (let [gt (fnil > -1 -1)]
   (or (= x y) (gt x y))))
