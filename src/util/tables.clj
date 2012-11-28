(ns util.tables)

(declare table-parse)

(defn add-param-if-absent [raw-params params entity]
  (if-let [param (params entity)]
    [raw-params param params]
    (let [param (gensym)]
      [(conj raw-params entity) param (merge {entity param} params)])))

(defn parse-subtables [[raw-params params body] element]
  (if (and (seq? element) (= 'table (first element)))
    (let [[raw-params params subtable] (table-parse raw-params params (rest element))]
      [raw-params params (conj body subtable)])
    (if (vector? element)
      (let [[raw-params params element] (reduce parse-subtables [raw-params params []] element)]
        [raw-params params (conj body element)])
      [raw-params params (conj body element)])))

(defn table-parse [raw-params params [[entity key] pred & body]]
  (if (vector? (first body))
    (let [col-data (first body)
          [raw-params params row-body] (table-parse raw-params params (rest body))
          [raw-params param params] (add-param-if-absent raw-params params entity)]
      [raw-params params
       `((zipmap ~col-data ~row-body)
         (some #(if (~pred (~param ~key) %) %) ~col-data))])
    (let [[raw-params param params] (add-param-if-absent raw-params params entity)
          [raw-params params body] (reduce parse-subtables [raw-params params []] body)]
      [raw-params params `(condp ~pred (~param ~key) ~@(seq body))])))

(defn raw-table [table-data]
  (let [[params param-map body] (table-parse [] {} table-data)]
    [(vec (map param-map params)) body]))

(defmacro table [& table-data]
  (let [[params body] (raw-table table-data)]
    `(fn ~params ~body)))