(ns dnd.util)

(defmacro table [& table-def]
  (let [{:keys [params body]} (apply parse-table {} table-def)]
    `(fn ~(vec (vals params)) ~body)))

(defn parse-table
  ([metadata [pred [entity key] & table-data]]
     (let [{:keys [params body]} (reduce process-subtables metadata table-data)
           params (merge {entity (gensym)} params)
           param (params entity)]
       {:params
        :body (
               `(condp ~pred (if (ifn? ~param) (~param ~key) ~param) ~@table-body))}))
  
  ([metadata [pred [entity key] col-data] table-rows]
     (let [{:keys [params body]} (parse-table metadata table-rows)
           params (merge {entity (gensym)} params)
           param (params entity)]
       {:params params
        :body `(let [expr# (if (ifn? ~param) (~param ~key) ~param)
                     column# (first (filter #(~pred % expr#) ~col-data))
                     index# ((zipmap ~col-data (range)) column#)]
                 (if (integer? index#) (~body index#) ~body))})))

(defn process-subtables [acc table-form]
  (if (and (seq? table-form) (= 'table (first table-form)))
    (apply parse-table acc (rest table-form))
    (merge-with conj-dl acc {:body table-form})))

3-d syntax
(table (lte (char int) 0 (table ...) 1 (table ...) 2 (table ...)))

(defn lte [x y]
  (let [lt (fnil > -1 Integer/MAX_VALUE)]
   (or (= x y) (lt x y))))

                                        ;lte nil 3  false
            ;lte 3 nil  false (this one might not matter; depends on order of rows in table)
                                        ;lte nil nil true
                                        ;lte 3 3 true
                                        ;lte 4 3 true

(defmacro table-a [[& params] row-key pred & body]
  (let [param-list (vec (repeatedly (count params) gensym))]
    `(fn ~param-list (condp #(~pred %2 %1) (apply ~key ~param-list) ~@body))))

