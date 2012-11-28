(ns util.tables)

(defn table-parse [[[entity key] pred & body]]
  (if (vector? (first body))
    (let [col-data (first body)]
      `(fn [row-param#]
         (fn table-2d#
           ([] (~(table-parse (rest body)) row-param#))
           ([col-param#]
              ((zipmap ~col-data (~(table-parse (rest body)) col-param#))
               (some #(if (~pred (row-param# ~key) %) %) ~col-data))))))
    `(fn [param#] (condp ~pred (param# ~key) ~@body))))

(defn raw-table [table-data]
  (table-parse table-data))

(defn apply-args [fn args]
  (if (= 1 (count args))
    (fn (first args))
    ((apply-args fn (rest args)) (first args))))

(defmacro table [& table-data]
  (let [raw-fn (raw-table table-data)]
    `(fn [& args#]
       (apply-args ~raw-fn args#))))

;; raw-table yields one-param functions which take a param and return a val
;;   (or another function in the case of multi-axis tables)
;; table collects args ((table ...) x y) => val | where table=3-axis definition
;;;  and applies the args to each return value for as long as there are args
;; raw-table for a 1-axis table is a function which yields a value
;;   for a 2-axis table is a function that is defined with two arities:
;;       parameterized version returns a function which looks up the value via the column def
;;       1-arg version returns the entire row

