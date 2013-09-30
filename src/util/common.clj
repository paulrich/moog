(ns util.common)

(defn if-is [x pred]
  (if (pred x) x))

(defn find-first
  ([pred coll]
     (some #(if (pred %) %) coll))
  ([fun expr coll]
     (some #(if (fun % expr) %))))

(defn remove-first [item sequence]
  (let [[head [_ & tail]] (split-with #(not= item %) sequence)]
    (concat head tail)))

(defmacro define-keyword-list [name & enumeration]
  (let [do-me (map #(list 'def % (keyword %)) enumeration)]
    (concat (cons 'do do-me)
            (list `(def ~name [~@enumeration])))))

(defmacro defkeywords [& names]
  (cons `do (map (fn [name]
              `(def ~name ~(keyword name))) names)))

(defn split-with-n [n pred coll]
  (let [first-n (take n coll)
        body (nthrest coll n)
        [name-doc body-start] (split-with pred first-n)]
    [name-doc (concat body-start body)]))
