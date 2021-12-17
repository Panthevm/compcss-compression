(ns compcss-compression.core)

(defn compression
  [stylesheets]
  (letfn [(add-unique [uniques identifier path node]
            (update uniques identifier
                    (fn [unique]
                      (if unique
                        (update unique path
                                #(compression
                                  (into (get node path) %)))
                        (update node path compression)))))]

    (loop [nodes   (seq stylesheets)
           uniques {}]
      (if nodes
        (let [node      (first nodes)
              node-type (:type node)]
          (cond

            (= :declaration node-type)
            (recur (next nodes)
                   (assoc uniques (:property node) node))

            (= :selector node-type)
            (recur (next nodes)
                   (assoc uniques (hash (:members node)) node))

            (= :style-rule node-type)
            (let [new-node (update node :selectors compression)]
              (recur (next nodes)
                     (add-unique uniques (hash (:selectors new-node))
                                 :declarations new-node)))

            (= :media-rule node-type)
            (recur (next nodes)
                   (add-unique uniques (hash (:queries node))
                               :rules node))

            (= :keyframes-block node-type)
            (recur (next nodes)
                   (add-unique uniques (hash (:selectors node))
                               :declarations node))

            (= :keyframes-rule node-type)
            (recur (next nodes)
                   (add-unique uniques (hash (:name node))
                               :blocks node))

            :else
            (recur (next nodes)
                   (assoc uniques (hash node) node))))

        (vals uniques)))))

(defn middleware
  [handler]
  (fn [configuration db]
    (->> (update db :compcss.core/output-stylesheets compression)
         (handler configuration))))
