(ns clj-rules.core)

(def rule1 {:rule #(> (get-in % [:person :age] -1) 21)
            :action #(assoc-in % [:policy :agebump] true)})

(def rule2 {:rule #(< (get-in % [:person :age] -1) 25)
            :action #(assoc-in % [:policy :agebump2] true)})

(def rule3 {:rule #(get-in % [:policy :agebump] false)
            :action #(update-in % [:price] (fn [x] (+ x 10)))})

(def rules [rule3 rule2 rule1])

(def state {:person {:name "Bob" :age 22} :price 5})

; Naive Approach #1: Single Pass Match & Execute
(defn run-rules [rules state]
  ((apply comp (map #(:action %) (filter #((:rule %) state) rules))) state))

; Niave Approach #2: Muli-Pass Match & Execute with no rule repeats
(defn rule-eng [rules state]
  (let [{rules-to-run true rules-to-skip false} (group-by #((:rule %) state) rules)
        result ((apply comp (map #(:action %) rules-to-run)) state)]
        (if (and (not (identical? state result)) (not-empty rules-to-skip))
          (recur rules-to-skip result)
          result)))

(rule-eng rules state)
