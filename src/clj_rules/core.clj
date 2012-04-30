(ns clj-rules.core)

; Classic "insurance" rules to play with
(def rules
  [ {:name "Increase cost by 20% for ages 21 to 25"
     :ordinal 2
     :rule #(let [{{age :age} :person} %] (and (>= age 21) (<= age 25)) )
     :action (fn [s] (update-in s [:price] #(* % 1.20) ))}
    
    {:name "Increase cost by 5% for males"
     :ordinal 2
     :rule #(let [{{sex :sex} :person} %] (= sex :male) )
     :action (fn [s] (update-in s [:price] #(* % 1.05) ))}

    {:name "Increase cost by $40 for red car"
     :ordinal 1
     :rule #(let [{{color :color} :car} %] (= color :red) )
     :action (fn [s] (update-in s [:price] #(+ % 40) ))}
    ])

; Customer data
(def customer {:person {:name "Bob"
                     :age 22
                     :sex :male}
            :car {:make "Honda"
                  :model "Civic"
                  :year 2000
                  :color :red}
            :price 439.99})

; Naive Approach #1: Single Pass Match & Execute
(defn run-rules [rules state]
  ((apply comp (map #(:action %) (filter #((:rule %) state) rules))) state))

; Niave Approach #2: Muli-Pass Match & Execute with no rule repeats
(defn rule-eng [rules state]
  (let [{rules-to-run true rules-to-skip false} (group-by #((:rule %) state) rules)
        sorted-rules-to-run (reverse (sort-by :ordinal rules-to-run))
        result ((apply comp (map #(:action %) sorted-rules-to-run)) state)]
    (if (and (not (identical? state result)) (not-empty rules-to-skip))
      (recur rules-to-skip result)
      result)))

; Niave Approach #3: Single Pass Chained Match & Execute
; Orders rules, transforms each rule to a function which executes the action based on the predicate
; in a chain of functions. The result of each rule is the input for the next (which allows
; eariler rules to trigger later rules, but not later rules to trigger earlier rules)
; Rules are reverse ordered so 'comp' nests the functions correctly
(defn rule-eng2 [rules state]
  (let [sorted-rules (reverse (sort-by :ordinal rules))
        rule-chain (map #(let [{rule :rule action :action} %]
                           (fn [s] (if (rule s) (action s) s)) ) sorted-rules)]
    ((apply comp rule-chain) state)))

(rule-eng2 rules customer)
