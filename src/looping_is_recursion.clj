(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp acc]
		 (if
		     (= 0 exp) acc
		     (recur base (dec exp) (* acc base) )
		     )
		 )	
	]
    (helper base exp 1)
    
   )
  )

(defn last-element [a-seq]
  (let [helper (fn [a-seq acc]
		 (if (empty? a-seq)
		   acc
		 (recur (rest a-seq)  (first a-seq))
		 )
		 )	
	]
    (helper a-seq nil)
    )
  )

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
		 (cond
		  (not (= (first a-seq) (first b-seq))) false
		  (and (empty? a-seq) (empty? b-seq)) true
		  (or (empty? a-seq) (empty? b-seq)) false
		  :else (recur (rest a-seq) (rest b-seq))
		  )
		 
		 )
	]
  (helper seq1 seq2)
    )
  )

(defn find-first-index [pred a-seq]
  (loop [pr pred
	 seq a-seq
	 idx 0]
    (cond
     (empty? seq) nil
     (pr (first seq)) idx
     :else (recur pr (rest seq) (inc idx) )
     )
    )  
  )

(defn avg [a-seq]
  (loop
      [seq a-seq
       count 0
       sum 0
       ]
    (if (empty? seq) (/ sum count)
	(recur (rest seq) (inc count) (+ sum (first seq)))
	)
    )
  )

(defn parity [a-seq]
  (loop
      [seq a-seq
       result #{}
       ]
    (if (empty? seq) result
    (recur  (rest seq)
	    (if (contains? result (first seq)) (disj result (first seq)) (conj result (first seq)))
	    )
    )
  )
 )

(defn fast-fibo [n]
  (loop  [ i 2
	  prev1 0
	  prev2 1
	  ]
    (cond
     (= 0 n) 0
     (= 1 n) 1
     (= n i) (+ prev1 prev2)
	:else (recur (inc i) prev2  (+ prev1 prev2)  )
	)
    )
  )

(defn cut-at-repetition [a-seq]
  (loop
      [seq a-seq
       lst []
       hdl #{}
       ]

    (cond (empty? seq) lst
	(contains? hdl (first seq)) lst
	:else (recur (rest seq ) (conj lst (first seq) ) (conj hdl (first seq))  )
     )
    )
  )

