(def matrix (vector [1 4 7]
                    [3 0 5]
                    [-1 9 11]))

(defn determinant [matrix]
  (def a (get (get matrix 0) 0))
  (def b (get (get matrix 0) 1))
  (def c (get (get matrix 1) 0))
  (def d (get (get matrix 1) 1))

  (- (* a d) (* b c)))

(defn get-minor-matrix [matrix, index]
  ; Remove the top row and the column determined by the index
  (def minor-matrix
    (mapv (fn [row]
            (remove #(= (get row index) %) row))
          (into (vector) (remove #(= (get matrix 0) %) matrix))))

  ; Turns seq into vector
  (mapv (fn [row] (into [] row)) minor-matrix))

(defn exp [x n]
  (if (zero? n) 1
      (* x (exp x (dec n)))))

(defn laplace-expansion [matrix]
  (def matrix-lenght (count matrix))

  ; Map the index to the array so they can be accessed by reduce
  (def mapped-array (zipmap (get matrix 0) (range matrix-lenght)))

  (if (= matrix-lenght 2)
    ; Base case of the recursion
    (determinant matrix)
    ; Else
    (reduce-kv
     (fn [result number index]
       (def minor-matrix (get-minor-matrix matrix index))

       ; Induction step of the recursion
       (def minor (* number (laplace-expansion minor-matrix)))

       (def sign (exp -1 index))
       (def cofactor (* sign minor))

       (+ cofactor result)) 0 mapped-array)))

(defn main []
  (println (laplace-expansion matrix)))

(main)
