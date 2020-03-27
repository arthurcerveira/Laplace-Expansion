(def matrix (vector [1 4 7]
                    [3 0 5]
                    [-1 9 11]))

(defn determinant [m]
  (def a (get (get m 0) 0))
  (def b (get (get m 0) 1))
  (def c (get (get m 1) 0))
  (def d (get (get m 1) 1))

  (- (* a d) (* b c)))

(defn laplace-expansion [m]
  (def matrix-lenght (count m))
  (def index 0)

  (if (= matrix-lenght 2)
    ; Base case of the recursion
    (determinant m)
    (reduce (fn [result number]
              (println "index:" index)
              (println "result:" result)
              (println "number:" number)
              (def index (+ index 1))
              (+ number result)) 0 (get m 0))))

(println (laplace-expansion matrix))