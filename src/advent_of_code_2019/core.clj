(ns advent-of-code-2019.core
  (:gen-class))

;; Day 1: The Tyranny of the Rocket Equation
;; - Fuel required to launch a given module is based on its mass.
;; - Specifically, to find the fuel required for a module, take its mass,
;;   divide by three, round down, and subtract 2.
;; - What is the sum of the fuel requirements for all of the modules on your spacecraft?

(def module-masses-day1
  [79620 58052 119910 138477 139102 78373 51937 63751 100937 56664 128939 115929 136981 68215 90317 97455 130858 94009 123221 81390 61726 78271 73354 103061 131261 140510 120555 117319 91154 96009 75491 90245 141689 118783 104601 121969 98547 108924 117114 65916 120037 66166 93973 105777 63501 89199 117551 126021 93466 107901 82323 104471 98794 57270 59457 120558 128142 137648 127375 103353 116578 97950 110725 96438 128425 75503 132178 138363 67009 127873 135747 108109 118818 75396 92822 63886 82973 116243 129066 74185 145298 83483 83417 54682 55648 142206 121420 149890 56561 107108 111376 139885 147373 131657 140634 79704 90263 139892 103841 50730])

(defn module-fuel-1
  "The fuel required for a module is: module mass, divide by three, round down, and subtract 2."
  [mass]
  ;; we must toIntExact in order to avoid propagating floating point errors
  (-> mass (/ 3) Math/floor (- 2) (Math/toIntExact)))

;; (map module-fuel [12 14 1969 100756])

(defn total-fuel
  [fuel-calc-fn module-masses]
  (apply + (map fuel-calc-fn module-masses)))

(comment
  (= 3412094 ;; correct answer
     (total-fuel module-fuel-1
                 module-masses-day1)))


;; Part 2: Calculate module fuel to its fixed point
;; - Terminate at negative additional fuel
;; - What is the sum of the fuel requirements for all of the modules on
;;   your spacecraft when also taking into account the mass of the added fuel?

(defn module-fuel-2
  "The fuel required for a module is: module mass, divide by three, round down, and subtract 2."
  [mass]
  ;; we must toIntExact in order to avoid propagating floating point errors
  (->> mass
       (iterate module-fuel-1)
       rest
       (take-while (comp not neg?))
       (apply +)))

(comment
  (take 5 (iterate module-fuel-1 14))

  (map module-fuel-2 [14 1969 100756])

  (= 5115267 ;; correct answer
     (total-fuel module-fuel-2
                 module-masses-day1)))


;; Day 2: 1202 program alarm

(def day2-input-raw
  [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,6,23,2,13,23,27,1,27,13,31,1,9,31,35,1,35,9,39,1,39,5,43,2,6,43,47,1,47,6,51,2,51,9,55,2,55,13,59,1,59,6,63,1,10,63,67,2,67,9,71,2,6,71,75,1,75,5,79,2,79,10,83,1,5,83,87,2,9,87,91,1,5,91,95,2,13,95,99,1,99,10,103,1,103,2,107,1,107,6,0,99,2,14,0,0])

(def day2-input
  "Once you have a working computer, the first step is to restore the
  gravity assist program (your puzzle input) to the \"1202 program alarm state\"
  it had just before the last computer caught fire.

  To do this, before running the program, replace position 1 with the value 12
  and replace position 2 with the value 2."
  (-> day2-input-raw
      (assoc 1 12)
      (assoc 2 2)))

(def opswidth 4)

(def exitcode 99)

(def opsmap
  {1 + ; (fnil + 0 0)
   2 * ; (fnil * 1 1)
   exitcode (constantly false)})


(defn legal-idx?
  [idx program-length]
  (when (number? idx)
    (< idx program-length)))


(defn update-program
  [program-listing
   [opcode arg1-idx arg2-idx result-idx]]
  (let [program-length (count program-listing)]
    (when (and (legal-idx? arg1-idx   program-length)
               (legal-idx? arg2-idx   program-length)
               (legal-idx? result-idx program-length))
      (let [op (get opsmap opcode exitcode)
            x  (nth program-listing arg1-idx)
            y  (nth program-listing arg2-idx)]
        (when-let [result (op x y)]
          (assoc program-listing result-idx result))))))


(defn run-program
  [program-listing]
  (reduce (fn [prog-lst program-ctr]
            (let [instruction-set (->> prog-lst
                                       (drop (* program-ctr opswidth))
                                       (take opswidth))]
              (or (update-program prog-lst instruction-set)
                  (reduced prog-lst))))
          program-listing
          (range)))


(comment
  (let [xs [1 5 6 7
            2 9 9 3]]
    [xs
     (update-program xs
                     (take opswidth xs))])

  (let [xs [1 5 6 7
            2 9 9 3]]
    (run-prog xs))

  [;; Given evaluation example
   (let [eg1 [1,9,10,3,2,3,11,0,99,30,40,50]]
     (= (run-prog eg1)
        [3500 9 10 70 2 3 11 0 99 30 40 50]))

   ;; Given test cases
   (= (run-prog [1,0,0,0,99])
      [2,0,0,0,99])

   (= (run-prog [2,3,0,3,99])
      [2,3,0,6,99])

   (= (run-prog [2,4,4,5,99,0])
      [2,4,4,5,99,9801])

   (= (run-prog [1,1,1,4,99,5,6,0,99])
      [30,1,1,4,2,5,6,0,99])
   ] ;; all given cases

  (= (first (run-prog day2-input))
     2842648) ;; correct answer
  )
