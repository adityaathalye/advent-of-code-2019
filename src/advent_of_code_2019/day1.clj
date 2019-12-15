(ns advent-of-code-2019.day1)

;; Day 1: The Tyranny of the Rocket Equation
;; - Fuel required to launch a given module is based on its mass.
;; - Specifically, to find the fuel required for a module, take its mass,
;;   divide by three, round down, and subtract 2.
;; - What is the sum of the fuel requirements for all of the modules on your spacecraft?

(def module-masses
  [79620 58052 119910 138477 139102 78373 51937 63751 100937 56664 128939 115929 136981 68215 90317 97455 130858 94009 123221 81390 61726 78271 73354 103061 131261 140510 120555 117319 91154 96009 75491 90245 141689 118783 104601 121969 98547 108924 117114 65916 120037 66166 93973 105777 63501 89199 117551 126021 93466 107901 82323 104471 98794 57270 59457 120558 128142 137648 127375 103353 116578 97950 110725 96438 128425 75503 132178 138363 67009 127873 135747 108109 118818 75396 92822 63886 82973 116243 129066 74185 145298 83483 83417 54682 55648 142206 121420 149890 56561 107108 111376 139885 147373 131657 140634 79704 90263 139892 103841 50730])


(defn module-fuel-1
  "The fuel required for a module is: module mass, divide by three, round down, and subtract 2."
  [mass]
  ;; we must toIntExact in order to avoid propagating floating point errors
  (-> mass (/ 3) Math/floor (- 2) (Math/toIntExact)))

(comment
  (map module-fuel-1 [12 14 1969 100756])
  )


(defn total-fuel
  [fuel-calc-fn module-masses]
  (apply + (map fuel-calc-fn module-masses)))

(comment
  (= 3412094 ;; correct answer
     (total-fuel module-fuel-1
                 module-masses))
  )


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
                 module-masses))
  )
