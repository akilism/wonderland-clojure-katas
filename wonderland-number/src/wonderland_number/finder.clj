(ns wonderland-number.finder)
;; You must find a way to generate this wonderland number.

;; It has six digits
;; If you multiply it by 2,3,4,5, or 6, the resulting number
;; has all the same digits in at as the original number.
;; The only difference is the position that they are in.
(defn same-digits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn is-wonderland? [wonder-number]
  (if (and
        (same-digits? wonder-number (* wonder-number 2))
        (same-digits? wonder-number (* wonder-number 3))
        (same-digits? wonder-number (* wonder-number 4))
        (same-digits? wonder-number (* wonder-number 5))
        (same-digits? wonder-number (* wonder-number 6))) true false))

(defn wonderland-number []
  (first (filter is-wonderland? (range 100000 999999))))
