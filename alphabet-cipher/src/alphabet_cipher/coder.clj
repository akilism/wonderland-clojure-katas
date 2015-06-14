(ns alphabet-cipher.coder)

(def alpha "abcdefghijklmnopqrstuvwxyz")

;(get-char-idx \m alpha)
(defn get-char-idx [c alpha-list]
  (loop [n 0 x alpha-list]
    (cond
      (empty? x) 0
      (= c (first x)) n
      :else (recur (inc n) (rest x)))))

;(get-char 10 alpha)
(defn get-char [n xs]
  (cond
    (= n 0) (first xs)
    :else (recur (dec n) (rest xs))))

;(get-adjusted-alpha \m alpha)
(defn get-adjusted-alpha [start-char alpha-list]
    (loop [n (get-char-idx start-char alpha-list) acc []]
      (cond
        (= (count acc) (count alpha-list)) acc
        (> n 25) (recur 0 acc)
        :else (recur (inc n) (conj acc (get-char n alpha-list))))))

;(get-encode-char \m \r alpha)
(defn get-encode-char [key-char msg-char alpha-list]
  (let [key-idx (get-char-idx key-char alpha-list) msg-idx (get-char-idx msg-char alpha-list) char-idx (+ key-idx msg-idx)]
    (cond
      (> char-idx 25) (get-char (- char-idx 26) alpha-list)
      :else (get-char char-idx alpha-list))))

;(get-decode-char \m \d alpha)
(defn get-decode-char [key-char msg-char a-list]
  (let [k-idx (get-char-idx key-char a-list)]
    (loop [letters a-list alpha-list (get-adjusted-alpha (first letters) a-list) decode-char (first letters)]
      (cond
        (= msg-char (get-char k-idx alpha-list)) decode-char
        :else (recur
                (rest letters)
                (get-adjusted-alpha (first (rest letters)) a-list)
                (first (rest letters)))))))

;(encode "hotdog" "doesthiswork")
(defn encode [keyword message]
  (loop [ks keyword ms message encoded []]
    (cond
      (empty? ms) (clojure.string/join encoded)
      (empty? ks) (recur keyword ms encoded)
      :else  (recur (rest ks) (rest ms) (conj encoded (get-encode-char (first ks) (first ms) alpha))))))

;(decode "hotdog" "kcxvhnpgprfq")
(defn decode [keyword message]
  (loop [ks keyword ms message decoded []]
    (cond
      (empty? ms) (clojure.string/join decoded)
      (empty? ks) (recur keyword ms decoded)
      :else (recur (rest ks) (rest ms) (conj decoded (get-decode-char (first ks) (first ms) alpha))))))
