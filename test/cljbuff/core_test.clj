(ns cljbuff.core-test
  (:require [clojure.test :refer [deftest is testing]])
  (:import [tutorial Hello$Person$Builder]
           [com.google.protobuf CodedInputStream]
           [java.nio ByteBuffer]))

(defn base-numbers [base n]
  (loop [exp 0
         r (range 0 n)
         acc []]
    (if (empty? r)
      acc
      (recur (inc exp) (rest r) (conj acc (Math/pow base exp))))))

;; 010101011010101010101010101101010  1 0
;; 0123456789
;; 0123456789ABCDEF


(deftest base-numbers-calculator-test
  (is (= [1 2 4 8 16 32 64 128] (map int (base-numbers 2 8))))
  (is (= [1 4 16 64 256] (map int (base-numbers 4 5))))
  (is (= [1 6 36] (map int (base-numbers 6 3))))
  (is (= [1 16 256 4096] (map int (base-numbers 16 4))))
  (is (= [1 128 16384] (map int (base-numbers 128 3)))))

(defn ->base10 [base coll]
  (loop [exp 0
         coll (reverse coll)
         acc 0]
    (if (empty? coll)
      acc
      (recur (inc exp) (rest coll) (+ acc (* (first coll) (Math/pow base exp)))))))

(deftest base-x->base10-test
  (is (= 1 (int (->base10 2 [1]))))
  (is (= 2 (int (->base10 2 [1 0]))))
  (is (= 3 (int (->base10 2 [1 1]))))
  (is (= 4 (int (->base10 2 [1 0 0]))))


  (is (= 1 (int (->base10 8 [1]))))
  (is (= 8 (int (->base10 8 [1 0]))))
  (is (= 9 (int (->base10 8 [1 1]))))

  ;; [1 128 16384]

  (is (= 1 (int (->base10 128 [1]))))
  (is (= 128 (int (->base10 128 [1 0]))))
  (is (= 16384 (int (->base10 128 [1 0 0]))))

;;000 0010  010 1100
;;→  000 0010 ++ 010 1100
;;→  100101100 ;; need to (bit-shift-left 2 7) to concatenate
;; (+ (bit-shift-left 2 7) 44)
;; (+ (bit-shift-left (Integer/parseInt "0000010" 2) 7) (Integer/parseInt "0101100" 2))
;;→  256 + 32 + 8 + 4 = 300

  )

(deftest parsing-base128-varint-test

  ;; example from https://developers.google.com/protocol-buffers/docs/encoding#varints
  ;; start with the number 300
  ;; 10101100 00000010
  ;; drop the msb (this tells us if we have reached the end of the number)
  ;; it's set in the first byte
  ;; 0101100 0000010 msb dropped - do this with bit-clear or masking with bit-and
  ;; change the order of the groups with reverse
  ;; 0000010 0101100
  ;; concatenate them (do this by shifting the first byte to the left then adding
  ;; so 0000010 becomes 00000100000000
  ;; (+ (bit-shift-left (Integer/parseInt "0000010" 2) 7) (Integer/parseInt "0101100" 2))

  (let [drop-most-significant-bit (fn [b]
                                    (bit-clear b 7))
        most-significant-byte-first (fn [coll]
                                      (reverse coll))
        concat-bytes (fn [coll]
                       (if (= 2 (count coll))
                         (+ (bit-shift-left (first coll) 7) (second coll))
                         (first coll)))

        base128-varint->base10 (fn [bytes]
                   (->> bytes
                        (map drop-most-significant-bit)
                        most-significant-byte-first
                        concat-bytes))]

    ;; (Integer/toBinaryString 172) 10101100
    ;; (Integer/toBinaryString 2)   10
    (is (= 1 (base128-varint->base10 [1])))
    (is (= 300 (base128-varint->base10 [172 2])))))

(deftest parsing-with-byte-buffer-and-coded-input-stream
  (let [coll [172 2]]
    (is (= 300 (-> coll
                   byte-array
                   ByteBuffer/wrap
                   CodedInputStream/newInstance
                   (.readInt32))))))
