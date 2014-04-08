(ns rand-logic.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]
            [clojure.math.numeric-tower :as m]
            [clojure.core.logic.arithmetic :as arith]))



(defn pad-bit-length-32 [bits]
  (if-not (= 32 (count bits))
    (vec
     (concat (repeat (- 32 (count bits))
                     0)
             bits))
    bits))

(defn int->bitvec [n]
  (loop [n n
         bits '()]
    (if (even? n)
      (recur  (/ n 2)
              (conj bits 0))
      (if (= 1 n)
        (pad-bit-length-32 (vec (conj bits 1)))
        (recur (/ (dec n) 2)
               (conj bits 1))))))

(def two32 (m/expt 2N 32))

(defn modo [x dividend modulus]
  (conde
   [(fd/< x dividend)
    (== modulus x)]
   [(fresh [j y z]
           (fd/> x dividend)
           (fd/in z (fd/interval 0 100))
           (fd/in dividend (fd/interval two32))
           (fd/+ x j y)
           (fd/<= 0 modulus)
           (fd/<= 0 x)
           (fd/<= 0 dividend)
           (fd/< modulus dividend)
           (fd/quot y dividend z)
           (fd/- dividend j modulus))]))

(defn bit-ando [u v w]
  (conde
   [(== u 1) (== v 1) (== w 1)]
   [(== u 0) (== w 0)]
   [(== v 0) (== u 1) (bit-ando v u w)]))

(defn bit-oro [u v w]
  (conde
   [(== v 1) (== u v) (== w 1)]
   [(== v 1) (== u 0) (== w 1)]
   [(== u 1) (== v 0) (bit-oro v u w)]
   [(== u 0) (== v 0) (== w 0)]))




(defn bit-xoro [a b c]
  (conde
   [(== a b) (== c 0)]
   [(== a 0) (== b 1) (== c 1)]
   [(== b 0) (== a 1) (== c 1)]))


(defn bit-addero [cin A B s cout]
  (fresh [At a-n-b c-in-and-At]
         (bit-xoro A B At)
         (bit-xoro At cin s)

         (bit-ando A B a-n-b)
         (bit-oro a-n-b c-in-and-At cout)

         (bit-ando cin At c-in-and-At)))

(defn secondo [l a]
  (fresh [r]
         (resto l r)
         (firsto r a)))

(defn lasto [l a]
  (conde
   [(== [a] l)]
   [(fresh [r]
           (resto l r)
           (lasto r a))]))

;; not finished
(defn butlasto [l d]
  (conde
   [(emptyo l) (emptyo d)]
   [(fresh [last-item]
           (lasto l last-item)
           (== ))]))


(defn two-bit-addero [cin bits1 bits2 sbits cout]
  (fresh [b11 b12 b21 b22 s1 t1 s2]
         (== [b12 b11] bits1)
         (== [b22 b21] bits2)
         (bit-addero cin b11 b21 s1 t1)
         (bit-addero t1 b12 b22 s2 cout)
         (== [s2 s1] sbits)))

(defn u32bit-adder [cin bits1 bits2 sbits cout]
  (fresh [a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
          b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31
          t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
          c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31]
         (== bits1 [a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31])
         (== bits2 [b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31])
         (== sbits [c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31])
         (bit-addero cin a0 b0 c0 t0)
         (bit-addero t0 a1 b1 c1 t1)
         (bit-addero t1 a2 b2 c2 t2)
         (bit-addero t2 a3 b3 c3 t3)
         (bit-addero t3 a4 b4 c4 t4)
         (bit-addero t4 a5 b5 c5 t5)
         (bit-addero t5 a6 b6 c6 t6)
         (bit-addero t6 a7 b7 c7 t7)
         (bit-addero t7 a8 b8 c8 t8)
         (bit-addero t8 a9 b9 c9 t9)
         (bit-addero t9 a10 b10 c10 t10)
         (bit-addero t10 a11 b11 c11 t11)
         (bit-addero t11 a12 b12 c12 t12)
         (bit-addero t12 a13 b13 c13 t13)
         (bit-addero t13 a14 b14 c14 t14)
         (bit-addero t14 a15 b15 c15 t15)
         (bit-addero t15 a16 b16 c16 t16)
         (bit-addero t16 a17 b17 c17 t17)
         (bit-addero t17 a18 b18 c18 t18)
         (bit-addero t18 a19 b19 c19 t19)
         (bit-addero t19 a20 b20 c20 t20)
         (bit-addero t20 a21 b21 c21 t21)
         (bit-addero t21 a22 b22 c22 t22)
         (bit-addero t22 a23 b23 c23 t23)
         (bit-addero t23 a24 b24 c24 t24)
         (bit-addero t24 a25 b25 c25 t25)
         (bit-addero t25 a26 b26 c26 t26)
         (bit-addero t26 a27 b27 c27 t27)
         (bit-addero t27 a28 b28 c28 t28)
         (bit-addero t28 a29 b29 c29 t29)
         (bit-addero t29 a30 b30 c30 t30)
         (bit-addero t30 a31 b31 c31 cout)))

(defn nbit-fn [bit-fn bits1 bits2 or-bits]
  (conde
   [(emptyo bits1) (emptyo bits2) (emptyo or-bits)]
   [(fresh [t1 t2 t3 r1 r2 r3]
           (conso t1 r1 bits1)
           (conso t2 r2 bits2)
           (conso t3 r3 or-bits)

           (bit-fn t1 t2 t3)
           (nbit-fn bit-fn r1 r2 r3))]))

(def nbit-oro (partial nbit-fn bit-oro))

(def nbit-xoro (partial nbit-fn bit-xoro))
(def nbit-ando (partial nbit-fn bit-ando))


(defn bit-noto [bit not-bit]
  (conde [(== 1 bit) (== 0 not-bit)]
         [(== 0 bit) (== 1 not-bit)]))


(defn nbit-noto [bits not-bits]
  (conde
   [(emptyo bits) (emptyo not-bits)]
   [(fresh [t1 r1 t2 r2]
           (conso t1 r1 bits)
           (conso t2 r2 not-bits)
           (nbit-noto r1 r2)
           (bit-noto t1 t2))]))

#_(run 3 [q] (nbit-noto q [1 1 0 0 ]))

(defn F-fn [B C D F]
  (fresh [LH RH not-B]
         (nbit-ando B C LH)
         (nbit-noto B not-B)
         (nbit-ando not-B D RH)
         (nbit-oro LH RH F)))

(defn G-fn [B C D G]
  (fresh [LH RH not-D]
         (nbit-noto D not-D)
         (nbit-ando C not-D RH)
         (nbit-ando B D LH)
         (nbit-oro LH RH G)))

(defn H-fn [B C D H]
  (fresh [LH]
         (nbit-xoro B C LH)
         (nbit-xoro LH D H)))

(defn I-fn [B C D I]
  (fresh [RH not-D]
         (nbit-noto D not-D)
         (nbit-oro B not-D RH)
         (nbit-xoro C RH I)))


(defn ntho [l n i]
  (conde
   [(== 0 n) (firsto l i)]
   [(!= 0 n)
    (fresh [r n--]
           (resto l r)
           (fd/- n 1 n--)
           (ntho r n-- i))]))

(comment
  (run 5 [q]
       (ntho [1 2 3 4] q 4)))


(def s
  [7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22
   5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20
   4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23
   6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21])


(def K
  (map int->bitvec
       [0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
        0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
        0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
        0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
        0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
        0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
        0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
        0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
        0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391]))

(def initial-A (int->bitvec 0x67452301))
(def initial-B (int->bitvec 0xefcdab89))
(def initial-C (int->bitvec 0x98badcfe))
(def initial-D (int->bitvec 0x10325476))


(defn left-rotateo [bits cnt out-bits]
  (all
   (fd/in cnt (fd/interval 0 32))
   (conde
    [(== 0 cnt) (== bits out-bits)]
    [(fd/< 0 cnt)
     (conde [(lvaro cnt)] [(lvaro out-bits)])
     (fresh [f1 r1 f1l temp-bits cnt--]
            (fd/in cnt-- (fd/interval 0 32))
            (fd/- cnt 1 cnt--)
            (resto bits r1)
            (firsto bits f1)
            (conso f1 '() f1l)
            (appendo r1 f1l temp-bits)
            (left-rotateo temp-bits cnt-- out-bits))]
    [(fd/< 0 cnt)
     (lvaro bits)
     (fresh [f1 r1 f1l temp-bits cnt--]
            (fd/in cnt-- (fd/interval 0 32))
            (fd/- cnt 1 cnt--)
            (left-rotateo temp-bits cnt-- out-bits)
            (resto bits r1)
            (firsto bits f1)
            (conso f1 '() f1l)
            (appendo r1 f1l temp-bits)
            )])))

(comment
  (run 4 [q]
       (fd/in q (fd/interval 0 2)) )

  (run 2 [q]
       (left-rotateo '(2 3 4 1) 3 q))

  (run 2 [q]
       (left-rotateo q 3 '(1 2 3 4)))

  (run 20 [q]
       (left-rotateo '(1 2 3 4) q '(2 3 4 1)))

  (run 10 [q]
       (u32bit-adder 0 [1 1] [1 1] q dc)))


(defn B-transform [B A F i g M new-B]
  (fresh [A+F A+F+K A+F+K+M K_i M_g s_i lrotted _dc1 _dc2 _dc3 _dc4]
         (ntho K i K_i)
         (ntho M g M_g)
         (ntho s i s_i)
         (u32bit-adder 0 A F A+F _dc1)
         (u32bit-adder 0 A+F K_i A+F+K _dc2)
         (u32bit-adder 0 A+F+K M_g A+F+K+M _dc3)
         (left-rotateo A+F+K+M s_i lrotted)

         (u32bit-adder 0 B lrotted new-B _dc4)))

(run 1 [q]
     (ntho s 0 q)
     )

(def M (repeat 16 (repeat 32 0)))

(run 1 [A B C D t1 t2 t3 t4]
     (all
      (F-fn initial-B initial-C initial-D t1)
      (B-transform initial-B initial-A t1 0 0 M B )
      (ntho K 0 t2)
      (ntho M 0 t3)
      (ntho s 0 t4)

      #_(el-passo2 0 M
                   initial-A initial-B initial-C initial-D
                   A B C D)))

(defn el-passo [pass-number M A B C D new-A new-B new-C new-D]
  (fresh [F g]
         (conde
          [(fd/<= 0 pass-number)
           (fd/<= pass-number 15)
           (F-fn B C D F)
           (== g pass-number)]
          [(fd/<= 16 pass-number)
           (fd/<= pass-number 31)
           (G-fn B C D F)
           (fresh [t1 t2]
                  (fd/* 5 pass-number t1)
                  (fd/+ t1 1 t2)
                  (modo t2 16 g))]
          [(fd/<= 32 pass-number)
           (fd/<= pass-number 47)
           (H-fn B C D F)
           (fresh [t1 t2]
                  (fd/* 3 pass-number t1)
                  (fd/+ t1 5 t2)
                  (modo t2 16 g))]
          [(fd/<= 48 pass-number)
           (fd/<= pass-number 63)
           (I-fn B C D F)
           (fresh [t1]
                  (fd/* 7 pass-number t1)
                  (modo t1 16 g))])
         (== new-D C)
         (== new-C B)
         (== new-A D)
         (B-transform B A F pass-number g M new-B)
         ))


; 512-bit chunk of message
; break chunk into sixteen 32-bit words M[j], 0 ≤ j ≤ 15

(defn bitvec->int [bits]
  (loop [pow2 0
         bits bits
         n 0]
    (let [bit (last bits)]
      (if (seq bits)
        (recur (inc pow2)
               (butlast bits)
               (+ n (* bit (m/expt 2 pow2))))
        n))))


(defn simple-log [& args]
  (spit "/tmp/log.log"
        (apply str args)
        :append true))

(defn M-chunk-hash [pass-number M A B C D final-A final-B final-C final-D]
  (all
   (fd/in pass-number (fd/interval 0 65))
   (fd/<= 0 pass-number)
   (project [pass-number A B C D]
            (do
              (when true (= 19 pass-number)
                (simple-log
                 "first pass number: " pass-number "\t"
                 #_(pr-str (map bitvec->int [A B C D]))
                 " "
                 #_(pr-str (map bitvec->int M))
                 "\n"))

              s#))
   (conde
    [(== pass-number 25)
     (== A final-A)
     (== B final-B)
     (== C final-C)
     (== D final-D)
     #_(project [final-A final-B final-C final-D]
              (do (simple-log "Done            "
                              (pr-str
                               (map bitvec->int [final-A final-B final-C final-D]))
                              "\n")
                s#))]
    [(fd/< pass-number 25)
     (fresh [next-pass next-A next-B next-C next-D]
            (fd/+ pass-number 1 next-pass)
            (fd/in next-pass (fd/interval 0 65))
            (el-passo pass-number M A B C D next-A next-B next-C next-D)
            (project [pass-number next-pass]
                     (do
                       (when true (= 19 pass-number)
                         (simple-log
                          "pass number: " pass-number "\t" next-pass "\n"))
                       s#))
            (M-chunk-hash next-pass M
                          next-A next-B next-C next-D
                          final-A final-B final-C final-D))])))


(run 1 [A B C D]
     ;(I-fn initial-B initial-C initial-D t1)
     ;(B-transform initial-B initial-A )
     (M-chunk-hash 23 M
                initial-A initial-B initial-C initial-D
                A B C D))

(run 1 [A B C D]
     ;(I-fn initial-B initial-C initial-D t1)
     ;(B-transform initial-B initial-A )
     (el-passo 23 M
                initial-A initial-B initial-C initial-D
                A B C D))


