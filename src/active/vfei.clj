(ns active.vfei
  "Encode and decode VFEI messages."
  (:require [active.clojure.record :refer :all]
            [active.clojure.lens :as lens]
            [active.clojure.condition :as c]
            [clojure.string :as string])
  (:import [java.time ZonedDateTime]))

(defn skip-whitespace
  [s]
  "Skip leading whitespace from a string, returning a seq of the remaining characters."
  (loop [s s]
    (cond
     (empty? s) s
     (Character/isWhitespace (char (first s))) (recur (rest s))
     :else s)))

(defn decode-vfei-string
  "Decode a VFEI-encoded string."
  [s]
  (let [s (skip-whitespace s)]
    (if (empty? s)
      (c/error `decode-vfei-string "expected a string literal" (string/join s))
      (let [c (first s)
            s (rest s)
            ^StringBuilder builder (StringBuilder.)]
        (case c
          ;; Systema extension
          \n (if (= [\u \l \l] (take 3 s))
               [nil (drop 3 s)]
               (c/error `decode-vfei-string "expected a string literal" (string/join (cons c s))))

          \" (loop [s s]
               (if (empty? s)
                 (c/error `decode-vfei-string "premature end of string literal" (.toString builder))
                 (let [c (first s)
                       s (rest s)]
                   (case c
                     \" [(.toString builder) s]
                     \\ (if (empty? s)
                          (c/error `decode-vfei-string "premature end of string literal" (.toString builder))
                          (case (first s)
                            \a (do (.append builder \u0007) (recur (rest s)))
                            \b (do (.append builder \u0008) (recur (rest s)))
                            \f (do (.append builder \u000c) (recur (rest s)))
                            \n (do (.append builder \u000a) (recur (rest s)))
                            \r (do (.append builder \u000d) (recur (rest s)))
                            \t (do (.append builder \u0009) (recur (rest s)))
                            \v (do (.append builder \u000b) (recur (rest s)))
                            \\ (do (.append builder \u005c) (recur (rest s)))
                            \' (do (.append builder \u0027) (recur (rest s)))
                            \" (do (.append builder \u0022) (recur (rest s)))
                            \? (do (.append builder \u003f) (recur (rest s)))
                            \0 (let [d1 (first (next s))
                                     d2 (first (next (next s)))
                                     octal (fn [d]
                                             (let [i (int d)]
                                               (if (and (>= i (int \0))
                                                        (<= i (int \7)))
                                                 (- i (int \0))
                                                 (c/error `decode-vfei-string "invalid octal digit" (string/join s)))))]
                            
                                 (if (not (and d1 d2))
                                   (c/error `decode-vfei-string "premature end of string literal" (.toString builder) d1 d2))
                                 (.append builder (char (+ (* 8 (octal d1)) (octal d2))))
                                 (recur (next (next (next s)))))
                            (c/error `decode-vfei-string "invalid escape code in string literal" (string/join s))))
                     (do
                       (.append builder c)
                       (recur s))))))

          (c/error `decode-vfei-string "expected a string literal" (string/join s)))))))

; value is a map
(define-record-type ListFormat
  (^{:doc "Make a list format for a VFEI data item."}
   make-list-format size)
  list-format?
  [size list-format-size])

(define-record-type ArrayFormat
  (^{:doc "Make an array format for a VFEI data item."} 
   make-array-format element-format size)
  array-format?
  [element-format array-format-element-format
   size array-format-size])

(define-record-type DataItem
  (^{:doc "Make a VFEI data item."}
   really-make-data-item name format value)
  data-item?
  [name data-item-name 
   format data-item-format
   value data-item-value])

(defn make-data-item
  "Assert that list length matches format."
  [name format value]
  (cond
    (list-format? format)
    (when (not= (list-format-size format) (count value))
      (c/error `make-data-item "list length does not match number of parsed values" name format (count value) value))
    (array-format? format)
    (when (not= (array-format-size format) (count value))
      (c/error `make-data-item "array size does not match number of parsed values" name format (count value) value)))
  (really-make-data-item name format value))

(defn parse-data-item-name
  "Parse the name of a VFEI data item, returning a pair of the name and the rest."
  [s]
  (let [^StringBuilder builder (StringBuilder.)]
    (loop [s s]
      (if (or (empty? s)
              (= \/ (first s))
              (Character/isWhitespace (char (first s))))
        [(.toString builder) s]
        (do
          (.append builder (first s))
          (recur (rest s)))))))

(defn- expect
  "Expect a character prefix, returning the rest or throwing an exception."
  [c s]
  (cond
   (empty? s)
   (c/error `expect (str "unexpected EOF, expected " c) (string/join s))

   (= (first s) c) (rest s)

   :else (c/error `expect (str "expected " c " but got " (first s)) (string/join s))))


(defn- parse-integer
  "Parse an integer from a string, returning it and the rest."
  [s]
  (let [s (skip-whitespace s)
        [sign s] (if (= \- (first s))
                   [- (rest s)]
                   [+ s])]
        
    (when-not (Character/isDigit (char (first s)))
      (c/error `parse-integer "expected digit" (first s)))
    (loop [s s
           v 0]
      (cond
       (empty? s) [(sign v) s]

       (Character/isDigit (char (first s)))
       (recur (rest s)
              (+ (* v 10)
                 (Character/digit (char (first s)) 10)))

       :else [(sign v) s]))))

(defn- parse-float
  [s]
  (let [s (skip-whitespace s)]
    (loop [s s
           seen-e? false
           t (transient [])]
      (if (empty? s) 
        [(Double/parseDouble (string/join (persistent! t))) s]
        (let [c (char (first s))]
       
          (cond

           (Character/isDigit c)
           (recur (rest s) false (conj! t c))

           (or (= \e c) (= \E c))
           (do
             (when seen-e?
               (c/error `parse-float "unexpected character in float literal" c))
             (recur (rest s) true (conj! t c)))
           
           (or (= \. c) (= \- c))
           (recur (rest s) false (conj! t c))

           ;; e can't terminate - dot or digit can
           seen-e?
           (c/error `parse-float "invalid termination of float literal" c)

           :else
           [(Double/parseDouble (string/join (persistent! t)))
            s]))))))

(defn- parse-zoned-date-time
  [s & [null-anywhere?]]
  (if (and null-anywhere? (= '(\n \u \l \l) (take 4 s)))
    [nil (drop 4 s)]
    (let [[v s] (decode-vfei-string s)]
      [(ZonedDateTime/parse v) s])))

(defn parse-size
  "Parse the size of a list or array item, returning it and the rest."
  [s]
  (let [[v s] (parse-integer (expect \[ s))
        s (skip-whitespace s)]
    [v (expect \] s)]))

(defn parse-format-code
  "Parse a VFEI format code, returning it and the rest."
  [s]
  (if (empty? s)
    (c/error `parse-format-code "unexpected EOF, expected format code")

    (let [f (first s)
          s (rest s)]
      (if (= f \L)
        (let [s (skip-whitespace s)
              [size s] (parse-size s)]
          [(make-list-format size) s])
        (let [prefix1 (fn [c code]
                        (and (= c f)
                             [code s]))
              [base-code s] (or (prefix1 \A :a)
                                (prefix1 \N :n) ; see decode-vfe-n
                                (prefix1 \D :d)
                                (if (empty? s)
                                  (c/error `parse-format-code "unexpected EOF, incomplete format code")
                                  (let [n (first s)
                                        s' (rest s)
                                        prefix2 (fn [c2 code]
                                                  (and (= c2 n)
                                                       [code s']))]
                                    (case f
                                      \U (or (prefix2 \1 :u1)
                                             (prefix2 \2 :u2)
                                             (prefix2 \4 :u4)
                                             (prefix2 \8 :u8)
                                             (c/error `parse-format-code "invalid format code" (str f n)))
                                      \I (or (prefix2 \1 :i1)
                                             (prefix2 \2 :i2)
                                             (prefix2 \4 :i4)
                                             (prefix2 \8 :i8)
                                             (c/error `parse-format-code "invalid format code" (str f n)))
                                      \F (or (prefix2 \4 :f4)
                                             (prefix2 \8 :f8)
                                             (c/error `parse-format-code "invalid format code" (str f n)))
                                      \B (or (prefix2 \L :bl)
                                             [:b s])))))
              s (skip-whitespace s)]
          (if (= \[ (first s))
            (let [s (skip-whitespace s)
                  [size s] (parse-size s)]
              [(make-array-format base-code size) s])
            [base-code s]))))))

(declare parse-data-item)

;; From: Michael.Kossatz@systemagmbh.de
;; Subject: Antwort: Re: PULSE-Gateway      
;; Der Datentyp N (NULL) ist eine Erweiterung der 
;; VFEI Spec (Ich kann Ihnen jetzt nicht sagen, von wem diese stammt), die 
;; vom Systema VFEI Treiber unterstützt wird.
;; Wenn der Wert von USR_DATA nicht NULL ist, dann muss das Format F8 sein.
;; Wenn ein USR_STRING keinen Wert hat, dass muss ein "" übertragen werden. 
;; Das sollte sich so auch in dem Request, aus dem Sie das USR/L[35] Item 
;; entnommen haben, so widerspiegeln.

(defn decode-vfei-n 
  [s]
  (let [s (skip-whitespace s)]
    (if (= '(\n \u \l \l) (take 4 s))
      [nil (drop 4 s)]
      (parse-float s))))

(defn parse-data-item-value
  "Parse the value of a VFEI data item, given its format, returning it and the rest."
  [format null-anywhere? s]
  (if (and null-anywhere?
           (= \n (first s)) ; optimization
           (= [\n \u \l \l] (take 4 s)))
    [nil (drop 4 s)]
    (case format
      :a (decode-vfei-string s)
      :n (decode-vfei-n s)
      :b (parse-integer s) ;; FIXME: is this binary? Is parse-integer OK?
      :i1 (parse-integer s) ;; FIXME: validation
      :i2 (parse-integer s) ;; FIXME: validation
      :i4 (parse-integer s) ;; FIXME: validation
      :i8 (parse-integer s) ;; FIXME: validation
      :u1 (parse-integer s)
      :u2 (parse-integer s)
      :u4 (parse-integer s)
      :u8 (parse-integer s)
      :f4 (parse-float s)
      :f8 (parse-float s)
      :d (parse-zoned-date-time s null-anywhere?)
      :bl (parse-integer s) ;; FIXME: probably integer representation of booleans
      ;; FIXME: other formats
      (cond
        (list-format? format)
        (if (= '(\n \u \l \l) (take 4 s))
          [nil (drop 4 s)]
          (loop [s (expect \[ s)
                 items (transient [])]
            (let [s (skip-whitespace s)]
              (cond
                (empty? s) (c/error `parse-data-item-value "unexpected EOF inside list")
                
                (= \] (first s))
                [(persistent! items) (rest s)]
                
                :else
                (let [[item s] (parse-data-item s null-anywhere?)
                      s (skip-whitespace s)]
                  (recur s (conj! items item)))))))
        
        (array-format? format)
        (let [el-format (array-format-element-format format)]
          (loop [s (expect \[ s)
                 items (transient [])]
            (let [s (skip-whitespace s)]
              (cond
                (empty? s) (c/error `parse-data-item-value "unexpected EOF inside array")
                
                (= \] (first s))
                [(persistent! items) (rest s)]
                
                :else
                (let [[v s] (parse-data-item-value el-format null-anywhere? s)
                      s (skip-whitespace s)]
                  (recur s (conj! items v)))))))
        
        :else
        (c/assertion-violation `parse-data-item-value "unhandled item format" format)))))

(defn parse-data-item
  "Parse a VFEI data item, returning it and the rest."
  [s null-anywhere?]
  (let [s (skip-whitespace s)
        [name s] (parse-data-item-name s)
        s (skip-whitespace s)
        s (expect \/ s)
        s (skip-whitespace s)
        [format s] (parse-format-code s)
        s (skip-whitespace s)
        s (expect \= s)
        s (skip-whitespace s)
        [value s] (parse-data-item-value format null-anywhere? s)]
    [(make-data-item name format value) s]))

(defn parse-vfei
  "Parse a VFEI message, returning a list of data items.

  `null-anywhere?` says whethere `null` (Systema extension) is allowed anywhere
  or just for `A`, `N`, `D`, and lists."
  [s & [null-anywhere?]]
  (loop [s (seq s)
         items (transient [])]
    (let [s (skip-whitespace s)]
      (if (empty? s)
        (persistent! items)
        (let [[item s] (parse-data-item s null-anywhere?)
              s (skip-whitespace s)]
          (recur s (conj! items item)))))))

(define-record-type AmbigousFieldsValues
  (^{:doc "Values (numbered) of ambiguous fields on one level."}
   make-ambiguous-fields-values mp)
  ambiguous-fields-values?
  [mp ambiguous-fields-values-mp])

(defn assoc-ambiguous
  [m k v]
  (update m k (fn [v*]
                (cond
                  ;; key not in map at all
                  (nil? v*)
                  v

                  ;; already multiple keys in map
                  (ambiguous-fields-values? v*)
                  (lens/overhaul v* ambiguous-fields-values-mp #(assoc % (str (count (keys %))) v))

                  ;; only one key in map -- check "untyped" equality to keep
                  ;; backwards compatibility of ambiguous fields semantics
                  (and (data-item? v*) (data-item? v)
                       (= (str (data-item-value v*)) (str (data-item-value v))))
                  v

                  :else
                  (make-ambiguous-fields-values {"0" v* "1" v})))))

(defn zipmap-ambiguous
  "Returns a map with the keys mapped to the corresponding vals.
  Ambigous keys yield a map with numbered values."
  [keys vals]
  (loop [map {}
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
      (recur (assoc-ambiguous map (first ks) (first vs))
             (next ks)
             (next vs))
      map)))

(defn data-items->map
  "Convert a list of VFEI data items into a map."
  [items]
  (zipmap-ambiguous (map data-item-name items)
                    items))

(declare vfei->map)

(defn data-item->map-value
  [div]
  (let [v (data-item-value div)]
    (if (list-format? (data-item-format div))
      (vfei->map v)
      v)))

(defn vfei->map
  "Recursivley convert the result of `parse-vfei` to a key-value map.
  Warning: This function does not keep the order of the elements in VFEI lists.
  Values of ambiguous fields on one level yield a map with numbers as keys and
  values of the ambiguous fields as the values to the numbered keys."
  [v]
  (reduce-kv #(assoc %1 %2
                     (cond
                       (data-item? %3)
                       (data-item->map-value %3)

                       (ambiguous-fields-values? %3)
                       (reduce-kv (fn [a af av]
                                    (assoc a af (data-item->map-value av)))
                                  {} (ambiguous-fields-values-mp %3))
                       :else
                       (assert false "Unexpected value.")))
             {} (data-items->map v)))

(declare format->expr value->expr data-item->expr data-items->expr)

(defn format->expr
  "Convert a format to a Clojure expression that generates it.

For generating tests."
  [f]
  (cond
    (list-format? f) `(~'vfei/make-list-format ~(list-format-size f))
    (array-format? f) `(~'vfei/make-array-format
                        ~(format->expr (array-format-element-format f))
                        ~(array-format-size f))
    :else f))
 

(defn value->expr
  "Convert a value to a Clojure expression that generates it.

For generating tests."
  [v]
  (cond
    (integer? v) v
    (string? v) v
    (nil? v) v
    (vector? v) (mapv value->expr v)
    (data-item? v) (data-item->expr v)
    (instance? ZonedDateTime v) `(ZonedDateTime/parse ~(str v))
    :else
    (c/assertion-violation `value->expr "unknown value" v)))

(defn data-item->expr
  "Convert a data item to a Clojure expression that generates it.

For generating tests."
  [di]
  `(~'vfei/make-data-item ~(data-item-name di)
    ~(format->expr (data-item-format di))
    ~(value->expr (data-item-value di))))

(defn data-items->expr
  "Convert a seq of data items to a Clojure expression that generates it.

For generating tests."
  [sq]
  (mapv data-item->expr sq))

(defn vfei-encode-char
  "Encode a character in VFEI format."
  [c]
  (case c
    \u0007 "\\a"
    \u0008 "\\b"
    \u000c "\\f"
    ;; Note: the systema decoder seems to disregard the quoted special
    ;; chars; as newlines are especially important, we insert them as
    ;; raw characters here:
    \u000a "\n"
    \u000d "\\r"
    \u0009 "\\t"
    \u000b "\\v"
    \u005c "\\\\"
    ;; \u0027 "\\'" ; Systema's decoder can't deal with this
    \u0022 "\\\""
    (let [i (int c)]
      (cond
       (>= i 256)
       (c/error `vfei-encode-char "invalid VFEI message character" c)

       (or (and (>= i (int \space))
                (<= i (int \~)))
           (>= i 128))
       (str c)

       :else (format "\\%03o" i)))))
     

(defn vfei-encode-string
  "Encode a string as a VFEI style, using C-style escapes."
  [s]
  (if (nil? s)
    "null"
    (str "\""
         (string/join
          (map vfei-encode-char s))
         "\"")))
