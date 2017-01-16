(ns active.vfei-test
  (:require [active.vfei :as vfei]
            [active.quickcheck :as qc]
            [clojure.test :refer :all]))

(deftest decode-vfei-string
  (is (= ["abc" ""]
         (vfei/decode-vfei-string "\"abc\"")))
  (is (= ["\u0007\u0008\u000c\n\r\t\u000b\\'\"?" ""]
         (vfei/decode-vfei-string "\"\\a\\b\\f\\n\\r\\t\\v\\\\\\'\\\"\\?\"")))
  (is (= ["\u0007" ""]
         (vfei/decode-vfei-string "\"\\007\"")))
  (is (= ["abc" "foobar"]
         (vfei/decode-vfei-string "\"abc\"foobar")))
  (is (= [nil " foobar"]
         (vfei/decode-vfei-string "null foobar"))))

(deftest parse-vfei
  (is (= 
       [(vfei/make-data-item "CMD" :a "executeCommand")
        (vfei/make-data-item "CT" :i4 0)
        (vfei/make-data-item "TX" :a "")
        (vfei/make-data-item "MODULE" :a "commandExecuter")
        (vfei/make-data-item "APPLICATION" :a "test")
        (vfei/make-data-item "CONTENTS" (vfei/make-list-format 1)
                             [(vfei/make-data-item "REPLY" :a
                                                   "SUCCESS
SERVICE1
SERVICE2
")])
        (vfei/make-data-item "RESTART" :bl 0)
        (vfei/make-data-item "INSTANCE" :a "0")]
       (vfei/parse-vfei "CMD/A=\"executeCommand\"
CT/I4=0
TX/A=\"\"
MODULE/A=\"commandExecuter\"
APPLICATION/A=\"test\"
CONTENTS/L[1]=[
    REPLY/A=\"SUCCESS
SERVICE1
SERVICE2
\"
]
RESTART/BL=0
INSTANCE/A=\"0\"")))

  (is (= [(vfei/make-data-item "CT" :i4 -5)]
         (vfei/parse-vfei "CT/I4=-5")))

  (is (= 
       [(vfei/make-data-item "CMD" :a "executeCommand")
        (vfei/make-data-item "CT" :i4 0)
        (vfei/make-data-item "TX" :a "")
        (vfei/make-data-item "MODULE" :a "commandExecuter")
        (vfei/make-data-item "APPLICATION" :a "test")
        (vfei/make-data-item "CONTENTS" (vfei/make-list-format 0) nil)
        (vfei/make-data-item "INSTANCE" :b 4)]
       (vfei/parse-vfei "CMD/A=\"executeCommand\"
CT/I4=0
TX/A=\"\"
MODULE/A=\"commandExecuter\"
APPLICATION/A=\"test\"
CONTENTS/L[0]=null
INSTANCE/B=4")))
  
  (is (= [(vfei/make-data-item "CT" :i4 -5)]
         (vfei/parse-vfei "CT/I4=-5")))

  (is (= [(vfei/make-data-item "COMPID" 
                               (vfei/make-array-format :a 25)
                               ["USR.1"
                                "USR.2"
                                "USR.3"
                                "USR.4"
                                "USR.5"
                                "USR.6"
                                "USR.7"
                                "USR.8"
                                "USR.9"
                                "USR.10"
                                "USR.11"
                                "USR.12"
                                "USR.13"
                                "USR.14"
                                "USR.15"
                                "USR.16"
                                "USR.17"
                                "USR.18"
                                "USR.19"
                                "USR.20"
                                "USR.21"
                                "USR.22"
                                "USR.23"
                                "USR.24"
                                "USR.25"])]
         (vfei/parse-vfei "COMPID/A[25]=[\"USR.1\" \"USR.2\" \"USR.3\" \"USR.4\" \"USR.5\" \"USR.6\" \"USR.7\" \"USR.8\" \"USR.9\" \"USR.10\" \"USR.11\" \"USR.12\" \"USR.13\" \"USR.14\" \"USR.15\" \"USR.16\" \"USR.17\" \"USR.18\" \"USR.19\" \"USR.20\" \"USR.21\" \"USR.22\" \"USR.23\" \"USR.24\" \"USR.25\"]")))

  (is (= [(vfei/make-data-item "CMD" :a "executeCommand")
             (vfei/make-data-item "CT" :i4 50000)
             (vfei/make-data-item "TX" :a "Kein Wert fÃ¼r Item CONTENTS vorhanden.")
             (vfei/make-data-item "MODULE" :a "commandExecuter")
             (vfei/make-data-item "APPLICATION" :a "test-test")
             (vfei/make-data-item "INSTANCE":a "0")]

         (vfei/parse-vfei
          "CMD/A=\"executeCommand\" CT/I4=50000 TX/A=\"Kein Wert fÃ¼r Item CONTENTS vorhanden.\" MODULE/A=\"commandExecuter\" APPLICATION/A=\"test-test\" INSTANCE/A=\"0\"")))

  (is (= [(vfei/make-data-item "USR0" :n nil)
           (vfei/make-data-item "USR1" :n 1.5)
           (vfei/make-data-item "USR2" :n 0.5)
           (vfei/make-data-item "USR3" :n 50000.0)]
         (vfei/parse-vfei "USR0/N=null
USR1/N=1.5
USR2/N=.5
USR3/N=.5e5")))

  (is (= [(vfei/make-data-item "CMD" :a "CHANGE")
          (vfei/make-data-item "EVENT_ID" :a "CHANGE")
          (vfei/make-data-item "ACTION" :a "CHANGE")
          (vfei/make-data-item "EVREASON" :a "11")
          (vfei/make-data-item "OLDSTATE" :a "RUN")
          (vfei/make-data-item "NEWSTATE" :a "WAIT")
          (vfei/make-data-item "LIST_1" 
                               (vfei/make-list-format 21) 
                               [(vfei/make-data-item "$VERSION" :a "1.0")
                                (vfei/make-data-item "$COUNTER" :a "1")
                                (vfei/make-data-item "$INTEGRITY" :a "1")
                                (vfei/make-data-item "$RANDOM" :a "9173289511")])
          (vfei/make-data-item "CATEGORY" :a "ABC123")]
         (vfei/parse-vfei "CMD/A=\"CHANGE\" EVENT_ID/A=\"CHANGE\" ACTION/A=\"CHANGE\" EVREASON/A=\"11\"OLDSTATE/A=\"RUN\" NEWSTATE/A=\"WAIT\" LIST_1/L[21]=[$VERSION/A=\"1.0\" $COUNTER/A=\"1\" $INTEGRITY/A=\"1\" $RANDOM/A=\"9173289511\"] CATEGORY/A=\"ABC123\"")))

)
         
(deftest vfei-encode-string
  (is (= "\"abc\""
         (vfei/vfei-encode-string "abc")))
  (is (= "\"äöüß\""
         (vfei/vfei-encode-string "äöüß")))
  ;; Note: no quoting of newline to match systema's decoder:
  (is (= "\"a\nb\nc\""
         (vfei/vfei-encode-string "a\nb\nc")))
  (is (= "\"\\a\\b\\f\n\\r\\t\\v\\\\'\\\"?\\003\""
         (vfei/vfei-encode-string
          (str \u0007
               \u0008
               \u000c
               \u000a
               \u000d
               \u0009
               \u000b
               \u005c
               \u0027
               \u0022
               \u003f
               \u0003))))
  (is (= "null" (vfei/vfei-encode-string nil))))

(defn- ascii<127?
  [s]
  (every? (fn [c]
            (< (int c) 127))
          s))

(deftest vfei-encode-decode-string
  (is
   (quickcheck
    (qc/property [s ascii-string]
                 (qc/==> (ascii<127? s)
                         (= [s ""]
                            (vfei/decode-vfei-string (vfei/vfei-encode-string s))))))))
