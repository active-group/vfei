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
       [(vfei/make-data-item "CMD" :a "Reply.executeTPCommand")
        (vfei/make-data-item "ECD" :i4 0)
        (vfei/make-data-item "ETX" :a "")
        (vfei/make-data-item "MODULE" :a "executeTPCommand")
        (vfei/make-data-item "APPLICATION" :a "promisgateway")
        (vfei/make-data-item "CONTENTS" (vfei/make-list-format 1)
                             [(vfei/make-data-item "TP_COMMAND_REPLY" :a
                                                   "SUCCESS
TOOL1
TOOL2
")])
        (vfei/make-data-item "INSTANCE" :a "0")]
       (vfei/parse-vfei "CMD/A=\"Reply.executeTPCommand\"
ECD/I4=0
ETX/A=\"\"
MODULE/A=\"executeTPCommand\"
APPLICATION/A=\"promisgateway\"
CONTENTS/L[1]=[
    TP_COMMAND_REPLY/A=\"SUCCESS
TOOL1
TOOL2
\"
]
INSTANCE/A=\"0\"")))

  (is (= [(vfei/make-data-item "ECD" :i4 -5)]
         (vfei/parse-vfei "ECD/I4=-5")))

  (is (= [(vfei/make-data-item "COMPID" 
                               (vfei/make-array-format :a 25)
                               ["UN503.1"
                                "UN503.2"
                                "UN503.3"
                                "UN503.4"
                                "UN503.5"
                                "UN503.6"
                                "UN503.7"
                                "UN503.8"
                                "UN503.9"
                                "UN503.10"
                                "UN503.11"
                                "UN503.12"
                                "UN503.13"
                                "UN503.14"
                                "UN503.15"
                                "UN503.16"
                                "UN503.17"
                                "UN503.18"
                                "UN503.19"
                                "UN503.20"
                                "UN503.21"
                                "UN503.22"
                                "UN503.23"
                                "UN503.24"
                                "UN503.25"])]
         (vfei/parse-vfei "COMPID/A[25]=[\"UN503.1\" \"UN503.2\" \"UN503.3\" \"UN503.4\" \"UN503.5\" \"UN503.6\" \"UN503.7\" \"UN503.8\" \"UN503.9\" \"UN503.10\" \"UN503.11\" \"UN503.12\" \"UN503.13\" \"UN503.14\" \"UN503.15\" \"UN503.16\" \"UN503.17\" \"UN503.18\" \"UN503.19\" \"UN503.20\" \"UN503.21\" \"UN503.22\" \"UN503.23\" \"UN503.24\" \"UN503.25\"]")))

  (is (= [(vfei/make-data-item "CMD" :a "Reply.executeTPCommand")
             (vfei/make-data-item "ECD" :i4 50000)
             (vfei/make-data-item "ETX" :a "Kein Wert fÃ¼r Item TP_COMMAND_CONTENT vorhanden.")
             (vfei/make-data-item "MODULE" :a "executeTPCommand")
             (vfei/make-data-item "APPLICATION" :a "promisgateway-test")
             (vfei/make-data-item "INSTANCE":a "0")]

         (vfei/parse-vfei
          "CMD/A=\"Reply.executeTPCommand\" ECD/I4=50000 ETX/A=\"Kein Wert fÃ¼r Item TP_COMMAND_CONTENT vorhanden.\" MODULE/A=\"executeTPCommand\" APPLICATION/A=\"promisgateway-test\" INSTANCE/A=\"0\"")))

  (is (= [(vfei/make-data-item "USR0" :n nil)
           (vfei/make-data-item "USR1" :n 1.5)
           (vfei/make-data-item "USR2" :n 0.5)
           (vfei/make-data-item "USR3" :n 50000.0)]
         (vfei/parse-vfei "USR0/N=null
USR1/N=1.5
USR2/N=.5
USR3/N=.5e5")))

  (is (= [(vfei/make-data-item "CMD" :a "LOTCHANGEETM")
          (vfei/make-data-item "EVENT_ID" :a "LOTCHANGEETM")
          (vfei/make-data-item "ACTION" :a "CHANGELOTETM")
          (vfei/make-data-item "EVREASON" :a "11")
          (vfei/make-data-item "LOTID" :a "C39915.1")
          (vfei/make-data-item "LOTTYPE" :a "A7")
          (vfei/make-data-item "OLDPRODAREA" :a "FAB2")
          (vfei/make-data-item "NEWPRODAREA" :a "FAB2")
          (vfei/make-data-item "OLDPARTID" :a "TIG580CF8_9.01")
          (vfei/make-data-item "NEWPARTID" :a "TIG580CF8_9.01")
          (vfei/make-data-item "OLDCURMAINQTY" :a "25.000")
          (vfei/make-data-item "NEWCURMAINQTY" :a "25.000")
          (vfei/make-data-item "OLDLOTSTATE" :a "RUN")
          (vfei/make-data-item "NEWLOTSTATE" :a "WAIT")
          (vfei/make-data-item "OLDEQPTYPE" :a "8_ETCH")
          (vfei/make-data-item "NEWEQPTYPE" :a "8_SP200")
          (vfei/make-data-item "OLDEQPID" :a "NT217")
          (vfei/make-data-item "NEWEQPID" :a nil)
          (vfei/make-data-item "OLDCAPABILITY" :a "8_LOX")
          (vfei/make-data-item "NEWCAPABILITY" :a "8_SL")
          (vfei/make-data-item "OLDRECPID" :a "L7SI1.01")
          (vfei/make-data-item "NEWRECPID" :a "SLSI2.01")
          (vfei/make-data-item "OLDSTAGE" :a "SI-LAM-OX")
          (vfei/make-data-item "NEWSTAGE" :a "SI-LAM-OX")
          (vfei/make-data-item "LIST_1" 
                               (vfei/make-list-format 21) 
                               [(vfei/make-data-item "$AUTODATA" :a "1.0")
                                (vfei/make-data-item "$COMPASS" :a "1")
                                (vfei/make-data-item "$DNASS1" :a "NN221")
                                (vfei/make-data-item "$DRECP1" :a "DTS08")
                                (vfei/make-data-item "$DROHR1" :a "ND312_1")
                                (vfei/make-data-item "$DROHR2" :a "ND316_3")
                                (vfei/make-data-item "$EPLWAF" :a "LASER_25")
                                (vfei/make-data-item "$ERG1" :a "527.1")
                                (vfei/make-data-item "$ERG2" :a "527.11")
                                (vfei/make-data-item "$ERG3" :a "528.31")
                                (vfei/make-data-item "$ERG4" :a "530.48")
                                (vfei/make-data-item "$ERG5" :a "530.86")
                                (vfei/make-data-item "$ERG6" :a "528.33")
                                (vfei/make-data-item "$ERG7" :a "527.27")
                                (vfei/make-data-item "$ERG8" :a "529.04")
                                (vfei/make-data-item "$ERG9" :a "527.97")
                                (vfei/make-data-item "$FAHRTNR" :a "9163446")
                                (vfei/make-data-item "$INTEGRITY" :a "1")
                                (vfei/make-data-item "$LAMOX" :a "NT217")
                                (vfei/make-data-item "$MATTSONPORT" :a "Linksaussen")
                                (vfei/make-data-item "$RUNID_OVERLAY" :a "9173289@NF511")])
          (vfei/make-data-item "PARTCATEGORY14" :a "A07S_F")]
         (vfei/parse-vfei "CMD/A=\"LOTCHANGEETM\" EVENT_ID/A=\"LOTCHANGEETM\" ACTION/A=\"CHANGELOTETM\" EVREASON/A=\"11\" LOTID/A=\"C39915.1\" LOTTYPE/A=\"A7\" OLDPRODAREA/A=\"FAB2\" NEWPRODAREA/A=\"FAB2\" OLDPARTID/A=\"TIG580CF8_9.01\" NEWPARTID/A=\"TIG580CF8_9.01\" OLDCURMAINQTY/A=\"25.000\" NEWCURMAINQTY/A=\"25.000\" OLDLOTSTATE/A=\"RUN\" NEWLOTSTATE/A=\"WAIT\" OLDEQPTYPE/A=\"8_ETCH\" NEWEQPTYPE/A=\"8_SP200\" OLDEQPID/A=\"NT217\" NEWEQPID/A=null OLDCAPABILITY/A=\"8_LOX\" NEWCAPABILITY/A=\"8_SL\" OLDRECPID/A=\"L7SI1.01\" NEWRECPID/A=\"SLSI2.01\" OLDSTAGE/A=\"SI-LAM-OX\" NEWSTAGE/A=\"SI-LAM-OX\" LIST_1/L[21]=[$AUTODATA/A=\"1.0\" $COMPASS/A=\"1\" $DNASS1/A=\"NN221\" $DRECP1/A=\"DTS08\" $DROHR1/A=\"ND312_1\" $DROHR2/A=\"ND316_3\" $EPLWAF/A=\"LASER_25\" $ERG1/A=\"527.1\" $ERG2/A=\"527.11\" $ERG3/A=\"528.31\" $ERG4/A=\"530.48\" $ERG5/A=\"530.86\" $ERG6/A=\"528.33\" $ERG7/A=\"527.27\" $ERG8/A=\"529.04\" $ERG9/A=\"527.97\" $FAHRTNR/A=\"9163446\" $INTEGRITY/A=\"1\" $LAMOX/A=\"NT217\" $MATTSONPORT/A=\"Linksaussen\" $RUNID_OVERLAY/A=\"9173289@NF511\"] PARTCATEGORY14/A=\"A07S_F\"")))

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
