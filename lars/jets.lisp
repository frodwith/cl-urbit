(defpackage #:urbit/lars/jets
  (:use #:cl #:urbit/hoon/k141 #:urbit/urcrypt #:urbit/nock/data)
  (:import-from #:urbit/nock/math #:uint #:met)
  (:import-from #:urbit/nock/common #:loob)
  (:export +tree+))

(in-package #:urbit/lars/jets)

; TODO: any calls to urcrypt functions should be wrapped in
;       (sb-sys:without-interrupts) because they are liable to use malloc,
;       and it is expected that lars will receive sigint/alrm as part of its
;       normal operation. Memory leaks could be created if these functions
;       were interrupted in between malloc and free.

(defund shal+< (@@len @@ruz)
  (@ (shal ruz len)))
(defgate +shal #'shal+<)

(defund sha1+< (@@ruz)
  (@ (sha-1 ruz (met 3 ruz))))
(defgate +sha1 #'sha1+<)

(defund shay+< (@@len @@ruz)
  (@ (shay ruz len)))
(defgate +shay #'shay+<)

(defmacro fit-bytes (n expr)
  `(typep ,expr `(unsigned-byte ,,(ash n 3))))

(defmacro hard-bytes (bytes int)
  (let ((tmp (gensym)))
    `(let ((,tmp ,int))
       (if (fit-bytes ,bytes ,tmp)
           ,tmp
           (error 'exit)))))

(defund sign+< (@@message @@seed)
  (@ (ed-sign message (met 3 message) (hard-bytes 32 seed))))
(defgate +sign #'sign+<)

(defund puck+< (@@seed)
  (@ (ed-puck (hard-bytes 32 seed))))
(defgate +puck #'puck+<)

(defund veri+< (@@signature @@message @@public)
  (loob (ed-veri message (met 3 message) public signature)))
(defgate +veri #'veri+<)

(define-condition punt (warning)
  ((name :type string :initarg :name :reader punt-name)))

(defmacro punt (name &body forms)
  (let ((var (gensym)))
    `(let ((,var (progn ,@forms)))
       (unless ,var
         (warn 'punt :name ,name))
       ,var)))

(defund shar+< (@@public @@seed)
  (punt "shar"
    (when (fit-bytes 32 public)
      (@ (ed-shar public (hard-bytes 32 seed))))))
(defgate +shar #'shar+<)

(defund point-add+< (@@a @@b)
  (punt "ed-point-add"
    (when (and (fit-bytes 32 a)
               (fit-bytes 32 b))
      (ed-point-add a b))))
(defgate +point-add #'point-add+<)

(defund scalarmult+< (@@a @@b)
  (punt "ed-scalarmult"
    (when (and (fit-bytes 32 a)
               (fit-bytes 32 b))
      (ed-scalarmult a b))))
(defgate +scalarmult #'scalarmult+<)

(defund scalarmult-base+< (@@a)
  (punt "ed-scalarmult-base"
    (when (and (fit-bytes 32 a))
      (ed-scalarmult-base a))))
(defgate +scalarmult-base #'scalarmult-base+<)

(defund add-scalarmult-scalarmult-base+< (@@a @@a-point @b)
  (punt "ed-add-scalarmult-scalarmult-base"
    (when (and (fit-bytes 32 a)
               (fit-bytes 32 a-point)
               (fit-bytes 32 b))
      (ed-add-scalarmult-scalarmult-base a a-point b))))
(defgate +add-scalarmult-scalarmult-base #'add-scalarmult-scalarmult-base+<)

(defund add-double-scalarmult+< (@@a @@a-point @@b @@b-point)
  (punt "ed-add-double-scalarmult"
    (when (and (fit-bytes 32 a)
               (fit-bytes 32 a-point)
               (fit-bytes 32 b)
               (fit-bytes 32 b-point))
      (ed-add-double-scalarmult a a-point b b-point))))
(defgate +add-double-scalarmult #'add-double-scalarmult+<)

(defparameter +tree+
  (hoon-jet-tree 141
    (layer one
      (~/ add #'+add)
      (~/ dec #'+dec)
      (~/ div #'+div)
      (~/ dvr #'+dvr)
      (~/ gte #'+gte)
      (~/ gth #'+gth)
      (~/ lte #'+lte)
      (~/ lth #'+lth)
      (~/ max #'+max)
      (~/ min #'+min)
      (~/ mod #'+mod)
      (~/ mul #'+mul)
      (~/ sub #'+sub)
      (~/ cap #'+cap)
      (~/ mas #'+mas)
      (~/ peg #'+peg)
      (layer two
        (~/ turn #'+turn)
        (~/ bex #'+bex)
        (~/ can #'+can)
        (~/ cat #'+cat)
        (~/ cut #'+cut)
        (~/ end #'+end)
        (~/ lsh #'+lsh)
        (~/ met #'+met)
        (~/ rap #'+rap)
        (~/ rep #'+rep)
        (~/ rip #'+rip)
        (~/ rsh #'+rsh)
        (~/ con #'+con)
        (~/ dis #'+dis)
        (~/ mix #'+mix)
        (~/ mug #'+mug)
        (~/ dor #'+dor)
        (~/ gor #'+gor)
        (~/ mor #'+mor)
        (~/ flop #'+flop)
        (~/ lent #'+lent)
        (~/ reap #'+reap)
        (~/ jam #'+jam)
        (~/ cue #'+cue)
        (leaf muk 27 #'+muk)
        (~/ weld #'+weld)
        (container in 3
          (~/ has #'+has-in)
          (~/ put #'+put-in)
          (~/ del #'+del-in))
        (container by 3
          (~/ get #'+get-by)
          (~/ put #'+put-by)
          (~/ del #'+del-by))
        (layer tri
          (~/ shal #'+shal)
          (~/ sha1 #'+sha1)
          (~/ shay #'+shay)
          (layer qua
            (~/ trip #'+trip)
            (~/ mink #'+mink)
            (~/ mule #'+mule)
            (layer pen
              (container ut 7
                (~/ crop #'+crop)
                (~/ fish #'+fish)
                (~/ fond #'+fond)
                (~/ fuse #'+fuse)
                (~/ mint #'+mint)
                (~/ mull #'+mull)
                (~/ peek #'+peek)
                (~/ play #'+play)
                (~/ rest #'+rest)
                (container nest 3
                  (container nest-in 3
                    (leaf nest-dext 1 #'+nest)))
                (layer hex
                  (leaf loss 31 #'+loss)
                  (leaf lore 31 #'+lore)
                  (container coed 31
                    (layer ed
                      (~/ sign #'+sign)
                      (~/ puck #'+puck)
                      (~/ veri #'+veri)
                      (~/ shar #'+shar)
                      (~/ point-add #'+point-add)
                      (~/ scalarmult #'+scalarmult)
                      (~/ scalarmult-base #'+scalarmult-base)
                      (~/ add-scalarmult-scalarmult-base
                          #'+add-scalarmult-scalarmult-base)
                      (~/ add-double-scalarmult
                          #'+add-double-scalarmult))))))))))))
