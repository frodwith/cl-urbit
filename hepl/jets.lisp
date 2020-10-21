(defpackage #:urbit/hepl/jets
  (:use #:cl #:urbit/hoon/k141 #:ironclad #:cl-intbytes)
  (:import-from #:urbit/nock/math #:uint #:met)
  (:export +tree+))

(in-package #:urbit/hepl/jets)

; it is desirable to ship hepl without a dependency on urcrypt
; so we use ironclad to implement the hoon kernel sha jets

(defun shal (len ruz)
  (declare (uint len ruz))
  (octets->uint
    (digest-sequence :sha512 (int->octets ruz len))
    64))
(defbinary shal+< #'shal)
(defgate +shal #'shal+<)

(defun shan (ruz)
  (declare (uint ruz))
  (octets->uint
    (reverse (digest-sequence :sha1 (int->octets ruz (met 3 ruz))))
    20))
(defunary shan+< #'shan)
(defgate +shan #'shan+<)

(defun shay (len ruz)
  (declare (uint len ruz))
  (octets->uint
    (digest-sequence :sha256 (int->octets ruz len))
    32))
(defbinary shay+< #'shay)
(defgate +shay #'shay+<)

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
        (layer tri
          (~/ shal #'+shal)
          (~/ shan #'+shan)
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
                  (leaf leer 31 #'+leer))))))))))
