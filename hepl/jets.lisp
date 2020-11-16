(defpackage #:urbit/hepl/jets
  (:use #:cl #:urbit/intbytes #:urbit/nock/math #:urbit/hoon/k141 #:ironclad)
  (:export +tree+))

(in-package #:urbit/hepl/jets)

; it is desirable to ship hepl without a dependency on urcrypt
; so we use ironclad to implement the hoon kernel sha jets

(defun shal (len ruz)
  (declare (uint len ruz))
  (bytes->int
    (digest-sequence :sha512 (int->bytes ruz len))
    64))
(defbinary shal+< #'shal)
(defgate +shal #'shal+<)

(defun sha1 (ruz)
  (declare (uint ruz))
  (bytes->int
    (reverse (digest-sequence :sha1 (int->bytes ruz (met 3 ruz))))
    20))
(defunary sha1+< #'sha1)
(defgate +sha1 #'sha1+<)

(defun shay (len ruz)
  (declare (uint len ruz))
  (bytes->int
    (digest-sequence :sha256 (int->bytes ruz len))
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
                  (leaf lore 31 #'+lore))))))))))
