\version "2.24.4"

#(set-object-property! 'barline-duration-line-start 'backend-type? ly:grob?)
#(set-object-property!
  'barline-duration-line-start
  'backend-doc
  "BarLine grob used as the visual start of a DurationLine.")
#(set-object-property! 'barline-duration-line-start-column 'backend-type? ly:grob?)
#(set-object-property!
  'barline-duration-line-start-column
  'backend-doc
  "PaperColumn parent of the BarLine used as the visual start of a DurationLine.")

#(define (barline-origin-in-column barline barline-column)
   (- (interval-end (ly:grob-extent barline barline-column X))
      (interval-end (ly:grob-extent barline barline X))))

#(define (barline-right-edge-at-staff-center barline barline-column)
   (let ((skyline-pair (ly:grob-property barline 'horizontal-skylines #f)))
     (if skyline-pair
         (let* ((right-skyline (cdr skyline-pair))
                (padded-skyline (ly:skyline-pad right-skyline 0.02))
                (origin (barline-origin-in-column barline barline-column))
                (skyline-height (ly:skyline-height padded-skyline 0)))
           (+ origin skyline-height))
         (let* ((origin (barline-origin-in-column barline barline-column))
                (stencil (ly:grob-property barline 'stencil #f))
                (stencil-right
                 (if (ly:stencil? stencil)
                     (interval-end (ly:stencil-extent stencil X))
                     (interval-end (ly:grob-extent barline barline X))))
                (edge (+ origin stencil-right)))
           edge))))

#(define (duration-line-left-info-from-barline grob)
   (let* ((left-bound (ly:spanner-bound grob LEFT))
          (left-column (ly:item-get-column left-bound))
          (barline (ly:grob-property grob 'barline-duration-line-start #f))
          (barline-column
           (or (ly:grob-property grob 'barline-duration-line-start-column #f)
               (and (ly:grob? barline) (ly:item-get-column barline))))
          (common
           (and (ly:grob? barline-column)
                (ly:grob-common-refpoint left-column barline-column X))))
     (if (and (ly:grob? barline)
              (ly:grob? common)
              (unbroken-or-first-broken-spanner? grob))
         (let* ((system (ly:grob-system left-bound))
                (left-x-system (ly:grob-relative-coordinate left-bound system X))
                (left-column-x-common
                 (ly:grob-relative-coordinate left-column common X))
                (left-x-column
                 (ly:grob-relative-coordinate left-bound left-column X))
                (barline-column-x-common
                 (ly:grob-relative-coordinate barline-column common X))
                (barline-right-delta
                 (- (+ barline-column-x-common
                       (barline-right-edge-at-staff-center
                        barline
                        barline-column))
                    (+ left-column-x-common left-x-column)))
                (bound-details (ly:grob-property grob 'bound-details '()))
                (left-details (assoc-get 'left bound-details '())))
           `((X . ,(+ left-x-system barline-right-delta))
             (padding . ,(assoc-get 'padding left-details 0))
             (start-at-dot . ,(assoc-get 'start-at-dot left-details #f))))
         (ly:horizontal-line-spanner::calc-left-bound-info grob))))

#(define (Barline_duration_line_anchor_engraver context)
   (make-engraver
    (acknowledgers
     ((duration-line-interface engraver grob source-engraver)
      (let ((barline (ly:context-property context 'currentBarLine #f)))
        (when barline
          (ly:grob-set-property!
           grob
           'barline-duration-line-start
           barline)
          (ly:grob-set-property!
           grob
           'barline-duration-line-start-column
           (ly:item-get-column barline))))))))

magneticaDaBarline =
#(define-music-function (music y) (ly:music? number?)
 #{
   \once \hide NoteHead
   \once \hide Stem
   \once \hide Dots
   \once \omit Flag
   \once \override NoteHead.no-ledgers = ##t
   \once \override NoteHead.staff-position = #0
   \once \override DurationLine.Y-offset = #y
   \once \override DurationLine.left-bound-info = #duration-line-left-info-from-barline
   \once \override DurationLine.bound-details.left.padding = #-0.02
   %\once \override DurationLine.bound-details.right.padding = #0
   \once \override DurationLine.thickness = #6
   $music
   \-
 #})

#(define (duration-note-glyph duration)
   (let ((direction (if (= (ly:duration-log duration) 0) 0 UP)))
     #{ \markup \note #duration #direction #}))

#(define (shorter-duration? a b)
   (ly:moment<? (ly:duration-length a) (ly:duration-length b)))

#(define (duration-glyph-run durations)
   (let loop ((remaining durations)
              (first? #t)
              (markups '()))
     (if (null? remaining)
         (make-concat-markup (reverse markups))
         (loop (cdr remaining)
               #f
               (cons (duration-note-glyph (car remaining))
                     (if first?
                         markups
                         (cons (make-hspace-markup 0.25) markups)))))))

#(define (duration-glyph-list durations)
   (let* ((ordered (sort durations shorter-duration?))
          (inner
           (if (null? (cdr ordered))
               (duration-glyph-run ordered)
               (make-override-markup
               '(direction . -1)
               (make-override-markup
                '(offset . 11)
                (make-override-markup
                 '(height-limit . 0.45)
                 (make-override-markup
                  '(shorten-pair . (1.0 . 1.0))
                  (make-tie-markup (duration-glyph-run ordered)))))))))
     (make-fontsize-markup
      -2
      (make-concat-markup
       (list (make-simple-markup "( ") inner (make-simple-markup " )"))))))

#(define duration-candidates
   (let ((durations '()))
     (do ((log 0 (+ log 1))) ((> log 8))
       (do ((dots 0 (+ dots 1))) ((> dots 2))
         (let ((duration (ly:make-duration log dots)))
           (when (moment<=? (ly:duration-length duration) (ly:make-moment 1))
             (set! durations (cons duration durations))))))
     (sort durations
           (lambda (a b)
             (ly:moment<? (ly:duration-length b) (ly:duration-length a))))))

#(define (moment->durations moment)
   (let loop ((remaining moment) (durations '()))
     (if (moment<=? remaining ZERO-MOMENT)
         (reverse durations)
         (let ((duration (find
                          (lambda (candidate)
                            (moment<=? (ly:duration-length candidate) remaining))
                          duration-candidates)))
           (if duration
               (loop (ly:moment-sub remaining (ly:duration-length duration))
                     (cons duration durations))
               (ly:error "barlineDurationLine: durata non rappresentabile"))))))

#(define (moment-note moment show-duration?)
   (let ((duration (make-duration-of-length moment)))
     (if show-duration?
         #{ \fixed c' { b$duration ^$(duration-glyph-list (moment->durations moment)) } #}
         #{ \fixed c' { b$duration } #})))

#(define (barline-duration-line-tail segments show-duration?)
   (let ((segment (car segments))
         (rest (cdr segments)))
     (if (null? rest)
         #{ $(moment-note segment show-duration?) ~ \grace { \fixed c' { b16 } } #}
         #{ $(moment-note segment show-duration?) ~
            $(barline-duration-line-tail rest show-duration?) #})))

#(define (barline-duration-line-segments total first-segment)
   (if (or (moment<=? total ZERO-MOMENT)
           (moment<=? first-segment ZERO-MOMENT)
           (ly:moment<? (ly:make-moment 1) first-segment))
       (ly:error "barlineDurationLine: usare durate positive; primo segmento massimo 1"))
   (let ((first (moment-min total first-segment))
         (whole (ly:make-moment 1)))
     (let loop ((remaining (ly:moment-sub total first)) (segments (list first)))
       (if (moment<=? remaining ZERO-MOMENT)
           (reverse segments)
           (let ((segment (moment-min remaining whole)))
             (loop (ly:moment-sub remaining segment) (cons segment segments)))))))

#(define (barline-duration-line-body duration y show-duration? first-segment)
   (let* ((segments (barline-duration-line-segments
                     (ly:duration-length duration)
                     (ly:duration-length first-segment)))
          (first (car segments))
          (rest (cdr segments)))
     (if (null? rest)
         #{ \magneticaDaBarline $(moment-note first show-duration?) #y #}
         #{ \magneticaDaBarline $(moment-note first show-duration?) #y ~
            $(barline-duration-line-tail rest show-duration?) #})))

barlineDurationLine =
#(define-music-function (show-duration? duration y first-segment) ((boolean? #f) ly:duration? number? (ly:duration? (ly:make-duration 0 0)))
 #{
   \hide NoteHead
   \hide Stem
   \hide Dots
   \hide Accidental
   \omit Flag
   \omit Tie
   \override NoteHead.no-ledgers = ##t
   \override NoteHead.staff-position = #0
   $(barline-duration-line-body duration y show-duration? first-segment)
   \undo \hide NoteHead
   \undo \hide Stem
   \undo \hide Dots
   \undo \hide Accidental
   \undo \omit Flag
   \undo \omit Tie
   \revert NoteHead.no-ledgers
   \revert NoteHead.staff-position
 #})

\paper {
  #(set-paper-size "a4")
  indent = 32\mm
  %line-width = 170\mm
  ragged-right = ##f
  system-system-spacing.basic-distance = #14
  score-system-spacing.basic-distance = #14
}

\layout {
  \context {
    \Voice
    \consists #Barline_duration_line_anchor_engraver
  }
  \context {
    \Voice
    \consists "Duration_line_engraver"
    \override TextScript.staff-padding = #2.4
    \override DurationLine.arrow-length = #1.5
    \override DurationLine.arrow-width = #1
    \override DurationLine.to-barline = ##t
  }
}

% Casi:
% 1. partenza dopo 1/4, chiusura a fine battuta: 2.
% 2. partenza dopo 2/4, durata 1/2: 2
% 3. partenza dopo 2/4, durata 2/1: 1*2
% 4. partenza dopo 3/4, durata 1/4: 4
% 5. partenza dopo 3/8, durata 5/8: 8*5
% 6. partenza dopo 7/16, durata 13/16: 16*13
% 7. durata senza markup: opzione show-duration omessa
% 8. attraversamento di molte misure e break automatico

\score {
  \new Staff \with { instrumentName = "1. 2." }
  \new Voice = "caso-1" \relative c' {
    \time 4/4
    r4
    \bar ":|."
    \barlineDurationLine ##t 2. 0 2.
    e8 f g4 a2
  }
}

\score {
  \new Staff \with { instrumentName = "2. 2" }
  \new Voice = "caso-2" \relative c' {
    \time 4/4
    r4 c4
    \bar ":|."
    \barlineDurationLine ##t 2 0 2
    e8 f g4 a2
  }
}

\score {
  \new Staff \with { instrumentName = "3. 1*2" }
  \new Voice = "caso-3" \relative c' {
    \time 4/4
    r4
    \bar ".|:"
    c4
    \bar ":|."
    \barlineDurationLine ##t 1*2 0 2
    e8 f g4 a2
  }
}

\score {
  \new Staff \with { instrumentName = "4. 4" }
  \new Voice = "caso-4" \relative c' {
    \time 4/4
    r4 c4 d4
    \bar ":|."
    \barlineDurationLine ##t 4 0 4
    e8 f g4 a2
  }
}

\score {
  \new Staff \with { instrumentName = "5. 8*5" }
  \new Voice = "caso-5" \relative c' {
    \time 4/4
    r8 c8 d8
    \bar ":|."
    \barlineDurationLine ##t 8*5 0 8*5
    e8 f g4 a2
  }
}

\score {
  \new Staff \with { instrumentName = "6. 16*13" }
  \new Voice = "caso-6" \relative c' {
    \time 3/4
    \bar ".|:" r16 c16 d16 e16 f16 g16 a16
    \bar ":|."
    \barlineDurationLine ##t 16*13 0 16*5
    e8 f g4 a2
  }
}

\score {
  \new Staff \with { instrumentName = "7. no mark" }
  \new Voice = "caso-7" \relative c' {
    \time 4/4
    r4 c4
    \bar ":|."
    \barlineDurationLine 1 0 2
    e8 f g4 a2
  }
}

\score {
  \new Staff \with { instrumentName = "8. auto" }
  \new Voice = "caso-8" \relative c' {
    \time 4/4
    r4 c4
    \bar ":|."
    \barlineDurationLine ##t 1*20 0 2
    e8 f g4 a2
  }
}
