#lang racket/base
(require racket/system)
(define file "for-acc")
;; Added this file because I can never fully remember the proper incantation.
(system (format "scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ ~a.scrbl" file))
(system (format "scribble ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ --markdown ~a.scrbl; mv ~a.md README.md" file file))