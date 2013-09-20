#lang racket
(require "for-acc.rkt")

(for/acc ([#:type list]) ([i 10]) i)