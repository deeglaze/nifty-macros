#lang scribble/manual
@(require scribble/eval 
          racket/sandbox
          (for-label scribble/struct
                     "for-accumulate.rkt"
                     racket/set
                     racket/base))
@(declare-exporting "for-accumulate.rkt")
@(define for-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
         (make-evaluator 'racket/base #:requires (list 'racket/set "for-accumulate.rkt"))))

@title[#:tag "for-accumulate"]{Defined accumulators for @racket[for]}

This library provides support for combining different accumulator
styles in the same form. For example, @racket[for/sum] and
@racket[for/list] can be combined to sum the first value returned in a
@racket[for] body, and make a list out of the second value returned in
that body. Accumulator styles are user-definable and are flexible
enough to support all current @racket[for/*] forms.

@defform/subs[(define-accumulator name [accumulator-id optionals] ...)
              ([optionals #:suppress
                          (code:line #:initial initial-expr)
                          (code:line #:bind bind-id/s)
                          (code:line #:inner inner-expr)
                          (code:line #:post post-expr)]
               [bind-id/s id
                          (id ...)])]{

Defines an accumulation style named @racket[name] that uses the
following accumulator identifiers with optional initial value,
optional post-processing (say to reverse the produced list), and how
to accumulate the value that corresponds to its position, optionally named with @racket[#:bind].

If an accumulation style requires several accumulators to perform the
task, you can suppress some bindings from being in the return values
of @racket[for/accumulate]. At least one accumulator must not be
suppressed.

@examples[#:eval for-eval
(define-accumulator list
    [lst #:initial '() #:bind v #:inner (cons v lst) #:post (reverse lst)])
(define-accumulator hash [h #:initial #hash() #:bind (k v) #:inner (hash-set h k v)])
(define-accumulator union [st #:initial (set) #:bind v #:inner (set-union v st)])
]}

@defform/subs[(for/accumulate (accumulator ...) for-clauses body-or-break ... body)
              ([accumulator [id initial-expr]
                            [optional-ids kw-optionals optional-initial-expr]]
               [optional-ids (code:line)
                             id
                             (id ...)]
               [optional-initial-expr (code:line)
                                      initial-expr]
               [kw-optionals (code:line #:type style-id)
                             (code:line #:initial initial-expr/s)
                             (code:line #:drop)]
               [initial-expr/s initial-expr
                               (initial-expr ...)])]{

Restrictions: an accumulator without @racket[#:type] in
@racket[kw-optionals] must be given in the first form of
@racket[accumulator]. If @racket[id] is specified, there must be the
same number of identifiers as non-suppressed identifiers for the
accumulator style. This restriction also exists for
@racket[#:initial], and additionally, if a style does not give default
initial values, @racket[#:initial] must be used.

An @racket[accumulator] that also has @racket[#:drop] specified will
not return the values of the accumulators as part of the values of the
entire @racket[for/accumulate] form.

The @racket[body] expression is expected to return as many values as
there are non-suppressed identifiers for accumulators.

Here @racket[for-clauses], @racket[body-or-break] and @racket[body] are the same as in @racket[for].
@racket[for/accumulate] is backwards-compatible with @racket[for/fold].

@examples[#:eval for-eval
(for/accumulate ([#:type set]
                 [#:type hash (hash -1 1)])
    ([i 5])
  (values i i (- i)))
(for/accumulate ([#:type sum]
                 [a '()]
                 [#:type prod])
    ([i (in-range 1 5)])
  (values i (cons i a) i))
]}

@defform[(for*/accumulate (accumulator ...) for-clauses body-or-break ... body)]{

Like @racket[for/accumulate], but uses @racket[for*/fold] as the base
iteration form. Backwards-compatible with @racket[for*/fold].}

@close-eval[for-eval]
