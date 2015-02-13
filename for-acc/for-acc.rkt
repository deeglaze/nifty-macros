#lang racket/base

(provide for/acc for*/acc
         let/for/acc let/for*/acc
         define/for/acc define/for*/acc
         define-accumulator)

;; Allow mix and match of accumulation styles, like for/list and for/set
;; for two different accumulators.

(require (for-syntax racket/base syntax/parse racket/syntax
                     syntax/for-body
                     syntax/id-table
                     racket/pretty
                     (only-in racket/dict in-dict-keys)
                     (only-in racket/bool implies)
                     racket/match))

(begin-for-syntax
 (define for/fold-types (make-free-id-table))
 ;; suppress/drop is (one-of 'suppress 'drop #f)
 (struct accumulator (id op-initial suppress/drop binds op-inner op-post) #:prefab)
 (struct folding-data (accumulators num-exported) #:prefab))

;; Each "accumulator" may have multiple accumulating bindings.
(define-syntax (define-accumulator stx)
  (syntax-parse stx
    [(_ name:id [acc:id
                 ;; Helper identifiers won't be in the result of the for. Suppress them.
                 ;; They are not just dropped: they don't count for the number of values the body
                 ;; should return.
                 (~or (~optional (~and #:suppress suppress?))
                      (~optional (~seq #:initial init:expr))
                      ;; Give the body's result(s) for this position (a) name(s) to use in inner.
                      (~optional (~seq #:bind (~or (~and bind:id (~bind [(binds 1) (list #'bind)]))
                                                   (binds:id ...)))
                                 #:defaults ([(binds 1) '()]))
                      (~optional (~seq #:inner inner:expr))
                      ;; Suppressed bindings may post-process for side-effect and bind nothing.
                      (~optional (~seq #:post post:expr))) ...] ...)
     #:fail-unless (memv (syntax-local-context) '(top-level module))
     "Can only define for types at the (module) top-level"
     #:fail-unless (for/or ([s (in-list (attribute suppress?))]) (not s))
     "Cannot suppress all accumulators"
     #:fail-when (for/or ([s (in-list (attribute suppress?))]
                          [bs (in-list (attribute binds))])
                   (and s (pair? bs)))
     "Suppressed bindings are hidden; cannot use #:bind."
     #:fail-when (for/or ([s (in-list (attribute suppress?))]
                          [i (in-list (attribute init))])
                   (and s (not i)))
     "Suppressed bindings must have an initial value."
     #:do [(define dups (check-duplicate-identifier (attribute acc)))]
     #:fail-when dups
     (format "Duplicate accumulator identifiers ~a" dups)

     (define accumulators
       (map accumulator
            (attribute acc)
            (attribute init)
            (map syntax? (attribute suppress?))
            (attribute binds)
            (attribute inner)
            (attribute post)))
     (define num-exported (for/sum ([s (attribute suppress?)] #:unless s) 1))
     (define (syntaxify a)
       (match-define (accumulator id initial sd binds op-inner op-post) a)
       (with-syntax ([(b ...) binds])
        #`(accumulator #'#,id
                       #'#,(or initial #'(quote #f))
                       ;; can't define an accumulator with a drop quality. Only suppress.
                       #,(and sd #'(quote suppress))
                       (list #'b ...)
                       #,(and op-inner #`(syntax #,op-inner))
                       #,(and op-post #`(syntax #,op-post)))))
     (with-syntax ([(a ...) (map syntaxify accumulators)])
       #`(begin-for-syntax
          (free-id-table-set! for/fold-types
                              #'name
                              (folding-data (list a ...) #,num-exported))))]))

(begin-for-syntax
 (define-syntax-class (folding orig-stx)
   #:attributes ((accs 1) introducer)
   (pattern [(~optional (~describe #:opaque "accumulator name(s)"
                                   (~or (~and g-acc:id (~bind [(g-accs 1) (list #'g-acc)]))
                                        (g-accs:id ...))))
             (~or (~optional (~seq #:type (~describe #:opaque "accumulation type" type:id)))
                  ;; suppressed accumulators cannot be named here, so no interface to it.
                  (~optional (~and #:drop drop))
                  (~optional (~seq #:initial
                                   ;; TODO: kinda sucks syntactically requiring `values` 
                                   (~describe #:opaque "initial value(s)"
                                              (~or ((~literal values) inits:expr ...)
                                                   (~and g-init:expr
                                                         (~bind [(inits 1) (list #'g-init)]))))))
                  (~optional (~seq #:named-initial
                                   (~describe #:opaque "initial value(s) for given accumulators"
                                              ([acc-init:id named-init:expr] ...)))))
             ...
             (~optional (~describe #:opaque "initial value" p-init:expr))]
            #:fail-when (and (not (attribute type)) (not (attribute g-accs)))
            "Accumulator without an accumulation type requires an identifier to bind"
            #:fail-when (and (not (attribute type)) (not (or (attribute inits) (attribute p-init))))
            "Accumulator without an accumulation type requires an initial value"
            #:fail-when (and (attribute inits) (attribute p-init))
            "Cannot specify initial value positionally and with #:initial."
            #:do [(define num-g-accs (length (or (attribute g-accs) '())))]
            #:fail-when (and (attribute p-init) (> num-g-accs 1))
            "Cannot give multiple initial values positionally. Must use #:initial (values v ...)"
            #:fail-when (and (attribute inits) (attribute acc-init))
            "Cannot specify initial value(s) positionally and by name."
            #:do [(define (bad)
                    (raise-syntax-error #f (format "Unknown accumulator type ~a" (syntax-e #'type))
                                        #'type))
                  (define acc-ids (make-free-id-table))
                  ;; Create first pass of accumulators used, either named or by #:type.
                  (define-values (accumulators num-exported)
                    (if (attribute type)
                        (match (free-id-table-ref for/fold-types #'type bad)
                          [(folding-data a n)
                           ;; Populate names of accumulators.
                           (for ([acc (in-list a)])
                             (free-id-table-set! acc-ids (accumulator-id acc) #t))
                           (values a n)])
                        (values (for/list ([g (in-list (attribute g-accs))]
                                           [i (in-list (or (attribute inits)
                                                           (list (attribute p-init))))])
                                  (free-id-table-set! acc-ids g #t)
                                  (accumulator g
                                               i
                                               (and (attribute drop) 'drop)
                                               (generate-temporaries (list g))
                                               #f
                                               #f))
                                num-g-accs)))
                  (define named-dups (check-duplicate-identifier (or (attribute acc-init) '())))
                  (define unknown-named-inits
                    (if (attribute acc-init)
                        (for/list ([aid (in-list (attribute acc-init))]
                                   #:unless (free-id-table-ref acc-ids aid #f))
                          aid)
                        '()))]
            #:fail-when named-dups
            (format "Duplicate named initial values for accumulators: ~a" named-dups)
            #:fail-unless (null? unknown-named-inits)
            (format "Initial value given for unknown accumulators: ~a" unknown-named-inits)
            #:fail-unless (implies (attribute g-accs) (= num-g-accs num-exported))
            (format "Type-exported bindings arity-mismatch in binders. Given ~a, expect ~a"
                    num-g-accs num-exported)
            #:fail-unless (implies (attribute inits) (= (length (attribute inits)) num-exported))
            (format "Type-exported bindings arity-mismatch in inital values. Given ~a, expect ~a"
                    (length (attribute inits)) num-exported)

            #:do [ ;; Associate initial expressions with accumulator identifier
                  (define acc-init-table (make-free-id-table))
                  ;; Initial values are given positionally or nominally?
                  (cond
                   [(attribute inits)
                    (for ([acc (in-list accumulators)]
                          [i (in-list (attribute inits))])
                      (free-id-table-set! acc-init-table (accumulator-id acc) i))]
                   [(attribute acc-init)
                    ;; Nominally
                    (for ([aid (in-list (attribute acc-init))]
                          [i (in-list (attribute named-init))])
                      (free-id-table-set! acc-init-table aid i))]
                   ;; only initial values are the ones from the type itself.
                   [else
                    (for ([acc (in-list accumulators)])
                      (free-id-table-set! acc-init-table
                                          (accumulator-id acc) 
                                          (accumulator-op-initial acc)))])
                  ;; Find if we have filled in all the missing initial values
                  (define undefined-initial-values
                    (for/list ([aid (in-dict-keys acc-ids)]
                               #:unless (free-id-table-ref acc-init-table aid #f))
                      aid))]
            #:fail-unless (null? undefined-initial-values)
            (let* ([plural? (pair? (cdr undefined-initial-values))]
                   [many (if plural? "s" "")])
              (format "Accumulator~a ~a unspecified initial value~a"
                      many (if plural? "have" "has") many))

            #:do [ ;; If we don't give accumulator ids, we want a fresh accumulator id each time
                  ;; we refer to the accumulation style
                  ;; Example: (for/acc ([#:type list] [#:type list]) ([x 5]) (values x x))
                  ;; => (values '(0 1 2 3 4) '(0 1 2 3 4))
                  ;; Without the introducer, we have a duplicated identifier in the clauses.
                  (define intro (make-syntax-introducer))
                  (define new-accs
                    (or (attribute g-accs)
                        (map (compose intro accumulator-id) accumulators)))
                  ;; We need to reinterpret user-given idents (or freshen idents) as the originals
                  ;; in order for the looked-up syntax to match.
                  (define (interp op-stx binds)
                    (and op-stx
                         (with-syntax ([(na ...) new-accs]
                                       [(oa ...)
                                        (map accumulator-id accumulators)]
                                       [(ob ...) binds]
                                       [(nb ...) (map intro binds)])
                           (quasisyntax/loc orig-stx
                              (let-syntax ([oa (make-rename-transformer #'na)] ...
                                           [ob (make-rename-transformer #'nb)] ...)
                                #,op-stx)))))]

            #:attr (accs 1)
            ;; Now override defaults with provided forms.
            (for/list ([acc (in-list accumulators)]
                       [aid (in-list new-accs)])
              (match-define (accumulator id initial sd binds op-inner op-post) acc)
              (accumulator aid initial
                           (or sd (and (attribute drop) 'drop))
                           (map intro binds)
                           (interp op-inner binds)
                           (interp op-post binds)))
            #:attr introducer intro)))

(define-syntax (for/acc-aux stx)
  (syntax-parse stx
    [(_ folder
        (~optional (~seq #:in-post-context in-post-form:id (in-post:expr ...)))
        ((~var accss (folding stx)) ...) guards . unsplit-body)
     (define/with-syntax ((pre-body ...) (post-body ...))
       (split-for-body #'stx #'unsplit-body))
     ;; Suppressed bindings are hidden values of the iteration.
     ;; Suppressing/dropping bindings requires a let binding, as does post-processing.
     ;; Binding order is the order that values must be given in values.
     (define has-involved-post?
       (for*/or ([accs (in-list (attribute accss.accs))]
                 [acc (in-list accs)])
         (or (accumulator-suppress/drop acc)
             (accumulator-op-post acc))))
     (define has-post?
       (or (attribute in-post)
           has-involved-post?))
     ;; Only add (let-values ([(x ...) ...]) ...) if there is a need to transform them.
     (define has-inner? (for*/or ([accs (in-list (attribute accss.accs))]
                                 [acc (in-list accs)])
                          (accumulator-op-inner acc)))
     (with-syntax ([(expected-ids ...) ;; dropped ids count
                    (reverse
                     (for*/fold ([ids '()]) ([accs (in-list (attribute accss.accs))])
                       (for/fold ([ids ids]) ([acc (in-list accs)])
                         (cond
                          [(eq? (accumulator-suppress/drop acc) 'suppress) ids]
                          ;; The binders should all have the introducer applied.
                          [else (append (reverse (accumulator-binds acc)) ids)]))))])
       (with-syntax ([(return-values ...)
                      (reverse
                       (for*/fold ([rvs '()])
                           ([accs (in-list (attribute accss.accs))]
                            [acc (in-list accs)])
                         ;; If no inner expr given, then
                         ;; if binding suppressed, just use the accumulator identifier,
                         ;; otherwise, use the expected-id.
                         (cond
                          ;; Inner expressions should refer to the renamed binders.
                          [(accumulator-op-inner acc) => (λ (i) (cons i rvs))]
                          [(eq? 'suppress (accumulator-suppress/drop acc))
                           (cons (accumulator-id acc) rvs)]
                          [else
                           (append (reverse (accumulator-binds acc)) rvs)])))])
         (define loop-body
           (if has-inner?
               (quasisyntax/loc stx
                 (let-values ([(expected-ids ...) (let () post-body ...)])
                   (values return-values ...)))
               #'(let () post-body ...)))
         (define loop
           (with-syntax ([(clauses ...)
                          (for*/list ([accs (in-list (attribute accss.accs))]
                                      [acc (in-list accs)])
                            (quasisyntax/loc stx [#,(accumulator-id acc)
                                                  #,(accumulator-op-initial acc)]))])
            (quasisyntax/loc stx
              (folder #,stx (clauses ...) guards pre-body ... #,loop-body))))
         (define (general-post all-of-it)
           (if (attribute in-post)
               (quasisyntax/loc stx
                 (in-post-form ([#,(for*/list
                                       ([accs (in-list (attribute accss.accs))]
                                        [acc (in-list accs)]
                                        #:unless (accumulator-suppress/drop acc))
                                     (accumulator-id acc))
                                 #,all-of-it])
                               in-post ...))
               all-of-it))
         (if has-post?
             (with-syntax ([(aids ...) (for*/list ([accs (in-list (attribute accss.accs))]
                                                   [acc (in-list accs)])
                                         (accumulator-id acc))])
               (cond
                [has-involved-post?
                 (general-post
                  (quasisyntax/loc stx
                    (let-values ([(aids ...) #,loop])
                      ;; Suppressed bindings get their post-processing run for side-effect.
                      #,@(for*/list
                             ([accs (in-list (attribute accss.accs))]
                              [acc (in-list accs)]
                              #:when (and (accumulator-op-post acc)
                                          (eq? 'suppress (accumulator-suppress/drop acc))))
                           (accumulator-op-post acc))
                      ;; Finally, only return unsuppressed/dropped accumulators.
                      (values #,@(for*/list
                                     ([accs (in-list (attribute accss.accs))]
                                      [acc (in-list accs)]
                                      #:unless (accumulator-suppress/drop acc))
                                   (or (accumulator-op-post acc)
                                       (accumulator-id acc)))))))]

                [else
                 ;; Easy post. Just bind and go.
                 (quasisyntax/loc stx (in-post-form ([(aids ...) #,loop]) in-post ...))]))
             ;; No post. Just loop.
             loop)))]))

(define-syntax-rule (for/acc accs guards body1 body ...)
  (for/acc-aux for/fold/derived accs guards body1 body ...))
(define-syntax-rule (for*/acc accs guards body1 body ...)
  (for/acc-aux for*/fold/derived accs guards body1 body ...))

(define-syntax-rule (let/for/acc (accs guards body1 body ...)
                                 lbody1 lbody ...)
  (for/acc-aux for/fold/derived
               #:in-post-context let-values ((let () lbody1 lbody ...))
               accs guards body1 body ...))
(define-syntax-rule (let/for*/acc (accs guards body1 body ...)
                                 lbody1 lbody ...)
  (for/acc-aux for*/fold/derived
               #:in-post-context let-values ((let () lbody1 lbody ...))
               accs guards body1 body ...))

(define-syntax-rule (rejigger-post-to-define ([(id ...) e]))
  (define-values (id ...) e))

(define-syntax-rule (define/for/acc accs guards body1 body ...)
  (for/acc-aux for/fold/derived
               #:in-post-context rejigger-post-to-define ()
               accs guards body1 body ...))

(define-syntax-rule (define/for*/acc accs guards body1 body ...)
  (for/acc-aux for*/fold/derived
               #:in-post-context rejigger-post-to-define ()
               accs guards body1 body ...))

(define-accumulator list
  [lst #:initial '() #:bind v #:inner (cons v lst) #:post (reverse lst)])

(require racket/set)
(define-accumulator set [st #:initial (set) #:bind v #:inner (set-add st v)])
(define-accumulator sum [sm #:initial 0 #:bind v #:inner (+ v sm)])
(define-accumulator prod [pd #:initial 1 #:bind v #:inner (* v pd)])
;; One accumulator that expects the body expression to return two values for it (positionally)
(define-accumulator hash [h #:initial #hash() #:bind (k v) #:inner (hash-set h k v)])

(module+ test
  (require rackunit racket/set)
#;
  (define-accumulator list
    [lst #:initial '() #:bind v #:inner (cons v lst) #:post (reverse lst)])

  (check equal?
         (for/acc ([#:type list]) ([i 10]) i)
         (for/list ([i 10]) i))

  (check equal?
         (call-with-values
             (λ () (for/acc ([#:type list]
                             [#:type list]
                             [mumble #:drop 'bork])
                            ([i 10])
                            (values i i 'bork)))
           list)
         (let ([v (for/list ([i 10]) i)])
           (list v v)))

  ;; Tricky internal-definition-context test
  (check equal?
         (let ()
           (define (post x) '(bork bork bork))
           (for/acc ([#:type list])
                    ([x 10])
                    (define (pre x) (if (even? x) (post x) x))
                    (begin
                      (define (post y) (pre (add1 y)))
                      (post x))))
         (for/list ([i 10]) (if (even? (add1 i)) (+ 2 i) (add1 i))))

  ;; Don't define dropped ids?
  (check equal?
         (let ()
           (define bad 'already-here)
           (define/for/acc ([l0 #:type list] [l1 #:type list] [bad #:drop 0])
             ([x 10])
             (values x x 0))
           (list l0 l1))
         (let ([v (for/list ([i 10]) i)]) (list v v)))

  (define-accumulator append [lst #:initial '() #:bind v #:inner (append (reverse v) lst) #:post (reverse lst)])
  (define-accumulator union [st #:initial (set) #:bind v #:inner (set-union v st)])

  (check equal?
         (call-with-values (λ ()
                              (for/acc ([#:type set]
                                        [hh #:type hash])
                                       ([i 5])
                                       (values i i (- i))))
           list)
         (list (set 0 1 2 3 4)
               (hash 0 0 1 -1 2 -2 3 -3 4 -4)))

  (check equal?
         (call-with-values (λ ()
                              (for/acc ([#:type sum]
                                        [a '()]
                                        [#:type prod])
                                       ([i (in-range 1 5)])
                                       (values i (cons i a) i)))
           list)
         (list 10 (list 4 3 2 1) 24))

  (define b (box #f))

  (define-accumulator max
    [m #:initial -inf.0 #:bind v #:inner (if (> v m) v m)]
    [find #:initial #f #:inner (= m 8) #:suppress #:post (when find (set-box! b #t))])

  (check equal?
         (let ([max (for/acc ([#:type max]) ([x 6]) (* 2 x))])
           (and (unbox b) max))
         10)
  
)
