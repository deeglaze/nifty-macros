#lang racket/base

(provide for/acc for*/acc define-accumulator)

;; Allow mix and match of accumulation styles, like for/list and for/set
;; for two different accumulators.

(require (for-syntax racket/base syntax/parse racket/syntax syntax/id-table racket/pretty))

(begin-for-syntax
 (define for/fold-types (make-free-id-table))

 (define-syntax-class folding
   #:attributes ((accs 1) ;; accumulator identifier used in inners
                 (clauses 1) ;; constructed accumulator clauses for the for[*]/fold form (not iteration clauses!)
                 (binds 1) ;; identifiers that stand for the positional value for the accumulator (used in inners)
                 (inners 1) ;; transforming expression for accumulated value(s)
                 drop ;; Should these accumulated values be dropped from the final result? (user specified)
                 (suppresses 1) ;; Which bindings should be dropped? (accumulator specified)
                 (posts 1)) ;; Transforming expressions for final values.
   (pattern [(~optional (~describe #:opaque "accumulator name(s)"
                                   (~or (~and g-acc:id (~bind [(g-accs 1) (list #'g-acc)]))
                                        (g-accs:id ...))))
             (~or (~optional (~seq #:type (~describe #:opaque "accumulation type" type:id)))
                  (~optional (~and #:drop drop))
                  (~optional (~seq #:initial (~describe #:opaque "initial value(s)"
                                                        (~or (~and g-init:expr (~bind [(inits 1) (list #'g-init)]))
                                                             (inits:expr ...)))))) ...
             (~optional (~describe #:opaque "initial value" p-init:expr))]
            #:fail-when (and (not (attribute type)) (not (attribute g-accs)))
            "Accumulator without an accumulation type requires an identifier to bind"
            #:fail-when (and (not (attribute type)) (not (or (attribute inits) (attribute p-init))))
            "Accumulator without an accumulation type requires an initial value"
            #:do [(define type-info
                    (and (attribute type)
                         (free-id-table-ref
                          for/fold-types
                          #'type
                          (λ () (raise-syntax-error #f (format "Unknown accumulator type ~a" #'type) #'type)))))]
            #:do [(define-values (accsv initsv num-exported suppressesv bindsv innersv postsv)
                    (if type-info
                        (apply values type-info)
                        (values (attribute g-accs) (or (attribute inits) (list #'p-init))
                                1 '(#f) '(#f) '(#f) '(#f))))]
            #:fail-unless (or (not (attribute g-accs)) (= (length (attribute g-accs)) num-exported))
            (format "Type-exported bindings arity-mismatch. Given ~a, expect ~a"
                    (length (attribute g-accs)) num-exported)
            #:fail-unless (or (not (attribute inits)) (= (length (attribute inits)) num-exported))
            (format "Type-exported bindings arity-mismatch. Given ~a, expect ~a"
                    (length (attribute inits)) num-exported)
            #:fail-when (and (attribute inits) (attribute p-init))
            "Cannot specify initial value positionally and with #:initial."
            ;; TODO: error if type does not have an initial value and no initial values given.
            #:attr (accs 1) (or (attribute g-accs) accsv)
            #:attr (clauses 1) (for/list ([acc (in-list (or (attribute g-accs) accsv))]
                                          [init (in-list initsv)])
                                 #`[#,acc #,init])
            #:attr (suppresses 1) suppressesv
            #:attr (binds 1) bindsv
            ;; inner/post need to rebind the accumulator identifiers if they are given.
            #:attr (inners 1) (if (attribute g-accs)
                                  (with-syntax ([(taccs ...) accsv]) ;; accumulators from definitions
                                    (for/list ([inner (in-list innersv)]) 
                                      (and inner
                                           #`(let ([taccs g-accs] ...) #,inner))))
                                  innersv)
            #:attr (posts 1) (if (attribute g-accs)
                                  (with-syntax ([(taccs ...) accsv])
                                    (for/list ([post (in-list postsv)])
                                      (and post
                                           #`(let ([taccs g-accs] ...) #,post))))
                                  postsv))))

(define-syntax (define-accumulator stx)
  (syntax-parse stx
    [(_ name:id [acc:id
                 ;; Helper identifiers won't be in the result of the for. Suppress them.
                 (~or (~optional (~and #:suppress suppress))
                      (~optional (~seq #:initial init:expr))
                      ;; Give the body's result(s) for this position (a) name(s) to use in inner.
                      (~optional (~seq #:bind (~or (~and bind:id (~bind [(binds 1) (list #'bind)]))
                                                   (binds:id ...))))
                      (~optional (~seq #:inner inner:expr))
                      ;; Suppressed bindings may post-process for side-effect and bind nothing.
                      (~optional (~seq #:post post:expr))) ...] ...)

     #:fail-unless (memv (syntax-local-context) '(top-level module))
     "Can only define for types at the (module) top-level"
     #:fail-unless (for/or ([s (in-list (attribute suppress))]) (not s))
     "Cannot suppress all accumulators"
     #:fail-when (for/or ([s (in-list (attribute suppress))]
                          [bs (in-list (attribute binds))])
                   (and s bs))
     "Suppressed bindings are hidden; cannot use #:bind."
     #:fail-when (for/or ([s (in-list (attribute suppress))]
                          [i (in-list (attribute init))])
                   (and s (not i)))
     "Suppressed bindings must have an initial value."

     (define num-exported (for/sum ([s (attribute suppress)] #:unless s) 1))
     (define value
       (list (attribute acc)
             (attribute init)
             num-exported
             (attribute suppress)
             (attribute binds)
             (attribute inner)
             (attribute post)))
     (define (listify lst)
       (for/list ([i (in-list lst)])
         (if i #`#'#,i #'#f)))
     (with-syntax ([(inits ...) (listify (attribute init))]
                   [(suppresss ...) (for/list ([s (in-list (attribute suppress))])
                                       (if s #'#t #'#f))]
                   [(inners ...) (listify (attribute inner))]
                   [(posts ...) (listify (attribute post))])
     #`(begin-for-syntax
        (free-id-table-set!
         for/fold-types
         #'name
         (list (list #'acc ...) (list inits ...) #,num-exported
               (list suppresss ...)
               (list (list #'binds ...) ...)
               (list inners ...)
               (list posts ...)))))]))

(define-syntax (for/acc-aux stx)
  (syntax-parse stx
    [(_ folder (accs:folding ...) guards
        (~and (~seq body-or-break ...)
              (~seq (~or (~seq #:break break:expr)
                         (~seq #:final final:expr)
                         body*:expr) ...))
        body:expr)
     ;; Suppressed bindings are hidden values of the iteration.
     ;; Dropping/suppressing bindings requires a let binding, as does post-processing.
     ;; Binding order is the order that values must be given in values.
     (define has-post? (or (for*/or ([ss (in-list (attribute accs.suppresses))]
                                     [s (in-list ss)])
                             s)
                           (for/or ([d (in-list (attribute accs.drop))]) d)
                           (for*/or ([ps (in-list (attribute accs.posts))]
                                     [p (in-list ps)]) p)))
     ;; Only add (let-values ([(x ...) ...]) ...) if there is a need to transform them.
     (define has-inner? (for*/or ([is (in-list (attribute accs.inners))]
                                  [i (in-list is)]) i))
     ;; Get the different accumulators, their defined and introduced bindings to muck
     ;; with to get the scope right.
     (define split-expected-ids
       (for/list
           ([ss (in-list (attribute accs.suppresses))]
            [as (in-list (attribute accs.accs))]
            [bss (in-list (attribute accs.binds))])
         (for/list
             ([s (in-list ss)]
              [a (in-list as)]
              [bs (in-list bss)])
           (cond [s '()]
                 ;; This is accumulated, so will be reversed. Reverse first to get right order.
                 [bs (reverse bs)]
                 [else (generate-temporaries (list a))]))))
     (define marked-split-expected-ids
       (for/list
           ([ss (in-list (attribute accs.suppresses))]
            [as (in-list (attribute accs.accs))]
            [bss (in-list (attribute accs.binds))]
            [ses (in-list split-expected-ids)])
         ;; Each accumulator gets an introducer to prevent binding clashes.
         (define intro (make-syntax-introducer))
         (for/list
             ([s (in-list ss)]
              [a (in-list as)]
              [bs (in-list bss)]
              [se (in-list ses)])
           (cond [s '()]
                 [bs (map intro se)] ;; mark given identifiers
                 [else se])))) ;; already fresh
     (with-syntax ([(expected-ids ...) ;; dropped ids count
                    (for*/fold ([acc '()]) ([lsts (in-list (reverse marked-split-expected-ids))]
                                            [lst (in-list lsts)])
                      (append (reverse lst) acc))])
       (with-syntax ([(return-values ...)
                      (reverse
                       (for/fold ([acc '()])
                           ([is (in-list (attribute accs.inners))] ;; should be wrapped appropriately
                            [ss (in-list (attribute accs.suppresses))]
                            [bss (in-list split-expected-ids)]
                            [sss (in-list marked-split-expected-ids)]
                            [as (in-list (attribute accs.accs))])
                         ;; If no inner expr given, then
                         ;; if binding suppressed, just use the accumulator identifier,
                         ;; otherwise, use the expected-id.
                         (for/fold ([acc acc]) ([i (in-list is)]
                                                [s (in-list ss)]
                                                [bs (in-list bss)]
                                                [ss (in-list sss)]
                                                [a (in-list as)])
                           (with-syntax ([(bs* ...) ss])
                             (cond
                              ;; Inner expressions should refer to the renamed binders.
                              [i (cons (quasisyntax/loc stx
                                         (let-syntaxes
                                             ([#,bs (values (make-rename-transformer #'bs*) ...)])
                                           #,i))
                                       acc)]
                              [s (cons a acc)]
                              [else (append ss acc)])))))])
         (define loop-body
           (if has-inner?
               (quasisyntax/loc stx
                 (let-values ([(expected-ids ...) body])
                   (values return-values ...)))
               #'body))
         (define loop
           (quasisyntax/loc stx
             (folder #,stx (accs.clauses ... ...) guards
                     body-or-break ...
                     #,loop-body)))
         (if has-post?
             (quasisyntax/loc stx
               (let-values ([(accs.accs ... ...) #,loop])
                 ;; Suppressed bindings get their post-processing run for side-effect.
                 #,@(reverse
                     (for/fold ([acc '()])
                         ([ps (in-list (attribute accs.posts))]
                          [ss (in-list (attribute accs.suppresses))])
                       (for/fold ([acc acc])
                           ([p (in-list ps)]
                            [s (in-list ss)]
                            #:when s)
                         (cons p acc))))
                 (values #,@(reverse
                             (for/fold ([acc '()])
                                 ([ps (in-list (attribute accs.posts))]
                                  [ss (in-list (attribute accs.suppresses))]
                                  [as (in-list (attribute accs.accs))]
                                  [d (in-list (attribute accs.drop))]
                                  #:unless d)
                               (for/fold ([acc acc])
                                   ([p (in-list ps)]
                                    [s (in-list ss)]
                                    [a (in-list as)]
                                    #:unless s)
                                 (cons (or p a) acc)))))))
             loop)))]))

(define-syntax-rule (for/acc accs guards body1 body ...)
  (for/acc-aux for/fold/derived accs guards body1 body ...))
(define-syntax-rule (for*/acc accs guards body1 body ...)
  (for/acc-aux for*/fold/derived accs guards body1 body ...))

(define-accumulator list
  [lst #:initial '() #:bind v #:inner (cons v lst) #:post (reverse lst)])

(require racket/set)
(define-accumulator set [st #:initial (set) #:bind v #:inner (set-add st v)])
(define-accumulator sum [sm #:initial 0 #:bind v #:inner (+ v sm)])
(define-accumulator prod [pd #:initial 1 #:bind v #:inner (* v pd)])
(define-accumulator hash [h #:initial #hash() #:bind (k v) #:inner (hash-set h k v)])


(module+ test
  (require rackunit racket/set)
  (define-accumulator list
    [lst #:initial '() #:bind v #:inner (cons v lst) #:post (reverse lst)])

  (check equal?
         (for/acc ([#:type list]) ([i 10]) i)
         (for/list ([i 10]) i))

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
         (list 10 (list 4 3 2 1) 24)))