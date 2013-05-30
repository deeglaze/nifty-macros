Defined accumulators for `for`

This library provides support for combining different accumulator styles
in the same form. For example, `for/sum` and `for/list` can be combined
to sum the first value returned in a `for` body, and make a list out of
the second value returned in that body. Accumulator styles are
user-definable and are flexible enough to support all current `for/*`
forms.

```racket
(define-accumulator name [accumulator-id optionals] ...)
                                                        
optionals = #:suppress                                  
          | #:initial initial-expr                      
          | #:bind bind-id/s                            
          | #:inner inner-expr                          
          | #:post post-expr                            
                                                        
bind-id/s = id                                          
          | (id ...)                                    
```

Defines an accumulation style named `name` that uses the following
accumulator identifiers with optional initial value, optional
post-processing (say to reverse the produced list), and how to
accumulate the value that corresponds to its position, optionally named
with `#:bind`.

If an accumulation style requires several accumulators to perform the
task, you can suppress some bindings from being in the return values of
`for/acc`. At least one accumulator must not be suppressed.

```racket
Examples:                                                                            
> (define-accumulator list                                                           
      [lst #:initial '() #:bind v #:inner (cons v lst) #:post (reverse lst)])        
>                                                                                    
(define-accumulator hash [h #:initial #hash() #:bind (k v) #:inner (hash-set h k v)])
>                                                                                    
(define-accumulator union [st #:initial (set) #:bind v #:inner (set-union v st)])    
```

```racket
(for/acc (accumulator ...) for-clauses body-or-break ... body)           
                                                                         
accumulator           = [id initial-expr]                                
                      | [optional-ids kw-optionals optional-initial-expr]
                                                                         
optional-ids          =                                                  
                      | id                                               
                      | (id ...)                                         
                                                                         
optional-initial-expr =                                                  
                      | initial-expr                                     
                                                                         
kw-optionals          = #:type style-id                                  
                      | #:initial initial-expr/s                         
                      | #:drop                                           
                                                                         
initial-expr/s        = initial-expr                                     
                      | (initial-expr ...)                               
```

Restrictions: an accumulator without `#:type` in `kw-optionals` must be
given in the first form of `accumulator`. If `id` is specified, there
must be the same number of identifiers as non-suppressed identifiers for
the accumulator style. This restriction also exists for `#:initial`, and
additionally, if a style does not give default initial values,
`#:initial` must be used.

An `accumulator` that also has `#:drop` specified will not return the
values of the accumulators as part of the values of the entire `for/acc`
form.

The `body` expression is expected to return as many values as there are
non-suppressed identifiers for accumulators.

Here `for-clauses`, `body-or-break` and `body` are the same as in `for`.
`for/acc` is backwards-compatible with `for/fold`.

```racket
Examples:                                                        
> (for/acc ([#:type set]                                         
                   [#:type hash (hash -1 1)])                    
      ([i 5])                                                    
    (values i i (- i)))                                          
(set        '#hash((0 . #t) (1 . #t) (2 . #t) (3 . #t) (4 . #t)))
'#hash((0 . 0) (1 . -1) (2 . -2) (3 . -3) (4 . -4))              
> (for/acc ([#:type sum]                                         
                   [a '()]                                       
                   [#:type prod])                                
      ([i (in-range 1 5)])                                       
    (values i (cons i a) i))                                     
10                                                               
'(4 3 2 1)                                                       
24                                                               
```

```racket
(for*/acc (accumulator ...) for-clauses body-or-break ... body)
```

Like `for/acc`, but uses `for*/fold` as the base iteration form.
Backwards-compatible with `for*/fold`.
