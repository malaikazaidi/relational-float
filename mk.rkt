#lang racket/base

; The MIT License (MIT)
;
; Copyright (c) 2015 William E. Byrd
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
;
; FROM: https://github.com/michaelballantyne/faster-miniKanren


(require racket/list
         racket/include)

(provide run run*
         == =/=
         fresh
         conde
         symbolo numbero stringo
         absento
         project
         var?)

(define empty-intmap (hasheq))
(define (intmap-count m) (hash-count m))
(define (intmap-ref m k) (hash-ref m k (lambda () unbound)))
(define (intmap-set m k v) (hash-set m k v))

;; extra stuff for racket
;; due mostly to samth
(define (list-sort f l) (sort l f))

(define (remp f l) (filter-not f l))

(define (call-with-string-output-port f)
  (define p (open-output-string))
  (f p)
  (get-output-string p))

(define (exists f l) (ormap f l))

(define for-all andmap)

(define (find f l)
  (cond [(memf f l) => car] [else #f]))

(define memp memf)

(define (var*? v) (var? (car v)))

; Scope object.
; Used to determine whether a branch has occured between variable
; creation and unification to allow the set-var-val! optimization
; in subst-add. Both variables and substitutions will contain a
; scope. When a substitution flows through a conde it is assigned
; a new scope.

; Creates a new scope that is not scope-eq? to any other scope
(define (new-scope) (list 'scope))

; Scope used when variable bindings should always be made in the
; substitution, as in disequality solving and reification. We
; don't want to set-var-val! a variable when checking if a
; disequality constraint holds!
(define nonlocal-scope (list 'non-local-scope))

(define scope-eq? eq?)

; Logic variable object.
; Contains:
;   val - value for variable assigned by unification using
;      set-var-val! optimization. unbound if not yet set or
;      stored in substitution.
;   scope - scope that the variable was created in.
;   idx - unique numeric index for the variable. Used by the
;      trie substitution representation.
; Variable objects are compared by object identity.

; The unique val for variables that have not yet been bound
; to a value or are bound in the substitution
(define unbound (list 'unbound))

(define var
  (let ((counter -1))
    (lambda (scope)
      (set! counter (+ 1 counter))
      (vector unbound scope counter))))

; Vectors are not allowed as terms, so terms that are vectors
; are variables.
(define (var? x)
  (vector? x))

(define var-eq? eq?)

(define (var-val x)
  (vector-ref x 0))

(define (set-var-val! x v)
  (vector-set! x 0 v))

(define (var-scope x)
  (vector-ref x 1))

(define (var-idx x)
  (vector-ref x 2))


; Substitution object.
; Contains:
;   map - mapping of variables to values
;   scope - scope at current program point, for set-var-val!
;     optimization. Updated at conde. Included in the substitution
;     because it is required to fully define the substitution
;     and how it is to be extended.
;
; Implementation of the substitution map depends on the Scheme used,
; as we need a map. See mk.rkt and mk-vicare.scm.

(define empty-subst-map empty-intmap)
(define subst-map-length intmap-count)
(define (subst-map-lookup u S)
  (intmap-ref S (var-idx u)))
(define (subst-map-add S var val)
  (intmap-set S (var-idx var) val))

(define (subst mapping scope)
  (cons mapping scope))

(define subst-map car)

(define subst-scope cdr)

(define (subst-length S)
  (subst-map-length (subst-map S)))

(define (subst-with-scope S new-scope)
  (subst (subst-map S) new-scope))

(define empty-subst (subst empty-subst-map (new-scope)))

(define (subst-add S x v)
  ; set-var-val! optimization: set the value directly on the
  ; variable object if we haven't branched since its creation
  ; (the scope of the variable and the substitution are the same).
  ; Otherwise extend the substitution mapping.
  (if (scope-eq? (var-scope x) (subst-scope S))
    (begin (set-var-val! x v)
           S)
    (subst (subst-map-add (subst-map S) x v) (subst-scope S))))

(define (subst-lookup x S)
  ; set-var-val! optimization.
  ; Tried checking the scope here to avoid a subst-map-lookup
  ; if it was definitely unbound, but that was slower.
  (if (not (eq? (var-val x) unbound))
    (var-val x)
    (subst-map-lookup x (subst-map S))))

; Association object.
; Describes an association mapping the lhs to the rhs. Returned by
; unification to describe the associations that were added to the
; substitution (whose representation is opaque) and used to represent
; disequality constraints.
(define lhs car)
(define rhs cdr)

; Constraint record object.
;
; Describes the constraints attached to a single variable.
;
; Contains:
;   T - type constraint. instance of type-constraint or #f to indicate
;         no constraint
;   D - list of disequality constraints. Each disequality is a list of
;         associations. The constraint is violated if all associated
;         variables are equal in the substitution simultaneously. D
;         could contain duplicate constraints (created by distinct =/=
;         calls). A given disequality constraint is only attached to
;         one of the variables involved, as all components of the
;         constraint must be violated to cause failure.
;   A - list of absento constraints. Each constraint is a term.
;         The list contains no duplicates.

(define empty-c '(#f () ()))

(define (c-T c) (car c))
(define (c-D c) (cadr c))
(define (c-A c) (caddr c))

(define (c-with-T c T) (list T (c-D c) (c-A c)))
(define (c-with-D c D) (list (c-T c) D (c-A c)))
(define (c-with-A c A) (list (c-T c) (c-D c) A))

; Constraint store object.
; Mapping of representative variable to constraint record. Constraints
; are always on the representative element and must be moved / merged
; when that element changes.

(define empty-C empty-intmap)

(define (set-c st x c)
  (state-with-C
    st
    (intmap-set (state-C st) (var-idx x) c)))

(define (lookup-c st x)
  (let ((res (intmap-ref (state-C st) (var-idx x))))
    (if (not (eq? unbound res))
      res
      empty-c)))

; t:unbind in mk-chez.scm either is buggy or doesn't do what I would expect, so
; I implement remove by setting the value to the empty constraint record.
(define (remove-c x st)
  (state-with-C st (intmap-set (state-C st) (var-idx x) empty-c)))


; State object.
; The state is the value that is monadically passed through the search
; Contains:
;   S - the substitution
;   C - the constraint store

(define (state S C) (cons S C))

(define (state-S st) (car st))
(define (state-C st) (cdr st))

(define empty-state (state empty-subst empty-C))

(define (state-with-C st C^)
  (state (state-S st) C^))

(define state-with-scope
  (lambda (st new-scope)
    (state (subst-with-scope (state-S st) new-scope) (state-C st))))

; Unification

(define (walk u S)
  (if (var? u)
    (let ((val (subst-lookup u S)))
      (if (eq? val unbound)
        u
        (walk val S)))
    u))

(define (occurs-check x v S)
  (let ((v (walk v S)))
    (cond
      ((var? v) (var-eq? v x))
      ((pair? v)
       (or (occurs-check x (car v) S)
           (occurs-check x (cdr v) S)))
      (else #f))))

(define (ext-s-check x v S)
  (if (occurs-check x v S)
    (values #f #f)
    (values (subst-add S x v) (list (cons x v)))))

; Returns as values the extended substitution and a list of
; associations added during the unification, or (values #f #f) if the
; unification failed.
;
; Right now appends the list of added values from sub-unifications.
; Alternatively could be threaded monadically, which could be faster
; or slower.
(define (unify u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    (cond
      ((eq? u v) (values s '()))
      ((var? u) (ext-s-check u v s))
      ((var? v) (ext-s-check v u s))
      ((and (pair? u) (pair? v))
       (let-values (((s added-car) (unify (car u) (car v) s)))
         (if s
           (let-values (((s added-cdr) (unify (cdr u) (cdr v) s)))
             (values s (append added-car added-cdr)))
           (values #f #f))))
      ((equal? u v) (values s '()))
      (else (values #f #f)))))

(define (unify* S+ S)
  (unify (map lhs S+) (map rhs S+) S))


; Search

; SearchStream: #f | Procedure | State | (Pair State (-> SearchStream))

; SearchStream constructor types. Names inspired by the plus monad?

; -> SearchStream
(define (mzero) #f)

; c: State
; -> SearchStream
(define (unit c) c)

; c: State
; f: (-> SearchStream)
; -> SearchStream
;
; f is a thunk to avoid unnecessary computation in the case that c is
; the last answer needed to satisfy the query.
(define (choice c f) (cons c f))

; e: SearchStream
; -> (-> SearchStream)
(define-syntax inc
  (syntax-rules ()
    ((_ e) (lambda () e))))

; Goal: (State -> SearchStream)

; e: SearchStream
; -> Goal
(define-syntax lambdag@
  (syntax-rules ()
    ((_ (st) e) (lambda (st) e))))

; Match on search streams. The state type must not be a pair with a
; procedure in its cdr.
;
; (() e0)     failure
; ((f) e1)    inc for interleaving. separate from success or failure
;               to ensure it goes all the way to the top of the tree.
; ((c) e2)    single result. Used rather than (choice c (inc (mzero)))
;               to avoid returning to search a part of the tree that
;               will inevitably fail.
; ((c f) e3)  multiple results.
(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((c^) e2) ((c f) e3))
     (let ((c-inf e))
       (cond
         ((not c-inf) e0)
         ((procedure? c-inf)  (let ((f^ c-inf)) e1))
         ((not (and (pair? c-inf)
                 (procedure? (cdr c-inf))))
          (let ((c^ c-inf)) e2))
         (else (let ((c (car c-inf)) (f (cdr c-inf)))
                 e3)))))))

; c-inf: SearchStream
;     f: (-> SearchStream)
; -> SearchStream
;
; f is a thunk to avoid unnecesarry computation in the case that the
; first answer produced by c-inf is enough to satisfy the query.
(define (mplus c-inf f)
  (case-inf c-inf
    (() (f))
    ((f^) (inc (mplus (f) f^)))
    ((c) (choice c f))
    ((c f^) (choice c (inc (mplus (f) f^))))))

; c-inf: SearchStream
;     g: Goal
; -> SearchStream
(define (bind c-inf g)
  (case-inf c-inf
    (() (mzero))
    ((f) (inc (bind (f) g)))
    ((c) (g c))
    ((c f) (mplus (g c) (inc (bind (f) g))))))

; Int, SearchStream -> (ListOf SearchResult)
(define (take n f)
  (if (and n (zero? n))
    '()
    (case-inf (f)
      (() '())
      ((f) (take n f))
      ((c) (cons c '()))
      ((c f) (cons c (take (and n (- n 1)) f))))))

; -> SearchStream
(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

; -> SearchStream
(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...)
     (mplus e0 (inc (mplus* e ...))))))

; -> Goal
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (st)
       ; this inc triggers interleaving
       (inc
         (let ((scope (subst-scope (state-S st))))
           (let ((x (var scope)) ...)
             (bind* (g0 st) g ...))))))))

; -> Goal
(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (st)
       ; this inc triggers interleaving
       (inc
         (let ((st (state-with-scope st (new-scope))))
           (mplus*
             (bind* (g0 st) g ...)
             (bind* (g1 st) g^ ...) ...)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (take n
       (inc
         ((fresh (q) g0 g ...
            (lambdag@ (st)
              (let ((st (state-with-scope st nonlocal-scope)))
                (let ((z ((reify q) st)))
                  (choice z (lambda () (lambda () #f)))))))
          empty-state))))
    ((_ n (q0 q1 q ...) g0 g ...)
     (run n (x)
       (fresh (q0 q1 q ...)
         g0 g ...
         (== (list q0 q1 q ...) x))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))


; Constraints
; C refers to the constraint store map
; c refers to an individual constraint record

; Constraint: State -> #f | State
;
; (note that a Constraint is a Goal but a Goal is not a Constraint.
;  Constraint implementations currently use this more restrained type.
;  See `and-foldl` and `update-constraints`.)

; Requirements for type constraints:
; 1. Must be positive, not negative. not-pairo wouldn't work.
; 2. Each type must have infinitely many possible values to avoid
;      incorrectness in combination with disequality constraints,
;      like: (fresh (x) (booleano x) (=/= x #t) (=/= x #f))

(define (type-constraint predicate symbol reified)
  (list predicate symbol reified))
(define type-constraint-predicate car)
(define type-constraint-symbol cadr)
(define type-constraint-reified caddr)

(define (apply-type-constraint tc)
  (lambda (u)
    (lambdag@ (st)
      (let ((type-pred (type-constraint-predicate tc))
            (type-id (type-constraint-symbol tc)))
        (let ((term (walk u (state-S st))))
          (cond
            ((type-pred term) st)
            ((var? term)
             (let* ((c (lookup-c st term))
                    (T (c-T c)))
               (cond
                 ((eq? T tc) st)
                 ((not T) (set-c st term (c-with-T c tc)))
                 (else #f))))
            (else #f)))))))

(define-syntax declare-type-constraints
  (syntax-rules ()
    ((_ tc-list (name predicate reified ordering) ...)
     (begin
       (define tc-list (list (type-constraint predicate 'name 'reified) ...))
       (define-values
         (name ...)
         (apply values (map apply-type-constraint tc-list)))))))

(declare-type-constraints type-constraints
  (numbero number? num <=)
  (stringo string? str string<=?)
  (symbolo symbol? sym (lambda (s1 s2) (string<? (symbol->string s1)
                                                 (symbol->string s2)))))

; Options:
;   table mapping symbol -> predicate
;   representation of type constraint as pair or struct of symbol and predicate
;   store both

(define (add-to-D st v d)
  (let* ((c (lookup-c st v))
         (c^ (c-with-D c (cons d (c-D c)))))
    (set-c st v c^)))

(define (=/=* S+)
  (lambdag@ (st)
    (let-values (((S added) (unify* S+ (subst-with-scope
                                         (state-S st)
                                         nonlocal-scope))))
      (cond
        ((not S) st)
        ((null? added) #f)
        (else
          ; Choose one of the disequality elements (el) to attach
          ; the constraint to. Only need to choose one because
          ; all must fail to cause the constraint to fail.
          (let ((el (car added)))
            (let ((st (add-to-D st (car el) added)))
              (if (var? (cdr el))
                (add-to-D st (cdr el) added)
                st))))))))

(define (=/= u v)
  (=/=* (list (cons u v))))

;; Generalized 'absento': 'term1' can be any legal term (old version
;; of faster-miniKanren required 'term1' to be a ground atom).
(define (absento term1 term2)
  (lambdag@ (st)
    (let ((term1 (walk term1 (state-S st)))
          (term2 (walk term2 (state-S st))))
      (let ((st^ ((=/= term1 term2) st)))
        (if st^
          (cond
            ((pair? term2)
             (let ((st^^ ((absento term1 (car term2)) st^)))
               (and st^^ ((absento term1 (cdr term2)) st^^))))
            ((var? term2)
             (let* ((c (lookup-c st^ term2))
                    (A (c-A c)))
               (if (memv term1 A)
                 st^
                 (let ((c^ (c-with-A c (cons term1 A))))
                   (set-c st^ term2 c^)))))
            (else st^))
          #f)))))

; Fold lst with proc and initial value init. If proc ever returns #f,
; return with #f immediately. Used for applying a series of
; constraints to a state, failing if any operation fails.
(define (and-foldl proc init lst)
  (if (null? lst)
    init
    (let ((res (proc (car lst) init)))
      (and res (and-foldl proc res (cdr lst))))))

(define (== u v)
  (lambdag@ (st)
    (let-values (((S^ added) (unify u v (state-S st))))
      (if S^
        (and-foldl update-constraints (state S^ (state-C st)) added)
        #f))))

; Not fully optimized. Could do absento update with fewer
; hash-refs / hash-sets.
(define (update-constraints a st)
  (let ((old-c (lookup-c st (lhs a))))
    (if (eq? old-c empty-c)
      st
      (let ((st (remove-c (lhs a) st)))
       (and-foldl (lambda (op st) (op st)) st
        (append
          (if (c-T old-c)
            (list ((apply-type-constraint (c-T old-c)) (rhs a)))
            '())
          (map (lambda (atom) (absento atom (rhs a))) (c-A old-c))
          (map =/=* (c-D old-c))))))))

(define (walk* v S)
  (let ((v (walk v S)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons (walk* (car v) S) (walk* (cdr v) S)))
      (else v))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (st)
       (let ((x (walk* x (state-S st))) ...)
         ((fresh () g g* ...) st))))))

(define succeed (== #f #f))
(define fail (== #f #t))

; Reification

; Bits from the old constraint implementation, still used for
; reification.

; In this part of the code, c refers to the
; old constraint store with components:
; S - substitution
; D - disequality constraints
; Y - symbolo
; N - numbero
; T - absento

(define (oldc S D T A)
  (list S D T A))

(define oldc-S (lambda (c) (car c)))
(define oldc-D (lambda (c) (cadr c)))
(define oldc-T (lambda (c) (caddr c)))
(define oldc-A (lambda (c) (cadddr c)))

(define (oldc-with-S c S^)
  (oldc S^ (oldc-D c) (oldc-T c) (oldc-A c)))
(define (oldc-with-D c D^)
  (oldc (oldc-S c) D^ (oldc-T c) (oldc-A c)))
(define (oldc-with-T c T^)
  (oldc (oldc-S c) (oldc-D c) T^ (oldc-A c)))
(define (oldc-with-A c A^)
  (oldc (oldc-S c) (oldc-D c) (oldc-T c) A^))

; Create a constraint store of the old representation from a state
; object, so that we can use the old reifier. Only accumulates
; constraints related to the variable being reified which makes things
; a bit faster.
(define (c-from-st st x)
  (let ((vs (vars (walk* x (state-S st)) '())))
    (foldl
      (lambda (v c-store)
        (let ((c (lookup-c st v)))
          (let ((T^ (c-T c))
                (D^ (c-D c))
                (A^ (c-A c)))
            (oldc
              (oldc-S c-store)
              (append (filter (lambda (x) x) (map (normalize-diseq (state-S st)) D^)) (oldc-D c-store))
              (if (c-T c)
                (subst-map-add (oldc-T c-store) v (c-T c))
                (oldc-T c-store))
              (append
                (map (lambda (absento-lhs) (cons (walk* absento-lhs (state-S st)) v)) (c-A c))
                (oldc-A c-store))))))
      (oldc (state-S st) '() empty-subst-map '())
      (remove-duplicates vs))))

(define (normalize-diseq S)
  (lambda (S+)
    (let-values (((S^ S+^) (unify* S+ S)))
      (and S^ (walk* S+^ S)))))

(define (vars term acc)
  (cond
    ((var? term) (cons term acc))
    ((pair? term)
     (vars (cdr term) (vars (car term) acc)))
    (else acc)))

; Simplification

(define (reify x)
  (lambda (st)
    (let* ((c (c-from-st st x))
           (c (simplify c))
           (S (oldc-S c)))
        (let* ((v (walk* x S))
               (R (reify-S v (subst empty-subst-map nonlocal-scope)))
               (any-var-unreified? (lambda (term) (anyvar? term R))))
          (reify+ v R
                  ; Drop disequalities that are satisfiable in any
                  ; assignment of the reified variables, because
                  ; they refer to unassigned variables that are not
                  ; part of the answer, which can be assigned as needed
                  ; to satisfy the constraint.
                  (let ((D^ (remp any-var-unreified? (oldc-D c))))
                    (rem-subsumed d-subsumed-by? D^))
                  (oldc-T c) ; don't remove unreified because later we pull out only the bits we need for reified result.
                  (let ((A^ (remp any-var-unreified? (oldc-A c))))
                    (rem-subsumed a-subsumed-by? A^)))))))

(define (simplify c)
  (foldl (lambda (f c) (f c)) c
         (list
           ; drop these first so that (drop-D d-subsumed-by-A?) does not drop corresponding diseqs.
           (drop-A absento-rhs-atomic?)
           (drop-A absento-rhs-occurs-lhs?)
           (drop-D d-subsumed-by-T?)
           (drop-D d-subsumed-by-A?))))

(define (drop-D pred)
  (lambda (c) (oldc-with-D c (filter (lambda (v) (not (pred c v))) (oldc-D c)))))
(define (drop-A pred)
  (lambda (c) (oldc-with-A c (filter (lambda (v) (not (pred c v))) (oldc-A c)))))

; Drop absento constraints where the RHS is known to be atomic, such that
; the disequality attached by absento solving is sufficient.
(define (absento-rhs-atomic? c a)
  ; absento on pairs is pushed down and type constraints are atomic,
  ; so the only kind of non-atomic RHS is an untyped var.
  (not (and (var? (rhs a)) (eq? unbound (var-type c (rhs a))))))

; Drop absento constraints that are trivially satisfied because
; any violation would cause a failure of the occurs check.
; Example:
;  (absento (list x y z) x) is trivially true because a violation would
;  require x to occur within itself.
(define (absento-rhs-occurs-lhs? c a)
  (occurs-check (rhs a) (lhs a) (oldc-S c)))

; Drop disequalities that are subsumed by an absento contraint
; interpreted as a disequality.
(define (d-subsumed-by-A? c d)
  (exists (lambda (a)
            (d-subsumed-by? d (absento->diseq a)))
          (oldc-A c)))

; Drop disequalities that are fully satisfied because the types are disjoint
; either due to type constraints or ground values.
; Examples:
;  * given (symbolo x) and (numbero y), (=/= x y) is dropped.
(define (d-subsumed-by-T? c d)
  (exists (lambda (pr) (not (var-types-match? c (lhs pr) (rhs pr))))
          d))

(define (var-types-match? c t1 t2)
  (or (eq? unbound (var-type c t1))
      (if (var? t2)
        (or (eq? unbound (var-type c t2))
            (eq? (var-type c t1) (var-type c t2)))
        ((type-constraint-predicate (var-type c t1))
         t2))))

(define (var-type c t) (subst-map-lookup t (oldc-T c)))

(define (absento->diseq t)
  (list t))

(define (anyvar? u r)
  (if (pair? u)
    (or (anyvar? (car u) r)
        (anyvar? (cdr u) r))
    (var? (walk u r))))

(define (rem-subsumed subsumed-by? el*)
  (define (subsumed-by-one-of? el el*)
    (ormap (lambda (el2) (subsumed-by? el el2)) el*))

  (let loop ((el* el*)
             (result '()))
    (cond
      ((null? el*) result)
      (else
        (let ((el (car el*)))
          (cond
            ((or (subsumed-by-one-of? el (cdr el*))
                 (subsumed-by-one-of? el result))
             (loop (cdr el*) result))
            (else (loop (cdr el*)
                        (cons el result)))))))))

; Examples:
; * (absento `(cat . ,S) y) is subsumed by (absento S y)
;
; Note that absento constraints are pushed down to tree leaves, so we would never have
;  (absento 'closure q) given (== q (list x)). Thus we do not need to consider subsumption
;  between absento constraints on q and x.
(define (a-subsumed-by? t1 t2)
  (and (var-eq? (rhs t1) (rhs t2)) (member* (lhs t2) (lhs t1))))

(define (member* u v)
  (cond
    ((equal? u v) #t)
    ((pair? v)
     (or (member* u (car v)) (member* u (cdr v))))
    (else #f)))

; (-> disequality/c disequality/c boolean?)
; Examples:
;  * ((a . 5) (b . 6)) is subsumed by ((a . 5)) because (not (== a 5)) is a stronger constraint
;    than (not (and (== a 5) (== b 6)))
(define (d-subsumed-by? d1 d2)
  (let*-values (((S ignore) (unify* d1 (subst empty-subst-map nonlocal-scope)))
                ((S+ added) (unify* d2 S)))
               (and S+ (null? added))))

(define (reify-S v S)
  (let ((v (walk v S)))
    (cond
      ((var? v)
       (let ((n (subst-length S)))
         (let ((name (reify-name n)))
           (subst-add S v name))))
      ((pair? v)
       (let ((S (reify-S (car v) S)))
         (reify-S (cdr v) S)))
      (else S))))

; Formatting

(define (reify-name n)
  (string->symbol
    (string-append "_" "." (number->string n))))

(define (reify+ v R D T A)
  (let ((vs (vars v '())))
    (let ((T^ (map
                (lambda (tc-type)
                  (cons tc-type
                        (filter (lambda (x) x)
                          (map
                            (lambda (v)
                              (let ((tc (subst-map-lookup v T)))
                                (and (not (eq? tc unbound))
                                     (eq? tc-type (type-constraint-reified tc))
                                     (walk* v R))))
                            (remove-duplicates vs)))))
                (map type-constraint-reified type-constraints))))
      ;; T^ : (alist type-constraint-symbol (list-of reified-var))
      (form (walk* v R) (walk* D R) T^ (walk* A R)))))

(define (form v D T^ A)
  (let ((fd (sort-D D))
        (ft
          (filter (lambda (x) x)
                  (map
                    (lambda (p)
                      (let ((tc-type (car p)) (tc-vars (cdr p)))
                        (and (not (null? tc-vars))
                             `(,tc-type . ,(sort-lex tc-vars)))))
                    T^)))
        (fa (sort-lex A)))
    (let ((fd (if (null? fd) fd
                (let ((fd (drop-dot-D fd)))
                  `((=/= . ,fd)))))
          (fa (if (null? fa) fa
                (let ((fa (drop-dot fa)))
                  `((absento . ,fa))))))
      (cond
        ((and (null? fd) (null? ft) (null? fa))
         v)
        (else (append `(,v) fd ft fa))))))

(define (sort-lex ls)
  (list-sort lex<=? ls))

(define (sort-D D)
  (sort-lex (map sort-d D)))

(define (sort-d d)
  (list-sort
    (lambda (x y)
      (lex<=? (car x) (car y)))
    (map sort-pr d)))

(define (sort-pr pr)
  (let ((l (lhs pr)) (r (rhs pr)))
    (cond
      ((lex<-reified-name? r) pr)
      ((lex<=? r l) `(,r . ,l))
      (else pr))))

(define (lex<-reified-name? r)
  (char<? (string-ref (datum->string r) 0)
          #\_))

(define (drop-dot-D D)
  (map drop-dot D))

(define (drop-dot X)
  (map (lambda (t) (list (lhs t) (rhs t)))
       X))

(define (lex<=? x y)
  ; TODO: order by type of value; possibly using order in
  ; declare-type-constraints?
  (string<=? (datum->string x) (datum->string y)))

(define (datum->string x)
  (call-with-string-output-port
    (lambda (p) (write x p))))
