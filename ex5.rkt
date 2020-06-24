
; Signature: get-value(assoc-list, key)
; Purpose: Find the value of 'key'. If 'key' is not found, return ‘fail.
; Type: [List<Pair(Symbol,T)> * Symbol -> T | ‘fail]
; Tests: (get-value '((a . 3) (b . 4)) 'b) → 4,(get-value '((a . 3) (b . 4)) 'c) → ‘fail
(define get-value
  (lambda (al key)
    (if (empty? al)
        'fail
        (if (equal? (car (car al)) key)
            (cdr (car al))
            (get-value (cdr al) key)))))
(get-value '((a . 3) (b . 4)) 'b)
(get-value '((a . 3) (b . 4)) 'c)

; Signature: get-value$(assoc-list, key, success, fail)
; Purpose: Find the value of 'key'. If 'key' is found, then apply the continuation 'success' on its value val. Otherwise,
; apply the continuation 'fail'.
; Type: [ List<Pair<Symbol,T>> * Symbol * [T->T1] * [Empty->T2] ] -> T1 | T2)
; Tests: > (get-value$ '((a . 3) (b . 4)) 'b (lambda(x) (* x x ))
; (lambda()#f)) → 16, (get-value$ '((a . 3) (b . 4)) 'c
; (lambda(x) (* x x)) (lambda()#f)) → #f
(define get-value$
  (lambda (al key success fail)
    (if (empty? al)
        (fail)
        (if (equal? (car (car al)) key)
            (success (cdr (car al)))
            (get-value$ (cdr al) key success fail)))))
(get-value$ '((a . 3) (b . 4)) 'b (lambda (x) x) (lambda () 'fail))
(get-value$ '((a . 3) (b . 4)) 'c (lambda (x) x) (lambda () 'fail))

; Signature: collect-all-values(list-assoc-lists, key)
; Purpose: Returns a list of all values of the first occurrence
; of 'key' in each of the given association lists. If there’s no
; such value, return the empty list.
; Type: [List<List<Pair<Symbol,T>>> * Symbol -> List<T>]
; Tests:
; > (define l1 '((a . 1) (e . 2)))
; > (define l2 '((e . 5) (f . 6)))
; > (collect-all-values (list l1 l2) 'e) → ‘(2 5)
; > (collect-all-values (list l1 l2) 'k)--> ‘()
(define collect-all-values-1
	(lambda (lal key)
		(if (empty? lal)
			'()
			(let ((val (get-value (car lal) key)))
				(if (equal? val 'fail)
					(collect-all-values-1 (cdr lal) key)
					(cons val (collect-all-values-1 (cdr lal) key)))))))

(define collect-all-values-2
  (lambda (lal key)
    (if (empty? lal)
        '()
        (get-value$ (car lal) key
                    (lambda (res) (cons res (collect-all-values-2 (cdr lal) key)))
                    (lambda () (collect-all-values-2 (cdr lal) key))))))
