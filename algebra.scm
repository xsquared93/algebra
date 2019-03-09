;;;; This is an extension of the simple system in SICP for manipulating fractions.
;;;; The extension includes functionality for operating on rational functions,
;;;; fractions whose numerator and denominator are polynomials. However, for
;;;; purposes of this program we will be dealing with univariable monomials.

;; A rational-expression is a pair consisting of either:
;;     - (number . number)
;;     - (pair . number)
;;     - (number . pair)
;;     - (pair . pair)
;; interpretation:
;; the first pair represents a fraction of numbers;
;; the second pair represents a fraction whose numerator is pair(monomial)
;; and whose denomiator is a number.
;; the third pair is similar to the second but in different order.
;; finally, the last one is a rational function, polynomial over polynomial.

(define (add-rat x y)
  (make-rat (make-addition (make-product (numer x) (denom y))
			   (make-product (numer y) (denom x)))
	    (make-product (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (make-product (numer x) (numer y))
	    (make-product (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))
	

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


;; Number (or monomial)  Number (or monomial) -> Pair
;; makes a rational number given two inputs
(define (make-rat n d)
  (cond
   ((and (number? n) (number? d))
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
   ((and (monomial? n) (number? d))
    (let ((g (gcd (coefficient n) d)))
      (cons (make-monomial (/ (coefficient n) g)
			   (variable n)
			   (exponent n))
	    (/ d g))))
   ((and (number? n) (monomial? d))
    (let ((g (gcd n (coefficient d))))
      (cons (/ n g)
	    (make-monomial (/ (coefficient d) g)
			   (variable d)
			   (exponent d)))))
   ((and (monomial? n) (monomial? d))                                           
    (let ((g (gcd (coefficient n) (coefficient d))))
	   (cons (make-monomial (/ (coefficient n) g)
				(variable n)
				(exponent n))
		 (make-monomial (/ (coefficient d) g)
				(variable d)
				(exponent d)))))
   (else (cons n d))))

;; constructor for doing some multiplication on simple algebra expressions
;; for the purposes of this program a monomial is a term with:
;;      - coefficient
;;      - variable
;;      - exponent

;; the first condition solves `(print-rat (mul-rat x-over-one x-over-one)) -> (** x 2)/1
(define (make-product x y)
  (cond ((monomial? x)
	 (if (not (monomial? y))
	     (make-monomial (* (coefficient x) y)
			    (variable x)
			    (exponent x))
	     (if (same-variable? x y)
		 (make-monomial (* (coefficient x) (coefficient y))
				(variable x)
				(+ (exponent x) (exponent y)))
		 (error "Only univariable monomial -- MAKE-PRODUCT" exp))))
	((monomial? y)
	 (if (not (monomial? x))
	     (make-monomial (* (coefficient y) x)
			    (variable y)
			    (exponent y))
	     (if (same-variable? x y)
		 (make-monomial (* (coefficient y) (coefficient x))
				(variable y)
				(+ (exponent y) (exponent x)))
		 (error "Only univariable monomial -- MAKE-PRODUCT" exp))))
	(else (* x y))))

(define (make-addition x y)
  (cond ((and (number? x) (number? y)) (+ x y))
	((and (monomial? x) (number? y)) (make-binomial '+ x y))
	((and (number? x) (monomial? y)) (make-binomial '+ x y))
	(else (if (same-variable? x y)
		  (if (same-exponent? x y)
		      (make-monomial (make-addition (coefficient x)
						    (coefficient y))
				     (variable x)
				     (exponent x))
		      (make-binomial '+ x y))
		  (make-binomial '+ x y)))))

(define (make-monomial coeff var exp)
  (cons coeff (cons var exp)))

(define (make-binomial op m1 m2)
  (cons op (cons m1 m2)))


(define (make-exponentiation x y)
  (if (same-variable? x y)
      (let ((var (variable x)))
	(list '** var (+ (exponent x) (exponent y))))
      (list '** x y)))

(define (monomial? x)
  (and (pair? x)
       (and (number? (car x))
            (variable? (car (cdr x)))
	    (number? (cdr (cdr x))))))

(define (binomial? x)
  (and (pair? x)
       (or (eq? (car x) '+)
	   (eq? (car x) '-))))

(define (same-variable? x y)
  (and (variable? (variable x))
       (variable? (variable y))
       (eq? (variable x) (variable y))))

(define (same-exponent? m1 m2)
  (and (number? (exponent m1))
       (number? (exponent m2))
       (eq? (exponent m1) (exponent m2))))

(define (variable? x) (symbol? x))

(define (coefficient x) (car x))

(define (variable x) (car (cdr x)))

(define (exponent x) (cdr (cdr x)))

(define (binomial-operator bi) (car bi))

(define (leftmost-monomial bi) (car (cdr bi)))

(define (rightmost-monomial bi) (cdr (cdr bi)))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (if (and (number? (car x)) (number? (cdr x)))
      (print-rational-number x)
      (print-rational-expression x)))

(define (print-rational-number x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; pair -> monomial (or number) "/"  number (or monomial)
;; prints a rational expression  
(define (print-rational-expression rat)
  (newline)
  (display (cond ((monomial? (numer rat))
		  (print-monomial (coefficient (numer rat))
				  (variable (numer rat))
				  (exponent (numer rat))))
		 ((binomial? (numer rat))
		  (cond ((number? (leftmost-monomial (numer rat)))
			 (print-binomial+
			  (leftmost-monomial (numer rat))
			  (print-monomial
			   (coefficient (rightmost-monomial (numer rat)))
			   (variable (rightmost-monomial (numer rat)))
			   (exponent (rightmost-monomial (numer rat))))))
			((number? (rightmost-monomial (numer rat)))
			 (print-binomial+
			  (print-monomial
			   (coefficient (leftmost-monomial (numer rat)))
				        (variable (leftmost-monomial (numer rat)))
					(exponent (leftmost-monomial (numer rat))))
			  (rightmost-monomial (numer rat))))
			(else (print-binomial+
			       (print-monomial (coefficient (leftmost-monomial (numer rat)))
					       (variable (leftmost-monomial (numer rat)))
					       (exponent (leftmost-monomial (numer rat))))
			       (print-monomial (coefficient (rightmost-monomial (numer rat)))
					       (variable (rightmost-monomial (numer rat)))
					       (exponent (rightmost-monomial (numer rat))))))))
		 (else (numer rat))))
  (display "/")
  (display (if (monomial? (denom rat))
	       (print-monomial (coefficient (denom rat))
			       (variable (denom rat))
			       (exponent (denom rat)))
	       (denom rat))))

(define (print-monomial coef var exp)
  (list coef
	'*
	(print-exponentiation var exp)))

(define (print-binomial+ m1 m2)
  (list m1 '+ m2))

(define (print-exponentiation var exp)
  (list var '** exp))
						   
		  
 
;;; tests

;; example

(define rat-1 (make-rat (make-monomial 2 'x 3)
			3))

(define rat-2 (make-rat (make-monomial 2 'x 4)
			5))

(print-rat (add-rat rat-1 rat-2))
;Value: ((10 * (x ** 3)) + (6 * (x ** 4)))/15
			
;; example 2

(define rat-3 (make-rat 3 (make-monomial 2 'x 3)))

(define rat-4 (make-rat 4 (make-monomial 2 'x 4)))

(print-rat (add-rat rat-3 rat-4))


;; example 3
(print-rat (add-rat rat-1 rat-3))
;Value: ((4 * (x ** 6)) + 9)/(6 * (x ** 3))
