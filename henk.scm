(load "pmatch.scm")

;;; Henk: a typed intermediate language (based on the lambda cube)
;;; http://research.microsoft.com/en-us/um/people/emeijer/Papers/Henk.pdf

;;; Figure 1: Syntax of Pure Type System expressions
;;;
;;; E ::= K          (Constant)
;;;       x          (Variable)
;;;       E E        (Application)
;;;       lambda  x : E.E (Abstraction)
;;;       pi x : E.E (Quantification)

(define empty-env
  (lambda ()
    '()))
(define env-extend
  (lambda (g k v)
    `((,k . ,v) . ,g)))
(define env-lookup
  (lambda (g k)
    (cond
     ((assq k g) => cdr)
     (else (error 'env-lookup "unbound var")))))

;;; Figure 5: Syntax Directed rules for the Lambda Cube
(define typ
  (lambda (g e)
    (pmatch
     e
     ;;; STAR
     (* '[])
     ;;; VAR
     ((var ,x) (env-lookup g x))
     ;;; APP
     ((app ,f ,a)
      (let ((tf (typ g f))
            (ta (typ g a)))
        (and tf ta
             (pmatch
              tf
              ((pi (,x ,tx) ,tb)
               (and (eqbeta tx ta)
                    (substi tb x a)))
              (else #f)))))
     ;;; LAM
     ((lambda (,x ,tx) ,b)
      (let ((tb (typ (env-extend g x tx) b)))
        (and tb
             (let ((te `(pi (,x ,tx) ,tb)))
               (and (typ g te)
                    te)))))
     ;;; PI
     ((pi (,x ,tx) ,tb)
      (let* ((s (redtyp g tx))
             (t (redtyp (env-extend g x tx) tb)))
        (and (to s t)
             t)))
     (else #f))))

(define redtyp
  (lambda (g e)
    (cond
     ((typ g e) => red)
     (else #f))))

(define eqbeta
  (lambda (ta tb)
    (eqalpha (red ta) (red tb))))

(define red
  (lambda (e)
    (pmatch
     e
     (((lambda (,x ,tx) ,b) ,a)
      (red (substi b x a)))
     (else e))))

(define eqalpha
  (lambda (a b)
    (equal? (canon a 0) (canon b 0))))

(define canon
  (lambda (e i)
    (pmatch
     e
     (* e)
     ([] e)
     ((var ,y) e)
     ((app ,f ,a)
      `(app ,(canon f i) ,(canon a i)))
     ((,abs (,y ,ty) ,b) (guard (or (eq? abs 'lambda) (eq? abs 'pi)))
      `(,abs (,i ,(canon ty i)) ,(canon (substi-var b y i) (+ i 1)))))))

(define fv
  (lambda (e)
    (pmatch
     e
     (*  '())
     ([] '())
     ((var ,y)
      `(,y))
     ((app ,f ,a)
      (append (fv f) (fv a)))
     ((,abs (,y ,ty) ,b) (guard (or (eq? abs 'lambda) (eq? abs 'pi)))
      (append (fv ty) (remq y (fv b)))))))

(define fresh-from
  (lambda es
    (gensym)))

(define substi
  (lambda (e x s)
    (pmatch
     e
     (* '*)
     ([] '[])
     ((var ,y)
      (if (eq? y x) s e))
     ((app ,f ,a)
      `(app ,(substi f x s) ,(substi a x s)))
     ((,abs (,y ,ty) ,b) (guard (or (eq? abs 'lambda) (eq? abs 'pi)))
      (cond
       ((eq? x y) `(,abs (,y ,(substi ty x s)) ,b))
       ((memq y (fv s))
        (let ((z (fresh-from e s `(var ,x))))
          `(,abs (,z ,(substi ty x s)) ,(substi (substi-var b y z) x s))))
       (else `(,abs (,y ,(substi ty x s)) ,(substi b x s))))))))

(define substi-var
  (lambda (e x z)
    (pmatch
     e
     (* '*)
     ([] '[])
     ((var ,y)
      (if (eq? y x) `(var ,z) e))
     ((app ,f ,a)
      `(app ,(substi-var f x z) ,(substi-var a x z)))
     ((,abs (,y ,ty) ,b) (guard (or (eq? abs 'lambda) (eq? abs 'pi)))
      `(,abs (,y ,(substi-var ty x z)) ,(substi-var b x z))))))

(define const?
  (lambda (s)
    (or (eq? s '*)
        (eq? s '[]))))

(define to
  (lambda (s t)
    (and (const? s)
         (const? t))))

(define arrow
  (lambda (a b)
    `(pi (,(fresh-from b) ,a) ,b)))

(define forall
  (lambda (x a)
    `(pi (,x *) ,a)))


