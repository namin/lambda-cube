(load "henk.scm")
(load "test-check.scm")

(eg (typ (env-extend (empty-env) 'Int '*)
         `(var Int))
    '*)

(eg (typ (env-extend (empty-env) 'Int '*)
         `(lambda (x (var Int)) (var x)))
    '(pi (x (var Int)) (var Int)))

(eg (typ (env-extend (env-extend (empty-env) 'Int '*) '+ (arrow '(var Int) (arrow '(var Int) '(var Int))))
         '(lambda (x (var Int)) (app (app (var +) (var x)) (var x))))
    '(pi (x (var Int)) (var Int)))

(eg (canon
     (typ (env-extend (env-extend (empty-env) 'Int '*) '+ (arrow '(var Int) (arrow '(var Int) '(var Int))))
          '(lambda (x (var Int)) (app (var +) (var x))))
     0)
    (canon
     (arrow '(var Int) (arrow '(var Int) '(var Int)))
     0))
