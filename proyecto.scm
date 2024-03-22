#lang eopl
;;(require "consultasOb.rkt")
;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= |<string>|
;;                      <string-exp (str)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> {"elseif" <expression> "then" <expression>}* else <expression>
;;                      <if-exp (exp1 exp2 exp3 exp4 exp5)>
;;                  ::= let {<identifier> = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= apply <expression> ({<expression>}*)
;;                      <app-exp (proc rands)>
;;                  ::= letrec  { ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> := <expression>
;;                     <set-exp (id rhsexp)>
;;                  ::= <bool> 
;;                      <bool-exp (boolean)>
;;  <bool>          ::= true | false 
;;  <primitive>     ::= + | - | * | / | add1 | sub1 | max |min | == | < | > | >= | <= | and | or | not list |cons | car | cdr

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  ;;PARA PROYECTO
  (number    
   (digit (arbno digit)) number) ;Números enteros
  (number    
   ("-" digit (arbno digit)) number) ;Números enteros
  ;;***************************************************************************************
  ;;MODIFICACIONES DERLYN 2/12/2017 scanner-spec-simple-interpreter 
  (str  ("|" (arbno (or letter digit whitespace "_" "-" "?" "." ",")) "|") string);cadena ;String está bien?? 
  ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program ((arbno class-decl) expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression)
                proc-exp)
    (expression ( "apply" expression "("  (arbno expression) ")")
                app-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
                letrec-exp)
    
    ; características adicionales
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier ":=" expression)
                set-exp)
    ;;;;;;
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("list") list-prim)
    ;**************************************************************************************
    ;;TOMADO DEL INTERPRETADOR DE OBJETOS DERLYN 13/12/2017 grammar-simple-interpreter 
    (primitive ("cons") cons-prim)
    ;;(primitive ("nil")  nil-prim)
    (primitive ("car")  car-prim)
    (primitive ("cdr")  cdr-prim)
    ;;(primitive ("null?") null?-prim)
    ;**************************************************************************************
    ;;MODIFICACIONES DERLYN 2/12/2017 grammar-simple-interpreter 
    (expression (str) string-exp) ;;Nueva definicion de string
    (primitive ("/") div-prim) ;Primitiva de division
    (primitive ("==") equal-prim) ;Primitiva de comparación
    (primitive ("max") max-prim) ;Primitiva máximo
    (primitive ("min") min-prim) ;Primitiva minimo
    (primitive (">") mayor-prim) ;Primitiva mayor
    (primitive ("<") menor-prim) ;Primitiva menor
    (primitive ("<=") menorigual-prim) ;Primitiva menor igual
    (primitive (">=") mayorigual-prim) ;Primitiva mayor igual
    (bool ("true") bool-true);; Definicion de primitiva booleana
    (bool ("false") bool-false);; Definicion de primitiva booleana
    (expression (bool) bool-exp);; Expresion de tipo booleana
    ;;MODIFICACIONES DERLYN 3/12/2017 grammar-simple-interpreter
    (primitive ("and") and-prim) ;
    (primitive ("or") or-prim) ;
    (primitive ("not") not-prim) ;
    ;;(expression (lista) list-exp);Expresión de tipo lista
    ;MODIFICACIONES DERLYN 12/12/2017 grammar-simple-interpreter
    (expression
     ("if" expression "then" expression (arbno "elseif" expression "then" expression) "else" expression)
     if-exp)  
    (expression ("for" expression "to" expression "each" expression "do"  (arbno expression) "end") for-exp)
     ;;select current from x where >(current 6)
    (expression ("select current from" expression "where" primitive "(current," (separated-list expression ",")")"  ) select-list-exp)
    (expression  ("null?" expression)  list-null)
    ;MODIFICACIONES DERLYN 14/12/2017 grammar-simple-interpreter
    (expression ("main" "(" expression ")") main-exp)

        ;;**Nuevas caracteristicas para objetos
    ;^;;;;;;;;;;;;;;; new productions for oop ;;;;;;;;;;;;;;;;
    (class-decl                         
      ("class" identifier "("  (separated-list identifier ",") (arbno method-decl)")") ;Declaración de clase
      a-class-decl)
     
    (method-decl ("method" identifier  "("  (separated-list identifier ",") ")" expression ) a-method-decl)

    (expression ("new" identifier "(" (separated-list expression ",") ")") new-object-exp);creando nuevo objeto

    (expression ("class." expression "." identifier "("  (separated-list expression ",") ")") method-app-exp); aplicación d eun metodo

    )
  )



;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?))
;  (proc-exp
;   (ids (list-of symbol?))
;   (body expression?))
;  (app-exp
;   (proc expression?)
;   (args (list-of expression?)))
;  (letrec-exp
;   (proc-names (list-of symbol?))
;   (idss (list-of (list-of symbol?)))
;   (bodies (list-of expression?))
;   (body-letrec expression?))
;  (begin-exp
;   (exp expression?)
;   (exps (list-of expression?)))
;  (set-exp
;   (id symbol?)
;   (rhs expression?)))
;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;*********************************************************************
;MODIFICACIONES DERLYN 3/12/2017 NUEVOS DATATYPES
;(define-datatype lista lista?
  ;(list (list-of expression?))
  ;(null)
  ;;(car (car expression?))
  ;;(cdr (list-of expression?))
  ;;(cons)
;;)






;*********************************************************************

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls body)
                 (elaborate-class-decls! c-decls) ;\new1
                 (eval-expression body (empty-env))))))


;*******************************************************************************************
;Construcción datatype de clases para objetos
(define-datatype class class?;;Modificación ya no tiene un elemento llamado super
  (a-class
    (class-name symbol?) 
    (field-length integer?)  
    (field-ids (list-of symbol?))
    (methods method-environment?)
    (class-env environment?)
    ))

;*******************************************************************************************
;Construcción de clases para objetos

(define elaborate-class-decls!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))


(define elaborate-class-decl! ;;Modificado se eliminó el valor super-name
  (lambda (c-decl)
    
      (let ((field-ids  (append '() (class-decl->field-ids c-decl))))
        (add-to-class-env!
          (a-class
            (class-decl->class-name c-decl)
            (length field-ids)
            field-ids
            (roll-up-method-decls
              c-decl field-ids)
            (extend-env  field-ids   (construyelistan (length field-ids) '()) (empty-env))
            ;;(class-decl->class-methods;;Modificación nueva en reemplazo de la busqueda de metodos del padre solo trae los metodos propios
              ;;c-decl)
            )))))

(define construyelistan
  (lambda (n lista)
    (if (equal? n 0) lista (construyelistan (- n 1) (cons (direct-target 0) lista)))
    )
  )

(define roll-up-method-decls
  (lambda (c-decl field-ids)
    (map
      (lambda (m-decl)
        (a-method m-decl field-ids))
      (class-decl->method-decls c-decl))))



;;;;;;;;;;;;;;;; declarations para objetos ;;;;;;;;;;;;;;;;
;;Obtinee los ids de una cuales
(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name field-ids m-decls)
        m-decls))))


(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name  field-ids m-decls)
        class-name))))


(define class-decl->class-methods;;Nueva creada por nosotros
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name  field-ids m-decls)
        m-decls))))


;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;
(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
        class-name))))


(define class->class-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name field-length field-ids methods class-env);;Modificación se elimina el argumento super-name
        class-name))))



(define class-env->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->env (lookup-class class-name)))))


(define class->env
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name field-length field-ids methods class-env);;Modificación se elimina el argumento super-name
        class-env))))

(define class-name->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->methods (lookup-class class-name)))))

(define class-ids->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->ids (lookup-class class-name)))))

(define class->ids
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name field-length field-ids methods class-env)
        class->ids))))

(define class-vals->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->ids (lookup-class class-name)))))



(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
        fields))))

(define class->methods
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name field-length field-ids methods class-env)
        methods))))


(define class-name->field-length
  (lambda (class-name)
    (if (eqv? class-name 'object)
        0
        (class->field-length (lookup-class class-name)))))

(define class->field-length
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name field-length field-ids methods class-env);; Modificación se elimina el argumento super-name
        field-length))))


(define method->method-name;; obtiene el nombre del metodo
  (lambda (method)
    (method-decl->method-name (method->method-decl method))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method->method-decl
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl  field-ids) meth-decl))))

(define method->ids
  (lambda (method)
    (method-decl->ids (method->method-decl method))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))


(define method->body
  (lambda (method)
    (method-decl->body (method->method-decl method))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

;(define method->super-name
;  (lambda (meth)
;    (cases method meth
;      (a-method (meth-decl super-name field-ids) super-name))))

(define method->field-ids
  (lambda (meth)
    (cases method meth
      (a-method (method-decl field-ids) field-ids))))


;;;;;;;;;;;;;;;; method environments para objetos;;;;;;;;;;;;;;;;
;^;;;;;;;;;;;;;;; methods datatype para objetos;;;;;;;;;;;;;;;;

(define-datatype method method?
  (a-method
    (method-decl method-decl?)
    (field-ids (list-of symbol?))))


(define method-environment? (list-of method?))

(define lookup-method                   
  (lambda (m-name methods)
    (cond
      ((null? methods) #f)
      ((eqv? m-name (method->method-name (car methods)))
       (car methods))
      (else (lookup-method m-name (cdr methods))))))

;^;;;;;;;;;;;;;;; methods para objetos;;;;;;;;;;;;;;;;

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (let loop ((host-name host-name))
      (if (eqv? host-name 'object)
          (eopl:error 'find-method-and-apply
            "No method for name ~s" m-name)
          (let ((method (lookup-method m-name ;^ m-decl -> method
                          (class-name->methods host-name))))
            (if (method? method)
                (apply-method method host-name self args)
                (loop (host-name host-name))))))))



(define apply-method
  (lambda (method host-name self args)                ;\new5
    (let ((ids (method->ids method))
          (body (method->body method))
          ;;(super-name (method->super-name method))
          (field-ids (method->field-ids method))       
          (fields (object->fields self))
          (class-ids (method->body method))
          )
          
      (eval-expression body
        (extend-env  ;;(syms vals env)
          (cons '%super (cons 'self ids))
          (cons 'a (cons self args))
           (class-env->methods host-name)))
      )
    ))

;^;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;
;^; an object is now just a single part, with a vector representing the
;^; managed storage for the all the fields. 

;^;;;;;;;;;;;;;;; objects datatype para objetos ;;;;;;;;;;;;;;;;

(define-datatype object object? 
  (an-object
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (make-vector (class-name->field-length class-name))))) ;\new1


;*******************************************************************************************
;Construcción de ambientes de clases para objetos
;;Construcción de ambiente vacio
(define the-class-env '()) ;Vector con ambiente vacio

(define initialize-class-env!
  (lambda ()
    (set! the-class-env '())))

(define add-to-class-env!
  (lambda (class)
    (set! the-class-env (cons class the-class-env))))

(define lookup-class                    
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env) (eopl:error 'lookup-class
                       "Unknown class ~s" name))
        ((eqv? (class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))


; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))

;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z f)
;     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) ())) ())))
;                      (empty-env)))
;     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**************************************************************************************

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (apply-primitive prim args)))
      (let-exp (ids rands body)
               (let ((args (eval-let-exp-rands rands env)))
                 (eval-expression body (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      (begin-exp (exp exps)
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))
      ;MODIFICACIONES DERLYN 2/12/2017 eval-expression
      (string-exp (datum) datum)
      (bool-exp (boolean)
                 (cases bool boolean
                          (bool-true ()  #t)
                          (bool-false () #f)
                          )
                 )
      ;MODIFICACIONES DERLYN 14/12/2017 eval-expression
      (if-exp (test-exp true-exp test-exps-in true-exps-in  false-exp) 

             (if (equal? (eval-expression test-exp env) #t)
                 (eval-expression true-exp env)
                 (apply-elseif  test-exps-in true-exps-in false-exp env)
                 )
              )
      (for-exp (num1 num2 increment exps)
               (for-each (lambda (x) (begin (display (eval-rands exps env)) (newline)))
                         (aux (- (eval-expression num2 env ) (eval-expression num1 env)) (eval-expression increment env)  ))
                 

               )     
      ( select-list-exp (list prim rands)
                        (let ((args (eval-primapp-exp-rands rands env)))
                           (seleccion-list (eval-expression list env) prim args  '() ))
                        )
      (list-null (lista) (if (null? lista) #t #f))
      (main-exp (exp) (eval-expression exp env))
      ;MODIFICACIONES DERLYN 14/12/2017 eval-expression
      ;;Eval-expression para objetos
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            class-name class-name obj args)
          obj))

      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))
      
      
      )))


;funcion auxiliar para • Se puede selección de elementos de cierta lista que cumplan con cierta propiedad
;entradas:
;Salidas:
(define seleccion-list
  (lambda(list prim args list-r)
      (cases primitive prim
      (add-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (substract-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (mult-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (incr-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (decr-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      ;***********************************************************************************
       ;;TOMADO DEL INTERPRETADOR DE OBJETOS DERLYN 13/12/2017 grammar-simple-interpreter 
      (list-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" )) 
      (car-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (cdr-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (cons-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      ;;(null?-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      ;***********************************************************************************
      ;MODIFICACIONES DERLYN 2/12/2017 apply-primitive
      (div-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (equal-prim () (if (null? list) list-r (if (equal? (apply-primitive prim (cons (car list) (cons (car args) empty))) #t)
                                                 ( seleccion-list (cdr list) prim args (append list-r (cons (car list) empty)) )
                                                 ( seleccion-list (cdr list) prim args list-r ))) )
      (max-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (min-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
        
      (mayor-prim () (if (null? list) list-r (if (equal? (apply-primitive prim (cons (car list) (cons (car args) empty))) #t)
                                                 ( seleccion-list (cdr list) prim args (append list-r (cons (car list) empty)) )
                                                 ( seleccion-list (cdr list) prim args list-r ))))
      (menor-prim () (if (null? list) list-r (if (equal? (apply-primitive prim (cons (car list) (cons (car args) empty))) #t)
                                                 ( seleccion-list (cdr list) prim args (append list-r (cons (car list) empty)) )
                                                 ( seleccion-list (cdr list) prim args list-r ))))
      (menorigual-prim () (if (null? list) list-r (if (equal? (apply-primitive prim (cons (car list) (cons (car args) empty))) #t)
                                                 ( seleccion-list (cdr list) prim args (append list-r (cons (car list) empty)) )
                                                 ( seleccion-list (cdr list) prim args list-r ))))
      (mayorigual-prim () (if (null? list) list-r (if (equal? (apply-primitive prim (cons (car list) (cons (car args) empty))) #t)
                                                 ( seleccion-list (cdr list) prim args (append list-r (cons (car list) empty)) )
                                                 ( seleccion-list (cdr list) prim args list-r ))))
      ;MODIFICACIONES DERLYN 3/12/2017 apply-primitive
      (and-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (or-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      (not-prim () (eopl:error 'apply-env-ref "Solo se pueden usar las propiedades de comparación ==,>,>=, <,<=" ))
      )
      )
  )

;define aux
 ; (lambda (fin inc)
  ;  (let loop ((next 0))
   ;   (if (>= next fin) '()
    ;    (cons next (loop (+ inc next)))))))


; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (aplica-primitiva-valores-lista "+" args))
      (substract-prim () (aplica-primitiva-valores-lista "-" args))
      (mult-prim () (aplica-primitiva-valores-lista "*" args))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      ;***********************************************************************************
       ;;TOMADO DEL INTERPRETADOR DE OBJETOS DERLYN 13/12/2017 grammar-simple-interpreter 
      (list-prim () args) 
      (car-prim () (car (car args)))
      (cdr-prim () (cdr (car args)))
      (cons-prim () (cons (car args) (cdr args)))
      ;;(null?-prim () (if (null? args) #t #f))
      ;***********************************************************************************
      ;MODIFICACIONES DERLYN 2/12/2017 apply-primitive
      (div-prim () (/ (car args) (cadr args)))
      (equal-prim () (if(equal? (car args) (cadr args)) #t #f))
      ;;(equal-prim () args)
      (max-prim () (if(> (car args) (cadr args)) (car args) (cadr args)))
      (min-prim () (if(< (car args) (cadr args)) (car args) (cadr args)))
      (mayor-prim () (if(> (car args) (cadr args)) #t #f))
      (menor-prim () (if(< (car args) (cadr args)) #t #f))
      (menorigual-prim () (if(<= (car args) (cadr args)) #t #f))
      (mayorigual-prim () (if(>= (car args) (cadr args)) #t #f))
      ;MODIFICACIONES DERLYN 3/12/2017 apply-primitive
      (and-prim () (aplica-and args))
      (or-prim () (aplica-or args))
      (not-prim () (if (equal? (car args) #f) #t (if (equal? (car args) #t) #f (eopl:error 'apply-env-ref "La expresión dentro del not debe retornar un resultado booleano" ))))
      )))

;************************************************************************************************************************************
;MODIFICACIONES DERLYN 2/12/2017 FUNCIONES NUEVAS
;Función que dado un string con la primitiva +, - ó *, y una lista de enteros suma, resta ó multiplica todos los valores de la lista
;Entradas:
;   prim = string con el valor de una primitiva puede ser +, - ó *
;   args = lista de enteros que se pueden sumar, restar o multiplicar
;Salidas: entero resultado de la suma, resta o multiplicacion de los numeros de la lista
(define aplica-primitiva-valores-lista
  (lambda (prim args)
    (if (null? args)
        (if (equal? prim "*")
            1
            0
            )
        (if (equal? prim "+")
            (+ (car args) (aplica-primitiva-valores-lista prim (cdr args)))
            (if (equal? prim "-")
                (- (car args) (aplica-primitiva-valores-lista prim (cdr args)))
                (if (equal? prim "*")
                    (* (car args) (aplica-primitiva-valores-lista prim (cdr args)))
                    #f
                    )
             )
         )
    )
  )
)
;MODIFICACIONES DERLYN 3/12/2017 FUNCIONES NUEVAS
;Función  que aplica la funcionalidad and sobre una lista de argumentos
;entradas:
;   args: lista con las expresiones evaluadas
;salidas: #t sí el resultado de todas las expresiones es true, #f si alguno de los resultados de las expresiones es false,
;         ó error las expresiones dentro del not deben retornar un resultado booleano
(define aplica-and
  (lambda (args)
    (if (null? args)
        #t
        (if
         (equal? (car args) #t)
         (aplica-and (cdr args))
         (if (equal? (car args) #f) #f (eopl:error 'apply-env-ref "Las expresiones dentro del not deben retornar un resultado booleano"))))
    )
  )

;Función  que aplica la funcionalidad or sobre una lista de argumentos
;entradas:
;   args: lista con las expresiones evaluadas
;salidas: #f sí el resultado de todas las expresiones es falso, #t si alguno de los resultados de las expresiones es true,
;         ó error las expresiones dentro del not deben retornar un resultado booleano
(define aplica-or
  (lambda (args)
    (if (null? args)
        #f
        (if
         (equal? (car args) #t)
         #t
         (if (equal? (car args) #f) (aplica-or (cdr args)) (eopl:error 'apply-env-ref "Las expresiones dentro del not deben retornar un resultado booleano"))))
    )
  )

;funcion auxiliar que se usa para aplicar la expresion elseif
;entradas:
;   test-exps-in: lista expresiones a evaluar como falsa o verdadera por cada elseif que contiene el if
;   true-exps-in: lista de expresiones a evaluar como resultado de if verdadero por cada elseif
;   false-exp: expresion a evaluar como resultado de if falso
;   env
;salidas: resultado de la expresión que decide evaluar el elseif
(define apply-elseif
        (lambda (test-exps-in true-exps-in false-exp env)
          (if (null? test-exps-in)
              (eval-expression false-exp env)
              (if (equal? (eval-expression (car test-exps-in) env) #t)
                  (eval-expression (car true-exps-in) env)
                  (apply-elseif true-exps-in true-exps-in false-exp)))))

;funcion auxiliar del for
;entradas:
;Salidas:
(define aux
  (lambda (fin inc)
    (let loop ((next 0))
      (if (>= next fin) '()
            (cons next (loop (+ inc next)))   ))))


;;;;;;;;;;;;;;;;;;;;;;;;;

;************************************************************************************************************************************
;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (procval? x)
        ;***********************************************************************************
        ;MODIFICACIONES DERLYN 3/12/2017 expval?
        (bool? x) (string? x) (boolean? x) (list? x) (object? x)
        )))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************
;Pruebas

(show-the-datatypes)
just-scan
scan&parse
(just-scan "add1(x)")
(just-scan "add1(   x   )%cccc")
(just-scan "add1(  +(5, x)   )%cccc")
(just-scan "add1(  +(5, %ccccc x) ")
(scan&parse "add1(x)")
(scan&parse "add1(   x   )%cccc")
(scan&parse "add1(  +(5, x)   )%cccc")
(scan&parse "add1(  +(5, %cccc
x)) ")
(scan&parse "if -(x,4) then +(y,11) else *(y,10)")
(scan&parse "let
x = -(y,1)
in
let
x = +(x,2)
in
add1(x)")

(define caso1 (primapp-exp (incr-prim) (list (lit-exp 5))))
(define exp-numero (lit-exp 8))
(define exp-ident (var-exp 'c))
(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
;;(define programa (a-program exp-app))
(define una-expresion-dificil (primapp-exp (mult-prim)
                                           (list (primapp-exp (incr-prim)
                                                              (list (var-exp 'v)
                                                                    (var-exp 'y)))
                                                 (var-exp 'x)
                                                 (lit-exp 200))))
;;(define un-programa-dificil
;;    (a-program una-expresion-dificil))

(interpretador)

;;(consultasDB)
