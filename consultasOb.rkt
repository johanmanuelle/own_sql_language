#lang racket
;;(require "proyecto.scm")



;; utilidades

(provide PANTALLA_INICIAL)
(provide CMD_PROMPT)
(provide ERROR_INPUT)
(provide ERROR_ARGUMENTOS)
(provide ERROR_TABLA)
(provide ERROR_LLAVE LLAVE_NO_VALIDA)
(provide carN)
(provide split or length)
(provide length)
(provide ObtenerIndice)
(provide ObtenerPosicion)
(provide in?)
(provide NOT)
(provide join)
(provide join_reves)



(define (PANTALLA_INICIAL) "Consultas ConOb \n>> ")
(define (CMD_PROMPT) "\n>> ")
(define (ERROR_INPUT)  "Comando no definido: ")
(define (ERROR_ARGUMENTOS) "Parámetros no válidos o insuficientes\n")
(define (ERROR_TABLA) "No se ha seleccionado una tabla existente o correcta en la base de datos\n")
(define (ERROR_LLAVE) "Campos ingresados a la Tabla\n")
(define (LLAVE_NO_VALIDA) "Ya existe un registro con los mismos campos en esta tabla \n")


;;Definicion del parametro or

(define (or . args)
  (cond
    [(null? args) #f]
    [(car args) #t]
    [#t (oraux (cdr args))]
    )
  )
(define (oraux args)
  (cond
    [(null? args) #f]
    [(car args) #t]
    [#t (oraux (cdr args))]
    )
  )

;;Definicion del parametro and
(define (and . args)
  (cond
    [(null? args) #t]
    [(car args) (andaux args)]
    [#t #f]
    )
  )
(define (andaux args)
  (cond
    [(null? args) #t]
    [(car args) (andaux (cdr args))]
    [#t #f]
    )
  )



(define (cdrSTR string [bool #f])
  (cond
    [(= (string-length string) 0) null]
    [(and (equal? (string-ref string 0) #\())(cdrSTR (substring string 1) #t )]
    [(and (equal? (string-ref string 0) #\))) (substring string 1)]
    [(and (equal? (string-ref string 0) #\space) (equal? #f bool)) (substring string 1)] 
    [(and (equal? (string-ref string 0) #\space) (equal? #t bool)) (cdrSTR (substring string 1) #t)] 
    [else (cdrSTR (substring string 1) #f)]
    )
  )

(define (carSTRAux string [bool #f] [elem null])
                                              (cond
                                                [(equal? (string-length string) 0)elem]
                                                [(and (equal? (string-ref string 0) #\() (null? elem))(carSTRAux (substring string 1) #t )]
                                                [(and (equal? (string-ref string 0) #\))) elem]
                                                [(and (equal? (string-ref string 0) #\space) (equal? #f bool)) elem] 
                                                [(and (equal? (string-ref string 0) #\space) (equal? #t bool)) (append elem (carSTRAux (substring string 1) #t elem))] 
                                                [else (append elem (list (make-string 1 (string-ref string 0))) (carSTRAux (substring string 1) #f elem))]
                                                )
  )

(define (carSTR string)(carSTRAux string))

(define (split string)
  (regexp-split #px" " string)
  )
;Se implementa un contador del tamaño de una lista 
(define (length lis)
  (cond
	[(NOT(null? lis)) (+ 1 (length (cdr lis)))]
        [else 0]
)
)

;;obtengoElemento-> Retorna el elemento en el indice especificado
;;
(define (obtengoElemento lis indice)(
                                if (equal? indice 0) 
                                   (car lis)
                                   (ObtenerIndice (cdr lis) (- indice 1))
                               )
 )

;; ObtenerIndice-> Evalua el indice y retorna el elemento que corresponde a este dicho indice 

(define (ObtenerIndice lis indice)(
                                if (< indice (length lis)) 
                                   (obtengoElemento lis indice)
                                   "Índice fuera del rango."
                               )
 )
;ObtenerPosicion-> Funcion complementaria de ObtenerIndice
(define (ObtenerPosicion L position)
  (cond [(null? L) (error  "Índice fuera del rango.")]
    [(= 0 position)(car L)]
    [#t (ObtenerPosicion (cdr L) (sub1 position))]
    )
  )

;;in?-> evalúa si un elemento se encuentra en una lista

(define (in? L element)
  (cond [(null? L) #f]
    [(equal? element (car L))#t]
    [#t (in? (cdr L) element)]
    )
  )

;retorna n cars
(define (carN lis f [i 0] [nL '()])
                                    (cond
                              [(NOT(null? lis))
                               (cond
                                 [(> i 0)(carN (cdr lis) f (- i 1))]
                                 [(> f i)(carN (cdr lis) f (+ i 1) (append nL (list (car lis))))]
                               )
                              ][#t nL]
                              )
  )

;; redefinicion del NOT
(define (NOT param)(cond[param #f][else #t]))


;Realizo un append e los strings de la lista
(define (join L)(cond [(null? L) ""][#t (string-append (car L) " " (join (cdr L)))]))
;;realizo un append de una lista que se encuentra al reves
(define (join_reves L)(cond [(null? L) ""][#t (string-append  (join (cdr L)) " " (car L) )]))



;;Actualizar
(provide actualizar)

;Funcion la cual me ayuda a actualizar un registro creado en la base de datos
(define actualizar (lambda(db args)
                (cond
                  [(< (length args) 3) (display (ERROR_ARGUMENTOS)) db]                 
                  [(equal? (searchpk (cdr db) (car args) (cadr args)) -1)   db ] ; caso en que no se encuentra un registro
                  [(null? (actualizoArgumentos (cddar (BuscarTablaObtenida (cdr db) (car args))) (cddr args)))  db]
                  [#t  (cons (car db) (updateaux (cdr db) args (actualizoArgumentos (cddar (BuscarTablaObtenida (cdr db) (car args))) (cddr args))) )]
                  )      
                )
)
;funcion homologa a select table en una base de datos relacional
(define updateaux (lambda(db args actualizoArgumentos)
                    (cond
                      [(NOT(equal? (car args) (caaar db))) (cons (car db) (updateaux (cdr db) args actualizoArgumentos))]
                      [#t (cons (cons (caar db) (updateaux2 (cddaar db) (cdar db) (cdr args) actualizoArgumentos)) (cdr db) )]
                      )                 
                 )
  )
;;define la llave primaria que hay en una base de datos
(define updateaux2 (lambda(header table args actualizoArgumentos)
                                  (cond
                      [(NOT(equal? (car args) (caar table))) (cons (car table) (updateaux2 header (cdr table) args actualizoArgumentos))]
                      [#t (cons (updateaux3 header (car table) actualizoArgumentos) (cdr table))]
                      )
                 )
  )
;; funcion la cual realiza un cambio en una tabla que ya ha sido creada, es decir actualiza los campos
(define updateaux3 (lambda(header registroDB actualizoArgumentos)
                   
                                  (cond
                      [(null? registroDB) '()]
                      [#t (cons (updatenamevalue (car header) (car registroDB) actualizoArgumentos) (updateaux3 (cdr header) (cdr registroDB) actualizoArgumentos))]
                      )
                 )
  )


;Comparo el valor de una fila y retorno el valor asigando
(define updatenamevalue (lambda(rowname original options)
                      (cond
                      [(null? options) original]
                      [(equal? rowname (caar options)) (cadar options)]
                      [#t (updatenamevalue rowname original (cdr options))]
                      )
                 )
  )

;; funcion que compara los argumentos actualizados 
(define actualizoArgumentos (lambda(header args [result '()] [gotHeader? #f])
                (cond
                  [(null? args) (cond
                                  [(null? result)  result]
                                  [(null? (cdar result)) (display (ERROR_ARGUMENTOS)) '()]
                                  [#t (cons (list (caar result) (join_reves (cdar result)))(cdr result))]
                                  )]
                  [(in? header (car args))
                   (cond
                     [gotHeader? (cond
                                   [(null? (cdar result))(display (ERROR_ARGUMENTOS)) '()]
                                   [#t (actualizoArgumentos header (cdr args) (cons (list (car args)) (cons (list (caar result) (join_reves (cdar result)))(cdr result))) #t)]
                                   )                                 
                     ]
                     [#t (actualizoArgumentos header (cdr args) (cons (list (car args)) result) #t)])
                   ]
                  [#t (cond
                        
                        [gotHeader? (actualizoArgumentos header (cdr args) (cons (append (list (caar result) (car args)) (cdar result)) (cdr result)) #t)]
                        [#t (display (ERROR_ARGUMENTOS)) '()]
                        )
                      ]
                )
        )
)



;;parte de salvar registros

(provide gProcedure)

;;retorna una palabra
(define (wifs str)(cond
                    [(string? str) (list (with-input-from-string str (lambda() (read))))]
                    [(list? str)(cond
                                  [(NOT(null? str))(append (wifs (car str))(wifs (cdr str)))]
                                  [else null]
                                  )
                     ]
                    )
  )
;graba un procedimiento en la base de datos 
(define (saveProcedure functions dictionary word meaning)(cond
                                                     [(NOT(null? functions))
                                                      (cond
                                                        [(equal? (car functions) (car meaning))
                                                         (cons (cons word meaning) dictionary)
                                                         ]
                                                        [else (saveProcedure (cdr functions) dictionary word meaning)]
                                                        )
                                                      ]
                                                     [else 
                                                      (display "\nError, procedimiento no encontrado")
                                                      dictionary
                                                      ]
                                                     )
)



; chequea el nuevo procedimiento
(define (gProcedure db args)(
                              append (list (list (append (caar db)) (saveProcedure (caar db) (cadar db) (car args) (cdr args)))) (cdr db)

                        )
  )


;;funciones adicionales de la base de datos




(provide prompt-read selectAll buscarTabla BuscarTablaObtenida searchpk insertarRegistro remover printTable deltable)
;usuario
(define prompt-read (lambda (Prompt)
                  (display Prompt)    
                  (read-line))
)
; imprime todos los elementos existentes en la base de datos
(define selectAll (lambda (db)
                  (cond
                    [(null? db ) 0]
                    [#t (printTable (car db)) (selectAll (cdr db))]
                    )
                  )
  )
;;
;; Imprime Solo una tabla que vayamos a consultar
(define printTable (lambda (table)
                  (newline)
                  (display "-----------------------" )
                  (newline)
                  (display "Desplegando la tabla: " )
                  (display (caar table))
                  (newline)
                  (newline)
                  (display "Atributos: " )
                  (newline)
                  (display (cddar table))
                  (newline)
                  (display "Campos: " )
                  (printRecords  (cdr table))
                  )
  )
;imprime los campos asociados a la tabla
(define printRecords( lambda (registrosEnDB)
                       (cond
                    [(null? registrosEnDB ) 0]
                    [#t (newline) (display (car registrosEnDB)) (printRecords (cdr registrosEnDB))]                      
                       )
                    )
  )

;;Retorna el indice de la tabla en la base de datos
(define buscarTabla (lambda(db nombreTablaDB [indice 0])
                        (cond
                          [(null? db) (display (ERROR_TABLA)) -1]
                          [(equal? nombreTablaDB (caaar db)) indice]
                          [#t (buscarTabla (cdr db) nombreTablaDB (+ 1 indice))]
                          )
                        )
)

;; Retorna una tabla requerida en la base de datos
(define BuscarTablaObtenida (lambda(db nombreTablaDB [indice 0])
                        (cond
                          [(null? db) (display (ERROR_TABLA)) -1]
                          [(equal? nombreTablaDB (caaar db)) (car db)]
                          [#t (BuscarTablaObtenida (cdr db) nombreTablaDB (+ 1 indice))]
                          )
                        )
)

;;Retorna el indice de la llave primaria en la Base de datos
(define searchpk (lambda(db nombreTablaDB primarykey [indice 0])
                        (cond
                          [(null? db) (display (ERROR_TABLA)) -1]
                          [(equal? nombreTablaDB (caaar db)) (searchpkaux (cdar db) primarykey)]
                          [#t (buscarTabla (cdr db) nombreTablaDB (+ 1 indice))]
                          )
                   )
)


(define searchpkaux (lambda(table primarykey [indice 0])
                        (cond
                          [(null? table) (display (ERROR_LLAVE)) -1]
                          [(equal? primarykey (caar table)) indice]
                          [#t (searchpkaux (cdr table) primarykey (+ 1 indice))]
                          )
                   )
  
)
;Inserto un registro en una tabla de la base de datos 
(define insertarRegistro (lambda(db nombreTablaDB registroDB)
                       (cond
                         [(null? db) (display (ERROR_TABLA)) db]
                         [(equal? nombreTablaDB (caaar db))
                          (cond
                            [(NOT (= (searchpkaux (cdar db) (car registroDB)) -1) ) (display (LLAVE_NO_VALIDA)) db]
                            [#t (cons (cons (caar db) (cons registroDB (cdar db))) (cdr db))]
                            )
                          ]
                         [#t (cons (car db) (insertarRegistro (cdr db) nombreTablaDB registroDB))]
                         )
                        )
)

;;Borra una tabla correspondiente a la base de datos 
(define deltable (lambda(db args)
                   (cond
                  [(NOT (= (length args) 1)) (display (ERROR_ARGUMENTOS)) db]  ; Caso de Parametros no válidos 
                  [#t  (cons (car db) (delaux (cdr db) args))]
                   )
                   )
  )
;;Encuentro una tabla que deseo borrar sólo si esta esta vacía 
(define delaux (lambda(db args)
                     (cond
                      [(NOT(equal? (car args) (caaar db))) (cons (car db) (delaux (cdr db) args))]
                      [(> (length (cdar db)) 0) (display "La Tabla NO se puede borrar ya que no esta vacía") db]
                      [#t  (cdr db) ]
                      )     
                     )
  )
;;Borro un registro de una tabla
(define remover (lambda(db args)
                  (cond
                  [(NOT (= (length args) 2)) (display (ERROR_ARGUMENTOS)) db]  ; argumento no válido              
                  [(equal? (searchpk (cdr db) (car args) (cadr args)) -1)   db ] ; no se encuentra el registro
                  [#t  (cons (car db) (removeraux (cdr db) args))]
                  )
               )
  )
;;Busca en las tablas para eliminar el registro
(define removeraux (lambda(db args)
                     (cond
                      [(NOT(equal? (car args) (caaar db))) (cons (car db) (removeraux (cdr db) args))]
                      [#t (cons (cons (caar db) (removeraux2  (cdar db) (cdr args))) (cdr db) )]
                      )     
                     )
  )

;;Selecciono el registro
(define removeraux2 (lambda(table args)(cond
                                         [(NOT(equal? (car args) (caar table))) (cons (car table) (removeraux2 (cdr table) args))]
                                         [#t (cdr table)]
                                         )
                      )
  )


;;;;;-*----------------consultas--------------------------------------------------!!!!!!!!!!!!!!!!!!!!



(provide ConsultaDB)
;;consultaDb
(define ConsultaDB (lambda (db args)
                (cond
                [(= 1 (length args) ) (printTable(BuscarTablaObtenida (cdr db) (car args))) db ]
                [#t (queryPrintRows (BuscarTablaObtenida (cdr db) (car args)) (cdr args))]
                )
             )
  )
;;funcion que muestra las consultas al usuario final

(define queryPrintRows (lambda (table args)
                  (newline)
                  (display "-----------------------" )
                  (newline)
                  (display "Desplegando la Tabla: " )
                  (display (caar table))
                  (newline)
                  (newline)
                  (display "Columnas: " )
                  (newline)
                  (display args)
                  (newline)
                  (display "Campos: " )
                  (printQueryRecords (cddar table) (cdr table) args)

                         )
  )
;Funcion auxiliar que me permite imprimir los campos de una tabla
(define (printQueryRecords header registrosEnDB args)
                 (cond
                    [(null? registrosEnDB ) 0]
                    [#t (newline) (printQueryRecord header (car registrosEnDB) args) (printQueryRecords header (cdr registrosEnDB) args)]                      
                  )
  )
;;Imprime solo un Registro de una lista de registros que obtengo
(define (printQueryRecord header registroDB args)
  (cond
                    [(null? registroDB) 0]
                    [(in? args (car header)) (display (car registroDB)) (printQueryRecord (cdr header) (cdr registroDB) args)]
                    [#t (printQueryRecord (cdr header) (cdr registroDB) args)]                      
                  )
  )

;;Funciones Para el usuario final
;((nombre numeroDeLlavesForaneas columnas.....) registros...)

(define (CrearListaComandos)( list '("table"  "insert" "actualizar" "remover" "deltable"  "ConsultaDB" "selectAll") '() ) )
;;Evalua las funciones que puede usar el usuario final recursivamente
(define (manageCommand db command)(
  cond [(equal? command "selectAll") (selectAll (cdr db))(manageCommand db (prompt-read (CMD_PROMPT)))] ;; evalua si el comando es selectAll
  [(equal? command "exit") (exit)]
  [(<= (length (regexp-split #px" " command)) 1) (display (ERROR_ARGUMENTOS)) (manageCommand db(prompt-read (CMD_PROMPT)))];muestra error
  [#t (manageCommand (manageCommandAux db (split command)) (prompt-read (CMD_PROMPT)))]
  )
)                  
                     
 
    ;;;Manejador auxiliar de comandos de usuario       
(define manageCommandAux (lambda (db args);(display db)
                                (cond
                             [(equal? (car args) "table")  (table db (cdr args))]
                             [(equal? (car args) "insert") (insert db (cdr args))]
                             [(equal? (car args) "actualizar") (actualizar db (cdr args))]
                             [(equal? (car args) "remover") (remover db (cdr args))]
                             [(equal? (car args) "deltable") (deltable db (cdr args))]
                             [(equal? (car args) "ConsultaDB") (ConsultaDB db (cdr args)) db]
                             [(equal? (car args) "gProcedure") (gProcedure db (cdr args)) ]
                             [(equal? (car args) "EvalProcedure") (ev db (cdr args)) ]
                             [#t (display (string-append (ERROR_INPUT) (car args) "\n")) db]
                             ))
)
;Evalua la funcion que llega del input
(define (ev db args)
  (evAux db (cadar db) (car args) (cdr args))
  )
;; Encuentra el comando asociado en la lista de comandos existentes
(define (evAux db functions alias params)(cond
                                        [(NOT(null? functions))
                                         (cond
                                           [(equal? (caar functions) alias)
                                            (manageCommandAux db (append (cdar functions) params))
                                            ]
                                           [else (evAux db (cdr functions) alias params)]
                                           )
                                         ]
                                        [else 
                                         (display "Funcion No Disponible o no Programada")
                                         db
                                         ]
                                       )
  )



  
  
;;;Agrego una Tabla a la base de datos !!FINALLYYYYYYYYYYYYYY =D
(define (table db args)(cond
                            [(= (length args) 1)(display (ERROR_ARGUMENTOS)) db]
                            [#t (cons (car db) (cons (list (append (list (car args) 0) (cdr args))) (cdr db)))]) ;agrego una tabla con header 0 para llaves foráneas                        
                        )
;Inserto un campo en una tabla!!!!!!!!
(define insert (lambda(db args)
                (cond
                  [(equal? (buscarTabla (cdr db) (car args)) -1) db]; Tabla no disponible
                  [(equal? (length (cddar (BuscarTablaObtenida (cdr db) (car args)))) (length (cdr args)) ) (cons (car db) (insertarRegistro (cdr db) (car args) (cdr args)))];Inserto el registro si el tamaño es igual
                  [#t (display (ERROR_ARGUMENTOS)) db]
                  )      
                )
)
;;"Constructor"
(define (consultasDB) (manageCommand (list (CrearListaComandos)) (prompt-read (PANTALLA_INICIAL))))

(consultasDB)
