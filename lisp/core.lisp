; Requerimiento 1: Estados de transicion;

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ========================================================

#|(defun transicion (color-actual cambiar-a)
				(cond  ((or (equalp color-actual cambiar-a)
						(and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
						(and (equalp color-actual 'rojo)(equalp cambiar-a 'verde))
						(and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo)))
						'accion-por-defecto)

						((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
						((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
						((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
						(t 'accion-por-defecto)))

TRANSICION
Break 5 [7]> (transicion 'amarillo 'rojo)

ACCION-POR-DEFECTO
Break 5 [7]> (transicion 'amarillo 'verde)

(AMARILLO "cambiar-a-verde")
Break 5 [7]> (transicion 'verde 'rojo)

(VERDE "cambiar-a-rojo")
Break 5 [7]> (transicion 'azul 'rojo)

(AZUL "cambiar-a-rojo") ERROR
Break 5 [7]> (transicion 'rojo 'azul)

ACCION-POR-DEFECTO
Break 5 [7]> (defun transicion (color-actual cambiar-a)
				(cond  ((or (equalp color-actual cambiar-a)
						(and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
						(and (equalp color-actual 'rojo)(equalp cambiar-a 'verde))
						(and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo)))
						'accion-por-defecto)

						((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
						((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
						((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
						(t 'accion-por-defecto)))

TRANSICION
Break 5 [7]> (defun transicion (color-actual cambiar-a)
				(cond  (not(or(and (equalp color-actual 'en-rojo)(equalp cambiar-a 'amarillo))
						(and (equalp color-actual 'en-amarillo)(equalp cambiar-a 'verde))
						(and (equalp color-actual 'verde) (equalp cambiar-a 'rojo))))
						'accion-por-defecto)

						((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
						((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
						((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
						(t 'accion-por-defecto))

*** - SYSTEM::%EXPAND-FORM: (EQUALP CAMBIAR-A 'ROJO) debe ser una expresi■ lambda
Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
Break 6 [8]> (defun transicion (color-actual cambiar-a)
				(cond  ((not(or(and (equalp color-actual 'rojo)(equalp cambiar-a 'amarillo))
						(and (equalp color-actual 'amarillo)(equalp cambiar-a 'verde))
						(and (equalp color-actual 'verde) (equalp cambiar-a 'rojo))))
						'accion-por-defecto)

						((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
						((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
						((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
						(t 'accion-por-defecto)) #|
(defun transicion (color-actual cambiar-a)
				(cond  ((not(or(and (equalp color-actual 'rojo)(equalp cambiar-a 'amarillo))
						(and (equalp color-actual 'amarillo)(equalp cambiar-a 'verde))
						(and (equalp color-actual 'verde) (equalp cambiar-a 'rojo))))
						'accion-por-defecto)

						((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
						((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
						((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
						(t 'accion-por-defecto)))

TRANSICION
Break 1 [3]> (transicion 'azul 'verde)

ACCION-POR-DEFECTO
Break 1 [3]> (defun transicion (color-actual cambiar-a)
				(cond  ((not(or(and (equalp color-actual 'en-rojo)(equalp cambiar-a 'amarillo))
						(and (equalp color-actual 'en-amarillo)(equalp cambiar-a 'verde))
						(and (equalp color-actual 'en-verde) (equalp cambiar-a 'rojo))))
						'accion-por-defecto)

						((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
						((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
						((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
						(t 'accion-por-defecto)))

TRANSICION
Break 1 [3]> (transicion 'en-rojo 'verde)

ACCION-POR-DEFECTO |#
(defun transicion (color-actual cambiar-a)
				(cond  ((not(or(and (equalp color-actual 'en-rojo)(equalp cambiar-a 'amarillo))
						(and (equalp color-actual 'en-amarillo)(equalp cambiar-a 'verde))
						(and (equalp color-actual 'en-verde) (equalp cambiar-a 'rojo))))
						(list color-actual 'accion-por-defecto))

						((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
						((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
						((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
						(t (list color-actual 'accion-por-defecto))))
#|
TRANSICION
Break 2 [4]> (transicion 'en-rojo 'verde)

(EN-ROJO ACCION-POR-DEFECTO)
Break 2 [4]> (Transicion 'en-verde 'rojo)

(EN-VERDE "cambiar-a-rojo")
Break 2 [4]> (Transicion 'en-amarillo 'rojo)

(EN-AMARILLO ACCION-POR-DEFECTO)
Break 2 [4]> (Transicion 'en-amarillo 'verde)

(EN-AMARILLO "cambiar-a-verde")
Break 2 [4]> 
Aseguramiento de calidad #|

;Requerimiento 2: Temporizador Automático;

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ========================================================
#| Break 1 [3]> (defun timer(tiempo_unix)
				(cond
				((> (mod tiempo_unix 216) 90) 'rojo)
				((< (mod tiempo_unix 216) 96) 'amarillo)
				( 'verde)))

TIMER
Break 1 [3]> (timer 216)

AMARILLO
Break 1 [3]> (timer 215)

ROJO
Break 1 [3]> (timer 2105)

ROJO
Break 1 [3]> (timer 120)

ROJO
Break 1 [3]> (defun timer(tiempo_unix)
				(cond
				((< (mod tiempo_unix 216) 90) 'rojo)
				((< (mod tiempo_unix 216) 96) 'amarillo)
				( 'verde)))

TIMER
Break 1 [3]> (timer 216)

ROJO
Break 1 [3]> (timer 215)


VERDE
Break 1 [3]> (timer 2105)

VERDE
Break 1 [3]> (timer 120)
(defun timer(tiempo_unix)
				(cond
				((< (mod tiempo_unix 216) 90) 'rojo)
				((< (mod tiempo_unix 216) 96) 'amarillo)
				(t 'verde)))

TIMER
Break 1 [3]> (timer 216)

ROJO
Break 1 [3]> (timer 215)

VERDE
Break 1 [3]> (timer 2105)

VERDE
Break 1 [3]> (timer 120)

VERDE 
Aseguramiento de calidad|#
(defun timer(tiempo_unix)
				(cond
				((< (mod tiempo_unix 216) 90) 'rojo)
				((< (mod tiempo_unix 216) 96) 'amarillo)
				(t 'verde)))

;Requerimiento 3: Temporizador Automático;
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ========================================================
#| (defun auditoria(tiempo-unix color-anterior color-nuevo)
			(format t "Tiempo ~A: La luz ha cambiado de ~A a ~A%" tiempo-unix color-anterior color-nuevo))

(defun auditoria(color-anterior color-nuevo)
			(format t "Tiempo ~A: La luz ha cambiado de ~A a ~A%" (-(get-universal-time)2208988800) color-anterior color-nuevo))
|#

(defun auditoria(color-anterior color-nuevo)
			(format t "Tiempo ~A: La luz ha cambiado de ~A a ~A%" (-(get-universal-time)2208988800) color-anterior color-nuevo))

;Requerimiento 5: Planificación Temporal;
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ========================================================

#| *** - LOAD: A file with name $file does not exist
Break 1 [3]> (defun ciclos-por-tiempo(duracionMinutos)
			(format t "La cantidad de ciclos es: ~A%" (/ (* 60 duracionMinutos) 216)))

CICLOS-POR-TIEMPO
Break 1 [3]> (ciclos-por-tiempo 500)
La cantidad de ciclos es: 1250/9%
NIL
Break 1 [3]> (defun ciclos-por-tiempo(duracionMinutos)
			(format t "La cantidad de ciclos es: ~A%" (truncate(/ (* 60 duracionMinutos) 216))))

CICLOS-POR-TIEMPO
Break 1 [3]> (ciclos-por-tiempo 500)
La cantidad de ciclos es: 138%
NIL
Break 1 [3]> (defun ciclos-por-tiempo(duracionMinutos)
			(format t "La cantidad de ciclos es: ~A" (truncate(/ (* 60 duracionMinutos) 216))))

CICLOS-POR-TIEMPO
Break 1 [3]> (ciclos-por-tiempo 500)
La cantidad de ciclos es: 138
NIL
Break 1 [3]> (ciclos-por-tiempo 216)
La cantidad de ciclos es: 60
NIL
Break 1 [3]> (ciclos-por-tiempo 240)
La cantidad de ciclos es: 66
NIL
Break 1 [3]> (ciclos-por-tiempo 120)
La cantidad de ciclos es: 33
NIL
Break 1 [3]> 
|#
(defun ciclos-por-tiempo(duracionMinutos)
			(format t "La cantidad de ciclos es: ~A%" (truncate(/ (* 60 duracionMinutos) 216))))

