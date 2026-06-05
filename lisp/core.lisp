; Requerimiento 1: Estados de transicion;

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ========================================================

#|Break 1 [3]> (defun transicion (color-actual cambiar-a)
			(cond( or(equalp color-actual cambiar-a
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo))
				 'accion-por-defecto))
				 
			((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
			((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
			((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
			(t 'accion-por-defecto)))

TRANSICION
Break 1 [3]> (transicion 'rojo 'verde)

*** - COND: variable OR has no value
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      Input a value to be used instead of OR.
STORE-VALUE    :R2      Input a new value for OR.
ABORT          :R3      Abort debug loop
Break 2 [4]> (defun transicion (color-actual cambiar-a)
			(cond( or(equalp color-actual cambiar-a
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo)))
				 'accion-por-defecto)
				 
			((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
			((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
			((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
			(t 'accion-por-defecto)))

TRANSICION
Break 2 [4]> (transicion 'rojo 'verde)

*** - COND: variable OR has no value
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      Input a value to be used instead of OR.
STORE-VALUE    :R2      Input a new value for OR.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
Break 3 [5]> (defun transicion (color-actual cambiar-a)
			(cond( or(equalp color-actual cambiar-a)
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo))
				 'accion-por-defecto)
				 
			((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
			((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
			((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
			(t 'accion-por-defecto)))

TRANSICION
Break 3 [5]> (transicion 'rojo 'verde)

*** - COND: variable OR has no value
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      Input a value to be used instead of OR.
STORE-VALUE    :R2      Input a new value for OR.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
Break 4 [6]> (defun transicion (color-actual cambiar-a)
			(cond( or(equalp color-actual cambiar-a)
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo)))
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
Break 5 [7]> (transicion 'rojo 'rojo)

*** - EVAL: la funci■ TRANSICION no est■definida
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      Input a value to be used instead of (FDEFINITION 'TRANSICION).
RETRY          :R2      Reintentar
STORE-VALUE    :R3      Input a new value for (FDEFINITION 'TRANSICION).
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
Break 6 [8]> (defun transicion (color-actual cambiar-a)
			(cond( or(equalp color-actual cambiar-a)
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo)))
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
ABORT          :R6      Abort debug loop
Break 7 [9]> (defun transicion (color-actual cambiar-a)
			(cond( or(equalp color-actual cambiar-a
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo))))
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
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
Break 8 [10]> (defun transicion (color-actual cambiar-a)
			(cond( or(equalp color-actual cambiar-a
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo)))
				 'accion-por-defecto)
				 
			((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
			((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
			((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
			(t 'accion-por-defecto)))

TRANSICION
Break 8 [10]> (transicion 'rojo 'rojo)

*** - COND: variable OR has no value
Es posible continuar en los siguientes puntos:
USE-VALUE      :R1      Input a value to be used instead of OR.
STORE-VALUE    :R2      Input a new value for OR.
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
Break 9 [11]> (defun transicion (color-actual cambiar-a)
			(cond( (or(equalp color-actual cambiar-a
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo))))
				 'accion-por-defecto)
				 
			((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
			((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
			((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
			(t 'accion-por-defecto)))

TRANSICION
Break 9 [11]> (transicion 'rojo 'rojo)

*** - EVAL: se han entregado demasiados argumentos a EQUALP:
       (EQUALP COLOR-ACTUAL CAMBIAR-A (AND (EQUALP COLOR-ACTUAL 'AMARILLO) (EQUALP CAMBIAR-A 'ROJO))
        (AND (EQUALP COLOR-ACTUAL 'ROJO) (EQUALP CAMBIAR-A 'VERDE))
        (AND (EQUALP COLOR-ACTUAL 'VERDE) (EQUALP CAMBIAR-A 'AMARILLO)))

Es posible continuar en los siguientes puntos:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort debug loop
ABORT          :R3      Abort debug loop
ABORT          :R4      Abort debug loop
ABORT          :R5      Abort debug loop
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
Break 10 [12]> (defun transicion (color-actual cambiar-a)
			(cond( (or(equalp color-actual cambiar-a)
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo))))
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
ABORT          :R6      Abort debug loop
ABORT          :R7      Abort debug loop
ABORT          :R8      Abort debug loop
ABORT          :R9      Abort debug loop
ABORT          :R10     Abort debug loop
Break 11 [13]> (defun transicion (color-actual cambiar-a)
			(cond((or(equalp color-actual cambiar-a)
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo)))
				 'accion-por-defecto)
				 
			((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
			((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
			((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
			(t 'accion-por-defecto)))

TRANSICION
Break 11 [13]> (transicion 'rojo 'rojo)

ACCION-POR-DEFECTO
Break 11 [13]> (transicion 'amarillo 'rojo)

ACCION-POR-DEFECTO
Break 11 [13]> (transicion 'verde 'rojo)

(VERDE "cambiar-a-rojo")
Break 11 [13]> (transicion 'rojo 'azul)

ACCION-POR-DEFECTO 
Aseguramiento de Calidad|#

(defun transicion (color-actual cambiar-a)
			(cond((or(equalp color-actual cambiar-a)
				 (and (equalp color-actual 'amarillo)(equalp cambiar-a 'rojo))
				 (and (equalp color-actual 'rojo) (equalp cambiar-a 'verde))
				 (and (equalp color-actual 'verde) (equalp cambiar-a 'amarillo)))
				 'accion-por-defecto)
				 
			((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
			((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
			((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
			(t 'accion-por-defecto)))

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

(defun auditoria(color-anterior color-nuevo)
			(format t "Tiempo ~A: La luz ha cambiado de ~A a ~A%" (-(get-universal-time)2208988800) color-anterior color-nuevo))

;Requerimiento 5: Planificación Temporal;
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ========================================================
(defun ciclos-por-tiempo(duracionMinutos)
			(format t "La cantidad de ciclos es: ~A%" (truncate(/ (* 60 duracionMinutos) 216))))

