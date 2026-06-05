; Requerimiento 1: Estados de transicion;

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ========================================================
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

(defun timer(tiempo_unix)
				(cond
				((< (mod tiempo_unix 216) 90) 'rojo)
				((< (mod tiempo_unix 216) 96) 'amarillo)
				(t 'verde)))


