; Requerimiento 1: Estados de transicion;

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== 

(defun transicion (color-actual cambiar-a)
				(cond  ((not(or(and (equalp color-actual 'en-rojo)(equalp cambiar-a 'verde))
						(and (equalp color-actual 'en-verde)(equalp cambiar-a 'amarillo))
						(and (equalp color-actual 'en-amarillo) (equalp cambiar-a 'rojo))))
						(list color-actual 'accion-por-defecto))

						((equalp cambiar-a 'rojo)(list color-actual "cambiar-a-rojo"))
						((equalp cambiar-a 'amarillo)(list color-actual "cambiar-a-amarillo"))
						((equalp cambiar-a 'verde)(list color-actual "cambiar-a-verde"))
						(t (list color-actual 'accion-por-defecto))))



;Requerimiento 2: Temporizador Automático;

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun timer(tiempo_unix)
				(cond
				((< (mod tiempo_unix 216) 90) 'rojo)
				((< (mod tiempo_unix 216) 210) 'verde)
				(t 'amarillo)))

#|;Requerimiento 3: Temporizador Automático;
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun auditoria(color-anterior color-nuevo)
			(format t "Tiempo ~A: La luz ha cambiado de ~A a ~A%" (-(get-universal-time)2208988800) color-anterior color-nuevo))

#|;Requerimiento 5: Planificación Temporal;
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun ciclos-por-tiempo(duracionMinutos)
			(format t "La cantidad de ciclos es: ~A%" (truncate(/ (* 60 duracionMinutos) 216))))

#| Requerimiento 6
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun porcentajeColor(segundos)
				(float(* (/ segundos 3600) 100)))

(defun intervalosCiclo(inicio)
				(cond
				((and(< inicio 90)(>(- (mod 3600 216) (- 90 inicio) 6)120)) (list (+ (- 90 inicio) (- (mod 3600 216) (- 90 inicio)6 120))6 120)) ; Arranca en rojo y el verde excede 120,
				((< inicio 90) (list (- 90 inicio) 6 (- (mod 3600 216) (- 90 inicio) 6)))														 ; Arranca en rojo normal
				((< inicio 96) (list (- (mod 3600 216)(- 96 inicio) 120)(- 96 inicio) 120))														 ; Arranca en amarillo normal
				((and(>= inicio 96) (> (- (mod 3600 216) (- 216 inicio)) 90))(list 90 6 (+ (- 216 inicio) (- 144 (- 216 inicio) 90 6))))		 ; Arranca en verde y el rojo excede 90
				(t (list(- (mod 3600 216) (- 216 inicio)) 0 (- 216 inicio)))))																	 ; Arranca en verde normal

(defun mostrarPorcentajes(hora-unix)
			(list
      (porcentajeColor (+ (* 16 90) (car (intervalosCiclo (mod hora-unix 216)))))
      (porcentajeColor (+ (* 16 6) (cadr (intervalosCiclo (mod hora-unix 216)))))
      (porcentajeColor (+ (* 16 120) (caddr (intervalosCiclo (mod hora-unix 216)))))))

