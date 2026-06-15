; Requerimiento 1: Estados de transicion;

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== 

(defun transicion (color-actual cambiar-a)
				(cond   ((and (equalp color-actual 'en-rojo)(equalp cambiar-a 'verde))(list color-actual "cambiar-a-verde"))
						((and (equalp color-actual 'en-verde)(equalp cambiar-a 'amarillo))(list color-actual "cambiar-a-amarillo"))
						((and (equalp color-actual 'en-amarillo) (equalp cambiar-a 'rojo))(list color-actual "cambiar-a-rojo"))
						( t(list color-actual 'accion-por-defecto))))
#|
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun transicionIntermitencia (color-actual cambiar-a)
				(cond   ((and (equalp color-actual 'en-rojo-intermitente)(equalp cambiar-a 'verde))(list color-actual "cambiar-a-verde"))
						((and (equalp color-actual 'en-verde-intermitente)(equalp cambiar-a 'amarillo))(list color-actual "cambiar-a-amarillo"))
						((and (equalp color-actual 'en-amarillo-intermitente)(equalp cambiar-a 'rojo))(list color-actual "cambiar-a-rojo"))
						((and (equalp color-actual 'en-rojo)(equalp cambiar-a 'rojo-intermitente))(list color-actual "cambiar-a-rojo-intermitente"))
						((and (equalp color-actual 'en-verde)(equalp cambiar-a 'verde-intermitente))(list color-actual "cambiar-a-verde-intermitente"))
						((and (equalp color-actual 'en-amarillo)(equalp cambiar-a 'amarillo-intermitente))(list color-actual "cambiar-a-amarillo-intermitente"))
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
;Requerimiento 2: Temporizador Automático;

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun timer(tiempo_unix)
				(cond
				((<= (mod tiempo_unix 216) 87) 'rojo)
				((<= (mod tiempo_unix 216) 90) 'rojo-intermitente)
				((<= (mod tiempo_unix 216) 207) 'verde)
				((<= (mod tiempo_unix 216) 210) 'verde-intermitente)
				((<= (mod tiempo_unix 216) 213) 'amarillo)
				(t 'amarillo-intermitente)))


#|;Requerimiento 3: Temporizador Automático;
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun auditoria(color-anterior color-nuevo)
			(format t "Tiempo ~A: La luz ha cambiado de ~A a ~A%" (-(get-universal-time)2208988800) color-anterior color-nuevo))


#|;Requerimiento 4: Análisis de ciclos semafóricos;
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun calcularCiclo()
			(+ 90 6 120))

#|
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun rangoOptimo()
			(if (and (> (calcularCiclo) 35)
					 (< (calcularCiclo) 150))
				(print "Esta en el rango optimo")
				 "No esta en el rango optimo"))

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
#|
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun intervalosCiclo(inicio)
				(cond
				((and(< inicio 90)(<(- (mod 3600 216) (- 90 inicio))120)) (list (- 90 inicio) (- (mod 3600 216) (- 90 inicio)) 0))
				((< inicio 90)(list (+ (- 90 inicio)(- (mod 3600 216) (- 90 inicio) 120 6)) 120 6))
				((< inicio 210)(list 90 (+ (- 210 inicio) (- (mod 3600 216) (- 210 inicio) 6 90)) 6))													 
				((and(>= inicio 210) (> (- (mod 3600 216) (- 216 inicio)) 90))(list 90 (- (mod 3600 216) (- 216 inicio) 90) (- 216 inicio)))		 
				(t (list(- (mod 3600 216) (- 216 inicio)) 0 (- 216 inicio)))))
#|
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun mostrarPorcentajes(hora-unix)
			(list
      'rojo (porcentajeColor (+ (* 16 90) (car (intervalosCiclo (mod hora-unix 216)))))
      'verde (porcentajeColor (+ (* 16 120) (cadr (intervalosCiclo (mod hora-unix 216)))))
      'amarillo (porcentajeColor (+ (* 16 6) (caddr (intervalosCiclo (mod hora-unix 216)))))))

#| Iteracion 2
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun informe (color-actual cambiar-a)
 (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output :if-exists :append)
   (format stream "Informe de Ejecución del Sistema Semafórico~%")
   (format stream "=========================================~%")
   (format stream(auditoriaInforme color-actual cambiar-a))
   (format stream "~% --- Fin del Informe --- ~%")))
#| Iteracion 2
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un timestamp, siempre retorna el mismo color)
;; ESTRATEGIA: Orden Superior (Implementada mediante mapcar y reduce)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun auditoriaInforme (color-anterior color-nuevo)
  (format nil "Tiempo ~A: La luz ha cambiado de ~A a ~A~%" (local-time:format-timestring nil (local-time:now) :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))
(car (transicion color-anterior color-nuevo)) (cadr(transicion color-anterior color-nuevo))))



