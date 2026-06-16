; Requerimiento 1: Estados de transicion;
;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura 
;; ESTRATEGIA: Funcion predicado (Evalua mediante el cond y equalp)
;; IMPACTO: No destructiva
;; ======================================================== 

(defun transicion (color-actual cambiar-a)
				(cond   ((and (equalp color-actual 'en-rojo)(equalp cambiar-a 'verde))(list color-actual "cambiar-a-verde"))
						((and (equalp color-actual 'en-verde)(equalp cambiar-a 'amarillo))(list color-actual "cambiar-a-amarillo"))
						((and (equalp color-actual 'en-amarillo) (equalp cambiar-a 'rojo))(list color-actual "cambiar-a-rojo"))
						( t(list color-actual 'accion-por-defecto))))
#|
Casos de prueba
TRANSICION
Break 1 [3]> (transicion 'en-verde 'amarillo)

(EN-VERDE "cambiar-a-amarillo")
Break 1 [3]> (transicion 'en-verde 'rojo)

(EN-VERDE ACCION-POR-DEFECTO)
Break 1 [3]> (transicion 'en-amarillo 'rojo)

(EN-AMARILLO "cambiar-a-rojo")
Break 1 [3]>  

;Requerimiento 2: Temporizador Automático;
;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un mismo tiempo, siempre retorna el mismo color)
;; ESTRATEGIA: Funcion predicado ("Descompone" el tiempo y los clasifica en sus intervalos con operadores logicos)
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun timer(tiempo_unix)
				(cond
				((<= (mod tiempo_unix 216) 90) 'rojo)
				((<= (mod tiempo_unix 216) 210) 'verde)
				(t 'amarillo)))
#| Casos de prueba
Break 2 [4]> (timer 500)
(timer 0)
(timer -1)
(timer 3)
(timer 6)
(timer 210)
(timer 209)

ROJO
Break 2 [4]> 
ROJO
Break 2 [4]> 
AMARILLO
Break 2 [4]> 
ROJO
Break 2 [4]> 
ROJO
Break 2 [4]> 
VERDE
Break 2 [4]> 
VERDE
Break 2 [4]>  #|

;Requerimiento 3: Temporizador Automático;
;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: impura (Imprime por pantalla)
;; ESTRATEGIA: Uso de funciones predicado y mostrar por pantalla
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun auditoria(color-anterior color-nuevo)
			(format t "Tiempo ~A: La luz ha cambiado de ~A a ~A%" 
					(-(get-universal-time)2208988800) (car(transicion color-anterior color-nuevo)) (cadr(transicion color-anterior color-nuevo))))
#| Casos de prueba
(auditoria 'en-rojo 'verde)
Tiempo 1781594993: La luz ha cambiado de EN-ROJO a cambiar-a-verde%
NIL
Break 1 [3]> (auditoria 'en-verde 'amarillo)
Tiempo 1781595014: La luz ha cambiado de EN-VERDE a cambiar-a-amarillo%
NIL 

;Requerimiento 4: Análisis de ciclos semafóricos;
;; ========================================================
;; FUNCIÓN: calcularCiclo
;; NATURALEZA: Pura (solo calcula)
;; ESTRATEGIA: Operacion aritmetica
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun calcularCiclo()
			(+ 90 6 120))

#|
;; ========================================================
;; FUNCIÓN: rangoOptimo
;; NATURALEZA: Impura (Imprime por pantalla)
;; ESTRATEGIA: Funcion predicado (Evaluacion condicional mediante if)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun rangoOptimo()
			(if (and (> (calcularCiclo) 35)
					 (< (calcularCiclo) 150))
				(print "Esta en el rango optimo")
				 "No esta en el rango optimo"))
#| Casos de prueba
(rangoOptimo)

"No esta en el rango optimo" 


;Requerimiento 5: Planificación Temporal;
;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Impura (Muestra un resultado)
;; ESTRATEGIA: Hace una operacion y muestra el valor
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun ciclos-por-tiempo(duracionMinutos)
			(format t "La cantidad de ciclos es: ~A" (truncate(/ (* 60 duracionMinutos) 216))))
#| Casos de prueba

Break 1 [3]> (ciclos-por-tiempo 10)
La cantidad de ciclos es: 2
NIL
Break 1 [3]> (ciclos-por-tiempo 15)
La cantidad de ciclos es: 4
NIL
Break 1 [3]> (ciclos-por-tiempo 1000)
La cantidad de ciclos es: 277
NIL
Break 1 [3]> (ciclos-por-tiempo -1)
La cantidad de ciclos es: 0
NIL




 Requerimiento 6
;; ========================================================
;; FUNCIÓN: PorcentajeColor
;; NATURALEZA: Pura (Calcula porcentaje)
;; ESTRATEGIA: Operacion Aritmetica
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun porcentajeColor(segundos)
				(float(* (/ segundos 3600) 100)))
#|
;; ========================================================
;; FUNCIÓN: intervalosCiclo
;; NATURALEZA: Pura (No muestra ni modifica nada)
;; ESTRATEGIA: Funcion predicado (Evalua a traves del numero ingresado por parametro y ve donde arranca el ciclo)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun intervalosCiclo(inicio)
				(cond
				((and(< inicio 90)(<(- (mod 3600 216) (- 90 inicio))120)) (list (- 90 inicio) (- (mod 3600 216) (- 90 inicio)) 0))
				((< inicio 90)(list (+ (- 90 inicio)(- (mod 3600 216) (- 90 inicio) 120 6)) 120 6))
				((< inicio 210)(list (- (mod 3600 216) (+ (- 210 inicio) 6)) (- 210 inicio) 6))												 
				((and(>= inicio 210) (> (- (mod 3600 216) (- 216 inicio)) 90))(list 90 (- (mod 3600 216) (- 216 inicio) 90) (- 216 inicio)))		 
				(t (list(- (mod 3600 216) (- 216 inicio)) 0 (- 216 inicio)))))
#|Casos de prueba
(intervalosCiclo1 0)
(intervalosCiclo1 52)
(intervalosCiclo1 85)
(intervalosCiclo1 90)
(intervalosCiclo1 100)
(intervalosCiclo1 150)
(intervalosCiclo1 210)
(intervalosCiclo1 212)
(intervalosCiclo1 215)

(90 54 0)
Break 2 [4]> 
(38 106 0)
Break 2 [4]> 
(18 120 6)
Break 2 [4]> 
(18 120 6)
Break 2 [4]> 
(28 110 6)
Break 2 [4]> 
(78 60 6)
Break 2 [4]> 
(90 48 6)
Break 2 [4]> 
(90 50 4)
Break 2 [4]> 
(90 53 1) 



;; ========================================================
;; FUNCIÓN: mostrarPorcentajes
;; NATURALEZA: Pura (No imprime nada por pantalla ni modifica)
;; ESTRATEGIA: Evaluacion directa (ejecuta las funciones creadas anteriormente)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun mostrarPorcentajes(hora-unix)
			(list
      'rojo (porcentajeColor (+ (* 16 90) (car (intervalosCiclo (mod hora-unix 216)))))
      'verde (porcentajeColor (+ (* 16 120) (cadr (intervalosCiclo (mod hora-unix 216)))))
      'amarillo (porcentajeColor (+ (* 16 6) (caddr (intervalosCiclo (mod hora-unix 216)))))))
#| Casos de prueba
(mostrarPorcentajes 1781639616) (mostrarPorcentajes 1781639701) (mostrarPorcentajes 1781639716) (mostrarPorcentajes 1781639826) (mostrarPorcentajes 1781639831)

(ROJO 41.333332 VERDE 55.833332 AMARILLO 2.8333333)
Break 2 [4]> 
(ROJO 43.694443 VERDE 53.47222 AMARILLO 2.8333333)
Break 2 [4]> 
(ROJO 42.38889 VERDE 54.944443 AMARILLO 2.6666667)
Break 2 [4]> 
(ROJO 41.166668 VERDE 56.0 AMARILLO 2.8333333)
Break 2 [4]> 
(ROJO 41.305557 VERDE 55.86111 AMARILLO 2.8333333)



Iteracion 1
;; ========================================================
;; FUNCIÓN: transicionIntermitencia
;; NATURALEZA: Pura 
;; ESTRATEGIA: Funcion predicado (Evalua mediante el cond y equalp)
;; IMPACTO: No destructiva
;; ========================================================  |#

(defun transicionIntermitencia (color-actual cambiar-a)
				(cond   ((and (equalp color-actual 'en-rojo-intermitente)(equalp cambiar-a 'verde))(list color-actual "cambiar-a-verde"))
						((and (equalp color-actual 'en-verde-intermitente)(equalp cambiar-a 'amarillo))(list color-actual "cambiar-a-amarillo"))
						((and (equalp color-actual 'en-amarillo-intermitente)(equalp cambiar-a 'rojo))(list color-actual "cambiar-a-rojo"))
						((and (equalp color-actual 'en-rojo)(equalp cambiar-a 'rojo-intermitente))(list color-actual "cambiar-a-rojo-intermitente"))
						((and (equalp color-actual 'en-verde)(equalp cambiar-a 'verde-intermitente))(list color-actual "cambiar-a-verde-intermitente"))
						((and (equalp color-actual 'en-amarillo)(equalp cambiar-a 'amarillo-intermitente))(list color-actual "cambiar-a-amarillo-intermitente"))
						(t (list color-actual 'accion-por-defecto))))
#|Casos de prueba
Break 1 [3]> (transicionIntermitencia 'en-rojo-intermitente 'verde)

(EN-ROJO-INTERMITENTE "cambiar-a-verde")
Break 1 [3]> (transicionIntermitencia 'en-rojo 'rojo-intermitente)

(EN-ROJO "cambiar-a-rojo-intermitente")
Break 1 [3]> (transicionIntermitencia 'en-amarillo-intermitente 'rojo)

(EN-AMARILLO-INTERMITENTE "cambiar-a-rojo")
Break 1 [3]> (transicionIntermitencia 'en-azul-intermitente 'rojo)

(EN-AZUL-INTERMITENTE ACCION-POR-DEFECTO)

;; ========================================================
;; FUNCIÓN: timerIntermitencia
;; NATURALEZA: Pura (Dado un mismo tiempo, siempre retorna el mismo color)
;; ESTRATEGIA: Funcion predicado ("Descompone" el tiempo y los clasifica en sus intervalos con operadores logicos)
;; IMPACTO: No destructiva
;; ========================================================|#| 

(defun timerIntermitencia(tiempo_unix)
				(cond
				((<= (mod tiempo_unix 216) 87) 'rojo)
				((<= (mod tiempo_unix 216) 90) 'rojo-intermitente)
				((<= (mod tiempo_unix 216) 207) 'verde)
				((<= (mod tiempo_unix 216) 210) 'verde-intermitente)
				((<= (mod tiempo_unix 216) 213) 'amarillo)
				(t 'amarillo-intermitente)))
#|
Casos de prueba:
Break 1 [3]> (timerIntermitencia 0)

ROJO
Break 1 [3]> (timerIntermitencia 91) 

VERDE
Break 1 [3]>  (timerIntermitencia 211)

AMARILLO
Break 1 [3]> (timerIntermitencia 208)

VERDE-INTERMITENTE
Break 1 [3]> (timerIntermitencia 1781584909)

ROJO#


#|
;; ========================================================
;; FUNCIÓN: intervalosCicloIntermitencia
;; NATURALEZA: Pura (No muestra ni modifica nada)
;; ESTRATEGIA: Funcion predicado (Evalua a traves del numero ingresado por parametro y ve donde arranca el ciclo)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun intervalosCicloIntermitencia(inicio)
  (cond
    ((and (< inicio 87) (< (- (mod 3600 216) (- 87 inicio) 3) 117)) (list (- 87 inicio) 3 (- (mod 3600 216) (- 87 inicio) 3) 0 0 0))
    ((< inicio 87) (list (+ (- 87 inicio) (- (mod 3600 216) (- 87 inicio) 3 117 3 3 3)) 3 117 3 3 3))
    ((< inicio 90) (list (- (mod 3600 216) (- 90 inicio) 117 3 3 3) (- 90 inicio) 117 3 3 3))
    ((< inicio 207) (list (- (mod 3600 216) (+ (- 207 inicio) 3 3 3 3)) 3 (- 207 inicio) 3 3 3))
    ((< inicio 210) (list 87 3 (- (mod 3600 216) (+ (- 210 inicio) 3 3 87 3))(- 210 inicio) 3 3))
    ((< inicio 213) (list 87 3 (- (mod 3600 216) (+ (- 213 inicio) 3 87 3)) 3 (- 213 inicio) 3))	 
    (t (list 87 3 (- (mod 3600 216) (+ (- 216 inicio) 87 3)) 3 3 (- 216 inicio)))))
#|Casos de prueba
(intervalosCicloIntermitencia 0)
(intervalosCicloIntermitencia 52)
(intervalosCicloIntermitencia 85)
(intervalosCicloIntermitencia 90)
(intervalosCicloIntermitencia 100)
(intervalosCicloIntermitencia 150)
(intervalosCicloIntermitencia 210)
(intervalosCicloIntermitencia 212)
(intervalosCicloIntermitencia 215)


(87 3 54 0 0 0)
Break 2 [4]> 
(35 3 106 0 0 0)
Break 2 [4]> 
(15 3 117 3 3 3)
Break 2 [4]> 
(15 3 117 3 3 3)
Break 2 [4]> 
(25 3 107 3 3 3)
Break 2 [4]> 
(75 3 57 3 3 3)
Break 2 [4]> 
(87 3 48 3 3 3)
Break 2 [4]> 
(87 3 50 3 1 3)
Break 2 [4]> 
(87 3 53 3 3 1)

;; ========================================================
;; FUNCIÓN: mostrarPorcentajesIntermitencia
;; NATURALEZA: Pura (No imprime nada por pantalla ni modifica)
;; ESTRATEGIA: Evaluacion directa (ejecuta las funciones creadas anteriormente)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun mostrarPorcentajesIntermitencia(hora-unix)
			(list
      'rojo 			 	 (porcentajeColor (+ (* 16 87) (car (intervalosCicloIntermitencia (mod hora-unix 216)))))
	  'rojo-intermitente 	 (porcentajeColor (+ (* 16 3) (cadr (intervalosCicloIntermitencia (mod hora-unix 216)))))
      'verde 			 	 (porcentajeColor (+ (* 16 117) (caddr (intervalosCicloIntermitencia (mod hora-unix 216)))))
	  'verde-intermitente	 (porcentajeColor (+ (* 16 3) (cadddr (intervalosCicloIntermitencia (mod hora-unix 216)))))
      'amarillo 		 	 (porcentajeColor (+ (* 16 3) (car(cddddr (intervalosCicloIntermitencia (mod hora-unix 216))))))
	  'amarillo-intermitente (porcentajeColor (+ (* 16 3) (cadr(cddddr (intervalosCicloIntermitencia(mod hora-unix 216))))))))
#| (mostrarPorcentajesIntermitencia 1781639616) (mostrarPorcentajesIntermitencia 1781639701) (mostrarPorcentajesIntermitencia 1781639716) (mostrarPorcentajesIntermitencia 1781639826) (mostrarPorcentajesIntermitencia 1781639831)

(ROJO 39.916668 ROJO-INTERMITENTE 1.4166666 VERDE 54.416668 VERDE-INTERMITENTE 1.4166666 AMARILLO 1.4166666
 AMARILLO-INTERMITENTE 1.4166666)
Break 3 [5]> 
(ROJO 42.27778 ROJO-INTERMITENTE 1.4166666 VERDE 52.055557 VERDE-INTERMITENTE 1.4166666 AMARILLO 1.4166666
 AMARILLO-INTERMITENTE 1.4166666)
Break 3 [5]> 
(ROJO 40.97222 ROJO-INTERMITENTE 1.4166666 VERDE 53.61111 VERDE-INTERMITENTE 1.3333334 AMARILLO 1.3333334
 AMARILLO-INTERMITENTE 1.3333334)
Break 3 [5]> 
(ROJO 39.75 ROJO-INTERMITENTE 1.4166666 VERDE 54.583332 VERDE-INTERMITENTE 1.4166666 AMARILLO 1.4166666 AMARILLO-INTERMITENTE
 1.4166666)
Break 3 [5]> 
(ROJO 39.88889 ROJO-INTERMITENTE 1.4166666 VERDE 54.444443 VERDE-INTERMITENTE 1.4166666 AMARILLO 1.4166666
 AMARILLO-INTERMITENTE 1.4166666)
Break 3 [5]> #|

#| Iteracion 2
;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura (Modifica un archivo)
;; ESTRATEGIA: Registro de control
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
;; FUNCIÓN: auditoriaInforme
;; NATURALEZA: Pura (format nil retorna un string, no imprime)
;; ESTRATEGIA: Uso de funciones predicado
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun auditoriaInforme (color-anterior color-nuevo)
  (format nil "Tiempo ~A: La luz ha cambiado de ~A a ~A~%" (local-time:format-timestring nil (local-time:now) :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))
(car (transicion color-anterior color-nuevo)) (cadr(transicion color-anterior color-nuevo))))



