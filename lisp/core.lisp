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

(defun timer (tiempo_unix)
				(cond
				((<= (mod tiempo_unix 216) 89) 'rojo)
				((<= (mod tiempo_unix 216) 209) 'verde)
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
Break 2 [4]>  

;Requerimiento 3: Temporizador Automático;
;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: impura (Imprime por pantalla)
;; ESTRATEGIA: Uso de funciones predicado y mostrar por pantalla
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun auditoria (color-anterior color-nuevo)
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
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (solo calcula)
;; ESTRATEGIA: Operacion aritmetica
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun duracion-ciclo()
			(+ 90 6 120))

#|
;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Impura (Imprime por pantalla)
;; ESTRATEGIA: Funcion predicado (Evaluacion condicional mediante if)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun recomendacion-ciclo()
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
;; FUNCIÓN: Porcentaje-color
;; NATURALEZA: Pura (Calcula porcentaje)
;; ESTRATEGIA: Operacion Aritmetica
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun porcentaje-color(segundos)
				(float(* (/ segundos 3600) 100)))
#|
;; ========================================================
;; FUNCIÓN: intervalos-ciclo
;; NATURALEZA: Pura (No muestra ni modifica nada)
;; ESTRATEGIA: Funcion predicado (Evalua a traves del numero ingresado por parametro y ve donde arranca el ciclo)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun intervalos-ciclo (inicio)
				(cond
				((and(< inicio 90)(<(- (mod 3600 216) (- 90 inicio))120)) (list (- 90 inicio) (- (mod 3600 216) (- 90 inicio)) 0))
				((< inicio 90)(list (+ (- 90 inicio)(- (mod 3600 216) (- 90 inicio) 120 6)) 120 6))
				((< inicio 210)(list (- (mod 3600 216) (+ (- 210 inicio) 6)) (- 210 inicio) 6))												 
				((and(>= inicio 210) (> (- (mod 3600 216) (- 216 inicio)) 90))(list 90 (- (mod 3600 216) (- 216 inicio) 90) (- 216 inicio)))		 
				(t (list(- (mod 3600 216) (- 216 inicio)) 0 (- 216 inicio)))))
#|Casos de prueba
(intervalos-ciclo 0)
(intervalos-ciclo 52)
(intervalos-ciclo 85)
(intervalos-ciclo 90)
(intervalos-ciclo 100)
(intervalos-ciclo 150)
(intervalos-ciclo 210)
(intervalos-ciclo 212)
(intervalos-ciclo 215)

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
;; FUNCIÓN: mostrar-porcentajes
;; NATURALEZA: Pura (No imprime nada por pantalla ni modifica)
;; ESTRATEGIA: Evaluacion directa (ejecuta las funciones creadas anteriormente)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun mostrar-porcentajes (hora-unix)
			(list
      'rojo (porcentaje-color (+ (* 16 90) (car (intervalos-ciclo (mod hora-unix 216)))))
      'verde (porcentaje-color (+ (* 16 120) (cadr (intervalos-ciclo (mod hora-unix 216)))))
      'amarillo (porcentaje-color (+ (* 16 6) (caddr (intervalos-ciclo (mod hora-unix 216)))))))
#| Casos de prueba
(mostrar-porcentajes 1781639616) (mostrar-porcentajes 1781639701) (mostrar-porcentajes 1781639716) (mostrar-porcentajes 1781639826) (mostrar-porcentajes 1781639831)

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
;; FUNCIÓN: transicion-intermitencia
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
Break 1 [3]> (transicion-intermitencia 'en-rojo-intermitente 'verde)

(EN-ROJO-INTERMITENTE "cambiar-a-verde")
Break 1 [3]> (transicion-intermitencia 'en-rojo 'rojo-intermitente)

(EN-ROJO "cambiar-a-rojo-intermitente")
Break 1 [3]> (transicion-intermitencia 'en-amarillo-intermitente 'rojo)

(EN-AMARILLO-INTERMITENTE "cambiar-a-rojo")
Break 1 [3]> (transicion-intermitencia 'en-azul-intermitente 'rojo)

(EN-AZUL-INTERMITENTE ACCION-POR-DEFECTO)

;; ========================================================
;; FUNCIÓN: timer-intermitencia
;; NATURALEZA: Pura (Dado un mismo tiempo, siempre retorna el mismo color)
;; ESTRATEGIA: Funcion predicado ("Descompone" el tiempo y los clasifica en sus intervalos con operadores logicos)
;; IMPACTO: No destructiva
;; ========================================================|# 

(defun timer-intermitencia(tiempo_unix)
				(cond
				((<= (mod tiempo_unix 225) 89) 'rojo)
				((<= (mod tiempo_unix 225) 92) 'rojo-intermitente)
				((<= (mod tiempo_unix 225) 212) 'verde)
				((<= (mod tiempo_unix 225) 215) 'verde-intermitente)
				((<= (mod tiempo_unix 225) 221) 'amarillo)
				(t 'amarillo-intermitente)))
#|
Casos de prueba:
Break 1 [3]> (timerIntermitencia 0)
(timer-intermitencia 91) 
(timer-intermitencia 211)
(timer-intermitencia 208)
(timer-intermitencia 1781584909)



ROJO
Break 1 [3]> 
ROJO-INTERMITENTE
Break 1 [3]> 
VERDE
Break 1 [3]> 
VERDE
Break 1 [3]> 
ROJO
Break 1 [3]> 

;; ========================================================
;; FUNCIÓN: duracion-ciclo-intermitencia
;; NATURALEZA: Pura (solo calcula)
;; ESTRATEGIA: Operacion aritmetica
;; IMPACTO: No destructiva
;; ========================================================  |#
(defun duracion-ciclo-intermitencia()
			(+ 90 6 120 9))				;; 9 segundos de intermitencia

;; ========================================================
;; FUNCIÓN: intervalos-ciclo-intermitencia
;; NATURALEZA: Pura (No muestra ni modifica nada)
;; ESTRATEGIA: Funcion predicado (Evalua a traves del numero ingresado por parametro y ve donde arranca el ciclo)
;; IMPACTO: No destructiva
;; ======================================================== 
(defun intervalos-ciclo-intermitencia (inicio)
  (cond
    ((<= inicio 89)  (list (- 90 inicio) 3 120 3 6 3))
    ((<= inicio 92)  (list 0 (- 3 (- inicio 90)) 120 3 6 3))
    ((<= inicio 212) (list 0 0 (- 120 (- inicio 93)) 3 6 3))
    ((<= inicio 215) (list 0 0 0 (- 3 (- inicio 213)) 6 3))
    ((<= inicio 221) (list 0 0 0 0 (- 6 (- inicio 216)) 3))
    (t               (list 0 0 0 0 0 (- 3 (- inicio 222))))))
#|Casos de prueba
(intervalos-ciclo-intermitencia 0)
(intervalos-ciclo-intermitencia 52)
(intervalos-ciclo-intermitencia 85)
(intervalos-ciclo-intermitencia 90)
(intervalos-ciclo-intermitencia 100)
(intervalos-ciclo-intermitencia 150)
(intervalos-ciclo-intermitencia 210)
(intervalos-ciclo-intermitencia 212)
(intervalos-ciclo-intermitencia 215)

(90 3 120 3 6 3)
Break 1 [3]> 
(38 3 120 3 6 3)
Break 1 [3]> 
(5 3 120 3 6 3)
Break 1 [3]> 
(0 3 120 3 6 3)
Break 1 [3]> 
(0 0 113 3 6 3)
Break 1 [3]> 
(0 0 63 3 6 3)
Break 1 [3]> 
(0 0 3 3 6 3)
Break 1 [3]> 
(0 0 1 3 6 3)
Break 1 [3]> 
(0 0 0 1 6 3)
Break 1 [3]> 

;; ========================================================
;; FUNCIÓN: mostrar-porcentajes-intermitencia
;; NATURALEZA: Pura (No imprime nada por pantalla ni modifica)
;; ESTRATEGIA: Evaluacion directa (ejecuta las funciones creadas anteriormente)
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun mostrar-porcentajes-intermitencia (hora-unix)
  (list
    'rojo                  (porcentaje-color (+ (* 15 90)  (car (intervalos-ciclo-intermitencia (mod hora-unix 225)))  (- 90 (car (intervalos-ciclo-intermitencia (mod hora-unix 225))))))
    'rojo-intermitente     (porcentaje-color (+ (* 15 3)   (cadr (intervalos-ciclo-intermitencia (mod hora-unix 225))) (- 3 (cadr (intervalos-ciclo-intermitencia (mod hora-unix 225))))))
    'verde                 (porcentaje-color (+ (* 15 120) (caddr (intervalos-ciclo-intermitencia (mod hora-unix 225))) (- 120 (caddr (intervalos-ciclo-intermitencia (mod hora-unix 225))))))
    'verde-intermitente    (porcentaje-color (+ (* 15 3)   (cadddr (intervalos-ciclo-intermitencia (mod hora-unix 225))) (- 3 (cadddr (intervalos-ciclo-intermitencia (mod hora-unix 225))))))
    'amarillo              (porcentaje-color (+ (* 15 6)   (car (cddddr (intervalos-ciclo-intermitencia (mod hora-unix 225)))) (- 6 (car (cddddr (intervalos-ciclo-intermitencia (mod hora-unix 225)))))))
    'amarillo-intermitente (porcentaje-color (+ (* 15 3)   (cadr (cddddr (intervalos-ciclo-intermitencia (mod hora-unix 225)))) (- 3 (cadr (cddddr (intervalos-ciclo-intermitencia (mod hora-unix 225))))))))) ;; Aca se le resta los segundos de cada color, que seria como el "maximo" menos el intervalo, para sacar el final o donde termina el ciclo)
#|

(mostrar-porcentajes-intermitencia 0)
(mostrar-porcentajes-intermitencia 50)
(mostrar-porcentajes-intermitencia 89)
(mostrar-porcentajes-intermitencia 90)
(mostrar-porcentajes-intermitencia 92)
(mostrar-porcentajes-intermitencia 93)
(mostrar-porcentajes-intermitencia 150)
(mostrar-porcentajes-intermitencia 212)
(mostrar-porcentajes-intermitencia 213)
(mostrar-porcentajes-intermitencia 216)
(mostrar-porcentajes-intermitencia 222)
(mostrar-porcentajes-intermitencia 224)
(mostrar-porcentajes-intermitencia 225)
(mostrar-porcentajes-intermitencia 0)
(mostrar-porcentajes-intermitencia 50)
(mostrar-porcentajes-intermitencia 150)
(mostrar-porcentajes-intermitencia 224)

(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)
Break 1 [3]> 
(ROJO 40.0 ROJO-INTERMITENTE 1.3333334 VERDE 53.333332 VERDE-INTERMITENTE 1.3333334 AMARILLO 2.6666667 AMARILLO-INTERMITENTE
 1.3333334)

 Iteracion 2
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
   (format stream(auditoria-informe color-actual cambiar-a))
   (format stream "~% --- Fin del Informe --- ~%")))
#| Iteracion 2
;; ========================================================
;; FUNCIÓN: auditoriaInforme
;; NATURALEZA: Pura (format nil retorna un string, no imprime)
;; ESTRATEGIA: Uso de funciones predicado
;; IMPACTO: No destructiva
;; ======================================================== |#
(defun auditoria-informe (color-anterior color-nuevo)
  (format nil "Tiempo ~A: La luz ha cambiado de ~A a ~A~%" (local-time:format-timestring nil (local-time:now) :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))
(car (transicion color-anterior color-nuevo)) (cadr(transicion color-anterior color-nuevo))))



