(load "ruta del quicklisp")
(ql:quickload "local-time")

;
;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: Impura 
;; ESTRATEGIA: Registro de cambio 
;; IMPACTO: No destructiva
;; ======================================================== |#

(defun auditoria (color-anterior color-nuevo)
  (format t "Tiempo ~A: La luz ha cambiado de ~A a ~A~%" (local-time:format-timestring nil (local-time:now) :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))
(car (transicion color-anterior color-nuevo)) (cadr(transicion color-anterior color-nuevo))))
#| Caso de prueba
(auditoria 'en-rojo 'verde)
Tiempo 2026-6-16 8:16:16: La luz ha cambiado de EN-ROJO a cambiar-a-verde
NIL  #|
