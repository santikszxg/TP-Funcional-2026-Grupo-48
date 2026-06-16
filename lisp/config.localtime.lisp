(load "ruta del quicklisp")
(ql:quickload "local-time")


(defun auditoria (color-anterior color-nuevo)
  (format t "Tiempo ~A: La luz ha cambiado de ~A a ~A~%" (local-time:format-timestring nil (local-time:now) :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec))
(car (transicion color-anterior color-nuevo)) (cadr(transicion color-anterior color-nuevo))))
