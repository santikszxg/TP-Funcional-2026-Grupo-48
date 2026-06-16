
(*
========================================================
   FUNCIÓN: transicion
   NATURALEZA: Pura
   ESTRATEGIA: Busca un patron mediante constructores
   IMPACTO: No destructiva
   ======================================================== *)
type color = EnRojo |  Verde | Rojo | EnVerde | Amarillo |EnAmarillo 

let transicion coloractual cambiara =
 match (coloractual, cambiara) with
| (EnRojo, Verde) -> (coloractual,"cambiar a verde")
| (EnVerde, Amarillo) -> (coloractual, "cambiar a amarillo")
| (EnAmarillo, Rojo) -> (coloractual, "cambiar a rojo")
| _ -> (coloractual, "Accion por defecto")


let prueba1 = transicion EnVerde Amarillo
let prueba2 = transicion EnRojo Verde
let prueba3 = transicion Verde Verde
let prueba4 = transicion EnAmarillo Rojo

type color = EnRojo | Verde | Rojo | EnVerde | Amarillo | EnAmarillo
val transicion : color -> color -> color * string = <fun>
val prueba1 : color * string = (EnVerde, "cambiar a amarillo")
val prueba2 : color * string = (EnRojo, "cambiar a verde")
val prueba3 : color * string = (Verde, "Accion por defecto")
val prueba4 : color * string = (EnAmarillo, "cambiar a rojo")
(*
========================================================
   FUNCIÓN: timer
   NATURALEZA: Pura 
   ESTRATEGIA: Función predicado (Evalúa condiciones lógicas con estructuras if/else)
   IMPACTO: No destructiva
   ======================================================== *)

let timer tiempo_unix =
 if(tiempo_unix mod 216) <= 90  then "rojo"
else if (tiempo_unix mod 216) <= 210 then "verde"
else "amarillo"

let prueba1 = timer 200
let prueba2 = timer 211
let prueba3 = timer 217
let prueba4 = timer 0
let prueba5 = timer 1000

val timer : int -> string = <fun>
val prueba1 : string = "verde"
val prueba2 : string = "amarillo"
val prueba3 : string = "rojo"
val prueba4 : string = "rojo"
val prueba5 : string = "verde" 

(*
========================================================
   FUNCIÓN: transicion
   NATURALEZA: Pura
   ESTRATEGIA: Busca un patron mediante constructores
   IMPACTO: No destructiva
   ======================================================== *)

type color = EnRojo |  Verde | Rojo | EnVerde | Amarillo |EnAmarillo | RojoIntermitente | VerdeIntermitente | AmarilloIntermitente


let transicion coloractual cambiara =
 match (coloractual, cambiara) with
| (EnRojo, Verde) -> (coloractual,"cambiar a verde")
| (EnVerde, Amarillo) -> (coloractual, "cambiar a amarillo")
| (EnAmarillo, Rojo) -> (coloractual, "cambiar a rojo")
| (EnRojo, RojoIntermitente) -> (coloractual, "cambiar a rojo intermitente")
| (EnVerde,VerdeIntermitente) -> (coloractual, "cambiar a verde intermitente")
| (EnAmarillo, AmarilloIntermitente) -> (coloractual, "cambiar a amarillo intermitente")
| _ -> (coloractual, "Accion por defecto")


let prueba1 = transicion EnVerde Amarillo
let prueba2 = transicion EnRojo Verde
let prueba3 = transicion Verde Verde
let prueba4 = transicion EnAmarillo Rojo
let prueba5 = transicion EnRojo RojoIntermitente
let prueba6 = transicion EnVerde VerdeIntermitente
let prueba7 = transicion EnAmarillo  AmarilloIntermitente

(*
========================================================
   FUNCIÓN: timer
   NATURALEZA: Pura 
   ESTRATEGIA: Función predicado (Evalúa condiciones lógicas con estructuras if/else)
   IMPACTO: No destructiva
   ======================================================== *)
let timer tiempo_unix =
 if(tiempo_unix mod 216) <= 86  then "rojo"
else if (tiempo_unix mod 216) <= 89 then "rojo intermitente"
else if (tiempo_unix mod 216) <= 206 then "verde"
else if (tiempo_unix mod 216) <= 209 then "verde intermitente"
else if (tiempo_unix mod 216) <= 212 then "amarillo"
else "amarillo intermitente"

let prueba1 = timer 500
let prueba2 = timer 215
let prueba3 = timer 212
let prueba4 = timer 100

val timer : int -> string = <fun>
val prueba1 : string = "rojo"
val prueba2 : string = "amarillo intermitente"
val prueba3 : string = "amarillo"
val prueba4 : string = "verde"

