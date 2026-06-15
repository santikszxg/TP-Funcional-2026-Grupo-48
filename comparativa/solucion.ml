
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
