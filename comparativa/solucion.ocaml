
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

let timer tiempo_unix =
 if(tiempo_unix mod 216) <= 90  then "rojo"
else if (tiempo_unix mod 216) <= 210 then "verde"
else "amarillo"

let prueba1 = timer 200
let prueba2 = timer 211
let prueba3 = timer 217
let prueba4 = timer 0
let prueba5 = timer 1000
