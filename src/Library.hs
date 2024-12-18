module Library where
import PdePreludat
import Data.Char (isLower)

-- 1) Modelar a los plomeros y sus herramientas.

data Plomero = UnPlomero {
    nombre :: String,
    dinero :: Number,
    reparaciones :: [Reparacion],
    herramientas :: [Herramienta]
}deriving(Show, Eq)

data Herramienta = UnaHerramienta {
    denominacion :: String,
    material :: Material,
    precio :: Number
}deriving(Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving(Show, Eq)

herramientaCara :: Herramienta
herramientaCara = UnaHerramienta "herramienta cara" Hierro 100000

martilloDeHierro :: Herramienta
martilloDeHierro = UnaHerramienta "martillo" Hierro 50000

destornilladorDeMadera :: Herramienta 
destornilladorDeMadera = UnaHerramienta "destornillador" Madera 100

llaveInglesa :: Herramienta
llaveInglesa = UnaHerramienta "llave inglesa" Hierro 200

-- Mario, un plomero que tiene $1200, no hizo ninguna reparación hasta ahora y en su caja de herramientas lleva una 
-- llave inglesa con mango de hierro que tiene un precio de $200 y un martillo con empuñadura de madera que le salió $20.

mario :: Plomero
mario = UnPlomero "Mario" 1200 [] [UnaHerramienta "llave inglesa" Hierro 200, UnaHerramienta "martillo" Madera 20]

-- Wario, tiene 50 centavos encima, no hizo reparaciones, lleva infinitas llaves francesas, obviamente de hierro, la primera 
-- le salió un peso, pero cada una que compraba le salía un peso más cara. La inflación lo está matando. 

wario :: Plomero
wario = UnPlomero "Wario" 0.50 [] (map (UnaHerramienta "llave francesa" Hierro) [1..])

-- 2) Saber si un plomero:

-- Tiene una herramienta con cierta denominación.

tieneHerramienta :: String -> Plomero -> Bool
tieneHerramienta herramienta = elem herramienta . map denominacion . herramientas

-- Es malvado: se cumple si su nombre empieza con Wa.

esMalvado :: Plomero -> Bool
esMalvado = (== "Wa") . take 2 . nombre 

-- Puede comprar una herramienta: esto sucede si tiene el dinero suficiente para pagar el precio de la misma.

puedeComprarHerramienta :: Herramienta -> Plomero -> Bool
puedeComprarHerramienta herramienta = (> precio herramienta) . dinero

-- 3) Saber si una herramienta es buena, cumpliendose solamente si tiene empuñadura de hierro que sale más de 
-- $10000 o es un martillo con mango de madera o goma.

esBuenaHerramienta :: Herramienta -> Bool
esBuenaHerramienta herramienta = 
    esDeMaterial Hierro herramienta || (precio herramienta > 10000) || 
    (esMartillo herramienta && (esDeMaterial Madera herramienta || esDeMaterial Goma herramienta))

esDeMaterial :: Material -> Herramienta -> Bool
esDeMaterial unMaterial = (== unMaterial) . material

-- una idea que quedo
--esUnMartilloConMangoDeMaderaOGoma :: Herramienta -> Bool
--esUnMartilloConMangoDeMaderaOGoma (UnaHerramienta "martillo" (Madera || Goma) _) = True

esMartillo :: Herramienta -> Bool
esMartillo (UnaHerramienta "martillo" _ _) = True

esMartillo' :: Herramienta -> Bool
esMartillo' = (== "martillo") . denominacion

-- 4) Todo plomero necesita comprar una herramienta, cuando lo hace paga su precio y agrega la herramienta a las suyas. 
-- Solo sucede si puede pagarlo.

intentarComprarHerramienta :: Herramienta -> Plomero -> Plomero
intentarComprarHerramienta herramienta plomero
    | puedeComprarHerramienta herramienta plomero = comprarHerramienta herramienta plomero
    | otherwise                                   = plomero

comprarHerramienta :: Herramienta -> Plomero -> Plomero 
comprarHerramienta herramienta = agregarHerramienta herramienta . disminuirDinero (precio herramienta)

-- Funciones Auxiliares --

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta nuevaHerramiena plomero = plomero {herramientas = nuevaHerramiena : herramientas plomero}

sacarHerramienta :: Herramienta -> Plomero -> Plomero
sacarHerramienta herramientaAEliminar plomero = plomero {herramientas = filter (/= herramientaAEliminar) (herramientas plomero)}

disminuirDinero :: Number -> Plomero -> Plomero
disminuirDinero valorADisminuir plomero = plomero {dinero = dinero plomero - valorADisminuir}

cobrarDinero :: Number -> Plomero -> Plomero
cobrarDinero valorAAumentar plomero = plomero {dinero = dinero plomero + valorAAumentar}

-- 5) Hay un sinfín de reparaciones que los plomeros deben resolver. Cada una de ellas goza de una descripción del 
-- problema a reparar y un requerimiento que varía dependiendo de la reparación. 
-- Por ejemplo, una filtración de agua requiere que un plomero tenga una llave inglesa en su caja de herramientas.

-- a) Modelar las reparaciones y la filtración de agua.

data Reparacion = UnaReparacion {
    descripcion :: String,
    requerimiento :: Herramienta
} deriving(Show, Eq)

filtracionDeAgua :: Reparacion
filtracionDeAgua = UnaReparacion "filtracion de agua" llaveInglesa

-- b) Saber si una reparación es difícil: esto ocurre cuando su descripción es complicada, 
-- es decir que tiene más de 100 caracteres y además es un grito, es decir está escrito totalmente en mayúsculas.

esDificil :: Reparacion -> Bool
esDificil = esComplicada . descripcion

esComplicada :: String -> Bool
esComplicada texto = length texto > 100 && esUnGrito texto

-- Porque isLower son aquellos caracteres en minuscula, por lo tanto si quiero los que no son minuscula entonces lo niego
esUnGrito :: String -> Bool
esUnGrito = all (not . isLower)

esMayuscula :: Char -> Bool
esMayuscula letra = letra `elem` ['A'..'Z']  

-- c) Saber el presupuesto de una reparación, el cual se calcula como el 300% de la longitud de su descripción
-- (por eso es importante describir los problemas de manera sencilla).

presupuesto :: Reparacion -> Number
presupuesto = (*3) . length . descripcion 

-- 6) Hacer que un plomero haga una reparación. Si no puede resolverla te cobra $100 la visita. 
-- Si puede hacerla, cobra el dinero por el presupuesto de la misma y agrega esa reparación a su historial de reparaciones, 
-- además de:

hacerReparacion :: Reparacion -> Plomero -> Plomero
hacerReparacion reparacion plomero 
    | puedeHacerla reparacion plomero = realizarReparacion reparacion plomero
    | otherwise                       = cobrarDinero 100 plomero  

-- Un plomero puede hacer una reparación si cumple su requerimiento o es un plomero malvado con un martillo.
puedeHacerla :: Reparacion -> Plomero -> Bool
puedeHacerla repacion plomero = cumpleConRequerimiento repacion plomero || (esMalvado plomero && tieneHerramienta "martillo" plomero)

cumpleConRequerimiento :: Reparacion -> Plomero -> Bool
cumpleConRequerimiento reparacion  = elem (requerimiento reparacion) . herramientas

realizarReparacion :: Reparacion -> Plomero -> Plomero
realizarReparacion reparacion = extrasDeLaReparacion reparacion . agregarReparacion reparacion . cobrarDinero (presupuesto reparacion) 
-- 1ero. El plomero cobra el presupuesto de la reparacion
-- 2dos. El plomero agrega la repacion a su historial de repaciones
-- 3ero. Al plomero le suceden cosas extras al realizar la reparacion que dependen de otras condiciones 

extrasDeLaReparacion :: Reparacion -> Plomero -> Plomero
extrasDeLaReparacion reparacion plomero
    | esMalvado plomero    = agregarHerramienta (UnaHerramienta "destornillador" Plastico 0) plomero
    | esDificil reparacion = perderTodasLasHerramientasBuenas plomero
    | otherwise            = olvidarLaPrimeraHerramienta plomero 

-- Funciones Axuliares
agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion nuevaReparacion plomero = plomero {reparaciones = nuevaReparacion : reparaciones plomero}

perderTodasLasHerramientasBuenas :: Plomero -> Plomero
perderTodasLasHerramientasBuenas plomero = (foldr sacarHerramienta plomero . filter esBuenaHerramienta . herramientas) plomero 

-- Es mas facil hacer un filtro de todas las herramientas que no son buenas y me quedo con ellas
perderTodasLasHerramientasBuenas' :: Plomero -> Plomero
perderTodasLasHerramientasBuenas' plomero = plomero {herramientas = filter (not. esBuenaHerramienta) (herramientas plomero)}

olvidarLaPrimeraHerramienta :: Plomero -> Plomero
olvidarLaPrimeraHerramienta plomero = plomero {herramientas = sacarPrimerElemento (herramientas plomero)}

sacarPrimerElemento :: [Herramienta] -> [Herramienta]
sacarPrimerElemento [] = []
sacarPrimerElemento (_:xs) = xs

-- COSAS EXTRAS QUE SUCEDEN AL REALIZAR LA REPACION
-- Si el plomero es malvado, le roba al cliente un destornillador con mango de plástico, claramente su precio es nulo.
-- Si no es malvado y la reparación es difícil, pierde todas sus herramientas buenas.
-- Si no es malvado ni es difícil la reparación, sólo se olvida la primera de sus herramientas.

-- 7) Nintendo, pese a ser una empresa de consolas y juegos, gana millones de dólares con su red de plomeros. 
-- Cada plomero realiza varias reparaciones en un día. Necesitamos saber cómo afecta a un plomero una jornada de trabajo. 
-- Bajan línea desde Nintendo que no usemos recursividad.

realizarJornadaDeTrabajo :: [Reparacion] -> Plomero -> Plomero
realizarJornadaDeTrabajo reparaciones plomero = foldr hacerReparacion plomero reparaciones

-- 8) Nintendo beneficia a sus plomeros según ciertos criterios, es por eso que necesita saber, dado un conjunto de reparaciones
-- a realizar en una jornada laboral, cuál de todos sus empleados es:

type Criterio =  Plomero -> Number

elEmpleadoMas :: Criterio -> [Reparacion] -> [Plomero] -> Plomero
elEmpleadoMas criterio reparaciones = foldr1 (elMejorDeLosDosSegun criterio) . map (realizarJornadaDeTrabajo reparaciones)
-- 1ero. Obtengo una lista de los plomeros luego de realizar una jornada laboral cada uno (aplico la jornada a cada plomero)
-- 2dos. Realizo una reduccion para obtener de esa lista el mejor empleado segun un criterio dado, la semilla que utilizo
-- es el primer empleado de la lista 

-- Para comparar y quedarme con el mejor plomero según un criterio
elMejorDeLosDosSegun :: Criterio -> Plomero -> Plomero -> Plomero
elMejorDeLosDosSegun criterio plomero1 plomero2
    | criterio plomero1 > criterio plomero2 = plomero1
    | otherwise                             = plomero2

-- CRITERIOS

-- a) El empleado más reparador: El plomero que más reparaciones tiene en su historial una vez realizada su jornada laboral.

reparador :: Criterio
reparador = length . reparaciones

-- b) El empleado más adinerado: El plomero que más dinero tiene encima una vez realizada su jornada laboral.

adinerado :: Criterio
adinerado = dinero

-- c) El empleado que más invirtió: El plomero que más plata invertida tiene entre las herramientas que le quedaron una vez
-- realizada su jornada laboral.

inversor :: Criterio
inversor = sum . map precio . herramientas
