module Library where
import PdePreludat

-- 1) Modelar a los plomeros y sus herramientas.

data Plomero = UnPlomero {
    nombre :: String,
    dinero :: Number,
    reparaciones :: Number,
    herramientas :: [Herramienta]
}deriving(Show, Eq)

data Herramienta = UnaHerramienta {
    denominacion :: String,
    material :: Material,
    precio :: Number
}deriving(Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving(Show, Eq)

-- Mario, un plomero que tiene $1200, no hizo ninguna reparación hasta ahora y en su caja de herramientas lleva una 
-- llave inglesa con mango de hierro que tiene un precio de $200 y un martillo con empuñadura de madera que le salió $20.

mario :: Plomero
mario = UnPlomero "Mario" 1200 0 [(UnaHerramienta "llave inglesa" Hierro 200), (UnaHerramienta "martillo" Madera 20)]

-- Wario, tiene 50 centavos encima, no hizo reparaciones, lleva infinitas llaves francesas, obviamente de hierro, la primera 
-- le salió un peso, pero cada una que compraba le salía un peso más cara. La inflación lo está matando. 

wario :: Plomero
wario = UnPlomero "Wario" 0.50 0 (map (UnaHerramienta "llave francesa" Hierro) [1..])