module Lib where
import Text.Show.Functions
import Data.List

data Festival = UnFestival {
    lugar :: String,
    cantidadPublico :: Int,
    estadoDeAnimoInicial :: String,
    bandas :: [Banda]
}

data Banda = UnaBanda {
    descripciones :: [String],
    decibeles :: Int,
    genero :: Genero
}

losRedondos = UnaBanda {descripciones = ["legendaria" , "pogosa"], decibeles = 45 , genero = rockNacional}
soda = UnaBanda {descripciones = ["irrepetible"], decibeles =40,genero= rockNacional}
miranda = UnaBanda {genero = pop, decibeles= 60,descripciones = ["insípida", "incolora" , "inodora"]}
metallica = UnaBanda {descripciones = ["legendaria" , "vendida"], decibeles = 60 , genero = metal "heavy"}
basuritas = UnaBanda {descripciones = ["basura"], decibeles = 100, genero = metal "trash"}
theStrokes = UnaBanda {descripciones = ["suicidio asistidos", "emocional","linda"], decibeles=45,genero = pop.metal "heavy"}

type Genero = Festival -> Festival

hullabalooza = UnFestival {lugar="Argentina",cantidadPublico=20000,estadoDeAnimoInicial = "indiferente", bandas=[miranda,losRedondos,metallica,soda]}

cambiarPublico :: (Int -> Int) -> Festival -> Festival
cambiarPublico f festival = festival {cantidadPublico = f (cantidadPublico festival)}

cambiarEstadoAnimo :: (String -> String) -> Festival -> Festival
cambiarEstadoAnimo f festival = festival {estadoDeAnimoInicial = f (estadoDeAnimoInicial festival)}

rockNacional :: Genero
rockNacional  = cambiarPublico (\publico -> publico + 100)

pop :: Genero
pop festival 
    | estanIndiferentes festival = cambiarEstadoAnimo (\_ -> "euforico").cambiarPublico (*2) $ festival
    | otherwise = festival

estanIndiferentes :: Festival -> Bool
estanIndiferentes = (=="indiferente").estadoDeAnimoInicial

metal :: String -> Genero
metal "heavy" festival = efectoMetal "pesado" festival
metal "trash" festival = efectoMetal "basura" festival
--metal _ festival = efectoMetal calificativo festival

efectoMetal :: String -> Genero
efectoMetal calificativo = cambiarPublico ((`div`100).(*101)).cambiarEstadoAnimo (++ calificativo)

-----1
tocar :: Festival -> Banda -> Festival
tocar festival = alterarPublico festival . genero 

alterarPublico ::  Festival -> Genero ->Festival
alterarPublico festival genero = genero festival

-----4
suceder :: Festival -> Festival
suceder festival = foldl tocar festival (bandas festival)

-----5
type Clasificaciones = Banda->Bool
vendida :: Clasificaciones
vendida banda = ((>=3).length.descripciones) banda || pertenece "vendida" banda

acustica :: Clasificaciones
acustica = (>55).decibeles

legendaria :: Clasificaciones
legendaria banda = pertenece "legendaria" banda && ((>40).decibeles) banda


pertenece :: String -> Banda -> Bool
pertenece palabra banda = elem palabra .descripciones $ banda

-----6
popularidad :: Banda -> [Clasificaciones] -> Int
popularidad banda clasificaciones = 100 * length (filter (cumpleCriterio banda) clasificaciones)

cumpleCriterio :: Banda -> Clasificaciones -> Bool
cumpleCriterio banda clasificacion = clasificacion banda

-----7
buenFest :: Festival -> [Clasificaciones] -> Bool
buenFest festival clasificaciones = (esMasPopularQueElAnterior (popularidadBandas (bandas festival) clasificaciones)) && (>1000)(sum (popularidadBandas (bandas festival) clasificaciones))

popularidadBandas :: [Banda] -> [Clasificaciones] -> [Int]    
popularidadBandas bandas clasificaciones = map (`popularidad` clasificaciones) bandas

esMasPopularQueElAnterior :: [Int] -> Bool
esMasPopularQueElAnterior listaPopularidad = sort listaPopularidad == listaPopularidad

-----8
{-
¿Fueron de utilidad los conceptos de aplicación parcial y composición? Indicar donde se los utilizó y 
justificar su impacto en la solución.

-}
-----9
{-
¿Sería posible que alguna de las listas fuera infinita y de todas maneras las funciones sigan funcionando 
correctamente? Justificar y ejemplificar.
-}