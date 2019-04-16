type Tesoro = (String, Int)
type Pirata =(String,[Tesoro])

cantidadDeTesoros :: Pirata -> Int
cantidadDeTesoros pirata = length (map (head.fst) (snd pirata))

esAfortunado :: Pirata -> Bool
esAfortunado pirata = sum (map (snd) (snd pirata)) > 1000

valorMax :: Pirata -> Int
valorMax pirata = maximum (map (snd) (snd pirata))

nuevoTesoro :: Pirata -> Tesoro -> [Tesoro]
nuevoTesoro pirata botin = (snd pirata) ++ [botin]

perderTesorosValiosos :: Pirata -> [Tesoro]
perderTesorosValiosos pirata = filter ((<100).snd) (snd pirata)

perderTesoroEspecifico :: Pirata -> String -> [Tesoro]
