
--PARTE 1 = TESOROS PIRATAS--

--Empiezan funciones que no pide el problema--

listaTesoros=["Brujula" , "Frasco de Arena Jack" , "Frasco de Arena Anne" , "Cajita Musical" , "Doblones" , "Moneda del Cofre Muerto" , "oro" , "Espada de Hierro" , "Frasco de Arena" , "Botella de Ron"]

botin tesoro
   | elem tesoro(listaTesoros) = "Su botin se llama " ++ show(tesoro) ++ " y vale " ++ show (valorTesoros tesoro)
   | otherwise = "Ese botin no existe"
   
valorTesoros tesoro
   | tesoro == "Brujula" = 10000
   | tesoro == "Frasco de Arena Jack" = 0
   | tesoro == "Cajita Musical" = 1
   | tesoro == "Doblones" = 100
   | tesoro == "Frasco de Arena Anne" = 1 
   | tesoro == "Oro" = 100
   | tesoro == "Moneda del Cofre Muerto" = 100
   | tesoro == "Espada de Hierro" = 50
   | tesoro == "Cuchullo" = 5
   | tesoro == "Frasco de Arena" = 1
   | tesoro == "Botella de Ron" = 25
   | otherwise = 0
   
valorBotin pirata 
   | pirata == "Jack Sparrow" = 10000
   | pirata == "David Jones" = 1
   | pirata == "Anne Bonny" = 101
   | pirata == "Elizabeth Swann" = 150
   | pirata == "Will Turner" = 5
   | otherwise = 0
   
tesoroPorPirataDuplicado pirata  
   | pirata == "Jack Sparrow" = "Frasco de Arena"
   | pirata == "Anne Bonny" = "Frasco de Arena"
   | otherwise = "Nada"
 
valorTesoroPorPirataDuplicado pirata   
   | pirata == "Jack Sparrow" = 0
   | pirata == "Anne Bonny" = 1
   | otherwise = 0
   
tesoroMasValioso pirata 
   | pirata == "Jack Sparrow" = "Brujula"
   | pirata == "David Jones" = "Cajita Musical"
   | pirata == "Anne Bonny" = "Doblones"
   | pirata == "Elizabeth Swann" = "Moneda del Cofre Muerto"
   | pirata == "Will Turner" = "Cuchillo"
   | otherwise = "Ese pirata no existe"
   
numeroTesoros pirata
   | pirata == "Jack Sparrow" = 2
   | pirata == "David Jones" = 1
   | pirata == "Anne Bonny" = 2 
   | pirata == "Elizabeth Swann" = 2
   | pirata == "Will Turner" = 1
   | otherwise = 0

piratas pirata
   | pirata == "Jack Sparrow" = ["Su nombre es 'Jack Sparrow'" , botin "Brujula" , botin "Frasco de Arena Jack"]
   | pirata == "David Jones" = ["Su nombre es 'David Jones'" , botin "Cajita Musical"]
   | pirata == "Anne Bonny" = ["Su nombre es 'Anne Bonny'" , botin "Doblones" , botin "Frasco de Arena Anne"]
   | pirata == "Elizabeth Swann" = ["Su nombre es 'Elizabeth Swann'" , botin "Moneda del Cofre Muerto", botin "Espada de Hierro"]
   | pirata == "Will Turner" = ["Su nombre es 'Will Turner'" , botin "Cuchillo"]
   | otherwise = ["Ese pirata no existe"]
   
--Terminan Funciones que no pide el problema-- 

--Empiezan Funciones que pide el problema--  
   
cantidadTesoros pirata = "El pirata " ++ show pirata ++ " tiene " ++ show(numeroTesoros pirata) ++ " tesoro/s"
   
esAfortunado pirata
   | (valorBotin pirata)>10000 = "Es Afortunado"
   | (valorBotin pirata)<10000 = "No es Afortunado"

mismoTesoroDiferenteValor piratauno piratados
   | (tesoroPorPirataDuplicado piratauno) == (tesoroPorPirataDuplicado piratados) && (valorTesoroPorPirataDuplicado piratauno) /= (valorTesoroPorPirataDuplicado piratados) = "Tienen un mismo tesoro pero de diferente valor"
   | (tesoroPorPirataDuplicado piratauno) == (tesoroPorPirataDuplicado piratados) && (valorTesoroPorPirataDuplicado piratauno) == (valorTesoroPorPirataDuplicado piratados) = "Tienen un mismo tesoro y de igual valor"
   | otherwise = "No tienen un mismo tesoro"   

valorTesoroMasValioso pirata = "El tesoro mas valioso del pirata es " ++ show (tesoroMasValioso pirata) ++ " y vale " ++ show (valorTesoros (tesoroMasValioso pirata))

comoQuedaPirataDespuesDeAdquirirNuevoTesoro pirata tesoro = "El pirata " ++ show pirata ++ " ahora tiene " ++ show ((numeroTesoros pirata)+1) ++ " tesoro/s y su nuevo tesoro es " ++ show(tesoro) ++ " de valor " ++ show (valorTesoros tesoro)

comoQuedaPirataDespuesDeAdquirirNuevoTesoro1 tesoro pirata = "El pirata " ++ show pirata ++ " ahora tiene " ++ show ((numeroTesoros pirata)+1) ++ " tesoro/s y su nuevo tesoro es " ++ show(tesoro) ++ " de valor " ++ show (valorTesoros tesoro)

comoQuedaPirataDespuesDePerderTesorosValiosos pirata = "El pirata " ++ show pirata ++ " ahora tiene " ++ show ((numeroTesoros pirata)-1) ++ " tesoro/s y un botin de " ++ show ((valorBotin pirata)-(valorTesoros (tesoroMasValioso pirata)))

comoQuedaPirataDespuesDePerderDeterminadoTesoro pirata tesoro = "El pirata " ++ show pirata ++ " ahora tiene " ++ show ((numeroTesoros pirata)-1) ++ " tesoro/s y un botin de " ++ show ((valorBotin pirata)-(valorTesoros tesoro))

--Terminan Funciones que pide el problema--


--PARTE 2 = TEMPORADA DE SAQUEOS--

--Empiezan funciones que no pide el problema--

tesorosConPalabraClave = ["Brujula","Frasco de Arena Jack","Frasco de Arena Anne"]

--Terminan funciones que no pide el problema--

--Empiezan Funciones que pide el problema--

saquear pirata forma tesoro
   | forma == "Tesoros Valiosos" && (valorTesoros tesoro) >= 100 = "El pirata " ++ show pirata ++ " se robo el tesoro valioso llamado " ++ show tesoro ++ " de valor " ++ show (valorTesoros tesoro)
   | forma == "Palabra Clave" && elem tesoro(tesorosConPalabraClave) = "El pirata " ++ show pirata ++ " se robo el tesoro con palabra clave llamado " ++ show tesoro ++ " de valor " ++ show (valorTesoros tesoro)
   | forma == "Ninguna" = "El pirata se da la vuelta con las manos vacias"
   | (forma == "Especial" && (valorTesoros tesoro) >= 100) || (forma == "Especial" && elem tesoro(tesorosConPalabraClave)) = "El pirata " ++ show pirata ++ " realiza su robo especial y logra robar el tesoro llamado " ++ show tesoro ++ " de valor " ++ show (valorTesoros tesoro)
   | otherwise = "Las decisiones del pirata no concuerdan con sus intenciones"
   
--Terminan Funciones que pide el problema-- 


--PARTE 3 = NAVEGANDO LOS SIETE MARES--

barcos barco
   | barco == "Perla Negra" = ["Jack Sparrow" , "Anne Bonny"]
   | barco == "Holandes Errante" = ["Elizabeth Swann" , "David Jones","jorjito"]
   | barco == "Interceptor" = ["Will Turner"]
   | otherwise = ["Ese barco no existe"]
   
lideresBarcos barco  
   | barco == "Perla Negra" = "Jack Sparrow"
   | barco == "Holandes Errante" = "David Jones"
   | barco == "Interceptor" = "Will Turner"
   | otherwise = "Ese barco no existe"
   
formasDeSaquear barco   
   | barco == "Perla Negra" = "El Lider se queda con todo el botin"
   | barco == "Holandes Errante" = "El botin se reparte a cada tripulante por igual"
   | barco == "Interceptor" = "El primero que llega se queda con todo el botin"
   | otherwise = "Ese barco no existe"
   
botinCiudades ciudad
   | ciudad == "Jamaica" = ["Oro" , "Moneda del Cofre Muerto" , "Espada de Hierro"]
   | ciudad == "Islas Caiman" = ["Botella de Ron" , "Oro" , "Frasco de Arena"]
   | ciudad == "Puerto Rico" = ["Doblones" , "Cajita Musical" , "Oro"]
   | ciudad == "Brasil" = ["Cuchillo"]

incorporarTripulacion tripulante barco = "Se incorporo " ++ show(tripulante) ++ " al barco " ++ show(barco) ++ " y ahora tiene esta tripulacion: " ++ show(tripulante:(barcos barco))

abandonarTripulacion tripulante barco
   | elem tripulante(barcos barco) && (length (barcos barco))>=2 = "El tripulante " ++ show(tripulante) ++ " abandono el barco " ++ show(barco) ++ " y ahora el mismo tiene esta tripulacion: " ++ show(filter (/=tripulante)(barcos barco))
   | elem tripulante(barcos barco) && (length (barcos barco))<2 = "El tripulante " ++ show(tripulante) ++ " abandono el barco " ++ show(barco) ++ " y ahora el mismo tiene esta tripulacion: " ++ show(filter (/=tripulante)(barcos barco)) ++ " es decir, ¡No hay mas tripulantes!"
   | otherwise = "Ese tripulante no se encuentra dentro del barco"
   
anclar isla barco
   | isla == "Isla Tortuga" = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro1 "Frasco de Arena") (barcos barco)
   | isla == "Isla del Ron" = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro1 "Botella de Ron") (barcos barco)
   
atacar ciudad barco   
   | ciudad == "Jamaica" && (formasDeSaquear barco) == "El botin se reparte a cada tripulante por igual" && length(botinCiudades ciudad) >= length(barcos barco) = zipWith (comoQuedaPirataDespuesDeAdquirirNuevoTesoro) (barcos barco) (botinCiudades ciudad)
   | ciudad == "Jamaica" && (formasDeSaquear barco) == "El Lider se queda con todo el botin" && length(botinCiudades ciudad) >= length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (lideresBarcos barco)) (botinCiudades ciudad) 
   | ciudad == "Jamaica" && (formasDeSaquear barco) == "El primero que llega se queda con todo el botin" && length(botinCiudades ciudad) >= length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (head (barcos barco))) (botinCiudades ciudad) 
   | ciudad == "Brasil" && (formasDeSaquear barco) == "El botin se reparte a cada tripulante por igual" && length(botinCiudades ciudad) >= length(barcos barco) = zipWith (comoQuedaPirataDespuesDeAdquirirNuevoTesoro) (barcos barco) (botinCiudades ciudad)
   | ciudad == "Brasil" && (formasDeSaquear barco) == "El Lider se queda con todo el botin" && length(botinCiudades ciudad) >= length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (lideresBarcos barco)) (botinCiudades ciudad) 
   | ciudad == "Brasil" && (formasDeSaquear barco) == "El primero que llega se queda con todo el botin" && length(botinCiudades ciudad) >= length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (head (barcos barco))) (botinCiudades ciudad)    
   | ciudad == "Islas Caiman" && (formasDeSaquear barco) == "El botin se reparte a cada tripulante por igual" && length(botinCiudades ciudad) >= length(barcos barco) = zipWith (comoQuedaPirataDespuesDeAdquirirNuevoTesoro) (barcos barco) (botinCiudades ciudad)
   | ciudad == "Islas Caiman" && (formasDeSaquear barco) == "El Lider se queda con todo el botin" && length(botinCiudades ciudad) >= length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (lideresBarcos barco)) (botinCiudades ciudad) 
   | ciudad == "Islas Caiman" && (formasDeSaquear barco) == "El primero que llega se queda con todo el botin" && length(botinCiudades ciudad) >= length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (head (barcos barco))) (botinCiudades ciudad) 
   | ciudad == "Puerto Rico" && (formasDeSaquear barco) == "El botin se reparte a cada tripulante por igual" && length(botinCiudades ciudad) >= length(barcos barco) = zipWith (comoQuedaPirataDespuesDeAdquirirNuevoTesoro) (barcos barco) (botinCiudades ciudad)
   | ciudad == "Puerto Rico" && (formasDeSaquear barco) == "El Lider se queda con todo el botin" && length(botinCiudades ciudad) >= length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (lideresBarcos barco)) (botinCiudades ciudad) 
   | ciudad == "Puerto Rico" && (formasDeSaquear barco) == "El primero que llega se queda con todo el botin" && length(botinCiudades ciudad) >= length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (head (barcos barco))) (botinCiudades ciudad) 
   | ciudad == "Brasil" && (formasDeSaquear barco) == "El botin se reparte a cada tripulante por igual" && length(botinCiudades ciudad) < length(barcos barco) = zipWith (comoQuedaPirataDespuesDeAdquirirNuevoTesoro) (barcos barco) (botinCiudades ciudad) ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Brasil" && (formasDeSaquear barco) == "El Lider se queda con todo el botin" && length(botinCiudades ciudad) < length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (lideresBarcos barco)) (botinCiudades ciudad) ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Brasil" && (formasDeSaquear barco) == "El primero que llega se queda con todo el botin" && length(botinCiudades ciudad) < length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (head (barcos barco))) (botinCiudades ciudad) ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Jamaica" && (formasDeSaquear barco) == "El botin se reparte a cada tripulante por igual" && length(botinCiudades ciudad) < length(barcos barco) = zipWith (comoQuedaPirataDespuesDeAdquirirNuevoTesoro) (barcos barco) (botinCiudades ciudad)  ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Jamaica" && (formasDeSaquear barco) == "El primero que llega se queda con todo el botin" && length(botinCiudades ciudad) < length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (head (barcos barco))) (botinCiudades ciudad)  ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Jamaica" && (formasDeSaquear barco) == "El Lider se queda con todo el botin" && length(botinCiudades ciudad) < length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (head (barcos barco))) (botinCiudades ciudad)  ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Islas Caiman" && (formasDeSaquear barco) == "El botin se reparte a cada tripulante por igual" && length(botinCiudades ciudad) < length(barcos barco) = zipWith (comoQuedaPirataDespuesDeAdquirirNuevoTesoro) (barcos barco) (botinCiudades ciudad)  ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Islas Caiman" && (formasDeSaquear barco) == "El Lider se queda con todo el botin" && length(botinCiudades ciudad) < length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (lideresBarcos barco)) (botinCiudades ciudad)  ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Islas Caiman" && (formasDeSaquear barco) == "El primero que llega se queda con todo el botin" && length(botinCiudades ciudad) < length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (head (barcos barco))) (botinCiudades ciudad)  ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Puerto Rico" && (formasDeSaquear barco) == "El botin se reparte a cada tripulante por igual" && length(botinCiudades ciudad) < length(barcos barco) = zipWith (comoQuedaPirataDespuesDeAdquirirNuevoTesoro) (barcos barco) (botinCiudades ciudad)  ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Puerto Rico" && (formasDeSaquear barco) == "El Lider se queda con todo el botin" && length(botinCiudades ciudad) < length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (lideresBarcos barco)) (botinCiudades ciudad)  ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
   | ciudad == "Puerto Rico" && (formasDeSaquear barco) == "El primero que llega se queda con todo el botin" && length(botinCiudades ciudad) < length(barcos barco) = map (comoQuedaPirataDespuesDeAdquirirNuevoTesoro (head (barcos barco))) (botinCiudades ciudad)  ++ ["Estos son los tripulanets que se quedaron sin tesoro y fueron abandonados en la ciudad: " ++ show(take(length(barcos barco)-length(botinCiudades ciudad))(reverse(barcos barco)))]
