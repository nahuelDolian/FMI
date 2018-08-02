data Pais = UnPais {ingresoPerCapita::Float , sectorPublico::Float, sectorPrivado::Float, recursosNaturales::RecursosNaturales ,deuda::Float} deriving (Show)
type RecursosNaturales=[[Char]]

namibia = UnPais {ingresoPerCapita=4140, sectorPublico=400000, sectorPrivado=650000, recursosNaturales=["mineria","ecoturismo"],deuda=50}
argentina = UnPais {ingresoPerCapita=201, sectorPublico=20000, sectorPrivado=12903, recursosNaturales=["mineria","ecoturismo","Petroleo"],deuda=70}
type Receta=Pais->Pais

--darReceta :: [Receta]->Pais
--darReceta estrategias pais = foldl (pais) estrategias

modificarDeuda:: Float->Receta
modificarDeuda valor  pais = pais{deuda = (deuda pais) + valor }


darPrestamo :: Float->Receta 
darPrestamo millones = modificarDeuda (millones * 1.5) 

recortarSectorPublico :: Float->Receta
recortarSectorPublico cantidadDeDesempleados pais= pais {ingresoPerCapita= (*) (ingresoPerCapita pais) (reduccionIngreso cantidadDeDesempleados) ,
												  sectorPublico = (sectorPublico pais) - cantidadDeDesempleados}

reduccionIngreso :: Float->Float
reduccionIngreso cantidadDeDesempleados | cantidadDeDesempleados > 100 = 0.8
										| otherwise = 0.85

type Socio = String
type Recurso = String
teHiceUnRecurso :: Socio->Recurso->Receta
teHiceUnRecurso socio recursoAPerder pais =   (perderRecurso recursoAPerder).modificarDeuda (-2) $ pais 

perderRecurso:: Recurso->Receta
perderRecurso recursoAPerder pais= pais{recursosNaturales =  filter (/=recursoAPerder) (recursosNaturales pais)}

blindaje:: Receta
blindaje pais = (darPrestamo (calcularPBI pais * 0.5) . recortarSectorPublico 500) pais

calcularPBI :: Pais->Float
calcularPBI pais = ingresoPerCapita pais * ((sectorPublico pais) + (sectorPrivado pais))


-- Modelando una Receta   para namibia (3.a)
--darReceta [(darPrestamo 200),(perderRecurso "mineria")] namibia


-- Dada una lista de paises, cuales son zafables?

zafan:: [Pais]->[Pais]
zafan  = filter esZafable 

esZafable:: Pais->Bool
esZafable  = esPetrolero 

esPetrolero:: Pais->Bool
esPetrolero pais = elem "Petroleo" (recursosNaturales pais)

deudaTotal::[Pais]->Float
deudaTotal paises = sum (map deuda paises)