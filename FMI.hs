data Pais = UnPais {ingresoPerCapita::Float , sectorPublico::Float, sectorPrivado::Float, recursosNaturales::RecursosNaturales ,deuda::Float} deriving (Show)
type RecursosNaturales=[[Char]]

namibia = UnPais {ingresoPerCapita=4140, sectorPublico=400000, sectorPrivado=650000, recursosNaturales=["mineria","ecoturismo"],deuda=50}

type Receta=Pais->Pais

--darReceta :: [Receta]->Pais
--darReceta estrategias pais = foldl (pais) estrategias

darPrestamo :: Float->Receta 
darPrestamo millones pais= pais {deuda=  (deuda pais) + ( millones * 1.5) }  

recortarSectorPublico :: Float->Receta
recortarSectorPublico cantidadDeDesempleados pais= pais {ingresoPerCapita= (*) (ingresoPerCapita pais) (reduccionIngreso cantidadDeDesempleados) ,
												  sectorPublico = (sectorPublico pais) - cantidadDeDesempleados}

reduccionIngreso :: Float->Float
reduccionIngreso cantidadDeDesempleados | cantidadDeDesempleados > 100 = 0.8
										| otherwise = 0.85

type Socio = String
type Recurso = String
teHiceUnRecurso :: Socio->Recurso->Receta
teHiceUnRecurso socio recursoAPerder pais =   (perderRecurso recursoAPerder).disminuirDeudaEn2Millones $ pais 

perderRecurso:: Recurso->Receta
perderRecurso recursoAPerder pais= pais{recursosNaturales =  filter (/=recursoAPerder) (recursosNaturales pais)}
											--   esta muy mal usar este filtro?
disminuirDeudaEn2Millones:: Receta
disminuirDeudaEn2Millones pais= pais{deuda= (deuda pais) - 2}

--blindaje:: Receta
--blindaje pais = darPrestamo ((calcularMitadPBI.recortarSectorPublico 500)  pais) $ pais

--calcularMitadPBI :: Pais->Float
--calcularMitadPBI pais = ((*) 0.5).(((*)ingresoPerCapita pais).(((+)sectorPublico pais) (sectorPrivado pais)))  


-- Modelando una Receta   para namibia (3.a)
--darReceta [(darPrestamo 200),(perderRecurso "mineria")] namibia