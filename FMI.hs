data Pais = UnPais {ingresoPerCapita::Float , sectorPublico::Float, sectorPrivado::Float, recursosNaturales::RecursosNaturales ,deuda::Float} deriving (Show)
type RecursosNaturales=[[Char]]

namibia = UnPais {ingresoPerCapita=4140, sectorPublico=400000, sectorPrivado=650000, recursosNaturales=["mineria","ecoturismo"],deuda=50}

type Receta=Pais->Pais

--darReceta :: [Receta]->Pais
--darReceta estrategias pais = foldl (pais) estrategias

darPrestamo :: Float->Receta 
darPrestamo millones pais= pais {deuda= (+) (deuda pais) ((*) millones  2) }  

recortarSectorPublico :: Float->Receta
recortarSectorPublico cantidadDeDesempleados pais= pais {ingresoPerCapita= (*) (ingresoPerCapita pais) (reduccionIngreso cantidadDeDesempleados) ,
												  sectorPublico = (-) (sectorPublico pais)  cantidadDeDesempleados}

reduccionIngreso :: Float->Float
reduccionIngreso cantidadDeDesempleados | cantidadDeDesempleados > 100 = 0.8
										| otherwise = 0.85

type Socio = String
type Recurso = String
teHiceUnRecurso :: Socio->Receta
teHiceUnRecurso socio pais =   perderRecurso.disminuirDeuda $ pais 

perderRecurso:: Recurso->Receta
perderRecurso recursoAPerder pais= pais{recursosNaturales =  filter (!=recursoAPerder) (recursosNaturales pais)}
											--   esta muy mal usar este filtro?
disminuirDeuda:: Receta
disminuirDeuda pais= pais{deuda= ((-) 20).deuda $ pais}

blindaje:: Receta
blindaje pais = darPrestamo ((calcularMitadPBI.recortarSectorPublico 500)  pais) $ pais

calcularMitadPBI :: Pais->Float
calcularPBI pais = ((*) 0.5).(((*)ingresoPerCapita pais).(((+)sectorPublico pais) (sectorPrivado pais)))  


