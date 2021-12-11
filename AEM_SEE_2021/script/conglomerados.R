#**************************************************************************************#
#**************************************************************************************#
#
#                       Taller de Análisis de Encuestas con R                        
#                        Sociedad Ecuatoriana de Estadística
#
#     Última actualización:   09/12/2021
#     Creado por:             Patricia Romero M.
#     Actualizado por:        Andrés Peña M.               
#     Contacto:               Andrés Peña M. (a.pena@rusersgroup.com)
#     Organización:           R Users Group - Ecuador
#                             
#
#**************************************************************************************#
#**************************************************************************************#

# Este programa selecciona una muestra aleatoria
# simple de conglomerados unietapica para los 
# datos del estado de Oaxaca, para estimar 
# el total de vp con tv y el porcentaje de 
# viviendas particulares con TV.
# Los municipios son los conglomerados y las
# unidades muestrales son las localidades.

setwd("")
pob <- read.csv("datos_Oaxaca.csv") #toda la pob de Oaxaca
set.seed(284)
library(sampling)

M <- length(pob$distrito)  #total de elementos en la poblacion

dis <- pob$distrito #distritos

mun <- pob$muni  #municipios

cong <- dis*100+mun  #combina distrito con municipio

municipio <- cleanstrata(cong) #quedan numerados los conglomerados
                            # entre 1 y max=570
aa <- table(municipio)  # frecuencia de municipio reenumerado
min(aa); max(aa); mean(aa); median(aa); hist(aa)
boxplot(aa)
# como aproximacion suponemos que los municipios tienen
# 15 localidades, por lo tanto debemos muestrear
# 2500/15=167 municipios

todos <- cbind(pob,municipio) #incluyo municipio en la base de datos

n <- 167 #numero de conglomerados en muestra

d <- cluster(todos,clustername=c("municipio"),size=n,method="srswor")

head(d)

muestra <- todos[d$ID_unit,]   # es la muestra

muestra <- cbind(muestra,d$Prob) #muestra completa con probabilidades

peso <- 1/d$Prob  #calculo el factor de expansion

muestra <- cbind(muestra,peso)  #incluyo el factor de expansion en la muestra d

colnames(muestra)<-c("distrito","muni","locali","pobtotal","vp","vpcontv",
"municipio","prob","peso")  #para cambiarle de nombre a las variables

table(muestra$municipio) # da el numero de localidades por municipio
                         # en muestra

library(survey)   #libreria para estimacion


###############################################################################
#     para hacerlo sin reemplazo
#     especifico el numero de conglomerados (municipios) en la poblacion


fpc <- rep(570,(length(muestra$distrito)))   #numero de conglomerados en la pob (N)

muestra <- cbind(muestra,fpc)     # los incluimos en la muestra

#diseño de muestra sin reemplazo
dclus2 <- svydesign(id=muestra$municipio,weights=muestra$peso,
                    fpc=muestra$fpc)  
									
# estimo el total de viviendas particulares con TV									
tot <- svytotal(muestra$vpcontv,design=dclus2)                      
tot

# estimo la proporcion de viviendas particulares con TV
rtv <- svyratio(muestra$vpcontv,muestra$vp,dclus2)
rtv

# Los valores reales
sum(pob$vpcontv)

sum(pob$vpcontv)/sum(pob$vp)
									