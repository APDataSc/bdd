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

# Este programa selecciona una muestra bietapica
# m.a.s.- m.a.s. para los datos del
# estado de Oaxaca, para estimar el total
# y el porcentaje de viviendas particulares con TV.
# Las UPM son los municipios y 
# las USM son las localidades

setwd("")
# el archivo de datos tiene solo los municipios con
# mas de tres localidades
datos <- read.csv("datos_Oaxaca_mas_mas_3.csv") 

library(sampling)
#############################################################
#       genero la variable municipio
#       con valores de 1 a 570 (ojo solo hay 471 municipios)

dis <- datos$distrito # distritos
mun <- datos$muni  #municipios
cong <- dis*100+mun  #combina distrito con municipio
municipio <- cleanstrata(cong) #quedan numerados los conglomerados
                            # entre 1 y max
todos <- cbind(datos,municipio) #incluyo municipio en la base de datos

###########################################################################
#            seleccion
set.seed(5643)
n <- 300          # numero de municipios en muestra (UPM),
                  # y se seleccionan 3 localidades en c/u
mm <- mstage(todos,stage=list("cluster",""),
            varnames=list("municipio","localida"),
            size=list(n,c(rep(3,n))),method=list("srswor","srswor"))
mm[1]   # lista los municipios en muestra con su prob de inclusion y todas las localidades que contiene
mm[2]   # lista las localidades en muestra, su probabilidad y la probabilidad de selecci?n total
mimuestra <- getdata(todos,mm)
muestra <- mimuestra[[2]] # tiene las localidades en muestra de los municipios en muestra
# mimuestra[[1]] tiene todas las localidades de los municipios en muestra
fexp <- 1/muestra$Prob
muestra <- cbind(muestra,fexp)

###########################################################################
#         estimacion

library(survey)   #libreria para estimacion

dcong <- svydesign(id=~municipio+localida,probs=~Prob,data=muestra)
summary(dcong)
tot <- svytotal(~vpcontv,design=dcong) 
tot
rtv <- svyratio(~vpcontv,~vp,dcong)
rtv

# valores poblacionales
sum(todos$vpcontv)
sum(todos$vpcontv)/sum(todos$vp)
