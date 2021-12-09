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

# programa estima_combinado.r

# este programa calcula las estimaciones para la proporcion
# de votos de los candidatos a presidente
# la estimacion es a traves de estimadores de razon combinada

library(survey)
setwd("")

muestra <- read.csv("muestra_estratificada.csv")
head(muestra,1)         # lista el primer registro de muestra
tail(muestra,1)         # lista el ultimo registro de muestra
attach(muestra)         # por facilidad
N <- sum(fexp)          # tamaño de la poblacion
n <- length(ID_ESTADO)  # tamaño de muestra
L <- max(estrato)       # numero de estratos, supone que
                        # estan numerados del 1 al L

# defino el diseño de muestra sin reemplazo 
# (con fpc= finite population correction)
# especificando variable de estrato
# y variable de factores de expansion en weights

disin <- svydesign(~1,strata=estrato,data=muestra,weights=fexp,
                   fpc=Nh)

# estimacion utilizando la libreria

# estimacion proporcion de votos (razon combinado)
svyratio(~RAC,~TOTAL_VOTOS,design=disin)

###########################################################
# Programando las expresiones de los estimadores
# Se calculan las estimaciones
# con razones combinadas

#Estimo el total del numerador y del denominador (PAN)

Y.RAC <- sum(RAC*fexp)
X <- sum(TOTAL_VOTOS*fexp)
R.RAC <- Y.RAC/X
vestrat <- rep(0,L)   #es el pedazo de la varianza de c/estrato

#calculo lo necesario para c/u de los L estratos
for(i in 1:L)
{
  mm <- subset(muestra,estrato==i,select=c(estrato,Nh,nh,TOTAL_VOTOS,RAC))
  e <- mm$RAC-R.RAC*mm$TOTAL_VOTOS
  Ngh <- mm$Nh[1]
  nph <- mm$nh[1]
  vestrat[i] <- Ngh^2*(1/nph-1/Ngh)*var(e)
}
VR.RAC <- 1/X^2*sum(vestrat)
EER.RAC <- sqrt(VR.RAC)
Prec.RAC <- 1.96*EER.RAC
LI.RAC <- R.RAC-Prec.RAC
LS.RAC <- R.RAC+Prec.RAC
# escribo todo
R.RAC;EER.RAC;Prec.RAC; LI.RAC; LS.RAC


# y como lo hicimos con la libreria survey
# estimacion proporcion de votos (razon combinado)
svyratio(~RAC,~TOTAL_VOTOS,design=disin)

# sale lo mismo!!!!!!
# y el valor del parametro es
# R=0.2226722

