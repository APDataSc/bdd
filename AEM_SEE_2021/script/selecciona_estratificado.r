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

# programa selecciona_estratificado.r

# Este programa selecciona una m.a.s.
# en cada uno de los 300 estratos (distritos)
# con nh proporcional al tamaño del estrato
# con respecto al numero de casillas,
# valor leido del archivo Nhynh.txt

setwd("")

# en datos esta toda la base de la votaciones 2018
datos <- read.table("completo.txt",sep=",",header=T)  #lee archivo entre comas
# en ncas esta la info de estrato, Nh y nh
ncas <- read.table("Nhynh.txt",header=T,sep=",")

N <- length(datos$ID_ESTADO) #tamaño de la poblacion
todos <- cbind(datos,ncas[datos$estrato,2],ncas[datos$estrato,3])
                #para incluir Nh y nh

names(todos)[19] <- "Nh"    # cambio el nombre de las 
names(todos)[20] <- "nh"    # dos ultimas variables
names(todos)

#especificacion de la semilla
print("dame semilla")
sem <- scan(,n=1)

set.seed(sem)

###########################################################
#		selecciono la muestra estratificada
#		con la funcion stratsrs de la libreria
#		pps
###########################################################
library(pps)        # tiene una funcion para seleccionar
                    # m.a.s. en estratos
#los tamaños de muestra
nh <- ncas$nh
n <- sum(nh)  # tamaño de muestra global
summary(nh)   # da estadisticas descriptivas de las nh

#los tamaños poblacionales en cada estrato
Nh <- ncas$Nh

strat <- todos$estrato   #tomo los valores de los estratos

mm <- stratsrs(strat,nh)  #indices de los registros en muestra

muestra <- todos[mm,]     #la muestra

# le añado el factor de expansion

fexp <- muestra$Nh / muestra$nh       #factor de expansion

muestra <- cbind(muestra,fexp)

# sumo los factores de expansion de toda la muestra
sum(muestra$fexp)      # es igual a N

write.table(muestra,"muestra_estratificada.csv",sep=",",row.names=F,
            quote=F)
frec <- table(muestra$estrato)  # frecuencia de cada estrato en muestra
sum(frec==nh) # compara si los dos vectores son iguales elemento
# a elemento y suma 

#####################################################################
#               La seleccion de la muestra
#             utilizando la libreria sampling
#               Otra forma de hacerlo
#####################################################################
# selecciono una m.a.s. en cada estrato de tamaño nh

library(sampling)   #para usar en algunos calculos
st<-strata(todos,c("estrato"),size=nh, method="srswor")
head(st)
tail(st)
muestra1<-getdata(todos,st)
fexp1 <- 1/muestra1$Prob
sum(fexp1)
muestra1 <- cbind(muestra1,fexp1)
head(muestra1)
nh1 <- tabulate(muestra1$estrato)
sum(nh==nh1)

##################################################################
#       A mano
##################################################################
# supongo que el data.frame todos esta ordenado por la variable estrato
# para cada estrato: copio la informacion a un data.frame
# y selecciono una m.a.s. de tamaño nh correspondiente
# y la voy pegando en otro lado

# para el primer estrato
temp <- subset(todos,todos$estrato==1)
mm <- temp[sample(1:ncas[1,2],ncas[1,3]),]
muestra_estr <- mm
# para los demas estratos
for (i in 2:300){
  temp <- subset(todos,todos$estrato==i)
  mm <- temp[sample(1:ncas[i,2],ncas[i,3]),]
  muestra_estr <- rbind(muestra_estr,mm)
}
cuantos <- table(muestra_estr$estrato)
sum(cuantos==nh)

