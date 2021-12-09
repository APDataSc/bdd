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

# programa calcula_nh.r

# este programa genera una base de datos
# con informacion del numero de casillas Nh
# en cada uno de los 300 distritos
# y calcula el tamaño de muestra nh en cada
# distrito con distribucion proporcional.

setwd("")

library(sampling) # lo uso para la instruccion cleanstrata
datos <- read.csv("presidencia_candidato_2018.csv", header = T)
names(datos)
names(datos)[1] <- "ID_ESTADO"

N <- length(datos$ID_ESTADO) #tamaño de la poblacion
n <- 7500   # tamaño de muestra global

# primero genero la variable estrato
# que toma valores del 1 al 300

x <- datos$ID_ESTADO     #x tiene la id de estado
y <- datos$ID_DISTRITO   #y tiene la id de distrito
estr <- x*100+y       #estr tiene la combinacion de edo y dist
head(estr)            # para ver que es la union de estado y distrito
tail(estr)
estrato <- cleanstrata(estr)  #para numerarlos bien, de la libreria sampling
head(estrato)
tail(estrato)
completo <- cbind(datos,estrato)
#write.table(completo,file="completo.txt",sep=",",quote=FALSE,row.names=FALSE)

Nh <- tabulate(estrato)   # calcula cuantas casillas en cada estrato
Nh   # tiene el numero de casillas en cada distrito
sum(Nh) # suman el total de casillas

# ahora calculo nh
# primero la ponderacion de cada estrato
wh <- Nh/N        # debe sumar 1
sum(wh)
rh <-  wh * n     # rh esta en los reales
head(rh)
nh <- floor(rh)   # parte entera de rh
head(nh)
dh <- rh - nh     # parte decimal de rh
head(dh)
m <- sum(dh)      # cuantas casillas faltan por distribuir (son 139)
dhord <- sort(dh, decreasing=TRUE)  # dh ordenados de mayor a menor
head(dhord)
tail(dhord)
indhord <- order(dh, decreasing=TRUE)  # da la permutacion que ordena
head(indhord)
tail(indhord)
# a los primeros nh (los mas grandes) les sumo 1 unidad
# y a los restantes nh los dejo igual

for (i in 1:m){
  nh[indhord[i]] <- nh[indhord[i]] + 1
}
sum(nh)

hh <- 1:300
result <- as.data.frame(cbind(hh,Nh,nh))
# Asi queda el archivo Nhynh.dat
head(result)
sum(result$Nh)
sum(result$nh)
  
#write.table(result,file="Nhynh.txt",sep=",",quote=FALSE,row.names=FALSE)
