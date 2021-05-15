#**************************************************************************************#
#**************************************************************************************#
#
#                       Taller de Análisis de Encuestas con R                        
#                        Sociedad Ecuatoriana de Estadística
#
#     Fecha de elaboración:   05/10/2020
#     Última actualización:   14/05/2021
#     Actualizado por:        Andrés Peña M.               
#     Contacto:               Andrés Peña M. (a.pena@rusersgroup.com)
#     Organización:           R Users Group - Ecuador
#                             
#
#**************************************************************************************#
#**************************************************************************************#

# Muestreo Aleatorio Simple

# Ejemplo de educacion:
# Este programa selecciona una muestra pequeña (m.a.s.) estima la 
# varianza para calcular tamaño de muestra selecciona por m.a.s. 
# una muestra y estima (puntual y por intervalos) el promedio de 
# calificaciones, el total de ingresos y la proporcion de alumnos 
# que trabajan.

setwd("") #carpeta de trabajo

datos <- read.csv("educacion.txt") #lectura de datos
names(datos)


"Forma de seleccionar una muestra por MAS"
N <- length(datos$folio)
np <- 50
ind1 <- sample(1:N,np)  # selecciona una m.a.s. de tamaño np
                        # de los números entre 1 y N
sort(ind1)  # solo para ver cuales son los registros en muestra
muestrap <- datos[ind1,]  # selecciono los números de registros
                          # que están en ind1 de datos


#--------------------------------------------------------------
"Tamaños de muestra"

"Para calificaciones"
s2.calif <- var(muestrap$calif)

# tamaño de muestra para estimar el promedio
# de calificaciones con una precision de +-2
n1 <- s2.calif*1.96^2/2^2
nn1 <- n1/(1+n1/N)
# proporcion que representa la muestra
# de la poblacion
nn1/N

"Para ingresos"
s2.ing <- var(muestrap$ingresos)
# tamaño de muestra para estimar el total
# de ingresos con una precision de +-5,000,000
n2 <- s2.ing*1.96^2*N^2/5000000^2
nn2 <- n2/(1+n2/N)
# proporcion que representa la muestra
# de la poblacion
nn2/N

"Para proporción de estudiantes que trabajan"
# tamaño de muestra para estimar la proporcion
# de estudiantes que trabajan con una
# precision de +-0.03
ppiloto <- mean(muestrap$trabaja)
n3 <- 1.96^2 *ppiloto*(1-ppiloto)/0.03^2
nn3 <- n3/(1+n3/N)
# proporcion que representa la muestra
# de la poblacion
nn3/N


#--------------------------------------------------------------
"Tamaños de muestra con diferentes precisiones para la proporción"

# tambien puedo calcular una tabla
# de tamaños de muestra con diferentes
# precisiones y una confianza fija
# o precision fija y diferentes confianzas
# para ayudar a seleccionar el tamaño de muestra
# por ejemplo para estimar una P
# confianza del 95%
# diferentes precisiones
delta <- seq(.01,.2,.01) # precisiones
n.prop <- 1.96^2 *ppiloto*(1-ppiloto)/delta^2
nn.prop <- n.prop/(1+n.prop/N)  # ajustado por N
tabla <- cbind(delta,ceiling(n.prop),ceiling(nn.prop))
tabla

# O dado el tamaño de muestra y la confianza
# cual es la precision que se alcanza
tam.muestra <- seq(600,2000,50)
tam.prec <- sqrt(1.96^2*ppiloto*(1-ppiloto)/tam.muestra)
tam.tabla <- cbind(tam.muestra,tam.prec)
tam.tabla


"Tamaño de muestra final"
nn1; nn2; nn3
# tomemos un tamaño de muestra n=max(nn1, nn2, nn3)
# seleccionamos una m.a.s.
n <- ceiling(max(c(nn1, nn2, nn3)))
ind <- sample(1:N,n)
muestra <- datos[ind,]
table(muestra$cve_ent)  #muestra en todos los estados?


"Estimación"
# estimamos el promedio de calificaciones
calif.prom <- mean(muestra$calif)
s2 <- var(muestra$calif)
var.calif.prom <- (1-n/N)*s2/n
ee.calif.prom <- sqrt(var.calif.prom)
prec <- 1.96*ee.calif.prom
li_calif.prom <- calif.prom-prec
ls_calif.prom <- calif.prom+prec
calif.prom;li_calif.prom;ls_calif.prom
prec
#el valor real
mean(datos$calif)

# estimamos el total de ingresos
ing.prom <- mean(muestra$ingresos)
ing.tot <- N*ing.prom
var.ing.tot <- N^2*(1-n/N)*s2.ing/n
ee.ing.tot <- sqrt(var.ing.tot)
prec <- 1.96*ee.ing.tot
li_ing.tot <- ing.tot- prec
ls_ing.tot <- ing.tot+ prec
ing.tot;li_ing.tot;ls_ing.tot
prec
# el valor real
sum(datos$ingresos)

# estimamos la proporcion de alumnos que trabajan
p.est <- mean(muestra$trabaja)
var.p.est <- (1-n/N)*p.est*(1-p.est)/(n-1)
ee.pest <- sqrt(var.p.est)
prec <- 1.96*ee.pest 
li_p.est <- p.est - prec
ls_p.est <- p.est + prec
p.est;li_p.est;ls_p.est
prec
# el valor real
mean(datos$trabaja)



################################################################
# calculo el intervalo de la mediana de ingresos
# por Bootstrap
hist(datos$ingresos, col="orange")
median(datos$ingresos)
mean(datos$ingresos)
M <- 1000 # numero de muestras bootstrap
ymed <- numeric(M)
for (i in 1:M)
{
  sid <- sample(1:n,n,replace=T)
  s <- muestra[sid,]
  ymed[i] <- median(s$ingresos)
}

# histograma de las 1000 medianas
hist(ymed, col = "green")
#Intervalo del 95% de confianza
quantile(ymed,probs=c(0.025,0.975))
# valor real
median(datos$ingresos)