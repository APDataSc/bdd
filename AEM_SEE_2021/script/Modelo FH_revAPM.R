#**************************************************************************************#
#**************************************************************************************#
#
#                       Taller de Análisis de Encuestas con R                        
#                        Sociedad Ecuatoriana de Estadística
#
#     Última actualización:   09/12/2021
#     Creado por:             CEPAL
#     Actualizado por:        Andrés Peña M.               
#     Contacto:               Andrés Peña M. (a.pena@rusersgroup.com)
#     Organización:           R Users Group - Ecuador
#                             
#
#**************************************************************************************#
#**************************************************************************************#

## Linpiando la memoria de R y fijando la cantidad de dígitos
rm(list = ls())
#options(digits = 2)

## Cargue de las librerías
library(sae)
library(tidyverse)
setwd("")

### Cargue de la muestra y las covariables necesarias para el proceso de estimaci?n

muestraSAE <- readRDS("muestraSAE.RDS")
auxiliar<- read.csv("Info_Auxiliar.csv", sep=";", dec=",")


### Exploración  de las bases de datos

# Número de casos en la encuesta
nrow(muestraSAE)

# Dominios - áreas
length(unique(muestraSAE$Dominio))

# Número de casos en el censo
nrow(auxiliar)

# Variables en el censo
names(auxiliar)

# Dominios - áreas
length(unique(auxiliar$Dominio))



#############################
# Modelo Fay - Herriot #
#############################

# Primero, estimemos la media de los ingresos con HT y su varianza

HorvitzT<- direct(y = Income, 
                  dom = Dominio, 
                  sweight = Fexp, 
                  domsize = auxiliar[,c(1,3)], 
                  data = muestraSAE)
HT<- HorvitzT[complete.cases(HorvitzT$Direct),]
colnames(HT)[1]<- "Dominio"
HT<- HT[, -2 ]

### Construyendo la base con la informaci?n para el c?lculo del estimador

area_values<- left_join(auxiliar, HT, "Dominio")
area_values<- area_values[complete.cases(area_values$Direct),] %>% 
              dplyr::select(-Nd)
head(area_values)

## Ajuste del modelo Fay Herriot
attach(area_values)
salida<- mseFH( Direct   ~  Expenditure, vardir = SD ^2, method = "REML")
salida
## Una vez ajustado el modelo Fay Herriot se extrae el EBLUP, 
## el Error Cuadr?tico Medio (ECM) y se calcula el Error Cuadrado Medio Relatico (ECMR)
## El cual es una medida de calidad para comparar la estimaci?n directa con la
## generada bajo el modelo Fay Herriot.

ECM<- salida$mse
Eblup<- salida$est$eblup
ECMR<- 100*sqrt(ECM)/Eblup

## Se construye una base de datos con la informaci?n de los resultados obtenidos
## con el modelo Fay Herriot para los dominios observados.

Estima_SAE_obs<- data.frame(Dominio = area_values$Dominio,
                              Est_Eblup = Eblup, 
                              ECM = ECM,
                              ECMR = ECMR)
head(Estima_SAE_obs)
## Construyamos una base para los dominios no muestreados. Primero, se deben extraer los betas
## Del modelo Fay Herriot. En segundo lugar, se multiplican los betas con las variables auxiliares
## en los dominios no observados para obtener las predicciones

## Extrayendo los betas estimados del modelo
betas<- as.matrix(salida$est$fit$estcoef$beta, ncol=1) 

## Generando la matriz con la variable auxiliar y el intercepto
X<- as.matrix(cbind(1, auxiliar$Expenditure)) 

## Haciendo las predicciones 
predicciones<- X%*%betas 

## Construyendo la base de datos con las predicciones para los dominios no observados
 
predicciones<- data.frame(Dominio = auxiliar$Dominio, Predicciones = predicciones)
Dom_no_muestre<- setdiff(predicciones$Dominio, unique(muestraSAE$Dominio))
predic_SAE_Nobs<- subset(predicciones, predicciones$Dominio %in% Dom_no_muestre)
colnames(predic_SAE_Nobs)[2]<- "Est_Eblup"

## Generar los insumos para el c?lculo del ECM en los dominios no observados

## Primero, se estima la varianza del efecto aleatorio y la varianza de los betas estimados

## Varianza estimada del efecto aleatorio
sigma2_u<- salida$est$fit$refvar 

## Varianza de los betas estimados

## Para generar la varianza de los betas, se organiza la informaci?n auxiliar en 
## en los dominios observados ya que esta varianza solo depende de las observaciones 
## muestrales.

Info_auxi_obs<- subset(auxiliar, predicciones$Dominio %in% unique(muestraSAE$Dominio)) %>% 
  dplyr::select(-Nd) %>% 
  mutate(inter = 1)

X_obs<- cbind(1, Info_auxi_obs$Expenditure)
Vi <- 1/( sigma2_u + (HT$SD^2) )
Vbeta_est <- solve( t(Vi * X_obs) %*% X_obs )

## Una vez se tiene la estimaci?n de la varianza de los betas, se organiza la informaci?n auxiliar 
## en los dominios no observados para estimar el ECM 

Info_auxi_Nobs<- subset(auxiliar, predicciones$Dominio %in% Dom_no_muestre) %>% 
                 dplyr::select(-Nd) %>% 
                 mutate(inter = 1)

X_Nobs<- cbind(1, Info_auxi_Nobs$Expenditure)

ECM_Nobs<- sigma2_u+ diag(as.matrix(X_Nobs, ncol=2)  %*% Vbeta_est %*% t(X_Nobs))

# ECM_Nobs<- NA
# 
# for(i in 1:dim(X_Nobs)[1]){
#   ECM_Nobs[i]<- sigma2_u + t(as.matrix(X_Nobs[i,], ncol=2) ) %*% Vbeta_est %*% X_Nobs[i,] 
# }

## C?lculo de los Errores est?ndares relativos
ECMR_Nobs<- 100*sqrt(ECM_Nobs)/predic_SAE_Nobs$Est_Eblup

## Agregar a la base de datos de predic_SAE las columnas ECM y ECMR
predic_SAE_Nobs$ECM<- ECM_Nobs
predic_SAE_Nobs$ECMR<- ECMR_Nobs

## Agregar a la base de datos global, todas las estimaciones con el modelo
## Fay Herriot
Base_completa_SAE<- rbind(Estima_SAE_obs, predic_SAE_Nobs)
dim(Base_completa_SAE)

## Se genera una base de datos completa que contenga la informaci?n de la estimaci?n
## directa y la obtenida con el modelo Fay Herriot.

Base_completa<- full_join(Base_completa_SAE, area_values) %>% 
                dplyr::select(-Expenditure) 
  
