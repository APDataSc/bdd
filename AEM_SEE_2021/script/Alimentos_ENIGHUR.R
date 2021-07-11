
rm(list=ls())

#Carga de librerías previamente instaladas
library(foreign)
library(survey)
library(srvyr)
library(reshape2)
library(dplyr)

setwd("C:/Users/apena/Desktop/INEC 2021/Tickets/ENIGHUR 11-12/bbd_ingresos_gastos_2011-2012/2011-2012/Tablas_Trabajo/02 TABLAS DE TRABAJO")

#Base de Gastos del Hogar
bdd <- read.spss("06 ENIGHUR11_GASTOS_V.sav", use.value.labels = F, 
                 to.data.frame = T)

names(bdd)


#solo variables necesarias
bdd<-bdd%>%select(Identif_2010, Identif_hog, Fexp_cen2010, Provincia, 
                  Área, nprod, codciif, gastomo, cantidad, unidad,      
                  frecuen, forma, pago, adquirio, consumo, destino)

bdd$ccif<-substr(bdd$codciif,1,7)
bdd$largo<-nchar(trimws(bdd$ccif))
bdd$division<-ifelse(bdd$largo==6, substr(bdd$codciif,1,1), substr(bdd$codciif,1,2))
bdd$clase<-ifelse(bdd$largo==6, substr(bdd$codciif,1,3), substr(bdd$codciif,1,4))
bdd$subclase<-ifelse(bdd$largo==6, substr(bdd$codciif,1,4), substr(bdd$codciif,1,5))


#bdd<-filter(bdd, division==1)
#bdd_carnes<-filter(bdd, clase==112)


table(bdd$unidad)
table(bdd$codciif)

class(bdd$unidad)

bdd$fact_conv<-with(bdd, ifelse(as.integer(unidad)==1, 1/1000,
                                ifelse(as.integer(unidad)==2, 1/(2.2*16),
                                       ifelse(as.integer(unidad)==3, 1/2.2,
                                              ifelse(as.integer(unidad)==4, 1,
                                                     ifelse(as.integer(unidad)==5, 25/2.2, 
                                                            ifelse(as.integer(unidad)==6, 100/2.2,
                                                                   ifelse(as.integer(unidad)==7, 1/1000,
                                                                          ifelse(as.integer(unidad)==8, 1/1000,
                                                                                 ifelse(as.integer(unidad)==9, 1,
                                                                                        ifelse(as.integer(unidad)==10, 3.785,
                                                                                               ifelse(as.integer(unidad)==18, 1,1
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
)
)

bdd$cant_fact<-bdd$cantidad*bdd$fact_conv

bdd$q<-with(bdd, ifelse(as.integer(frecuen)<=2, cant_fact*4.29,
                        ifelse(as.integer(frecuen)==3, cant_fact*2,
                               ifelse(as.integer(frecuen)==4, cant_fact*1,
                                      ifelse(as.integer(frecuen)==5, cant_fact*1/3,
                                             ifelse(as.integer(frecuen)==6, cant_fact*1/6,
                                                    cant_fact*1/12
                                             ) 
                                      )
                               )
                        )
)
)

bdd$Provincia <- factor(bdd$Provincia, levels = 1:24, 
                        labels = c("AZUAY",
                                   "BOLIVAR",
                                   "CAÑAR",
                                   "CARCHI",
                                   "COTOPAXI",
                                   "CHIMBORAZO",
                                   "EL ORO",
                                   "ESMERALDAS",
                                   "GUAYAS",
                                   "IMBABURA",
                                   "LOJA",
                                   "LOS RIOS",
                                   "MANABI",
                                   "MORONA SANTIAGO",
                                   "NAPO",
                                   "PASTAZA",
                                   "PICHINCHA",
                                   "TUNGURAHUA",
                                   "ZAMORA CHINCHIPE",
                                   "GALAPAGOS",
                                   "SUCUMBIOS",
                                   "ORELLANA",
                                   "SANTO DOMINGO",
                                   "SANTA ELENA"))


#bdd<-bdd%>%filter(as.integer(forma)<=3)

bdd_q <- bdd %>%
  group_by(Provincia, Identif_2010, Identif_hog, clase, subclase, codciif) %>%
  summarise(Fexp_cen2010=mean(Fexp_cen2010), q=sum(q))

base <- dcast(bdd_q, Provincia + Identif_2010 + Identif_hog + Fexp_cen2010 ~ codciif)
base_clase <- dcast(bdd_q, Provincia + Identif_2010 + Identif_hog + Fexp_cen2010 ~ clase, fun.aggregate = sum)
base_subclase <- dcast(bdd_q, Provincia + Identif_2010 + Identif_hog + Fexp_cen2010 ~ subclase, fun.aggregate = sum)


"-------------------------------------------------------------------------------"
"Pan corriente trigo (bollo, redondo, cachos)"

d1 <- base %>% as_survey_design(ids = Identif_2010,
                                 strata = Provincia,
                                 weights = Fexp_cen2010,
                                 nest = T)
options(survey.lonely.psu = "certainty")

sum(weights(d1))


#Tabulado Pan corriente trigo
(tab1 <- d1 %>%
  mutate(pob = ifelse(!is.na(`111008`), 1, NA)) %>%
  summarise(N=sum(Fexp_cen2010*pob, na.rm = T),
            n=sum(pob, na.rm = T),
            media_pan = survey_mean(`111004`, na.rm = T,
                                      vartype=c("se", "ci", "cv"))
  ))

#Tabulado Arroz blanco
(tab2 <- d1 %>%
    mutate(pob = ifelse(!is.na(`111402`), 1, NA)) %>%
    summarise(N=sum(Fexp_cen2010*pob, na.rm = T),
              n=sum(pob, na.rm = T),
              media_arroz = survey_mean(`111402`, na.rm = T,
                                      vartype=c("se", "ci", "cv"))
    ))

#Tabulado Papa chola
(tab3 <- d1 %>%
    mutate(pob = ifelse(!is.na(`117208`), 1, NA)) %>%
    summarise(N=sum(Fexp_cen2010*pob, na.rm = T),
              n=sum(pob, na.rm = T),
              media_papa = survey_mean(`117208`, na.rm = T,
                                        vartype=c("se", "ci", "cv"))
    ))


"-------------------------------------------------------------------------------"
"CL 01.1.6 FRUTAS (ND)"

d2 <- base_clase %>% as_survey_design(ids = Identif_2010,
                                 strata = Provincia,
                                 weights = Fexp_cen2010,
                                 nest = T)
options(survey.lonely.psu = "certainty")

sum(weights(d1))


#Tabulado Frutas
(tab4 <- d2 %>%
    mutate(pob = ifelse(`116`!=0, 1, NA)) %>%
    summarise(N=sum(Fexp_cen2010*pob, na.rm = T),
              n=sum(pob, na.rm = T),
              media_frutas = survey_mean(`116`, na.rm = T,
                                       vartype=c("se", "ci", "cv"))
    ))


"-------------------------------------------------------------------------------"
"SCL 01.1.6.100 Frutas secas y en conserva (procesadas)"

d3 <- base_subclase %>% as_survey_design(ids = Identif_2010,
                                      strata = Provincia,
                                      weights = Fexp_cen2010,
                                      nest = T)
options(survey.lonely.psu = "certainty")

sum(weights(d1))


#Tabulado Frutas secas y en conserva (procesadas)
(tab5 <- d3 %>%
    mutate(pob = ifelse(`1161`!=0, 1, NA)) %>%
    summarise(N=sum(Fexp_cen2010*pob, na.rm = T),
              n=sum(pob, na.rm = T),
              media_frutas_sec = survey_mean(`1161`, na.rm = T,
                                         vartype=c("se", "ci", "cv"))
    ))


#Hortalizas 
(tab6 <- d3 %>%
    mutate(pob = ifelse(`1170`!=0, 1, NA)) %>%
    summarise(N=sum(Fexp_cen2010*pob, na.rm = T),
              n=sum(pob, na.rm = T),
              media_hortalizas = survey_mean(`1170`, na.rm = T,
                                         vartype=c("se", "ci", "cv"))
    ))


#-----------------------------------------------------
write.table(tab1, "clipboard", sep="\t", row.names = F)
write.table(tab2, "clipboard", sep="\t", row.names = F)
write.table(tab3, "clipboard", sep="\t", row.names = F)
write.table(tab4, "clipboard", sep="\t", row.names = F)
write.table(tab5, "clipboard", sep="\t", row.names = F)
write.table(tab6, "clipboard", sep="\t", row.names = F)



#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------


rm(list=ls())

#Base de Gastos del Hogar
bdd <- read.spss("10 ENIGHUR11_HOGARES_AGREGADOS.SAV", use.value.labels = F, 
                 to.data.frame = T)

names(bdd)


#Solo variables necesarias
bdd<-bdd%>%select(Identif_2010, Identif_hog, Fexp_cen2010, Provincia, 
                  Área, numpers, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10,
                  d11, d12, gas_gru_cor, ot_gas_mon, gas_mon_cor, gas_nm_cor,   
                  gas_cor_tot, dec_gas_per_nac)

#Diseño muestral
d1 <- bdd %>% as_survey_design(ids = Identif_2010,
                               strata = Provincia,
                               weights = Fexp_cen2010,
                               nest = T)
options(survey.lonely.psu = "certainty")


# Tabulado
(tab1 <- d1 %>%
    group_by(dec_gas_per_nac) %>%
    summarise(N=sum(Fexp_cen2010),
              n=unweighted(n()),
              alimentos = survey_mean(d1, na.rm = T,
                                   vartype=c("se", "ci", "cv"),)
    ))

write.table(tab1, "clipboard", sep="\t", row.names = F)
