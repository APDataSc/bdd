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

# Ejemplo de Encuesta Multipropósito 2020

#   *==============================================================================*
#   * INSTRUCCIONES:
#   *==============================================================================*
#   * 1. Descargue las bases de la encuesta Multipropósito de la página web del INEC. 
#   * 2. Coloque en una sola carpeta las bases de datos.
#   * 3. No cambie el nombre original de los archivos.
#   * 4. En setwd(""), dentro de las comillas se debe colocar la dirección de la carpeta 
#   *    que contiene las bases.


rm(list=ls())

#Carga de librerías previamente instaladas
library(readstata13)
library(survey)
library(srvyr)

#Colocar directorio entre las comillas ""
setwd("C:\\Users\\usuario\\Desktop\\RUGE\\bdd\\BDD_MULTI_2020_12_STATA14")
setwd("C:\\Users\\apena\\Desktop\\INEC 2021\\TICs\\BDD_MULTI_2020_12_STATA14")
# ==============================================================================*
#     INDICADORES A NIVEL DE PERSONAS                                                                                                                                                                              
# ==============================================================================* 

#Base de Educación Actividad Física y TIC´s
bdd_educación_actfisica_tics <- read.dta13("202012_multibdd_educación_actfisica_tics.dta", 
                                           convert.factors = FALSE)

#Base de datos de Personas
bdd_personas <- read.dta13("202012_multibdd_personas.dta", 
                           convert.factors = FALSE)

#Base de Educación Actividad Física y TIC´s con sexo
bdd_educación_actfisica_tics <- merge(bdd_educación_actfisica_tics,
                                      bdd_personas[,c(7, 27)], by="id_per", 
                                      all.x = T)

#Diseño muestral
d2 <- bdd_educación_actfisica_tics %>% as_survey_design(ids = upm,
                                                        strata = estrato,
                                                        weights = fexp,
                                                        nest = T)
options(survey.lonely.psu = "certainty")


#Recodificación de edad
d2 <- d2 %>%
  mutate(edad = ifelse(s1p3>=5 & s1p3<=15, 1,
                       ifelse(s1p3>=16 & s1p3<=24, 2,
                              ifelse(s1p3>=25 & s1p3<=34, 3, 
                                     ifelse(s1p3>=35 & s1p3<=44, 4, 
                                            ifelse(s1p3>=45 & s1p3<=54, 5,
                                                   ifelse(s1p3>=55 & s1p3<=64, 6, 
                                                          ifelse(s1p3>=65 & s1p3<=98, 7, NA)
                                                   )
                                            )
                                     )
                              )
                       )
  )
  )

d2$variables$edad <- factor(d2$variables$edad, levels = 1:7, labels = c("5 a 15",
                                                                        "16 a 24",
                                                                        "25 a 34",
                                                                        "35 a 44",
                                                                        "45 a 54",
                                                                        "55 a 64",
                                                                        "65 a 98"))

# Sexo
table(d2$variables$s1p2)


# Área
table(d2$variables$area)

# Horas de uso de internet
table(d2$variables$s7p2)
table(d2$variables$s7p5)

# Edad
table(d2$variables$s1p3)
table(d2$variables$edad)


# Peso medio por edad
tab_weights <- d2 %>%
  group_by(edad) %>%
  summarize(avg_wt = mean(fexp))


table(d2$variables$s7p2)

# Uso de internet
tab_w <- svytable(~s7p2, design = d2)
class(tab_w)


"Grafica de variables categóricas"
# Agregar las proporciones a la tabla
tab_w <- tab_w %>%
  as.data.frame() %>%
  mutate(Prop = Freq/sum(Freq))

# Crear un barplot
ggplot(data = tab_w,
       mapping = aes(x = s7p2, y = Prop)) + 
  geom_col()

"Tablas de contingencia"
# 
tab_D <- svytable(~s7p2,
                  design = d2)
tab_D

# 
tab_H <- svytable(~edad,
                  design = d2)
tab_H

# 
tab_DH <- svytable(~s7p2 + edad,
                   design = d2)
tab_DH


"Gráfica de barras segmentadas"

# Adicionando proporciones condicionales
tab_DH_cond <- tab_DH %>%
  as.data.frame() %>%
  group_by(edad) %>%
  mutate(n_edad = sum(Freq), Prop_internet = Freq/n_edad) %>%
  ungroup()

print(tab_DH_cond)


# 
ggplot(data = tab_DH_cond,
       mapping = aes(x = edad, y = Prop_internet, fill = s7p2)) + 
  geom_col() + 
  coord_flip()


"Uso de svytotal()"

# 
tab_totals <- svytotal(x = ~interaction(s7p2, edad),
                       design = d2,
                       na.rm = TRUE)

# 
print(tab_totals)

# 
tab_means <- svymean(x = ~interaction(s7p2, edad),
                     design = d2,
                     na.rm = TRUE)

# 
print(tab_means)
tab_means <- as.data.frame(tab_means)
sum(tab_means$mean)


"Chi cuadrado"
# 
svychisq(~s7p2 + edad, 
         design = d2, 
         statistic = "Chisq")

svychisq(~s7p2 + edad, 
         design = d2, 
         statistic = "Wald")




"----------------------------------------------------------"
"Variables numericas"

# 
svymean(x = ~s7p5, 
        design = d2,
        na.rm = TRUE)

# 
svyby(formula = ~s7p5, 
      by = ~s1p2, 
      design = d2, 
      FUN = svymean, 
      na.rm = TRUE, 
      keep.names = FALSE)

"Cuantiles"
# 
svyquantile(x = ~s7p5, 
            design = d2, 
            na.rm = TRUE, 
            quantiles = c(0.01,
                          0.25,
                          0.50,
                          0.75,
                          0.99))

"Gráfica de barra"
# 
out <- svyby(formula = ~s7p5, 
             by = ~s1p2, 
             design = d2, 
             FUN = svymean, 
             na.rm = TRUE, 
             keep.names = FALSE)

# 
ggplot(data = out, mapping = aes(x=s1p2, y=s7p5)) +
  geom_col() + 
  labs(y="Average Nightly Sleep")


"Gráfica de barra con error"
# 
out_col <- mutate(out, 
                  lower = s7p5 - 2*se, 
                  upper = s7p5 + 2*se)

# 
ggplot(data = out_col, 
       mapping = aes(x = s1p2, y = s7p5, 
                     ymin = lower, ymax = upper)) +
  geom_col(fill = "gold") +
  labs(y = "Promedio de horas de uso de internet") +
  geom_errorbar(width = 0.7)


"Histogramas"
# 
ggplot(data = bdd_educación_actfisica_tics,
       mapping = aes(x=s7p5, weight=fexp)) + 
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Horas de uso de internet")


"Density plots"
# 
bdd_educación_actfisica_tics %>%
  filter(!is.na(s7p5), !is.na(s1p2)) %>%
  group_by(s1p2) %>%
  mutate(fexp_std = fexp/sum(fexp)) %>%
  ggplot(mapping = aes(x = s7p5, weight = fexp_std)) + 
  geom_density(bw = 0.6,  fill = "gold") +
  labs(x = "Horas de uso de internet") + 
  facet_wrap(~s1p2, labeller = "label_both")

"T-test"
# 
svyttest(formula = s7p5 ~ s1p2,
         design = d2)

"Logit"
d2 <- d2 %>%
        mutate(internet=ifelse(s7p2==1, 1, 0))  
  
m1 <- svyglm(internet~s1p2+edad, design=d2,
               family=quasibinomial())

summary(m1)