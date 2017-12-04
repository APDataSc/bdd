#***************Taller "ggplot"***********#
#                                         #
#   Elaboración:      Peña, Andrés        # 
#   Modificado:       04-12-2017          #
#                                         #
#                                         #  
#*****************************************#

setwd("C:\\Users\\apena\\Desktop\\I+D+i\\ggplot")

library(foreign)
genero <- as.data.frame(read.spss("IOP_1212_01_B.sav"))

library(ggplot2)
ggplot(genero, aes(x = SEXO)) + geom_bar()

gr1 <- ggplot(genero, aes(x = SEXO)) + geom_bar()
gr1 + xlab("Sexo del entrevistado") + ylab ("Número de casos")# etiquetas de los ejes

gr1 + xlab("Sexo del entrevistado") + ylab ("Número de casos") + 
  ggtitle("Sexo del entrevistado")

gr1 <- ggplot(genero, aes(x = SEXO)) + geom_bar(width=0.5)
gr1 + xlab("Sexo del entrevistado") + ylab ("Número de casos") + 
  ggtitle("Sexo del entrevistado")

gr1 <- ggplot(genero, aes(x = SEXO)) + geom_bar(width=0.5, colour="blue", fill="red")
gr1 + xlab("Sexo del entrevistado") + ylab ("Número de casos") + 
  ggtitle("Sexo del entrevistado")


gr1 + xlab("Sexo del entrevistado") + ylab ("Número de casos") + 
  ggtitle("Sexo del entrevistado") + theme_bw()


tab.sex1 <- as.data.frame(prop.table(table(genero$SEXO))*100)
tab.sex1

colnames(tab.sex1) <- c("Sexo", "Porcentaje")

ggplot(tab.sex1, aes(x=Sexo, y=Porcentaje)) + geom_bar(stat="identity")


gr2 <- ggplot(tab.sex1, aes(x=Sexo, y = Porcentaje)) + 
  geom_bar(stat="identity", width=0.5, fill = "grey") 

gr2 + xlab(NULL) + ylab("% de entrevistados") + 
  ggtitle("Distribución de los entrevistados\n por sexo (%)") + theme_bw()


library(scales) # requiere instalar el paquete "scales"
gr2.a <- ggplot(genero, aes(SEXO)) + 
  geom_bar(aes(SEXO, (..count..)/sum(..count..)), width=0.5, fill = "grey") 

gr2.a + scale_y_continuous(labels=percent) + xlab(NULL) + 
  ylab("% de casos") + 
  ggtitle("Distribución de los\n entrevistados por sexo") +  theme_bw()



ggplot(genero, aes(P36, fill=SEXO)) + geom_bar(position="dodge")




gr3 <- ggplot(genero[genero$P36!="NS/NR", ], (aes(P36, fill=SEXO))) + 
  geom_bar(aes(P36, (..count..)/sum(..count..)), position="dodge") + 
  scale_y_continuous(labels=percent) + scale_fill_grey() + theme_bw()

gr3

gr3 + xlab(NULL) + ylab("% de casos") + ggtitle("¿Los demás miembros del hogar deberían pagarle un sueldo o salario\n al")



tab.5 <- as.data.frame(prop.table(table(genero$P51D, genero$DOMINIO), 2)*100)
tab.5

colnames(tab.5) <- c("Rptas", "Dominios", "Pct")
gr4 <- ggplot(tab.5, aes(x=Rptas, y=Pct)) + geom_bar(stat="identity") 
gr4

gr4 + facet_grid(Dominios ~.)

gr4 + facet_grid(.~ Dominios)

gr4 + facet_wrap(~ Dominios)

gr4 + coord_flip() + facet_wrap(~ Dominios)



ggplot(genero, aes(P19A)) + geom_histogram()

range(genero$P19A, na.rm=TRUE)

ggplot(genero, aes(P19A)) + geom_histogram(binwidth = 10)


hist1 <- ggplot(genero, aes(P19A)) + 
  geom_histogram(binwidth = 10,
                 fill="red", 
                 colour="black")
hist1


hist1 + facet_grid(SEXO ~.)


## Las sintaxis nos dan el mismo resultado. 
#La segunda evita dibujar la línea de abajo.
ggplot(genero, aes(P23)) + geom_density()
ggplot(genero, aes(P23)) + geom_line(stat="density")


ggplot(genero, aes(P23)) + geom_line(stat="density") + facet_grid(NSEGrup ~.)

ggplot(genero, aes(P23)) + geom_line(stat="density") + facet_grid(NSEGrup ~ SEXO)

edad <- genero$EDAD
gr.edad <- as.factor(cut(edad, breaks = c(18, 25, 35, 45, 55, 92), 
                         include.lowest = TRUE))
table(gr.edad)










