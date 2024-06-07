
################################# EJERCICIO 3 #################################

#Naomi Neriyah Casanova Rivera
#Pablo Andres Montejo Aponte
#Ángel David Sosa Ildefonso

#Limpieza
rm(list = ls(all.names = TRUE))

# LIBRERIAS --------------------------------------------------------------------
pacman::p_load( "dplyr","ggplot2","kableExtra","multcomp","GGally",
                "factoextra" , "psych" , "corrplot" ,"GPArotation")

# Cargamos los datos -----------------------------------------------------------
datos <- read.csv("Dat3EX.csv",header = TRUE)
View(datos)
# Reducimos nuestra tabla a las variables que solicita el problema
data_red <- datos %>%
  dplyr::select( V1, V2, V3, V6, V8, V12, V13, V16,
                 V17, V26, V27, V28, V31, V33, V37 )
View(data_red) 
summary(data_red)
str(data_red)

# INCISO I _________________________________________________________________----
# Asumiendo que las variables son continuas, obtenga los componentes principales
# e indique si se pueden identificar dimensiones interesantes de estos datos.
# Explore el uso de los datos en la escala original y con alguna escala 
# transformada.

# ANALISIS COMPONENTES PRINCIPALES 
# Matriz de correlaciones                                                    ----
matriz_corr <- round(cor(data_red),2)
# Diagrama de la matriz de correlaciones 
X11()
corrplot(matriz_corr)

# Escala original                                                            ----
R.CP=prcomp(data_red, scale = FALSE)
X11()
biplot(R.CP)

#  Visualizamos los resultados de mejor manera 
X11()
fviz_eig(R.CP) # Grafico de Sedimentacion para la variabilidad 
fviz_pca_var(R.CP,
             col.var = "contrib") # grafico bonito de las flechas 
fviz_pca_ind(R.CP) # Datos sin las flechas 

# Mapa de los componentes con las variables 
PC_org <-principal(data_red, cor="cov",
                   covar = TRUE, nfactor = 5, rotate = "none")
X11()
fa.diagram(PC_org, cut = .5, digits = 2)
summary(R.CP)

# Escala Transformada                                                        ----
#        Datos Estandarizados                                                ----
Standarized.CP=prcomp(data_red, scale = TRUE)
X11()
biplot(Standarized.CP)

#  Visualizamos los resultados de mejor manera 
X11()
fviz_eig(Standarized.CP) # Grafico de Sedimentacion para la variabilidad 
fviz_pca_var(Standarized.CP,
             col.var = "contrib") # grafico bonito de las flechas 
fviz_pca_ind(Standarized.CP) # Datos sin las flechas 

# Mapa de los componentes con las variables 
PC_St <-principal(data_red, cor="cor",
                   covar = TRUE, nfactor = 5, rotate = "none")
X11()
fa.diagram(PC_St, cut = .5, digits = 2)
summary(Standarized.CP)
PC_St
#        Datos logaritmicos                                                  ----
Logaritmic.CP=prcomp(log10(data_red), scale = TRUE)
X11()
biplot(Logaritmic.CP)

#  Visualizamos los resultados de mejor manera 
X11()
fviz_eig(Logaritmic.CP) # Grafico de Sedimentacion para la variabilidad 
fviz_pca_var(Logaritmic.CP,
             col.var = "contrib") # grafico bonito de las flechas 
fviz_pca_ind(Logaritmic.CP) # Datos sin las flechas 

# Mapa de los componentes con las variables 
PC_Log10 <-principal(log10(data_red), cor="cor",
                     covar = TRUE, nfactor = 5, rotate = "none")
X11()
fa.diagram(PC_Log10, cut = .5, digits = 2)
PC_Log10

#        Cuadraticos  ----
Square.CP=prcomp(data_red^2, scale = TRUE)
X11()
biplot(Square.CP)

#  Visualizamos los resultados de mejor manera 
X11()
fviz_eig(Square.CP) # Grafico de Sedimentacion para la variabilidad 
fviz_pca_var(Square.CP,
             col.var = "contrib") # grafico bonito de las flechas 
fviz_pca_ind(Square.CP) # Datos sin las flechas 

# Mapa de los componentes con las variables 
PC_Square<-principal(data_red^2, cor="cor",
                     covar = TRUE, nfactor = 5, rotate = "none")
X11()
fa.diagram(PC_Square, cut = .5, digits = 2)
PC_Square

# Elegiendo el mejor modelo PCA                                              ----

# Escogemos uno conforme a mayor facilidad de interpretacion del indice 
# comparar el modelo sin transformacion y el modelo elegido con transformacion 
# elegir el mejor 

# INCISO II ________________________________________________________________----
# Asumiendo que las variables son continuas, aplique la técnica de análisis 
# exploratorio factorial e indique si se pueden identificar dimensiones
# interesantes de estos datos. Explore el uso de los datos en la escala 
# original y con alguna escala transformada.

# ANALISIS FACTORIAL EXPLORATORIO
# Escala original                                                            ----

FA_org <-fa(data_red, cor="cov",
            covar = TRUE, nfactor = 5, rotate = "none")
print(FA_org)
  X11()
fa.diagram(FA_org, cut = 0.40, digits = 2) # 0.5 y 0.4

# Escala Transformada                                                        ----
#       Estandarizados                                                       ----
FA_Esc <-fa(data_red, cor="cor",
            covar = FALSE, nfactor = 5, rotate = "none")
print(FA_Esc)
X11()
fa.diagram(FA_Esc, cut = .5, digits = 2)

#       Logaritmicos                                                         ----
FA_Log <-fa(log10(data_red), cor="cor",
            covar = FALSE, nfactor = 5, rotate = "none")
print(FA_Log)
X11()
fa.diagram(FA_Log, cut = .5, digits = 2)

#       Cuadraticos                                                        ----
FA_Square <-fa(data_red^2, cor="cor",
            covar = FALSE, nfactor = 5, rotate = "none")
print(FA_Square)
X11()
fa.diagram(FA_Square, cut = .5, digits = 2)



# Elegiendo el mejor modelo FA                                               ----

# INCISO III _______________________________________________________________ -------------------------------------------------------------------
# Realice modificaciones en i) y ii) considerando:
# a) que los datos son categóricos ordinales 
# b) rotaciones a los resultados. 
# Considerando todos los resultados, sólo seleccione un conjunto de componentes 
# o factores, los que le parecen más adecuados, e interprete.

# Tratamos a los datos como variables categoricas ordinales 
data_ord <- data_red %>%
  dplyr::mutate_all(~as.numeric(. , ordered = TRUE))

str(data_ord)
summary(data_ord)

library(psych)
library(polycor)

# Matriz de correlaciones tetracorica                                       ---- 
corr_tetra <-polychoric(data_red)
rho <- corr_tetra$rho
X11()
cor.plot(rho , numbers = T , main = "Correlacion Tetracorica")#opcion 1
corrplot::corrplot(rho) #opcion 2

# ANALISIS COMPONENTES PRINCIPALES ************                                         ----
# Escala original                                                            ----
PC_org <-principal(data_ord, cor="poly",
                   covar = TRUE, nfactor = 5, rotate = "none")
print(PC_org)
X11()
fa.diagram(PC_org, cut = .5, digits = 2)
#      Con rotaciones 
#            Varimax                                                         ----
PC_org_varimax <-principal(data_ord, cor="poly",
                           covar = TRUE, 
                           nfactor = 5, 
                           rotate = "varimax")
print(PC_org_varimax, cut = .5)
X11()
fa.diagram(PC_org_varimax, cut = .5, digits = 2)
#            Oblim                                                           ----
PC_org_oblimin <-principal(data_ord, cor="poly",
                           covar = TRUE, nfactor = 5, rotate = "oblimin")
print(PC_org_oblimin, cut = .5)
X11()
fa.diagram(PC_org_oblimin, cut = .5, digits = 2)
#            Cluster                                                         ----
PC_org_cluster <-principal(data_ord, cor="poly",
                           covar = TRUE, nfactor = 5, rotate = "cluster")
print(PC_org_cluster, cut = .5)
X11()
fa.diagram(PC_org_cluster, cut = .5, digits = 2)
#            Simplimax                                                         ----
PC_Org_Simplimax <-prcomp(data_ord, cor="poly",
                        covar = TRUE, ncp = 5, rotate = "simplimax")
print(PC_Org_Simplimax, cut = .5)
summary(PC_Org_Simplimax)

# ------------------------------------------------------------------------------
# ANALISIS FACTORIAL EXPLORATORIO ************                                          ----
# Escala original                                                            ----

library(GPArotation)
res_factorial <-  fa(rho, nfactors = 5, n.obs = nrow(data_ord))
diagram(res_factorial)
title("Diagrama Factorial")
res_factorial$loadings

#            Varimax                                                         ----
FA_Org_varimax <-fa(data_ord, cor="poly",
                     covar = TRUE, nfactor = 5, rotate = "varimax")

print(FA_Org_varimax, cut = .5)
X11()
fa.diagram(FA_Org_varimax, cut = .5, digits = 2)

#            Oblim                                                           ----
FA_Org_oblim <-fa(data_ord, cor="poly",
                     covar = TRUE, nfactor = 5, rotate = "oblimin")

print(FA_Org_oblim, cut = .5)
X11()
fa.diagram(FA_Org_oblim, cut = .5, digits = 2)

#            Cluster                                                         ----
FA_Org_cluster <-fa(data_ord, cor="poly",
                     covar = TRUE, nfactor = 5, rotate = "cluster")

print(FA_Org_cluster, cut = .5)
X11()
fa.diagram(FA_Org_cluster, cut = .5, digits = 2)
#            Simplimax                                                         ----
FA_Org_Simplimax <-fa(data_ord, cor="poly",
                    covar = TRUE, nfactor = 5, rotate = "simplimax")

print(FA_Org_Simplimax, cut = .5)
X11()
fa.diagram(FA_Org_Simplimax, cut = .5, digits = 2)


# SELECCION DEL MEJOR MODELO PARA INTERPRETACION ***************************----


