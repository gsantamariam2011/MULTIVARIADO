################################################################################################
################################################################################################
################################################################################################
                     #  GEOVANNY SANTAMARIA MANOBANDA

################################################################################################
################################################################################################
################################################################################################


#############################################################################################
#LIBRERIAS

library(readxl)
library(GPArotation)
library(Matrix)
library(nFactors)
library(paran)
library(descr)
library(foreign)
library(psych)
library(haven)
library(MVN)
library(openxlsx)
library(readxl)
library(tidyverse)
library(dplyr)
library(lavaan)

#########################################################################################
# CARGAR LOS DATOS
getwd()
setwd("C:/Users/santamarias/Desktop/CLASES PERU/2020 MULTIVARIADO/DEBER")
base <- read_excel("C:/Users/santamarias/Desktop/CLASES PERU/2020 MULTIVARIADO/DEBER/DATA ANALISIS FACTORIAL.xlsx")
dim(base)
str(base)

#########################################################################################
# GENERAR LOS 10 REGISTROS FALTANTES
n <- 10
muestramia<- sample(1:nrow(base),size=n,replace=FALSE)
muestramia
#Asignar los elementos de la muestra al data frame de datos
muestramia<- base[muestramia, ]
dim(muestramia)
base <- rbind(base,muestramia)
str(base)
#PARA EL ANALISIS SOLO LAS 27 VARIABLES
base <- base[2:28]
dim(base)
str(base)
# GUARDAR LA DATA
write.xlsx(base, file = "DATOS0K.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=FALSE , append=FALSE)

############################################################################################
#########################################################################################

# ABRIR LA BASE PARA REALIZAR EL ANALISI

base <- read_excel("DATOS0K.xlsx")
dim(base)
str(base)


#################################################################################
##     TEMA 1 

# Probando supuestos
# ============================
# Esfericidad de Bartlet
# ----------------------------

library(psych)
r.poly=polychoric(base)
r.poly
R=r.poly$rho    # La matriz de correlaciones polycórica
View(R)
n = nrow(base)  # Tamaño de la muestra
cortest.bartlett(R,n)

#KMO(Kaiser-Meyer-Olkin)
# ----------------------------
KMO(R)

##############################################################################################################
##     TEMA 2
#Identificando el N° de factores
# ----------------------------
fap=fa.parallel(base,fa="fa",cor="poly")
fap

# Corriendo el modelo para 3 factores
# ----------------------------
factorial3=fa.poly(base, nfactors=3, cor="poly",rotate="varimax")
factorial3


# Gráfica de segmentación de factores es el grafico como se asocia los factores y preg
# ----------------------------
fa.diagram(factorial3, e.size=.05,rsize=4.5)

##     TEMA 2.2
#Identificando el N° de factores

##################   con un MODELO  EXPLORATORIO
## coriendo el modelo para 4 factores

factorial4=fa.poly(base, nfactors=4, cor="poly",rotate="varimax")
factorial4


# Gráfica de segmentación de factores es el grafico como se asocia los factores y preg
# ----------------------------
fa.diagram(factorial4, e.size=.05,rsize=4.5)

#ANALISIS DE 4 FACTORES EXPLORATORIO CON INTERACCIONES
#PARAN MUESTRA LOS ENVALUE .. GRAFICOS AJUSTADOS NO AJUSTADOS ME DA TBN EL GRAFICO DE SEDIMENTACIÓN

paran(base, iterations=5000,graph=TRUE,color=TRUE, width=1000, height=1000) 

#pARA REALIZAR EL GRAFICO DE  LOS AUTO VALORES AJUSTADOS CUANTO INDICAN

# Estimación por componentes principales pARA MOSTRAR A QUE GRAFICO PERTENECE  MUESTA LA VARIANZA ACUMULADA 0,747
fit_1<-principal(cor(base),nfactors=4,rotate="varimax" )
fit_1$loadings


# REALIZA EL GRAFICO DE LAS COMPONENTES.DE COMO SON LOS GRAFICOS 
fa.diagram(fit_1, e.size=.05,rsize=4.5)
#################################################################################
##     TEMA 3

# Probando supuestos

library(lavaan)

#Creando el modelo DE ANALISIS DE FACTOR CONFIRMATORIO  YO CREO EL MODELO
# ----------------------------
# ----------------------------

modelo <- 'HUMAN = ~ HUMAN1 +	HUMAN2 +	HUMAN3 +	HUMAN4 + HUMAN5 +	HUMAN6 +
HUMAN7 +	HUMAN8  + HUMAN9
MATERIA = ~ MATERIA1 +	MATERIA2 +	MATERIA3 + MATERIA4 +	MATERIA5 +	MATERIA6 + 
MATERIA7 +	MATERIA8 +	MATERIA9
COGNITIVO = ~ COGNITIVO1 +	COGNITIVO2 +	COGNITIVO3 + COGNITIVO4 +	COGNITIVO5 +	COGNITIVO6 
COGEMOC = ~ COGEMOC1 +	COGEMOC2 +	COGEMOC3'


#Obteniendo indicadores
# ----------------------------
fit <- cfa(modelo, data = base,ordered = TRUE)
summary(fit, fit.measures = TRUE)

# Gr?fico de senderos
# ----------------------------
library(semPlot)
library(semTools)

semPaths(fit, "std", rotation = 2, layout = "tree2", nCharNodes = 0, 
         sizeLat= 14, sizeLat2 = 6, sizeMan = 4.3,
         mar=c(2,6,2,4), curvePivot = TRUE,
         edge.label.cex=1.5,residuals = F)

semPaths(fit, what="std",residuals = T, rotation = 2,nCharNodes = 0,fade=F,sizeMan = 6)


##### para nuevos factores lo que pide el punto 4 del deber


#################################################################################
##     TEMA 4

# Probando supuestos
#Identificando el N° de factores
# ----------------------------
fap=fa.parallel(base,fa="fa",cor="poly")
fap


# Corriendo el modelo
# ----------------------------
factorial5=fa.poly(base, nfactors=5, cor="poly",rotate="varimax")
factorial5


# Gráfica de segmentación de factores es el grafico como se asocia los factores y preg
# ----------------------------
fa.diagram(factorial5, e.size=.05,rsize=4.5)

# Estimación por componentes principales pARA MOSTRAR A QUE GRAFICO PERTENECE  MUESTA LA VARIANZA ACUMULADA 0,747
fit_1<-principal(cor(base),nfactors=5,rotate="varimax" )
fit_1$loadings


# REALIZA EL GRAFICO DE LAS COMPONENTES.DE COMO SON LOS GRAFICOS 
fa.diagram(fit_1, e.size=.05,rsize=4.5)

# NOMBRE DE LOS FACTORES 5 FACTORES
# f1 =  HUMANISMO 
# f2 = MATERIALISMO 
# f3 = COGNITIVO
# f4 = COGNITIVO EMOCIONAL 
# f5 = COGNITIVO EMOCIONAL - HUMANISTA


 #VARIABILIDAD EXPLICADA POR MODELO 

# MODELO CO 3  FACTORES  -->   42%**
# MODELO CO 4  FACTORES -->  45%**
# MODELO CO 5  FACTORES -->  53%**
  

#############################################################################################
##############################################################################################

#################################################################################
##     TEMA 4.2 

#  La matriz de correlacion polycórica

r.poly=polychoric(base)
r.poly
R=r.poly$rho    # La matriz de correlaciones polycórica
R
#guardo en formato excel la matriz coropólica
write.xlsx(R, file = "correlacion_policoricas.xlsx", sheetName="Sheet1", 
                                col.names=TRUE, row.names=TRUE , append=FALSE)

#KMO(Kaiser-Meyer-Olkin)
# ----------------------------
KMO(R)

###########################


#Formando factores
# ============================
#Factor MATERIA
# ----------------------------

#Seleccionamos los fcatores   codigo Geovanny

MATERIA <- base %>% select(c(3, 4, 8, 11, 14, 16, 20, 23, 25))
HUMAN<- base %>% select(c(1	,5	,6	,9,	12,	15,	17,	19,	21))
COGNITIVO <-  base %>% select(c(2,	7,	10,	13,	18,	22))
COGEMOC <-  base %>% select(c(24,	26,	27))


#  Validación Subtest-Test, o Factor-Test                                       #
#------------------------------------------------------------------#
subtest1=rowSums(MATERIA)
subtest2=rowSums(HUMAN)
subtest3=rowSums(COGNITIVO)
subtest4=rowSums(COGEMOC)
subtest=data.frame(subtest1,subtest2,subtest3,subtest4)
Sum.total =rowSums(base)
Subtest.test=cor(subtest,Sum.total)
Subtest.test
dim(subtest)
dim(Subtest.test)



#------------------------------------------------------------------#
#  Validación item-subtest o item-factor                                       #
#------------------------------------------------------------------#
item.subtest1=cor(subtest1,MATERIA)
item.subtest1
item.subtest2=cor(subtest2,HUMAN)
item.subtest2
item.subtest3=cor(subtest3,COGNITIVO)
item.subtest3
item.subtest4=cor(subtest4,COGEMOC)
item.subtest4





#################  corremos el modelo  codigo Geovanny


base2 <- base %>% select(-c(26,	1,	6,	9,	19,	21,	4,	23,	25))
head(base2)
dim(base2)
str(base2)


######################  ANALISIS CON  4 FACTORES NUEVA BASE

# Probando supuestos
#Identificando el N° de factores
# ----------------------------
fap=fa.parallel(base2,fa="fa",cor="poly")
fap


# Corriendo el modelo
# ----------------------------
factorial4b=fa.poly(base2, nfactors=4, cor="poly",rotate="varimax")
factorial4b


# Gráfica de segmentación de factores es el grafico como se asocia los factores y preg
# ----------------------------
fa.diagram(factorial4b, e.size=.05,rsize=4.5)

# Estimación por componentes principales pARA MOSTRAR A QUE GRAFICO PERTENECE  MUESTA LA
fit_1<-principal(cor(base2),nfactors=4,rotate="varimax" )
fit_1$loadings


# REALIZA EL GRAFICO DE LAS COMPONENTES.DE COMO SON LOS GRAFICOS 
fa.diagram(fit_1, e.size=.05,rsize=4.5)


#############  CONFIRMATORIO CON LA NUEVA BASE


modelo2 <- 'HUMAN = ~ HUMAN2 +	 HUMAN5 +	HUMAN6 + HUMAN7 
MATERIA = ~ MATERIA1  +	MATERIA3 + MATERIA4 +	MATERIA5 +	MATERIA6 + 	MATERIA8 
COGNITIVO = ~ COGNITIVO1 +	COGNITIVO2 +	COGNITIVO3 + COGNITIVO4 +	COGNITIVO5 +	COGNITIVO6 
COGEMOC = ~ COGEMOC1 +	COGEMOC2'


#Obteniendo indicadores
# ----------------------------
fit <- cfa(modelo2, data = base2 ,ordered = TRUE)
summary(fit, fit.measures = TRUE)

# Gr?fico de senderos
# ----------------------------
library(semPlot)
library(semTools)

semPaths(fit, "std", rotation = 2, layout = "tree2", nCharNodes = 0, 
         sizeLat= 14, sizeLat2 = 6, sizeMan = 4.3,
         mar=c(2,6,2,4), curvePivot = TRUE,
         edge.label.cex=1.5,residuals = F)

semPaths(fit, what="std",residuals = T, rotation = 2,nCharNodes = 0,fade=F,sizeMan = 6)





