###################  Creacion de la base 1984 #####################

#ENIGH 84
library(foreign)
library(survey)
library(doBy)
library(reldist)
library(tidyverse)
library("writexl")
options(survey.lonely.psu="adjust")

#reading the data
setwd("C:/Users/Erick/OneDrive/GIC/ENIGH_1984")
Conc<-read.dbf("CONCE84.dbf",as.is = T)

Hogares<-read.dbf("hogares.dbf")

Conc<-merge(Conc,Hogares,by="FOLIO")

Conc<-Conc%>%
  mutate(INGCOR=INGCOR/N_OCUP)

Conc<-Conc%>%
  filter(TRABAJO>0)



#apparently this is a "flag", IDK what is this shit yet
Conc$Nhog <- 1

########################################## DECILES 

#Attaching the data frame
attach(Conc) #this is for not writing the name of the data frame and the $ ever time

#Sort Conc according to ing_cor, folioviv, foliohog
Conc<- orderBy (~+INGCOR+FOLIO, data=Conc) #this give us the households sorted by total income

#Adding the values of the expansion factor. The sum is 34 million, which is the number of households in the country.
tot_hogares<-sum(Conc$FACTOR,to.data.frame=TRUE)

#Dividing the number of households into 10 without decimals
tam_dec<-trunc(tot_hogares/100) #the result is 3.5 million housholds per decile

#Adding a variable to the data frame with this value
Conc$tam_dec<-tam_dec

############################# Creating Deciles of Income 

Conc$MAXT<-Conc$INGCOR #vamos a crear en esta base la variable MAXT que es una copia de la columna de ingresos

Conc<-Conc[with(Conc, order(rank(MAXT))),]  #lo que hicimos aqu? fue reordenar la base con respecto a MAXT. Cosa que en realidad, ya estaba.

Conc$ACUMULA<-cumsum(Conc$FACTOR) #aqu? creamos una variable de suma acumulada del factor de viviendas.



############################Ahora viene la creaci?n de los deciles

#no se que es esto de a1 y b1. Pero s? e simportante. Los resultados cmabian por poquito si no lo haces 
for(i in 1:99)
{
  a1<-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1,]$FACTOR
  Conc<-rbind(Conc[1:(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),],
              Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1):dim(Conc[1])[1],])
  b1<-tam_dec*i-Conc[dim(Conc[Conc$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+1),]$FACTOR<-b1
  Conc[(dim(Conc[Conc$ACUMULA<tam_dec*i,])[1]+2),]$FACTOR<-(a1-b1)
}

#aqu? estamos creando otra variable de suma acumulada del n?mero de hogares
Conc$ACUMULA2<-cumsum(Conc$FACTOR)

#aqu? estamos creando una variable que se llama decil que solo tiene ceros
Conc$DECIL<-0

#recordemos que el tama?o de cada decil es de 3,474,481. 
#loq ue hicimos aqu? es pedirle que ponga un uno a los primeros hogares menores al tama?o de decil. 
#es decir, que el primer decil tiene ya UNOS
Conc[(Conc$ACUMULA2<=tam_dec),]$DECIL<-1

#para una sucesi?n del 1 al 9, cuando la variable acumulado2 sea mayor que el tama?o de decil multiplicado por
#1, 2, 3... pone en la variable decil el n?mero i+1
for(i in 1:99)
{
  Conc[((Conc$ACUMULA2>tam_dec*i)&(Conc$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}

# a lo que le qued? cero (que es la ?ltima observaci?n), ponle el decil 10
Conc[Conc$DECIL%in%"0",]$DECIL<-100



# TOTAL HOGARES
x<-tapply(Conc$FACTOR,Conc$Nhog,sum)
# DECILES
y<-tapply(Conc$FACTOR,Conc$DECIL,sum)
# se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
ing_cormed_t<-tapply(Conc$FACTOR*Conc$INGCOR,Conc$Nhog,sum)/x
ing_cormed_d<-tapply(Conc$FACTOR*Conc$INGCOR,Conc$DECIL,sum)/y
########################## C U A D R O S 
# guardamos los resultados en un data frame
prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))
# agregamos el nombre a las filas
Numdec<-c("Mean","1","2","3","4","5","6","7","8","9",
          "10","11","12","13","14","15","16","17","18","19",
          "20","21","22","23","24","25","26","27","28","29",
          "30","31","32","33","34","35","36","37","38","39",
          "40","41","42","43","44","45","46","47","48","49",
          "50","51","52","53","54","55","56","57","58","59",
          "60","61","62","63","64","65","66","67","68","69",
          "70","71","72","73","74","75","76","77","78","79",
          "80","81","82","83","84","85","86","87","88","89",
          "90","91","92","93","94","95","96","97","98","99","100")

row.names(prom_rub)<-Numdec

# GINI Nacional (sobre los 10 deciles) por hogar usando el promedio del ingreso corriente (ingcor)
percentiles_hog_ingcor <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x),
                                 ingreso=c(ing_cormed_d[1],ing_cormed_d[2],ing_cormed_d[3],ing_cormed_d[4],ing_cormed_d[5],
                                           ing_cormed_d[6],ing_cormed_d[7],ing_cormed_d[8],ing_cormed_d[9],ing_cormed_d[10],
                                           ing_cormed_d[11],ing_cormed_d[12],ing_cormed_d[13],ing_cormed_d[14],ing_cormed_d[15],
                                           ing_cormed_d[16],ing_cormed_d[17],ing_cormed_d[18],ing_cormed_d[19],ing_cormed_d[20],
                                           ing_cormed_d[21],ing_cormed_d[22],ing_cormed_d[23],ing_cormed_d[24],ing_cormed_d[25],
                                           ing_cormed_d[26],ing_cormed_d[27],ing_cormed_d[28],ing_cormed_d[29],ing_cormed_d[30],
                                           ing_cormed_d[31],ing_cormed_d[32],ing_cormed_d[33],ing_cormed_d[34],ing_cormed_d[35],
                                           ing_cormed_d[36],ing_cormed_d[37],ing_cormed_d[38],ing_cormed_d[39],ing_cormed_d[40],
                                           ing_cormed_d[41],ing_cormed_d[42],ing_cormed_d[43],ing_cormed_d[44],ing_cormed_d[45],
                                           ing_cormed_d[46],ing_cormed_d[47],ing_cormed_d[48],ing_cormed_d[49],ing_cormed_d[50],
                                           ing_cormed_d[51],ing_cormed_d[52],ing_cormed_d[53],ing_cormed_d[55],ing_cormed_d[55],
                                           ing_cormed_d[56],ing_cormed_d[57],ing_cormed_d[58],ing_cormed_d[59],ing_cormed_d[60],
                                           ing_cormed_d[61],ing_cormed_d[62],ing_cormed_d[63],ing_cormed_d[64],ing_cormed_d[65],
                                           ing_cormed_d[66],ing_cormed_d[67],ing_cormed_d[68],ing_cormed_d[69],ing_cormed_d[70],
                                           ing_cormed_d[77],ing_cormed_d[72],ing_cormed_d[73],ing_cormed_d[74],ing_cormed_d[75],
                                           ing_cormed_d[76],ing_cormed_d[77],ing_cormed_d[78],ing_cormed_d[79],ing_cormed_d[80],
                                           ing_cormed_d[81],ing_cormed_d[82],ing_cormed_d[83],ing_cormed_d[84],ing_cormed_d[85],
                                           ing_cormed_d[86],ing_cormed_d[87],ing_cormed_d[88],ing_cormed_d[89],ing_cormed_d[90],
                                           ing_cormed_d[91],ing_cormed_d[92],ing_cormed_d[93],ing_cormed_d[94],ing_cormed_d[95],
                                           ing_cormed_d[96],ing_cormed_d[97],ing_cormed_d[98],ing_cormed_d[99],ing_cormed_d[100]))
# se efectua la función Gini y se guarda en nuestro vector a.
a<-gini(percentiles_hog_ingcor$ingreso,weights=percentiles_hog_ingcor$hogares)
# se renombran las variables (columnas)
names(prom_rub)=c("INGRESO CORRIENTE")
names(a)="GINI"
##### Mostramos el resultado en pantalla ound(prom_rub)
round(a,5)



write.dbf(Conc,file="Conc84.dbf")

write_xlsx(prom_rub,"C:/Users/Erick/OneDrive/GIC/ENIGH_1984/percentiles_84.xlsx")

rm(list=ls())
