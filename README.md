# Caqueta-cameratrap
Cameratraping from Field Museum RI-30 in Bajo Caguan with WCS and UniAmazonia

##########
##R code##
##########

---
title: "proyecto caqueta"
author: "Angela Moreras"
date: "June 29, 2018"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Proyecto de fototrampeo Caqueta

Los datos solo registran las ocaciones en las que se captura el inviduo con la camara trampa. 
Por lo tanto es necesario construir la matriz de presencias y ausencias asociadas a cada camara.
Independencia de datos usando 24h

```{r Covariables}

library(readr)

#Cargar datos 

fotos <- read_delim("C:/Users/Angela Moreras/Documents/caqueta/data/Wild_ID_RI30_C1_C2_C3_C4_Consolidado.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)

View(fotos)
str(fotos)

fotos$camera_trap<-as.factor(fotos$camera_trap) #Mirar cuantas camaras trampa hay. Hay 35


### Crear covariables 
### Covariables van a ser distancia al borde de el resguardo y si estan o no en el resguardo

# Importar el mapa en formato sharp image
library(sf)
resguardo<-st_read("C:/Users/Angela Moreras/Documents/caqueta/data/Resguardos.shp")

## Mirar si hay NAs
#b<-is.na(fotos$Latitude)
#for (i in 1:length(b)){
#if(b[i]==TRUE){print (i)}
#}


coorcam<-st_as_sf(fotos, coords=c("Longitude","Latitude"), na.fail = FALSE)
plot(resguardo["RINOMBRE"], axes=TRUE)
plot(coorcam$geometry, col="red", add=T)


resguardo1<-st_read("C:/Users/Angela Moreras/Documents/caqueta/data/Res.shp")

coorcam<-st_as_sf(fotos, coords=c("Longitude","Latitude"), na.fail = FALSE)
plot(resguardo1["RINOMBRE"], axes=TRUE)
plot(coorcam$geometry, col="red", add=T)

###Para calcular la distancia voy a utilizar QGIS
#Crear un cvs para utilizarlo

write.csv(fotos, file="fotos.csv",dec = ".", row.names = TRUE,
                 col.names = TRUE)

###Importante hacer registro unico por que me aprecen todas las veces que lo utilizo
###Se eliminan las camaras que no tienen ubicacion. 

camera_trap1<-data.frame(unique(fotos$camera_trap))
camera_trap1<-camera_trap1[-c(35,30,28,31,32),1]
camera_trap1<-data.frame(camera_trap1)

# Crear la matriz una sola camara con dimensiones

matcam<-matrix(0,dim(camera_trap1)[1],3)


for (i in 1:dim(camera_trap1)[1]){ 
 
  a<-data.frame(subset(fotos, camera_trap== camera_trap1[i,1]))
a$camera_trap<-as.character(a$camera_trap)
matcam[i,1]<- a[i,3]
matcam[i,2]<- a[i,4]
matcam [i,3]<- a[i,5]}

matcam<-data.frame(matcam)
colnames(matcam)<-c("camera_trap","latutide","longitude")

# cambiarlo a cvs

write.csv(matcam, file="matcam1.csv",dec = ".", row.names = TRUE,
                 col.names = TRUE)

# Utilizar herramientas para mapas

library(raster)
library(rgdal)
library(maptools)
library(sp)
 

all.pa<-readOGR("C:/Users/Angela Moreras/Documents/caqueta/data/Resguardos.shp")
plot (all.pa)
plot(coorcam$geometry, col="red", add=T)

```

```{r Matriz}

#Crear una matriz con los NA
#Columna numero 1 camara trampa

camera_trap1<-data.frame(unique(fotos$camera_trap))
camera_trap1<-camera_trap1[-c(35,30,28,31,32),1]
camera_trap1<-data.frame(camera_trap1)

diascam<-data.frame(matrix(0, dim(camera_trap1)[1], 2), row.names=as.character(camera_trap1[,1])) #Crea una matrz con 35 filas y una columna

#Loop para crear columna de dia de comienzo de muestreo para cada camara

for (i in 1:dim(camera_trap1)[1]){ 
 
  a<-subset(fotos, camera_trap== camera_trap1[i,1])
  a$camera_trap_start_date<-as.character.Date(a$camera_trap_start_date)

diascam[i,1]<- a[1,19]}

#Loop para crear columna del dia final de muestreo para cada camara

for (i in 1:dim(camera_trap1)[1]){ 
 as
  a<-subset(fotos, camera_trap== camera_trap1[i,1])
  a$camera_trap_end_date<-as.character.Date(a$camera_trap_end_date)

diascam[i,2]<- a[1,20]}

#Poner nombres a las columnas

colnames(diascam)<-c("camera_trap_start_date","camera_trap_end_date")

#Establecer todo como dias

diascam$camera_trap_start_date<-as.Date(diascam$camera_trap_start_date)
diascam$camera_trap_end_date<-as.Date(diascam$camera_trap_end_date)


#Crear un vector con el numero de dias totales de muestro

dias<-seq.Date(min(fotos$camera_trap_start_date),max(fotos$camera_trap_end_date),1)

#FUNCION PARA ARMAR TABLA DE PRESENCIA/AUSENCIA
#Si entre el dia que comenza y el dia que termina cada camara no esta en dias Asiganar NA

#AsignarNA<-function(x, muestreo, comienzo, final) {
 

## Crear la matriz con las dimensiones adecuadas

mat<-matrix(0,dim(camera_trap1)[1],length(dias))
mat<-data.frame(mat, row.names=as.character(camera_trap1[,1])) 
colnames(mat)<-dias
  
# Loop  para crear los NA antes de comenzar el registro de cada camara

for (i in 1: dim(diascam)[1]) {
  secuen<-seq.Date(diascam$camera_trap_start_date[i],diascam$camera_trap_end_date[i],1) 

  x<-dias
  j<-1
    while (secuen [1]!= dias[j])
    {x[j]<- NA
      j=j+1}
  
#Loop para crear los NA despues del ultimo dia que estuvo activa la camara
  
  j=j+length(secuen)
  y=j
 
    for (y in j:length(x))
    {x [j]<- NA
      j=j+1}
  
 
  a<-as.Date(t(x))
  mat[i,] <-as.character.Date(a)

} 

 #Aparece un error ya que la camara 33 se utilizo todos los dias.

print (mat)


```
### Matriz


```{r Por especie (Pacca)}

#Hace que los NA sean un caracter

matpacca<- replace(mat, is.na(mat), "NA")


# Separar los datos de cada especie

fotos$Species<-as.factor(fotos$Species)
fotos$Genus<-as.factor(fotos$Genus)

Pacca<- data.frame(subset(fotos, Genus=="Cuniculus"))  
Pacca$Photo_Date<-as.character(Pacca$Photo_Date)



#loop para crear los 1 y 0 para pacca 

for (i in 1:dim(matpacca)[1]) {
  for (j in 1:dim(Pacca)[1]){
  while (Pacca$camera_trap[j]==row.names(matpacca)[i])
  {for (k in 1:dim(matpacca)[2])
    {if (matpacca[i,k]=="NA"){
      matpacca[i,k]="NA"}
    else if (matpacca[i,k]==Pacca$Photo_Date[j]){
      matpacca[i,k]=1}
    else {
      matpacca[i,k]=matpacca[i,k]}
    }
  j=j+1
  if (j==348){
    j=1
  }
  }
  }
}

for (i in 1:dim(matpacca)[1]) {
  for (k in 1:dim(matpacca)[2])
    {if (matpacca[i,k]=="NA"){
      matpacca[i,k]= NA}
    else if (matpacca[i,k]==1){
      matpacca[i,k]=1}
    else {
      matpacca[i,k]=0}
    }
  }
print (matpacca)
````


````{r crear el modelo para pacca}

#Cambiar datos a numericos
mat2<-data.frame(apply(matpacca,2,as.numeric))


#Cargar covariables

covariables<-read.csv("C:/Users/Angela Moreras/Documents/caqueta/data/distancia_camaras.csv")
str(covariables)
covariables$distancia<-scale(covariables$distancia)

#Crear el objeto

library(unmarked)
pacca_un <- unmarkedFrameOccu(y =as.matrix(mat2))
plot(pacca_un)


siteCovs(pacca_un)<-covariables # esta linea le pega las covariables a la matriz unmarked

####chequear el orden de las camaras de la matriz igual a las covariables
###los modelos tienen el orden detectabilidad y despues ocupacion

#modelos 

  fm0<-occu(~1~1,pacca_un) #este es el modelo nulo
  fm1<-occu(~1~Refugio,pacca_un) 
  fm2<-occu(~1~distancia,pacca_un)
  
  fm3<-occu(~1~Refugio+distancia,pacca_un)
  fm4<-occu(~1~Refugio:distancia,pacca_un)
  
  modelos<-fitList(
    "p(.) occu(.)"=fm0,
    "p(.) occu(Ref)"=fm1,
    "p(.) occu(Dis)"=fm2,
    "p(.) occu(Ref+Dis)"=fm3,
    "p(.) occu(Ref:Dis)"=fm4)
  
  
  modSel(modelos)
  ##el mejor mmodelo es el  nulo pero por poco podriamos utilizar el de Refugio
  
  
  summary(fm1)
  
  #goodness of fit
  
pb <- parboot(fm1, nsim=500, report=100)
  plot (pb)
````  
````{r crear matriz agouti} 

#Hace que los NA sean un caracter

matagouti<- replace(mat, is.na(mat), "NA")

#Crear subset con la especie

Agouti<-data.frame(subset(fotos, Genus=="Dasyprocta"))
Agouti$Photo_Date<-as.character(Agouti$Photo_Date)

#loop para crear los 1 y 0

for (i in 1:dim(matagouti)[1]) {
  for (j in 1:dim(Agouti)[1]){
  while (Agouti$camera_trap[j]==row.names(matagouti)[i])
  {for (k in 1:dim(matagouti)[2])
    {if (matagouti[i,k]=="NA"){
      matagouti[i,k]="NA"}
    else if (matagouti[i,k]==Agouti$Photo_Date[j]){
      matagouti[i,k]=1}
    else {
      matagouti[i,k]=matagouti[i,k]}
    }
  j=j+1
  if (j==dim(Agouti)[1]+1){
    j=1
  }
  }
  }
}

for (i in 1:dim(matagouti)[1]) {
  for (k in 1:dim(matagouti)[2])
    {if (matagouti[i,k]=="NA"){
      matagouti[i,k]= NA}
    else if (matagouti[i,k]==1){
      matagouti[i,k]=1}
    else {
      matagouti[i,k]=0}
    }
  }
print (matagouti)

````




````{r crear el modelo para agouti}

#Cambiar datos a numericos
mat3<-data.frame(apply(matagouti,2,as.numeric))


#Cargar covariables

covariables<-read.csv("C:/Users/Angela Moreras/Documents/caqueta/data/distancia_camaras.csv")
str(covariables)
covariables$distancia<-scale(covariables$distancia)

#Crear el objeto

library(unmarked)
ago_un <- unmarkedFrameOccu(y =as.matrix(mat3))
plot(ago_un)


siteCovs(ago_un)<-covariables # esta linea le pega las covariables a la matriz unmarked

####chequear el orden de las camaras de la matriz igual a las covariables
###los modelos tienen el orden detectabilidad y despues ocupacion

#modelos 

  fm0a<-occu(~1~1,ago_un) #este es el modelo nulo
  fm1a<-occu(~1~Refugio,ago_un) 
  fm2a<-occu(~1~distancia,ago_un)
  
  fm3a<-occu(~1~Refugio+distancia,ago_un)
  fm4a<-occu(~1~Refugio:distancia,ago_un)
  
  modelos<-fitList(
    "p(.) occu(.)"=fm0a,
    "p(.) occu(Ref)"=fm1a,
    "p(.) occu(Dis)"=fm2a,
    "p(.) occu(Ref+Dis)"=fm3a,
    "p(.) occu(Ref:Dis)"=fm4a)
  
  
  modSel(modelos)
  ##el mejor mmodelo es el  nulo pero por poco podriamos utilizar el de Refugio
  
  
  summary(fm2a)
  
  #goodness of fit
  
pba <- parboot(fm2a, nsim=500, report=100)
  plot (pba)
````  
