#Paquetes a instalar (requiere uso de internet)
install.packages(c("FactoMineR", "factoextra"))

#instalando paquetes para el analisis cluster
install.packages(c("factoextra","cluster","corrplot","ade4","fpc",
                   "NbClust","ape","dendextend","clustertend","e1071",
                   "clValid","seriation","pvclust", "StatMatch",
                   "aplpack","TeachingDemos","ggplot2"),
                 dependencies = c("Depends", "Suggests"))

#Librerias
library(tidyverse)
library(data.table)
library(corrplot)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(ade4)
library(acp)
library(FactoMineR)
library(factoextra)
library(clustertend)
library(seriation)

#Lectura de dataset
bd <- fread('01bd/Indicadores_ContExo.csv',
            colClasses=list(character=c("ruc","ubigeo_150101","ubigeo_150115","ubigeo_230101",
                                        "ubigeo_211101","ubigeo_150140","act_51313",
                                        "act_52322","act_51906","act_51395","act_18100","condicion",
                                        "dependencia","sin_registros","decla_trab")),stringsAsFactors = TRUE)
#Visualizando nombre de variables
variable.names(bd)


#Seleccionando las variables cuantitativas que ingresaran en el analisis
vnumericas = data.frame(bd[,c(2:3,23:41,43,44)])


#visualizando nombres de columnas
nomnumericas<-colnames(vnumericas)



#Eliminando variable sin_registros 
vnumericas= vnumericas[,-13] 

#Se guardan las variables categoricas
vcategoricas = data.frame(bd[,c(1,4:22,42)])

#Llamando variables categoricas
colnames(vcategoricas) 

### Analisis de Correlacion
library(corrplot)

#Obteniendo correlacion de las variables numericas
M <- cor(vnumericas)
corrplot::corrplot(M, method = "number") 

#Interpretacion
#correlaci?n despreciable: r < |0.1|
#correlaci?n baja: |0.1| < r <= |0.3|
#correlaci?n mediana : |0.3| < r <= |0.5|
#correlaci?n fuerte o alta: r > |0.5|


### Analisis de reduccion de dimensiones: ACP
#############################################

#ejecutando pca para variables cuantitativas
acp<-FactoMineR::PCA(vnumericas, scale.unit = TRUE,  graph = FALSE, ncp = 11)
print(acp)

#obteniendo los eigen values a fin de determinar la cantidad de dimensiones y la varianza acumulada 
eig.val <- factoextra::get_eigenvalue(acp)
print(eig.val)
factoextra::fviz_eig(acp, addlabels = TRUE, ylim = c(0, 20))

#       eigenvalue    variance.percent        cumulative.variance.percent
#Dim.1   2.6970012       12.2590963                    12.25910
#Dim.2   1.9787048        8.9941128                    21.25321
#Dim.3   1.4442360        6.5647091                    27.81792
#Dim.4   1.4071771        6.3962595                    34.21418
#Dim.5   1.2614038        5.7336535                    39.94783
#Dim.6   1.1008119        5.0036904                    44.95152
#Dim.7   1.0766226        4.8937389                    49.84526
#Dim.8   1.0418254        4.7355700                    54.58083
#Dim.9   1.0254724        4.6612380                    59.24207
#Dim.10  1.0041414        4.5642790                    63.80635
#Dim.11  0.9626287        4.3755849                    68.18193 ->Var.Acum escogida
#Dim.12  0.9040663        4.1093923                    72.29132
#Dim.13  0.8894830        4.0431046                    76.33443
#Dim.14  0.8696669        3.9530312                    80.28746
#Dim.15  0.8298446        3.7720210                    84.05948
#Dim.16  0.7885633        3.5843786                    87.64386
#Dim.17  0.7702829        3.5012859                    91.14515
#Dim.18  0.6560538        2.9820627                    94.12721
#Dim.19  0.6279375        2.8542614                    96.98147
#Dim.20  0.3097782        1.4080829                    98.38955
#Dim.21  0.1923602        0.8743646                    99.26392
#Dim.22  0.1619381        0.7360825                   100.00000


#Resultados del PCA por variable
var <-factoextra::get_pca_var(acp)
var

head(var$contrib)
factoextra::fviz_pca_var(acp, col.var = "black")

#Visualizando la calidad de los componentes
#Los valores de cos2 se utilizan para estimar la calidad de la representaci?n.
corrplot(var$cos2, is.corr=FALSE)
factoextra::fviz_cos2(acp, choice = "var", axes = 1:2)
#Un cos2 bajo indica que la variable no est? perfectamente representada por las PC.
#En este caso, la variable est? cerca del centro del c?rculo.


factoextra::fviz_pca_var(acp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE )

#Las variables con valores bajos de cos2 se colorear?n en "blanco"
#Las variables con valores medios de cos2 se colorear?n en "azul"
#Las variables con valores altos de cos2 ser?n coloreadas en rojo.


#editando la notacion cientifica
options(scipen = 999)
round(head(var$contrib, 14),2)

#Visualizando la contribucion de las variables
corrplot(var$contrib, is.corr=FALSE)
factoextra::fviz_contrib(acp, choice = "var", axes = 1, top = 10)


####Analisis Cluster
####################
#Se realiza analisis cluster en base a las dimensiones escogidas.
#Creando nombre de columnas a las dimensiones
namescol<-c("dim1","dim2","dim3","dim4","dim5","dim6","dim7","dim8","dim9","dim10","dim11")

#extrayendo la matrix que contiene las dimensiones
bd_cluster <- as.data.frame(acp[["svd"]][["U"]],colnames(namescol))

#asignando nombre de las columnas a las dimensiones
names(bd_cluster) <- namescol

#Escribiendo archivos
#write.csv(bd_cluster,file = "bd_cluster.csv")

#Tranformacion de Valores 
View(bd_cluster)
Z=scale(bd_cluster,center = T,scale=T)
View(Z)

#Escribiendo archivos a fin de analizarlos en el cluster
bd_cluster2<-cbind(bd,bd_cluster)
write.csv(bd_cluster2,file = "bd_cluster2.csv")

#names(vnumericas)

#Se emplea pyspak para ejecutar el modelo no supervisado KMeans con 3 y 4 agrupaciones

#############CREANDO DATASET PARA CARACTERIZAR INDIVIDUOS##########

df7 <- fread('tt_km3.csv')
df7
prediccion <- fread('features_prediccionkm3.csv') #documento con la etiqueta de clase 

write.csv((cbind(df7,prediccion)),"bd_kmeans3.csv")  # uniendo la bd original y la etiqueta de clase.

#########CARACTERIZANDO EL CLUSTER
library(data.table)
df8 <- data.table::fread('bd_kmeans3.csv')

df10<- as.data.frame(df8)
str(df10)

options(scipen = 999)

#EJECUTAR
library(ggplot2)
for (i in 23:44) {
  
  png(paste('ResultadosKM3//Kmeans3_box', names(df10)[i] , '.png', sep=''),
      width=2000,height=1200,units="px",
      pointsize=12,bg="white",res=300,restoreConsole = TRUE)
  
  print(boxplot(df10[,i]~df10$prediction, main=names(df10[i]), type="l"))
  
  dev.off()
}


#MUESTRA PARA EVALUAR NUMERO DE CLUSTER OPTIMO
##############################################
seed(123)
#24314
#SE TOMA MUESTRA DE 2200 margen de error 2%


dsample<-sample(nrow(df10),2200)
dataset_sample<-df10[dsample,]



dsnum<-dataset_sample[45:55]
#EVALUANDO NUMERO OPTIMO DE CLUSTERS 

set.seed(123)


Z <- scale(dsnum,center = T,scale=T)


  fviz_nbclust(Z,clara, method = "gap_stat") 

#Medida interna especialmente usada para evaluar la Cohesion de los
#clusteres que el algoritmo de agrupamiento general.


factoextra::fviz_nbclust(Z, clara, method = "silhouette") #6
#+  
  #geom_vline(xintercept = 2, linetype = 2)
#

factoextra::fviz_nbclust(Z, kmeans, method = "wss") #+  
  #geom_vline(xintercept = 2, linetype = 2)
#4

#

###############
##############################
#SELECCION DE VARIABLES
##############################

library(Boruta)

df8 <-  data.table::fread('bd_kmeans3.csv')
df10<- as.data.frame(df8)
dsample<-sample(nrow(df10),2200)
dataset_sample<-df10[dsample,]
colnames(dataset_sample)

#Tomando campo de dimensiones 
dsnum<-dataset_sample[,c(23:44,57)]
#dsnum<-dsnum[,-12]

boruta_output <- Boruta(prediction ~ .,data = dsnum,doTrace=2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision 
                                                   %in% c("confirmed","Tentative")])
print(boruta_signif)
plot(boruta_output,cex.axis=.7,las=2,xlab="",main="Importancia de Variables")

importancia <- data.frame(importancia = apply((data.frame(boruta_output$ImpHistory)),2,mean))
head(importancia)
decision <- data.frame(decision = boruta_output$finalDecision)
head(decision)

importancia
# La dimensiones dim4,dim10,dim9,dim11,dim5 de las cuales se extraeran las variables que las representan

bd <- data.table::fread('01bd/Indicadores_ContExo.csv')
colnames(bd)

vnumericas = data.frame(bd[,c(26,28,29,30,31,32,36,39)])
cluster<-vnumericas

#dsample<-sample(nrow(vnumericas),2200)
#dataset_sample<-df10[dsample,]

#dsnum<-dataset_sample[,45:57]
#dsnum<-dsnum[,-12]

Z <- scale(cluster,center = T,scale=T)


#factoextra::fviz_nbclust(Z, cluster::clara, method = "gap_stat") #

factoextra::fviz_nbclust(Z,cluster::clara, method = "silhouette") #2-3

#factoextra::fviz_nbclust(Z,cluster::clara, method = "wss") #+  

#clara_clusters
library(cluster)
library(FactoMineR)
library(factoextra)
set.seed(1984)

Cluster_clara <- clara(Z, k = 3, metric = "manhattan", stand = FALSE,samples = 50, pamLike = T)



fviz_cluster(object = Cluster_clara, ellipse.type = "t", geom = "point",
             pointsize = 2.5) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

### silueta
fviz_silhouette(sil.obj = Cluster_clara, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 

# Media silhouette por cluster
Cluster_clara$silinfo$clus.avg.widths
# Coeficiente silhouette para cada observaci?n
head(Cluster_clara$silinfo$widths)

#se

BDCluster <- cbind(bd,Cluster_clara$clustering)

#Escriendo
write.csv(BDCluster,file = "bd_clara3.csv")




