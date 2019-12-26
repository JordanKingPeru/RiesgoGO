# PROYECTO SUBVALUACIÓN ADUANAS - ANALYTICS 2
#-----sEGMENTACIÓN------
# Carga de librerias necesarias 
rm(list=ls())

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

# Lectura de input para segmentación 
input_seg <- fread("input_cluster.csv")
input_seg <- as.data.frame(input_seg)

# Creamos copia para hacer cambios de escala
aux <- input_seg[,-ncol(input_seg)]

# centralizamos datos 
aux$compras_prom <- log(aux$compras_prom+1)
aux$ventas_prom <- log(aux$ventas_prom+1)
aux$fob_prom <- log(aux$fob_prom+1)
aux$adv_prom <- log(aux$adv_prom+1)

# llevamos todo a una misma escala
maximo <- apply(aux, 2, max)
minimo <- apply(aux, 2, min)
aux_esc <- scale(aux,center = minimo, scale = maximo-minimo)

#ejemplo<-fread("data_ejemplo.csv")
#ejemplo<-as.data.frame(ejemplo)
#aux_ejemplo<-ejemplo[,-1]
#aux_ejemplo$EDAD<-log(aux_ejemplo$EDAD+1)
#aux_ejemplo$SALARIO<-log(aux_ejemplo$SALARIO+1)
#maximo<-apply(aux_ejemplo, 2, max)
#minimo<-apply(aux_ejemplo, 2, min)
#aux_esc <- scale(aux_ejemplo,center = minimo, scale = maximo-minimo)

# Plantamos semillas para reproducibilidad
set.seed(666)
nclust <- 5
model <- kmeans(aux_esc[,-1], nclust) ####Consultar Lucho
summary(model)
model$size
dscm <- melt(model$centers) ###Promedio para cada cluster y variable
aux3iliar <- (model$size)
maximo=max(aux3iliar)
minimo=min(aux3iliar)
aux3iliar= scale(aux3iliar, minimo, maximo-minimo)
aux3iliar <- data.frame(aux3iliar)
aux3iliar$v1 <- row.names(aux3iliar)
aux3iliar$v2 <- "tamaño"
aux3iliar <- aux3iliar[,c(2,3,1)]
names(aux3iliar) <- c("Cluster", "Variable", "Value")
names(dscm) <- c("Cluster", "Variable", "Value")
dscm <- rbind(dscm, aux3iliar)
dscm$Cluster <- factor(dscm$Cluster)
str(dscm)
dscm$Order <- as.vector(sapply(1:(ncol(aux_esc[,-1])+1), rep, nclust))
p <- ggplot(dscm,
            aes(x=reorder(Variable, Order),
                y=Value, group=Cluster, colour=Cluster))
p <- p + coord_polar()
p <- p + geom_point()
p <- p + geom_path()
p <- p + labs(x=NULL, y=NULL)
p <- p + ylim(-1, 1)
p <- p + ggtitle("Clusters profile (variables were scaled)")
p

input_seg$cluster <- model$cluster
resumen <- aggregate(.~cluster, input_seg[,-which(names(input_seg) %in% "ruc")], mean)
resumen <- round(resumen, 2)
resumen$size <- model$size
resumen

output_seg <- input_seg[,c("ruc", "cluster")]

write.csv(output_seg, "output_seg.csv", row.names = F)
write.csv(resumen, "resumen_segmentos.csv", row.names = F)

#-----MODELO DE POSIBLE SUBVALUADORES-----
rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(caret)
library(ROSE)

input_mod <- fread("input_modelar.csv", stringsAsFactors = T)
input_mod <- as.data.frame(input_mod)
input_mod$condicion <- as.factor(input_mod$condicion)

# bosque <- randomForest(condicion ~ ., data=df_modelar[,-1])
# variables_bosque <- as.data.frame(varImp(bosque))
# variables_bosque$var <- row.names(variables_bosque)
# variables_bosque <- arrange(variables_bosque, -Overall)
# varImpPlot(bosque)
# variables_final <- variables_bosque[1:30,2]

variables_final <- c("fec_ini_act", "nac_en_prom", "ratio_igvcred_igvdeb_prom", 
                     "Itn_pc2015_cat", "Itn_pc2017_cat", "tr_ne_prom", "I_M_ventas", 
                     "rango_med_min", "tipoempresa_tipo_cod", "automotriz", 
                     "prestamo", "dependencia", "I_M_compras", "Itn_pc2016_cat", 
                     "I_IGVAdu_IGDDeb2015", "ventas_Prom", "compras_Prom", "hipotecario",  
                     "cluster", "mto_total_prom", "decla_trab", "regimen_2018", "segmento", 
                     "Normal")

aux <- input_mod[, c("ruc", variables_final, "condicion")]

df_modelar <- aux %>% filter(condicion == 1 | condicion == 0)
df_evaluar <- aux %>% filter(is.na(condicion))


# Bosque Final 
# Cluster 1
df1 <- df_modelar %>% filter(cluster == 1)
df1_ev <- df_evaluar %>% filter(cluster == 1)

train <- df1
train <- ovun.sample(condicion~., data = train, method = "over")$data

bosque1 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold", "cluster"))], ntree=10, mtry= 5)
varImpPlot(bosque1)
variables1 <- as.data.frame(varImp(bosque1))
variables1 <- cbind(variables1, var = row.names(variables1))
variables1 <- arrange(variables1, -Overall)
variables1 <- as.character(variables1[1:5,2])

df1 <- df1[,c(variables1, "condicion")]

df1 <- ovun.sample(condicion~., data = df1, method = "over")$data

bosque1 <- randomForest(condicion ~ ., data=df1, ntree=200, mtry= 3)
bosque1_pred1 <- predict(bosque1, df1_ev, type = "response") # Predicción sobre test
bosque1_pred2 <- as.data.frame(predict(bosque1, df1_ev, type = "prob")) # Predicción sobre test
names(bosque1_pred2) <- c("No_Sub", "Sub")

df1_ev$condicion <- bosque1_pred1
df1_ev <- cbind(df1_ev, bosque1_pred2)

df1_ev$condicion2 <- ifelse(df1_ev$Sub>=0.8,1,0)
table(df1_ev$condicion2)

# Cluster 2
df2 <- df_modelar %>% filter(cluster == 2)
df2_ev <- df_evaluar %>% filter(cluster == 2)

train <- df2
train <- ovun.sample(condicion~., data = train, method = "over")$data

bosque2 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold", "cluster"))], ntree=10, mtry= 5)
varImpPlot(bosque2)
variables2 <- as.data.frame(varImp(bosque2))
variables2 <- cbind(variables2, var = row.names(variables2))
variables2 <- arrange(variables2, -Overall)
variables2 <- as.character(variables2[1:5,2])

df2 <- df2[,c(variables2, "condicion")]

df2 <- ovun.sample(condicion~., data = df2, method = "over")$data

bosque2 <- randomForest(condicion ~ ., data=df2, ntree=200, mtry= 3)
bosque_pred1 <- predict(bosque2, df2_ev, type = "response") # Predicción sobre test
bosque_pred2 <- as.data.frame(predict(bosque2, df2_ev, type = "prob")) # Predicción sobre test
names(bosque_pred2) <- c("No_Sub", "Sub")

df2_ev$condicion <- bosque_pred1
df2_ev <- cbind(df2_ev, bosque_pred2)

df2_ev$condicion2 <- ifelse(df2_ev$Sub>=0.8,1,0)
table(df2_ev$condicion2)



# Cluster 3
df3 <- df_modelar %>% filter(cluster == 3)
df3_ev <- df_evaluar %>% filter(cluster == 3)

train <- df3
train <- ovun.sample(condicion~., data = train, method = "over")$data

bosque3 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold", "cluster"))], ntree=10, mtry= 5)
varImpPlot(bosque3)
variables3 <- as.data.frame(varImp(bosque3))
variables3 <- cbind(variables3, var = row.names(variables3))
variables3 <- arrange(variables3, -Overall)
variables3 <- as.character(variables3[1:5,2])

df3 <- df3[,c(variables3, "condicion")]

df3 <- ovun.sample(condicion~., data = df3, method = "over")$data

bosque3 <- randomForest(condicion ~ ., data=df3, ntree=200, mtry= 3)
bosque_pred1 <- predict(bosque3, df3_ev, type = "response") # Predicción sobre test
bosque_pred2 <- as.data.frame(predict(bosque3, df3_ev, type = "prob")) # Predicción sobre test
names(bosque_pred2) <- c("No_Sub", "Sub")

df3_ev$condicion <- bosque_pred1
df3_ev <- cbind(df3_ev, bosque_pred2)

df3_ev$condicion2 <- ifelse(df3_ev$Sub>=0.8,1,0)
table(df3_ev$condicion2)

# Cluster 4
df4 <- df_modelar %>% filter(cluster == 4)
df4_ev <- df_evaluar %>% filter(cluster == 4)

train <- df4
train <- ovun.sample(condicion~., data = train, method = "over")$data

bosque4 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold", "cluster"))], ntree=10, mtry= 5)
varImpPlot(bosque4)
variables4 <- as.data.frame(varImp(bosque4))
variables4 <- cbind(variables4, var = row.names(variables4))
variables4 <- arrange(variables4, -Overall)
variables4 <- as.character(variables4[1:5,2])

df4 <- df4[,c(variables4, "condicion")]

df4 <- ovun.sample(condicion~., data = df4, method = "over")$data

bosque4 <- randomForest(condicion ~ ., data=df4, ntree=200, mtry= 3)
bosque_pred1 <- predict(bosque4, df4_ev, type = "response") # Predicción sobre test
bosque_pred2 <- as.data.frame(predict(bosque4, df4_ev, type = "prob")) # Predicción sobre test
names(bosque_pred2) <- c("No_Sub", "Sub")

df4_ev$condicion <- bosque_pred1
df4_ev <- cbind(df4_ev, bosque_pred2)

df4_ev$condicion2 <- ifelse(df4_ev$Sub>=0.8,1,0)
table(df4_ev$condicion2)


# Cluster 5
df5 <- df_modelar %>% filter(cluster == 5)
df5_ev <- df_evaluar %>% filter(cluster == 5)

train <- df5
train <- ovun.sample(condicion~., data = train, method = "over")$data

bosque5 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold", "cluster"))], ntree=10, mtry= 5)
varImpPlot(bosque5)
variables5 <- as.data.frame(varImp(bosque5))
variables5 <- cbind(variables5, var = row.names(variables5))
variables5 <- arrange(variables5, -Overall)
variables5 <- as.character(variables5[1:5,2])

df5 <- df5[,c(variables5, "condicion")]

df5 <- ovun.sample(condicion~., data = df5, method = "over")$data

bosque5 <- randomForest(condicion ~ ., data=df5, ntree=200, mtry= 3)
bosque_pred1 <- predict(bosque5, df5_ev, type = "response") # Predicción sobre test
bosque_pred2 <- as.data.frame(predict(bosque5, df5_ev, type = "prob")) # Predicción sobre test
names(bosque_pred2) <- c("No_Sub", "Sub")

df5_ev$condicion <- bosque_pred1
df5_ev <- cbind(df5_ev, bosque_pred2)

df5_ev$condicion2 <- ifelse(df5_ev$Sub>=0.8,1,0)
table(df5_ev$condicion2)

# Consolidamos los predict
df_evaluar <- rbind(df1_ev, df2_ev, df3_ev, df4_ev, df5_ev)

# df_evaluar$condicion <- df_evaluar$condicion2
importadores <- rbind(df_modelar, df_evaluar[,-c(27,28,29)])
importadores <- importadores[, c("ruc", "condicion")]
write.csv(importadores, "Importadores_con_probSUb.csv", row.names = F)

variables <- rbind(cbind(variables1,1), cbind(variables2,2), cbind(variables3,3), cbind(variables4,4), cbind(variables5,5))
write.csv(variables, "import_var_clus.csv", row.names = F)


