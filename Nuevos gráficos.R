library(tidyverse)
library(data.table)
library(corrplot)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(ade4)
library(stringi)


#Lectura de dataset
bd <- fread('01bd/Indicadores_ContExo.csv',
            colClasses=list(character=c("ruc","ubigeo_150101","ubigeo_150115","ubigeo_230101",
                                        "ubigeo_211101","ubigeo_150140","act_51313",
                                        "act_52322","act_51906","act_51395","act_18100","condicion",
                                        "dependencia","sin_registros","decla_trab")),stringsAsFactors = TRUE)


vcategoricas = data.frame(bd[,c(1,4:22,42)])
vcategoricas$cluster = factor(vcategoricas$cluster)

for (i in 3:length(names(vcategoricas))) {
  
  png(paste('Resultados3/myplot_', names(vcategoricas)[i] , '.png', sep=''),
      width=1200,height=820,units="px",
      pointsize=12,bg="white",res=300,restoreConsole = TRUE)
  
  print(ggplot(vcategoricas,aes(x=cluster))+
          geom_bar(aes(fill = vcategoricas[,i] ), position = "dodge") +
          coord_flip() +
          theme(legend.position = "top")+
          labs(title=names(vcategoricas)[i]))
  
  
  dev.off()
}


######################################################################################
bd_aduana <- fread('01bd/Indicadores_FOB.csv')
bd_aduana <- fread('01bd/Indicadores_FOB.csv',
                   colClasses = list(character=c("a.libr_tribu","a.codi_aduan","a.ano_prese","a.nume_corre","a.codi_agent","a.pais_adqui")),
                   stringsAsFactors = TRUE)

head(bd_aduana)
str(bd_aduana)
names(bd_aduana)

###################################################################
#Análisis descriptivo, creación de indicadores
###################################################################

nombre_variables <- names(bd_aduana)
nombre_variables <- stri_sub(nombre_variables,from = 3,length = 50)
names(bd_aduana) <- nombre_variables

#cantidad de registros
bd_aduana %>% summarise(n())

#RUCs únicos
bd_aduana %>% summarise(n_distinct(libr_tribu))

#Ruc únicos y registros, por año
bd_aduana %>% group_by(ano_prese) %>% summarise(n_distinct(libr_tribu),n())

#Incluir el FOB a la consulta anterior
bd_aduana %>% group_by(ano_prese) %>% summarise(n_distinct(libr_tribu),n(),sum(vfob_factu))

#Incluir una división entre la suma del fob respecto a la cantidad de registros (facturas)
bd_aduana %>% 
  group_by(ano_prese) %>% 
  summarise(Contribuyentes=n_distinct(libr_tribu),facturas_aduanas = n(),total_FOB=sum(vfob_factu)) %>% 
  mutate(ind1_FOBxFACTURA = total_FOB / facturas_aduanas)

#código de identificación de las DAM: libr_tribu,codi_aduan,ano_prese,nume_corre
bd_aduana %>% 
  group_by(libr_tribu,codi_aduan,ano_prese,nume_corre) %>% 
  summarise(n()) %>% 
  group_by() %>% 
  summarise(n())
  
#Cuántas importaciones se realizaron por año
bd_aduana %>% 
  group_by(libr_tribu,codi_aduan,ano_prese,nume_corre) %>% 
  summarise(n(),total_FOB=sum(vfob_factu)) %>% 
  group_by(ano_prese) %>% 
  summarise(n=n(),total_FOB = sum(total_FOB)) %>% 
  mutate(total_FOB/n)

#Filtrar sólo el año 2017
bd_aduana %>% 
  group_by(libr_tribu,codi_aduan,ano_prese,nume_corre) %>% 
  summarise(n(),total_FOB=sum(vfob_factu)) %>% 
  group_by(ano_prese) %>% 
  summarise(n=n(),total_FOB = sum(total_FOB)) %>% 
  mutate(total_FOB/n) %>% 
  filter(ano_prese==17)

#Incluir el RUC
bd_aduana %>% 
  group_by(libr_tribu,codi_aduan,ano_prese,nume_corre) %>% 
  filter(ano_prese==17) %>% 
  summarise(n(),total_FOB=sum(vfob_factu)) %>% 
  group_by(libr_tribu) %>% 
  summarise(n=n(),total_FOB = sum(total_FOB)) %>% 
  mutate(ind2_FOBxIMPORTACION=total_FOB/n)
  
#Guardar los indicadores en diferentes variables
ruc_ind1 <- bd_aduana %>% 
  group_by(libr_tribu,codi_aduan,ano_prese,nume_corre) %>% 
  filter(ano_prese==17) %>% 
  summarise(n_facturas=n(),total_FOB=sum(vfob_factu)) %>% 
  group_by(libr_tribu) %>% 
  summarise(n=n(),total_FOB = sum(total_FOB),facturas_aduanas=sum(n_facturas)) %>% 
  mutate(ind1_FOBxFACTURA = total_FOB / facturas_aduanas,ind2_FOBxIMPORTACION=total_FOB/n)

ruc_ind1

##############################################################################
# Inner Join
##############################################################################

bd_nuevos_indicadores<- inner_join(ruc_ind1,bd,by=c("libr_tribu"="ruc"))

bd_cluster3 <- fread("01bd/Cluster3.csv",colClasses = list(character=1))

bd_nuevos_indicadores <- inner_join(bd_nuevos_indicadores,bd_cluster3,by=c("libr_tribu"="ruc"))
##############################################################################
# Gráficos
##############################################################################


bd_nuevos_indicadores$Cluster3 <- factor(bd_nuevos_indicadores$Cluster3)


ggplot(bd_nuevos_indicadores,aes(x=Cluster3,y=n))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(name = "Mean ozone in\nparts per billion",
                     breaks = seq(0, quantile(bd_nuevos_indicadores$n,0.80), 1),
                     limits = c(0,quantile(bd_nuevos_indicadores$n,0.80)))

library(tidyverse)


  labs(title='CPE Factura emitidas',x=names(vcategoricas)[2],y="Frecuencia")+
  theme(axis.text.x=element_text(angle=0, size=7, vjust=0.5),
        plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5))

quantile(bd_nuevos_indicadores$ind1_FOBxFACTURA,0.85)

nums <- unlist(lapply(bd_nuevos_indicadores, is.numeric)) #identificar a las columnas numéricas
charac <- unlist(lapply(bd_nuevos_indicadores, is.factor)) #identificar a las columnas numéricas


vnumericas = data.frame(bd_nuevos_indicadores[,nums])
vcategoricas = bd_nuevos_indicadores[ ,charac]


for (i in 1:length(names(vnumericas))) {
  
  png(paste('Resultados_cuanti//myplot_', names(vnumericas)[i] , '.png', sep=''),
      width=1200,height=820,units="px",
      pointsize=12,bg="white",res=300,restoreConsole = TRUE)
  
  
  print(ggplot(bd_nuevos_indicadores,aes(x=Cluster3,y=vnumericas[,i]))+
    geom_boxplot(outlier.shape = NA)+
    scale_y_continuous(name = names(vnumericas)[i],
                       breaks = seq(0, quantile(vnumericas[,i],0.80), quantile(vnumericas[,i],0.80)/10),
                       limits = c(0,quantile(vnumericas[,i],0.80))))
  
  
  #print(ggplot(vnumericas,aes(x=vnumericas[,i]))+
  #        geom_histogram()+
  #        labs(title='CPE Factura emitidas',x=names(vnumericas)[i],y="Frecuencia")+
  #        theme(axis.text.x=element_text(angle=0, size=7, vjust=0.5),
  #              plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5)))
  
  
  dev.off()
}


for (i in 1:length(names(vcategoricas))) {
  
  png(paste('Resultados_cuali//myplot_', names(vcategoricas)[i] , '.png', sep=''),
      width=1200,height=820,units="px",
      pointsize=12,bg="white",res=300,restoreConsole = TRUE)
  
  vcategoricas.graph <- vcategoricas[,c(i,21)]
  columns_vcategorias <- names(vcategoricas.graph)
  names(vcategoricas.graph) <- c("index","Cluster3")
  
  print(ggplot(vcategoricas.graph,aes(Cluster3,fill=index))+
          geom_bar(position = "fill") +
          coord_flip()+
          xlab(columns_vcategorias)+
          theme(axis.text.x=element_text(angle=0, size=7, vjust=0.5),
                plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5)))
  
  #print(ggplot(vcategoricas,aes(x=vcategoricas[,i]))+
  #        geom_bar(stat="identity")+
  #        labs(title='CPE Factura emitidas',x=names(vcategoricas)[i],y="Frecuencia")+
  #        theme(axis.text.x=element_text(angle=0, size=7, vjust=0.5),
  #              plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5)))
  
  
  dev.off()
}

bd_nuevos_indicadores %>% group_by(Cluster3) %>% summarise(n())

ggplot(bd_nuevos_indicadores,aes(adv_prom))+
  geom_histogram()

bd_nuevos_indicadores %>% select(prestamo) %>%  arrange(desc(prestamo))

bd_nuevos_indicadores %>% 
  group_by(Cluster3) %>% 
  summarise(sum(adv_prom),mean(adv_prom))


bd_nuevos_indicadores %>% select(adv_prom) %>%  arrange(desc(adv_prom))

##############################################################################################################
# ONR
##############################################################################################################
library(data.table)
bd1_onr <- fread('01bd/onr.csv',select = c("ruc_imp","nat_jur"),
             colClasses=list(character=c("ruc_imp")),stringsAsFactors = TRUE)

##############################################################################################################
# CPE
##############################################################################################################
bd2_cpe_emisor <- fread('01bd/CPE_EMISOR.csv',colClasses=list(character=c("emisor_num_ruc")),stringsAsFactors = TRUE)
bd2_cpe_receptor <- fread('01bd/CPE_RECEPTOR.csv',colClasses=list(character=c("receptor_num_ruc")),stringsAsFactors = TRUE)


##############################################################################################################
# Anexos
##############################################################################################################
bd3_anexos_contribuyentes <- fread('01bd/anexos_contribuyentes.csv',colClasses=list(character=c("ruc_anexos")),stringsAsFactors = TRUE)


##############################################################################################################
# Subvaluador
##############################################################################################################
bd4_subvaluador <- fread('01bd/subvaluador_T7.csv',colClasses=list(character=c("ruc_team7")),stringsAsFactors = TRUE)


##############################################################################################################
# Subvaluador
##############################################################################################################
bd5_ficha_nacional <- fread('01bd/ficha_nacional.csv',select = c("a.ruc","a.razon_social","a.cod_cond","a.condicion"),colClasses=list(character=c("a.ruc")),stringsAsFactors = TRUE)


##############################################################################################################
# left join
##############################################################################################################
bd0_nuevos_indicadores <- left_join(bd_nuevos_indicadores,bd1_onr,by=c("libr_tribu"="ruc_imp"))
bd0_nuevos_indicadores <- left_join(bd0_nuevos_indicadores,bd2_cpe_emisor,by=c("libr_tribu"="emisor_num_ruc"))
bd0_nuevos_indicadores <- left_join(bd0_nuevos_indicadores,bd2_cpe_receptor,by=c("libr_tribu"="receptor_num_ruc"))
bd0_nuevos_indicadores <- left_join(bd0_nuevos_indicadores,bd3_anexos_contribuyentes,by=c("libr_tribu"="ruc_anexos"))
bd0_nuevos_indicadores <- left_join(bd0_nuevos_indicadores,bd4_subvaluador,by=c("libr_tribu"="ruc_team7"))
bd0_nuevos_indicadores <- left_join(bd0_nuevos_indicadores,bd5_ficha_nacional,by=c("libr_tribu"="a.ruc"))

##############################################################################################################
# Consultas
##############################################################################################################

bd0_nuevos_indicadores %>% group_by(Cluster3,nat_jur) %>% summarise(n())
bd0_nuevos_indicadores %>% group_by(Cluster3,categoria_subvaluardor) %>% summarise(n())
bd0_nuevos_indicadores %>% filter(cantidad>0) %>% group_by(Cluster3) %>% summarise(n())

bd0_nuevos_indicadores %>% group_by(Cluster3) %>% 
  summarise(n(),sum(is.na(emisor_mto_importe_total)),sum(is.na(receptor_mto_importe_total)),
            sum(emisor_facturas,na.rm = TRUE),sum(emisor_mto_importe_total,na.rm = TRUE),
            #sum(receptor_facturas,na.rm = TRUE),sum(receptor_mto_importe_total,na.rm = TRUE),
            mean(emisor_mto_importe_total,na.rm = TRUE)/mean(ventas_Prom))


bd0_nuevos_indicadores %>% group_by(Cluster3) %>% 
  filter(!is.na(emisor_mto_importe_total)) %>% 
  summarise(n(),sum(emisor_mto_importe_total,na.rm = TRUE)/sum(ventas_Prom))

bd0_nuevos_indicadores %>% group_by(Cluster3) %>% 
  filter(!is.na(receptor_mto_importe_total)) %>% 
  summarise(n(),sum(receptor_mto_importe_total,na.rm = TRUE)/sum(compras_Prom))

write.csv(bd0_nuevos_indicadores,"bd_indicadores.csv")
