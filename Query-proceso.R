library(tidyverse)
library(data.table)

bd <- fread('01bd/Indicadores_ContExo.csv',colClasses=list(character=c("ruc","ubigeo_150101","ubigeo_150115")),stringsAsFactors = TRUE)

summary(bd)


nums <- unlist(lapply(bd, is.numeric)) #identificar a las columnas numéricas

vnumericas = data.frame(bd[,nums])
vcategoricas = bd[ , -nums]


### Análisis de correlación
library(corrplot)

### Análisis de reducción de dimensiones: ACP


#Crear gráficos de forma automática
for (i in 1:length(names(vnumericas))) {
  
  png(paste('Resultados2//myplot_', names(vnumericas)[i] , '.png', sep=''),
      width=1200,height=820,units="px",
      pointsize=12,bg="white",res=300,restoreConsole = TRUE)
  
  print(ggplot(vnumericas,aes(x=vnumericas[,i]))+
    geom_histogram()+
    labs(title='CPE Factura emitidas',x=names(vnumericas)[i],y="Frecuencia")+
    theme(axis.text.x=element_text(angle=0, size=7, vjust=0.5),
          plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5)))
  
  
  dev.off()
}


for (i in 1:length(names(vcategoricas))) {
  
  png(paste('Resultados2//myplot_', names(vcategoricas)[i] , '.png', sep=''),
      width=1200,height=820,units="px",
      pointsize=12,bg="white",res=300,restoreConsole = TRUE)
  
  print(ggplot(vcategoricas,aes(x=vcategoricas[,i]))+
          geom_bar(stat="identity")+
          labs(title='CPE Factura emitidas',x=names(vcategoricas)[i],y="Frecuencia")+
          theme(axis.text.x=element_text(angle=0, size=7, vjust=0.5),
                plot.title =element_text(size=12, face="bold", vjust=0.5,hjust=0.5)))
  
  
  dev.off()
}
