#-------Carga limpieza y preparación de tabla liquida------
rm(list = ls())

library(data.table)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(readr)
library(cluster)

memory.limit(size = 10000)

liquida <- fread("liquida_detalle.csv")
liquida <- as.data.frame(liquida)

# limitada a los años de analisis 
liquida <- liquida %>% filter(rlanoaso==15 | rlanoaso==16 | rlanoaso==17)

# Solo los que se cancelo en banco tipcan != 6
table(liquida$rltipcan)
# liquida <- liquida %>% filter(rltipcan!=6)
liquida <- liquida %>% filter(rltipcan==2)


liquida$sustento <- paste(liquida$rldetall, liquida$rldetal2, liquida$rldetal3, 
                          liquida$rldetal4, liquida$rldetal5, sep = "")
liquida$sustento <- tolower(liquida$sustento)
liquida$sustento <- gsub("[[:punct:]]+","", liquida$sustento)
liquida$sustento <- gsub("[[:cntrl:]]","", liquida$sustento)
liquida$sustento <- removePunctuation(liquida$sustento)
liquida$sustento <- removeNumbers(liquida$sustento)
liquida$sustento <- stripWhitespace(liquida$sustento)

liquida <- liquida[,-which(names(liquida) %in% c("rldetall", "rldetal2", "rldetal3", "rldetal4", "rldetal5"))]

palabras <- c("SIVEP", "VALOR", "AJUSTE", "A.V", "OMC", "FOB", "DUDA RAZ", "A.V.", "A/V", "AV", 
              "AUSTE", "AHYSTE", "AJ SERIE", "AJUSTTE", "AQJUSTE", "RAZON", "SIVAEP", "SIVPE", "REFERENCIA",
              "AUTOLIQUIDACION DE TRIBUTOS")

palabras <- tolower(palabras)

aux <- NULL
for (i in palabras) {
  aaa <- grep(i, liquida$sustento)
  aux <- c(aux, aaa)
}
aux <- unique(aux)
rm(i,aaa)

liquida_av <- liquida[aux,]
table(liquida_av$rlano)
liquida_av$aj_va <- 1

liquida_no_av <- liquida[-c(aux),]
liquida_no_av$aj_va <- 0

liquida_com <- rbind(liquida_av, liquida_no_av)

write.csv(liquida_com, "liq_a3.csv", row.names = F)

#-------Resumen de tabla liquida------
rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)

liq_q3 <- fread("liq_a3.csv")
liq_q3 <- as.data.frame(liq_q3)

# liq_q3 <- liq_q3[,-which(names(liq_q3) %in% c("rldetall", "rldetal2", "rldetal3", "rldetal4", "rldetal5"))]
liq_q3$sustento <- gsub(",", "", liq_q3$sustento)
liq_q3$periodo <- substr(liq_q3$rlfecliq,1 ,6)
liq_q3$periodo <- as.integer(liq_q3$periodo)

# Carga de tipos de cambio
tipo_cambio <- fread("tipo_cambio_periodo.csv")

liq_q3 <- merge(liq_q3, tipo_cambio, by = "periodo")

liq_q3$liq_soles <- liq_q3$rlsmontot + liq_q3$rldmontot*liq_q3$`tipo cambio`
liq_q3$liq_dolar <- liq_q3$rldmontot + liq_q3$rlsmontot/liq_q3$`tipo cambio`


# Quitando los blancos
liq_q3 <- liq_q3 %>% filter(rllibtri!="" & rllibtri!=" " & rllibtri!=".")
liq_q3$DAM <- paste(liq_q3$rladuaso, liq_q3$rlanoaso, liq_q3$rlnumaso)

# Liquida final
write.csv(liq_q3, "liquida_final.csv", row.names = F)


cuenta_unico <- function(x) {length(unique(x))}

aux <- liq_q3 %>% group_by(rllibtri, rlano) %>% summarise(conteo = cuenta_unico(DAM))
aux1 <- aux %>% spread(rlano, conteo)
names(aux1) <- c("libr_tribu", "tiene_liq_15", "tiene_liq_16", "tiene_liq_17")
aux1 <- as.data.frame(aux1)
aux1$tiene_liq_15 <- ifelse(is.na(aux1$tiene_liq_15), 0, aux1$tiene_liq_15)
aux1$tiene_liq_16 <- ifelse(is.na(aux1$tiene_liq_16), 0, aux1$tiene_liq_16)
aux1$tiene_liq_17 <- ifelse(is.na(aux1$tiene_liq_17), 0, aux1$tiene_liq_17)

aux <- liq_q3 %>% group_by(rllibtri, rlano, aj_va) %>% summarise(liq_soles = sum(liq_soles))
aux$llave <- paste(aux$rlano, aux$aj_va)
aux <- as.data.frame(aux)
aux2 <- aux %>% select(rllibtri, llave, liq_soles) %>% spread(llave, liq_soles)
names(aux2) <- c("libr_tribu", 
                 "liq__sol_15_nav", "liq__sol_15_av", 
                 "liq__sol_16_nav", "liq__sol_16_av", 
                 "liq__sol_17_nav", "liq__sol_17_av")
aux2$liq__sol_15_nav <- ifelse(is.na(aux2$liq__sol_15_nav), 0, aux2$liq__sol_15_nav)
aux2$liq__sol_16_nav <- ifelse(is.na(aux2$liq__sol_16_nav), 0, aux2$liq__sol_16_nav)
aux2$liq__sol_17_nav <- ifelse(is.na(aux2$liq__sol_17_nav), 0, aux2$liq__sol_17_nav)
aux2$liq__sol_15_av <- ifelse(is.na(aux2$liq__sol_15_av), 0, aux2$liq__sol_15_av)
aux2$liq__sol_16_av <- ifelse(is.na(aux2$liq__sol_16_av), 0, aux2$liq__sol_16_av)
aux2$liq__sol_17_av <- ifelse(is.na(aux2$liq__sol_17_av), 0, aux2$liq__sol_17_av)

aux <- liq_q3 %>% group_by(rllibtri, rlano, aj_va) %>% summarise(liq_soles = mean(liq_soles))
aux$llave <- paste(aux$rlano, aux$aj_va)
aux <- as.data.frame(aux)
aux3 <- aux %>% select(rllibtri, llave, liq_soles) %>% spread(llave, liq_soles)
names(aux3) <- c("libr_tribu", 
                 "liq__sol_15_prom_nav", "liq__sol_15_prom_av", 
                 "liq__sol_16_prom_nav", "liq__sol_16_prom_av", 
                 "liq__sol_17_prom_nav", "liq__sol_17_prom_av")
aux3$liq__sol_15_prom_nav <- ifelse(is.na(aux3$liq__sol_15_prom_nav), 0, aux3$liq__sol_15_prom_nav)
aux3$liq__sol_16_prom_nav <- ifelse(is.na(aux3$liq__sol_16_prom_nav), 0, aux3$liq__sol_16_prom_nav)
aux3$liq__sol_17_prom_nav <- ifelse(is.na(aux3$liq__sol_17_prom_nav), 0, aux3$liq__sol_17_prom_nav)
aux3$liq__sol_15_prom_av <- ifelse(is.na(aux3$liq__sol_15_prom_av), 0, aux3$liq__sol_15_prom_av)
aux3$liq__sol_16_prom_av <- ifelse(is.na(aux3$liq__sol_16_prom_av), 0, aux3$liq__sol_16_prom_av)
aux3$liq__sol_17_prom_av <- ifelse(is.na(aux3$liq__sol_17_prom_av), 0, aux3$liq__sol_17_prom_av)


aux <- liq_q3 %>% group_by(rllibtri, rlano, aj_va) %>% summarise(liq_doles = sum(liq_dolar))
aux$llave <- paste(aux$rlano, aux$aj_va)
aux <- as.data.frame(aux)
aux4 <- aux %>% select(rllibtri, llave, liq_doles) %>% spread(llave, liq_doles)
names(aux4) <- c("libr_tribu", 
                 "liq__dol_15_nav", "liq__dol_15_av", 
                 "liq__dol_16_nav", "liq__dol_16_av", 
                 "liq__dol_17_nav", "liq__dol_17_av")
aux4$liq__dol_15_nav <- ifelse(is.na(aux4$liq__dol_15_nav), 0, aux4$liq__dol_15_nav)
aux4$liq__dol_16_nav <- ifelse(is.na(aux4$liq__dol_16_nav), 0, aux4$liq__dol_16_nav)
aux4$liq__dol_17_nav <- ifelse(is.na(aux4$liq__dol_17_nav), 0, aux4$liq__dol_17_nav)
aux4$liq__dol_15_av <- ifelse(is.na(aux4$liq__dol_15_av), 0, aux4$liq__dol_15_av)
aux4$liq__dol_16_av <- ifelse(is.na(aux4$liq__dol_16_av), 0, aux4$liq__dol_16_av)
aux4$liq__dol_17_av <- ifelse(is.na(aux4$liq__dol_17_av), 0, aux4$liq__dol_17_av)

aux <- liq_q3 %>% group_by(rllibtri, rlano, aj_va) %>% summarise(liq_doles = mean(liq_dolar))
aux$llave <- paste(aux$rlano, aux$aj_va)
aux <- as.data.frame(aux)
aux5 <- aux %>% select(rllibtri, llave, liq_doles) %>% spread(llave, liq_doles)
names(aux5) <- c("libr_tribu", 
                 "liq__dol_15_prom_nav", "liq__dol_15_prom_av", 
                 "liq__dol_16_prom_nav", "liq__dol_16_prom_av", 
                 "liq__dol_17_prom_nav", "liq__dol_17_prom_av")
aux5$liq__dol_15_prom_nav <- ifelse(is.na(aux5$liq__dol_15_prom_nav), 0, aux5$liq__dol_15_prom_nav)
aux5$liq__dol_16_prom_nav <- ifelse(is.na(aux5$liq__dol_16_prom_nav), 0, aux5$liq__dol_16_prom_nav)
aux5$liq__dol_17_prom_nav <- ifelse(is.na(aux5$liq__dol_17_prom_nav), 0, aux5$liq__dol_17_prom_nav)
aux5$liq__dol_15_prom_av <- ifelse(is.na(aux5$liq__dol_15_prom_av), 0, aux5$liq__dol_15_prom_av)
aux5$liq__dol_16_prom_av <- ifelse(is.na(aux5$liq__dol_16_prom_av), 0, aux5$liq__dol_16_prom_av)
aux5$liq__dol_17_prom_av <- ifelse(is.na(aux5$liq__dol_17_prom_av), 0, aux5$liq__dol_17_prom_av)

aux <- liq_q3 %>% filter(aj_va == 1) %>% group_by(rllibtri, rlano) %>% summarise(conteo = cuenta_unico(DAM))
aux6 <- aux %>% spread(rlano, conteo)
names(aux6) <- c("libr_tribu", "tiene_av_15", "tiene_av_16", "tiene_av_17")
aux6 <- as.data.frame(aux6)
aux6$tiene_av_15 <- ifelse(is.na(aux6$tiene_av_15), 0, aux6$tiene_av_15)
aux6$tiene_av_16 <- ifelse(is.na(aux6$tiene_av_16), 0, aux6$tiene_av_16)
aux6$tiene_av_17 <- ifelse(is.na(aux6$tiene_av_17), 0, aux6$tiene_av_17)

aux7 <- merge(aux1, aux6, by = "libr_tribu", all.x = T)
aux7$tiene_av_15 <- ifelse(is.na(aux7$tiene_av_15), 0, aux7$tiene_av_15)
aux7$tiene_av_16 <- ifelse(is.na(aux7$tiene_av_16), 0, aux7$tiene_av_16)
aux7$tiene_av_17 <- ifelse(is.na(aux7$tiene_av_17), 0, aux7$tiene_av_17)

resumen_liquida <- cbind(aux7, aux2[,-1], aux3[,-1], aux4[,-1], aux5[,-1])

write.csv(resumen_liquida, "resumen_liquida.csv", row.names = F)

#-------Resumen Poliza---------
rm(list = ls())
library(data.table)
library(dplyr)
library(tidyr)

sigad_2015 <- fread("sigad_a_2015.csv")
nombres <- names(sigad_2015)
nombres <- gsub("sigad_a_2015.", "",nombres)
names(sigad_2015) <- nombres

sigad_2016 <- fread("sigad_a_2016.csv")
nombres <- names(sigad_2016)
nombres <- gsub("sigad_a_2016.", "",nombres)
names(sigad_2016) <- nombres

sigad_2017 <- fread("sigad_a_2017.csv")
nombres <- names(sigad_2017)
nombres <- gsub("sigad_a_2017.", "",nombres)
names(sigad_2017) <- nombres


sigad_15_17 <- rbind(sigad_2015, sigad_2016, sigad_2017)
rm(sigad_2015, sigad_2016, sigad_2017)
sigad_15_17 <- as.data.frame(sigad_15_17)

sigad_15_17 <- sigad_15_17 %>% filter(tipo_cance!=6 & cdocigv!=3)

variables <- c("codi_aduan", "ano_prese", "nume_corre", "tipo_docum", "libr_tribu", 
               "pais_adqui", "tfob_dolpo", "tfle_dolar", "tseg_dolar", "tpeso_neto", 
               "tpeso_brut", "tadv_dolar", "tisc_dolar", "tigv_dolar", "tipm_dolar",
               "fech_ingsi", "tipo_aforo", "codi_regi")

poliza <- sigad_15_17[,variables]
poliza$DAM <- paste(poliza$codi_aduan, poliza$ano_prese, poliza$nume_corre)
poliza$periodo <- substr(poliza$fech_ingsi, 1,6)
poliza$CIF_dolar <- poliza$tfob_dolpo + poliza$tfle_dolar + poliza$tseg_dolar

# Carga de tipos de cambio
tipo_cambio <- fread("tipo_cambio_periodo.csv")

poliza <- merge(poliza, tipo_cambio, by = "periodo")

poliza$CIF_soles <- poliza$CIF_dolar*poliza$`tipo cambio`

anios <- unique(poliza$ano_prese)
cuenta_valor <- function(x, v) {sum(x==v)}

poliza$fob_soles <- poliza$tfob_dolpo*poliza$`tipo cambio`

write.csv(poliza, "poliza_15_16_17.csv", row.names = F)

# Fob acumulado por anio
aux <- poliza %>% group_by(periodo) %>% summarise(fob_acum = sum(fob_soles))
write.csv(aux,"fob_periodo.csv")

# Resumen canal
resumen_canal <- poliza %>% group_by(tipo_aforo, ano_prese) %>% summarise(conteo = n(), cif_dol = sum(CIF_dolar))
write.csv(resumen_canal, "resumen_canal.csv", row.names = F)


# Resumen imp importaciones por año
aux0 <- poliza %>% group_by(libr_tribu) %>% summarise(imp_15 = sum(ano_prese==15),
                                                      imp_16 = sum(ano_prese==16),
                                                      imp_17 = sum(ano_prese==17))

# resumen imp valores cif
aux <- poliza %>% group_by(libr_tribu, ano_prese) %>% summarise(CIF_dolar_anual = sum(CIF_dolar),
                                                                CIF_dolar_media = mean(CIF_dolar),
                                                                CIF_dolar_max = max(CIF_dolar),
                                                                CIF_dolar_min = min(CIF_dolar),
                                                                CIF_dolar_mediana = median(CIF_dolar))


head(aux)
aux <- as.data.frame(aux)
str(aux)

aux1 <- cbind(aux[,1:3] %>% spread(ano_prese, CIF_dolar_anual), 
              (aux[,c(1,2,4)] %>% spread(ano_prese, CIF_dolar_media))[,2:4],
              (aux[,c(1,2,5)] %>% spread(ano_prese, CIF_dolar_max))[,2:4],
              (aux[,c(1,2,6)] %>% spread(ano_prese, CIF_dolar_min))[,2:4],
              (aux[,c(1,2,7)] %>% spread(ano_prese, CIF_dolar_mediana))[,2:4])

names(aux1) <- c("libr_tribu", "CIF_dolar_anual_15", "CIF_dolar_anual_16", "CIF_dolar_anual_17", 
                 "CIF_dolar_media_15", "CIF_dolar_media_16", "CIF_dolar_media_17", 
                 "CIF_dolar_max_15", "CIF_dolar_max_16", "CIF_dolar_max_17", 
                 "CIF_dolar_min_15", "CIF_dolar_min_16", "CIF_dolar_min_17", 
                 "CIF_dolar_mediana_15", "CIF_dolar_mediana_16", "CIF_dolar_mediana_17")


# resumen imp valores fob
aux <- poliza %>% group_by(libr_tribu, ano_prese) %>% summarise(FOB_dolar_anual = sum(tfob_dolpo),
                                                                FOB_dolar_media = mean(tfob_dolpo),
                                                                FOB_dolar_max = max(tfob_dolpo),
                                                                FOB_dolar_min = min(tfob_dolpo),
                                                                FOB_dolar_mediana = median(tfob_dolpo))

aux <- as.data.frame(aux)

aux2 <- cbind(aux[,1:3] %>% spread(ano_prese, FOB_dolar_anual), 
              (aux[,c(1,2,4)] %>% spread(ano_prese, FOB_dolar_media))[,2:4],
              (aux[,c(1,2,5)] %>% spread(ano_prese, FOB_dolar_max))[,2:4],
              (aux[,c(1,2,6)] %>% spread(ano_prese, FOB_dolar_min))[,2:4],
              (aux[,c(1,2,7)] %>% spread(ano_prese, FOB_dolar_mediana))[,2:4])

names(aux2) <- c("libr_tribu", "FOB_dolar_anual_15", "FOB_dolar_anual_16", "FOB_dolar_anual_17", 
                 "FOB_dolar_media_15", "FOB_dolar_media_16", "FOB_dolar_media_17", 
                 "FOB_dolar_max_15", "FOB_dolar_max_16", "FOB_dolar_max_17", 
                 "FOB_dolar_min_15", "FOB_dolar_min_16", "FOB_dolar_min_17", 
                 "FOB_dolar_mediana_15", "FOB_dolar_mediana_16", "FOB_dolar_mediana_17")


# Resumen imp por % de canal
aux <- poliza %>% group_by(libr_tribu) %>% summarise(porc_v = sum(tipo_aforo=="V"),
                                                     porc_D = sum(tipo_aforo=="D"),
                                                     porc_F = sum(tipo_aforo=="F"),
                                                     porc_Blanco = sum(tipo_aforo==" "),
                                                     total_cant = n()) 

aux$porc_v <- round((aux$porc_v/aux$total_cant)*100,0)
aux$porc_D <- round((aux$porc_D/aux$total_cant)*100,0)
aux$porc_F <- round((aux$porc_F/aux$total_cant)*100,0)
aux$porc_Blanco <- round((aux$porc_Blanco/aux$total_cant)*100,0)

aux3 <- aux

# Resumen importador igv
aux <- poliza %>% group_by(libr_tribu, ano_prese) %>% summarise(ADV_media = mean(tadv_dolar),
                                                                IGV_media = mean(tigv_dolar),
                                                                ISC_media = mean(tisc_dolar),
                                                                IPM_media = mean(tipm_dolar))

aux <- as.data.frame(aux)

aux4 <- cbind(aux[,1:3] %>% spread(ano_prese, ADV_media), 
              (aux[,c(1,2,4)] %>% spread(ano_prese, IGV_media))[,2:4],
              (aux[,c(1,2,5)] %>% spread(ano_prese, ISC_media))[,2:4],
              (aux[,c(1,2,6)] %>% spread(ano_prese, IPM_media))[,2:4])

names(aux4) <- c("libr_tribu", 
                 "ADV_media_15", "ADV_media_16", "ADV_media_17",
                 "IGV_media_15", "IGV_media_16", "IGV_media_17", 
                 "ISC_media_15", "ISC_media_16", "ISC_media_17", 
                 "IPM_media_15", "IPM_media_16", "IPM_media_17")

head(aux4)

resumen_1 <- cbind(aux0, aux1[,-1], aux2[,-1], aux3[,-1], aux4[,-1])

# write.csv(resumen_1, "resumen_importadores_1.csv", row.names = F)
write.csv(resumen_1, "resumen_poliza.csv", row.names = F)

# Guardamos las DAMs con los RUCs corespondientes
dam <- poliza[,c("DAM", "tipo cambio", "libr_tribu", "codi_regi", "tipo_docum")]
write.csv(dam, "DAM_15_16_17.csv", row.names = F)

#-------Resumen ITF 2015 al 2017-----------
rm(list = ls())
memory.limit(size = 12000)

library(data.table)
library(dplyr)
library(tidyr)

## A?O 2015
itf_2015 <- fread("ITF_2015.csv")
nombres <- names(itf_2015)
nombres <- gsub("vw_itf_impo_det2015.", "", nombres)
names(itf_2015) <- nombres

itf_2015$mto_base <- ifelse(itf_2015$mto_base==20100023203,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20516711559,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20382036655,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20228319768,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20101036813,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20513074370,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20255993225,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20122967205,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20100105862,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20100064571,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20100209641,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20100053455,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20100043140,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20348067053,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20504565794,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20100047218,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20100130204,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20509507199,NA,itf_2015$mto_base)
itf_2015$mto_base <- ifelse(itf_2015$mto_base==20231269071,NA,itf_2015$mto_base)

itf_2015$mto_base_estimado <- itf_2015$mto_impuesto/0.00005
itf_2015$mto_base_estimado <- ifelse(is.na(itf_2015$mto_base) | itf_2015$mto_base == 0, itf_2015$mto_base_estimado, itf_2015$mto_base)

itf_2015 <- as.data.frame(itf_2015)
itf_2015 <- itf_2015[,-which(names(itf_2015) %in% c("nota_abono", "formulario", "n_orden", "tipo_operacion"))]

extranj_salidas <- itf_2015 %>% filter(cod_operacion == 14 | cod_operacion == 16 | cod_operacion == 18)
extranj_entradas <- itf_2015 %>% filter(cod_operacion == 12 | cod_operacion == 20)
nacional_entradas <- itf_2015 %>% filter(cod_operacion == 13 | cod_operacion == 3)
nacional <- itf_2015 %>% filter(cod_operacion != 14 & cod_operacion != 16 & cod_operacion != 18 & 
                                  cod_operacion != 12 & cod_operacion != 20 & cod_operacion != 1 & 
                                  cod_operacion != 5)
transferencias_netas<-itf_2015

fernando <- itf_2015 %>% filter(cod_operacion == 14)

# Resumen anual
aux1 <- extranj_salidas %>% group_by(n_dcto_declado) %>% summarise(ex_sa_mto = sum(mto_base_estimado))
aux2 <- extranj_entradas %>% group_by(n_dcto_declado) %>% summarise(ex_en_mto = sum(mto_base_estimado))
aux3 <- nacional %>% group_by(n_dcto_declado) %>% summarise(nac_mto = sum(mto_base_estimado))
aux4 <- nacional_entradas %>% group_by(n_dcto_declado) %>% summarise(nac_en_mto = sum(mto_base_estimado))
aux5 <- transferencias_netas %>% group_by(n_dcto_declado) %>% summarise(tr_ne_mto = sum(mto_base_estimado))
aux6 <- fernando %>% group_by(n_dcto_declado) %>% summarise(cod_14 = sum(mto_base_estimado))

res_anual_itf_2015 <- merge(aux1, aux2, by = "n_dcto_declado", all = T)
res_anual_itf_2015 <- merge(res_anual_itf_2015, aux3, by = "n_dcto_declado", all = T)
res_anual_itf_2015 <- merge(res_anual_itf_2015, aux4, by = "n_dcto_declado", all = T)
res_anual_itf_2015 <- merge(res_anual_itf_2015, aux5, by = "n_dcto_declado", all = T)
res_anual_itf_2015 <- merge(res_anual_itf_2015, aux6, by = "n_dcto_declado", all = T)

# Resumen periodo
aux1 <- extranj_salidas %>% group_by(n_dcto_declado, periodo_operacion) %>% summarise(ex_sa_mto = sum(mto_base_estimado))
aux1$llave <- paste(aux1$n_dcto_declado, aux1$periodo_operacion)
aux2 <- extranj_entradas %>% group_by(n_dcto_declado, periodo_operacion) %>% summarise(ex_en_mto = sum(mto_base_estimado))
aux2$llave <- paste(aux2$n_dcto_declado, aux2$periodo_operacion)
aux2 <- as.data.frame(aux2)

aux3 <- nacional %>% group_by(n_dcto_declado, periodo_operacion) %>% summarise(nac_mto = sum(mto_base_estimado))
aux3$llave <- paste(aux3$n_dcto_declado, aux3$periodo_operacion)
aux3 <- as.data.frame(aux3)

res_periodo_itf_2015 <- merge(aux1[,3:4], aux2[,-c(1:2)], by = "llave", all = T)
res_periodo_itf_2015 <- merge(res_periodo_itf_2015, aux3[,-c(1,2)], by = "llave", all = T)

## A?O 2016
itf_2016 <- fread("ITF_2016.csv")
nombres <- names(itf_2016)
nombres <- gsub("vw_itf_impo_det2016.", "", nombres)
names(itf_2016) <- nombres

itf_2016$mto_base <- ifelse(itf_2016$mto_base==20100023203,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20516711559,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20382036655,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20228319768,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20101036813,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20513074370,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20255993225,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20122967205,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20100105862,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20100064571,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20100209641,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20100053455,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20100043140,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20348067053,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20504565794,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20100047218,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20100130204,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20509507199,NA,itf_2016$mto_base)
itf_2016$mto_base <- ifelse(itf_2016$mto_base==20231269071,NA,itf_2016$mto_base)

itf_2016$mto_base_estimado <- itf_2016$mto_impuesto/0.00005
itf_2016$mto_base_estimado <- ifelse(is.na(itf_2016$mto_base) | itf_2016$mto_base == 0, itf_2016$mto_base_estimado, itf_2016$mto_base)

itf_2016 <- as.data.frame(itf_2016)
itf_2016 <- itf_2016[,-which(names(itf_2016) %in% c("nota_abono", "formulario", "n_orden", "tipo_operacion"))]

extranj_salidas <- itf_2016 %>% filter(cod_operacion == 14 | cod_operacion == 16 | cod_operacion == 18)
extranj_entradas <- itf_2016 %>% filter(cod_operacion == 12 | cod_operacion == 20)
nacional_entradas <- itf_2016 %>% filter(cod_operacion == 13 | cod_operacion == 3)
nacional <- itf_2016 %>% filter(cod_operacion != 14 & cod_operacion != 16 & cod_operacion != 18 & 
                                  cod_operacion != 12 & cod_operacion != 20 & cod_operacion != 1 & 
                                  cod_operacion != 5)
transferencias_netas<-itf_2016

fernando <- itf_2016 %>% filter(cod_operacion == 14)

# Resumen anual
aux1 <- extranj_salidas %>% group_by(n_dcto_declado) %>% summarise(ex_sa_mto = sum(mto_base_estimado))
aux2 <- extranj_entradas %>% group_by(n_dcto_declado) %>% summarise(ex_en_mto = sum(mto_base_estimado))
aux3 <- nacional %>% group_by(n_dcto_declado) %>% summarise(nac_mto = sum(mto_base_estimado))
aux4 <- nacional_entradas %>% group_by(n_dcto_declado) %>% summarise(nac_en_mto = sum(mto_base_estimado))
aux5 <- transferencias_netas %>% group_by(n_dcto_declado) %>% summarise(tr_ne_mto = sum(mto_base_estimado))
aux6 <- fernando %>% group_by(n_dcto_declado) %>% summarise(cod_14 = sum(mto_base_estimado))

res_anual_itf_2016 <- merge(aux1, aux2, by = "n_dcto_declado", all = T)
res_anual_itf_2016 <- merge(res_anual_itf_2016, aux3, by = "n_dcto_declado", all = T)
res_anual_itf_2016 <- merge(res_anual_itf_2016, aux4, by = "n_dcto_declado", all = T)
res_anual_itf_2016 <- merge(res_anual_itf_2016, aux5, by = "n_dcto_declado", all = T)
res_anual_itf_2016 <- merge(res_anual_itf_2016, aux6, by = "n_dcto_declado", all = T)

# Resumen periodo
aux1 <- extranj_salidas %>% group_by(n_dcto_declado, periodo_operacion) %>% summarise(ex_sa_mto = sum(mto_base_estimado))
aux1$llave <- paste(aux1$n_dcto_declado, aux1$periodo_operacion)
aux2 <- extranj_entradas %>% group_by(n_dcto_declado, periodo_operacion) %>% summarise(ex_en_mto = sum(mto_base_estimado))
aux2$llave <- paste(aux2$n_dcto_declado, aux2$periodo_operacion)
aux2 <- as.data.frame(aux2)

aux3 <- nacional %>% group_by(n_dcto_declado, periodo_operacion) %>% summarise(nac_mto = sum(mto_base_estimado))
aux3$llave <- paste(aux3$n_dcto_declado, aux3$periodo_operacion)
aux3 <- as.data.frame(aux3)

res_periodo_itf_2016 <- merge(aux1[,3:4], aux2[,-c(1:2)], by = "llave", all = T)
res_periodo_itf_2016 <- merge(res_periodo_itf_2016, aux3[,-c(1,2)], by = "llave", all = T)


## A?O 2017
itf_2017 <- fread("ITF_2017.csv")
nombres <- names(itf_2017)
nombres <- gsub("vw_itf_impo_det2017.", "", nombres)
names(itf_2017) <- nombres

itf_2017$mto_base <- ifelse(itf_2017$mto_base==20100023203,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20516711559,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20382036655,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20228319768,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20101036813,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20513074370,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20255993225,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20122967205,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20100105862,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20100064571,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20100209641,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20100053455,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20100043140,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20348067053,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20504565794,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20100047218,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20100130204,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20509507199,NA,itf_2017$mto_base)
itf_2017$mto_base <- ifelse(itf_2017$mto_base==20231269071,NA,itf_2017$mto_base)

itf_2017$mto_base_estimado <- itf_2017$mto_impuesto/0.00005
itf_2017$mto_base_estimado <- ifelse(is.na(itf_2017$mto_base) | itf_2017$mto_base == 0, itf_2017$mto_base_estimado, itf_2017$mto_base)

itf_2017 <- as.data.frame(itf_2017)
itf_2017 <- itf_2017[,-which(names(itf_2017) %in% c("nota_abono", "formulario", "n_orden", "tipo_operacion"))]

extranj_salidas <- itf_2017 %>% filter(cod_operacion == 14 | cod_operacion == 16 | cod_operacion == 18)
extranj_entradas <- itf_2017 %>% filter(cod_operacion == 12 | cod_operacion == 20)
nacional_entradas <- itf_2017 %>% filter(cod_operacion == 13 | cod_operacion == 3)
nacional <- itf_2017 %>% filter(cod_operacion != 14 & cod_operacion != 16 & cod_operacion != 18 & 
                                  cod_operacion != 12 & cod_operacion != 20 & cod_operacion != 1 & 
                                  cod_operacion != 5)
transferencias_netas<-itf_2017

fernando <- itf_2017 %>% filter(cod_operacion == 14)

# Resumen anual
aux1 <- extranj_salidas %>% group_by(n_dcto_declado) %>% summarise(ex_sa_mto = sum(mto_base_estimado))
aux2 <- extranj_entradas %>% group_by(n_dcto_declado) %>% summarise(ex_en_mto = sum(mto_base_estimado))
aux3 <- nacional %>% group_by(n_dcto_declado) %>% summarise(nac_mto = sum(mto_base_estimado))
aux4 <- nacional_entradas %>% group_by(n_dcto_declado) %>% summarise(nac_en_mto = sum(mto_base_estimado))
aux5 <- transferencias_netas %>% group_by(n_dcto_declado) %>% summarise(tr_ne_mto = sum(mto_base_estimado))
aux6 <- fernando %>% group_by(n_dcto_declado) %>% summarise(cod_14 = sum(mto_base_estimado))

res_anual_itf_2017 <- merge(aux1, aux2, by = "n_dcto_declado", all = T)
res_anual_itf_2017 <- merge(res_anual_itf_2017, aux3, by = "n_dcto_declado", all = T)
res_anual_itf_2017 <- merge(res_anual_itf_2017, aux4, by = "n_dcto_declado", all = T)
res_anual_itf_2017 <- merge(res_anual_itf_2017, aux5, by = "n_dcto_declado", all = T)
res_anual_itf_2017 <- merge(res_anual_itf_2017, aux6, by = "n_dcto_declado", all = T)

# Resumen periodo
aux1 <- extranj_salidas %>% group_by(n_dcto_declado, periodo_operacion) %>% summarise(ex_sa_mto = sum(mto_base_estimado))
aux1$llave <- paste(aux1$n_dcto_declado, aux1$periodo_operacion)
aux2 <- extranj_entradas %>% group_by(n_dcto_declado, periodo_operacion) %>% summarise(ex_en_mto = sum(mto_base_estimado))
aux2$llave <- paste(aux2$n_dcto_declado, aux2$periodo_operacion)
aux2 <- as.data.frame(aux2)

aux3 <- nacional %>% group_by(n_dcto_declado, periodo_operacion) %>% summarise(nac_mto = sum(mto_base_estimado))
aux3$llave <- paste(aux3$n_dcto_declado, aux3$periodo_operacion)
aux3 <- as.data.frame(aux3)

res_periodo_itf_2017 <- merge(aux1[,3:4], aux2[,-c(1:2)], by = "llave", all = T)
res_periodo_itf_2017 <- merge(res_periodo_itf_2017, aux3[,-c(1,2)], by = "llave", all = T)


# el anual 15 al 17
nombres <- names(res_anual_itf_2015)
nombres <- paste(nombres, "_2015", sep = "")
nombres[1] <- "n_dcto_declado"
names(res_anual_itf_2015) <- nombres

nombres <- names(res_anual_itf_2016)
nombres <- paste(nombres, "_2016", sep = "")
nombres[1] <- "n_dcto_declado"
names(res_anual_itf_2016) <- nombres

nombres <- names(res_anual_itf_2017)
nombres <- paste(nombres, "_2017", sep = "")
nombres[1] <- "n_dcto_declado"
names(res_anual_itf_2017) <- nombres

res_anual_itf <- merge(res_anual_itf_2015, res_anual_itf_2016, by = "n_dcto_declado",all = T)
res_anual_itf <- merge(res_anual_itf, res_anual_itf_2017, by = "n_dcto_declado",all = T)

# el period 15 al 17
res_periodo_itf <- rbind(res_periodo_itf_2015, res_periodo_itf_2016, res_periodo_itf_2017)
res_periodo_itf <- as.data.frame(res_periodo_itf)
res_periodo_itf$n_dcto_declado <- substr(res_periodo_itf$llave, 1,11)
res_periodo_itf$periodo <- substr(res_periodo_itf$llave,12, 18)
res_periodo_itf <- res_periodo_itf[,-1]

res_periodo_itf <- res_periodo_itf[,c("n_dcto_declado", "periodo", "ex_sa_mto", "ex_en_mto", "nac_mto")]

write.csv(res_periodo_itf, "res_periodo_itf.csv", row.names = F)
write.csv(res_anual_itf, "res_anual_itf.csv", row.names = F)

#-------Resumen PDT 621-------
rm(list = ls())

library(data.table)
library(dplyr)


## PDT 2015
pdt_2015 <- fread("pdt0621_2015.csv")

nombres <- names(pdt_2015)
nombres <- gsub("dg_pdt0621_2015.", "", nombres)
names(pdt_2015) <- nombres

pdt_2015$n_doc_declado <- as.character(pdt_2015$n_doc_declado)

# Calculo de las compras 
suma_na <- function(x){ sum(x,na.rm = T)}

pdt_2015$compras_nac_grav <- apply(pdt_2015[,c("cas107", "cas110", "cas113")], 1,suma_na)
pdt_2015$compras_imp_grav <- apply(pdt_2015[,c("cas114", "cas116", "cas119")], 1,suma_na)

pdt_2015$compras_nac_nograv <- ifelse(is.na(pdt_2015$cas120),0,pdt_2015$cas120)
pdt_2015$compras_imp_nograv <- ifelse(is.na(pdt_2015$cas122),0,pdt_2015$cas122)

pdt_2015$compras_nac <- pdt_2015$compras_nac_nograv + pdt_2015$compras_nac_grav
pdt_2015$compras_imp <- pdt_2015$compras_imp_nograv + pdt_2015$compras_imp_grav
pdt_2015$compras <- pdt_2015$compras_nac + pdt_2015$compras_imp

# Calculo de las ventas 
aux1 <- apply(pdt_2015[,c("cas100", "cas124", "cas160", "cas106")], 1,suma_na)
aux2 <- apply(pdt_2015[,c("cas102", "cas126", "cas162")], 1,suma_na)

pdt_2015$ventas_grav <- aux1-aux2
pdt_2015$ventas_nograv <- apply(pdt_2015[,c("cas105", "cas109")], 1, suma_na)
pdt_2015$ventas_otras <- apply(pdt_2015[,c("cas112")], 1, suma_na)
pdt_2015$ventas <- pdt_2015$ventas_grav + pdt_2015$ventas_nograv + pdt_2015$ventas_otras
pdt_2015$igv_debito <- ifelse(is.na(pdt_2015$cas131), 0, pdt_2015$cas131)
pdt_2015$igv_credito <- ifelse(is.na(pdt_2015$cas178), 0, pdt_2015$cas178)
pdt_2015$igv_imp <- ifelse(is.na(pdt_2015$cas115 + pdt_2015$cas115), 0, pdt_2015$cas115 + pdt_2015$cas115)


## PDT RESUMIDO
pdt_2015 <- pdt_2015[,c("n_orden", "periodo", "n_doc_declado", "cic", "f_presenta", "compras_nac_grav", 
                        "compras_imp_grav", "compras_nac_nograv", "compras_imp_nograv", "compras_nac", 
                        "compras_imp", "compras", "ventas", "igv_debito", "igv_credito", "igv_imp")]

## PDT FINAL - ULTIMA FECHA 
aux <- pdt_2015 %>% group_by(n_doc_declado, periodo) %>% summarise(n_orden = max(n_orden))
aux$llave <- paste(aux$n_doc_declado, aux$periodo, aux$n_orden)
pdt_2015$llave <- paste(pdt_2015$n_doc_declado, pdt_2015$periodo, pdt_2015$n_orden)

pdt_2015_uni <- merge(pdt_2015, aux[,"llave"], by = "llave")

contar_nozero <- function(x){sum(x>0)}

pdt_2015_res <- pdt_2015_uni %>% group_by(n_doc_declado) %>% summarise(ult_fecha = max(periodo),
                                                                       nro_pdt = n(),
                                                                       nro_compras_imp = contar_nozero(compras_imp),
                                                                       nro_compras_nac = contar_nozero(compras_nac),
                                                                       nro_compras = contar_nozero(compras),
                                                                       nro_ventas = contar_nozero(ventas),
                                                                       compras_imp = suma_na(compras_imp),
                                                                       compras_nac = suma_na(compras_nac),
                                                                       compras = suma_na(compras),
                                                                       ventas = suma_na(ventas),
                                                                       igv_debito = suma_na(igv_debito),
                                                                       igv_credito = suma_na(igv_credito),
                                                                       igv_imp = suma_na(igv_imp))
rm(aux, pdt_2015, pdt_2015_uni)

## PDT 2016
pdt_2016 <- fread("pdt0621_2016.csv")

nombres <- names(pdt_2016)
nombres <- gsub("dg_pdt0621_2016.", "", nombres)
names(pdt_2016) <- nombres

pdt_2016$n_doc_declado <- as.character(pdt_2016$n_doc_declado)

# cuenta <- function(x){length(unique(x))}

# Calculo de las compras 
suma_na <- function(x){ sum(x,na.rm = T)}

pdt_2016$compras_nac_grav <- apply(pdt_2016[,c("cas107", "cas110", "cas113")], 1,suma_na)
pdt_2016$compras_imp_grav <- apply(pdt_2016[,c("cas114", "cas116", "cas119")], 1,suma_na)

pdt_2016$compras_nac_nograv <- ifelse(is.na(pdt_2016$cas120),0,pdt_2016$cas120)
pdt_2016$compras_imp_nograv <- ifelse(is.na(pdt_2016$cas122),0,pdt_2016$cas122)

pdt_2016$compras_nac <- pdt_2016$compras_nac_nograv + pdt_2016$compras_nac_grav
pdt_2016$compras_imp <- pdt_2016$compras_imp_nograv + pdt_2016$compras_imp_grav
pdt_2016$compras <- pdt_2016$compras_nac + pdt_2016$compras_imp

# Calculo de las ventas 
aux1 <- apply(pdt_2016[,c("cas100", "cas124", "cas160", "cas106")], 1,suma_na)
aux2 <- apply(pdt_2016[,c("cas102", "cas126", "cas162")], 1,suma_na)

pdt_2016$ventas_grav <- aux1-aux2
pdt_2016$ventas_nograv <- apply(pdt_2016[,c("cas105", "cas109")], 1, suma_na)
pdt_2016$ventas_otras <- apply(pdt_2016[,c("cas112")], 1, suma_na)
pdt_2016$ventas <- pdt_2016$ventas_grav + pdt_2016$ventas_nograv + pdt_2016$ventas_otras
pdt_2016$igv_debito <- ifelse(is.na(pdt_2016$cas131), 0, pdt_2016$cas131)
pdt_2016$igv_credito <- ifelse(is.na(pdt_2016$cas178), 0, pdt_2016$cas178)
pdt_2016$igv_imp <- ifelse(is.na(pdt_2016$cas115 + pdt_2016$cas115), 0, pdt_2016$cas115 + pdt_2016$cas115)


## PDT RESUMIDO
pdt_2016 <- pdt_2016[,c("n_orden", "periodo", "n_doc_declado", "cic", "f_presenta", "compras_nac_grav", 
                        "compras_imp_grav", "compras_nac_nograv", "compras_imp_nograv", "compras_nac", 
                        "compras_imp", "compras", "ventas", "igv_debito", "igv_credito", "igv_imp")]

## PDT FINAL - ULTIMA FECHA 
aux <- pdt_2016 %>% group_by(n_doc_declado, periodo) %>% summarise(n_orden = max(n_orden))
aux$llave <- paste(aux$n_doc_declado, aux$periodo, aux$n_orden)
pdt_2016$llave <- paste(pdt_2016$n_doc_declado, pdt_2016$periodo, pdt_2016$n_orden)

pdt_2016_uni <- merge(pdt_2016, aux[,"llave"], by = "llave")

contar_nozero <- function(x){sum(x>0)}

pdt_2016_res <- pdt_2016_uni %>% group_by(n_doc_declado) %>% summarise(ult_fecha = max(periodo),
                                                                       nro_pdt = n(),
                                                                       nro_compras_imp = contar_nozero(compras_imp),
                                                                       nro_compras_nac = contar_nozero(compras_nac),
                                                                       nro_compras = contar_nozero(compras),
                                                                       nro_ventas = contar_nozero(ventas),
                                                                       compras_imp = suma_na(compras_imp),
                                                                       compras_nac = suma_na(compras_nac),
                                                                       compras = suma_na(compras),
                                                                       ventas = suma_na(ventas),
                                                                       igv_debito = suma_na(igv_debito),
                                                                       igv_credito = suma_na(igv_credito),
                                                                       igv_imp = suma_na(igv_imp))
rm(aux, pdt_2016, pdt_2016_uni)


## PDT AÑO 2017
pdt_2017 <- fread("pdt0621_2017.csv")

nombres <- names(pdt_2017)
nombres <- gsub("dg_pdt0621_2017.", "", nombres)
names(pdt_2017) <- nombres

pdt_2017$n_doc_declado <- as.character(pdt_2017$n_doc_declado)

# cuenta <- function(x){length(unique(x))}

# Calculo de las compras 
suma_na <- function(x){ sum(x,na.rm = T)}

pdt_2017$compras_nac_grav <- apply(pdt_2017[,c("cas107", "cas110", "cas113")], 1,suma_na)
pdt_2017$compras_imp_grav <- apply(pdt_2017[,c("cas114", "cas116", "cas119")], 1,suma_na)

pdt_2017$compras_nac_nograv <- ifelse(is.na(pdt_2017$cas120),0,pdt_2017$cas120)
pdt_2017$compras_imp_nograv <- ifelse(is.na(pdt_2017$cas122),0,pdt_2017$cas122)

pdt_2017$compras_nac <- pdt_2017$compras_nac_nograv + pdt_2017$compras_nac_grav
pdt_2017$compras_imp <- pdt_2017$compras_imp_nograv + pdt_2017$compras_imp_grav
pdt_2017$compras <- pdt_2017$compras_nac + pdt_2017$compras_imp

# Calculo de las ventas 
aux1 <- apply(pdt_2017[,c("cas100", "cas124", "cas160", "cas106")], 1,suma_na)
aux2 <- apply(pdt_2017[,c("cas102", "cas126", "cas162")], 1,suma_na)

pdt_2017$ventas_grav <- aux1-aux2
pdt_2017$ventas_nograv <- apply(pdt_2017[,c("cas105", "cas109")], 1, suma_na)
pdt_2017$ventas_otras <- apply(pdt_2017[,c("cas112")], 1, suma_na)
pdt_2017$ventas <- pdt_2017$ventas_grav + pdt_2017$ventas_nograv + pdt_2017$ventas_otras
pdt_2017$igv_debito <- ifelse(is.na(pdt_2017$cas131), 0, pdt_2017$cas131)
pdt_2017$igv_credito <- ifelse(is.na(pdt_2017$cas178), 0, pdt_2017$cas178)
pdt_2017$igv_imp <- ifelse(is.na(pdt_2017$cas115 + pdt_2017$cas115), 0, pdt_2017$cas115 + pdt_2017$cas115)


## PDT RESUMIDO
pdt_2017 <- pdt_2017[,c("n_orden", "periodo", "n_doc_declado", "cic", "f_presenta", "compras_nac_grav", 
                        "compras_imp_grav", "compras_nac_nograv", "compras_imp_nograv", "compras_nac", 
                        "compras_imp", "compras", "ventas", "igv_debito", "igv_credito", "igv_imp")]

## PDT FINAL - ULTIMA FECHA 
aux <- pdt_2017 %>% group_by(n_doc_declado, periodo) %>% summarise(n_orden = max(n_orden))
aux$llave <- paste(aux$n_doc_declado, aux$periodo, aux$n_orden)
pdt_2017$llave <- paste(pdt_2017$n_doc_declado, pdt_2017$periodo, pdt_2017$n_orden)

pdt_2017_uni <- merge(pdt_2017, aux[,"llave"], by = "llave")

contar_nozero <- function(x){sum(x>0)}

pdt_2017_res <- pdt_2017_uni %>% group_by(n_doc_declado) %>% summarise(ult_fecha = max(periodo),
                                                                       nro_pdt = n(),
                                                                       nro_compras_imp = contar_nozero(compras_imp),
                                                                       nro_compras_nac = contar_nozero(compras_nac),
                                                                       nro_compras = contar_nozero(compras),
                                                                       nro_ventas = contar_nozero(ventas),
                                                                       compras_imp = suma_na(compras_imp),
                                                                       compras_nac = suma_na(compras_nac),
                                                                       compras = suma_na(compras),
                                                                       ventas = suma_na(ventas),
                                                                       igv_debito = suma_na(igv_debito),
                                                                       igv_credito = suma_na(igv_credito),
                                                                       igv_imp = suma_na(igv_imp))
rm(aux, pdt_2017, pdt_2017_uni)

## JUNTAMOS CON POLIZA CONSOLIDADO
poliza <- fread("poliza_15_16_17.csv")


# Para el 2015
poliza_15 <- poliza %>% filter(ano_prese == 15 & tipo_docum == 4)

aux <- poliza_15 %>% group_by(libr_tribu) %>% summarise(cant_imp = n(),
                                                        CIF_soles = sum(CIF_soles), 
                                                        FOB_soles = sum(fob_soles))
aux2 <- merge(pdt_2015_res, aux, by.x = "n_doc_declado", by.y = "libr_tribu", all = T)



dg_imp <- fread("dg_importadores.csv")
dg_imp <- dg_imp[,c("dg_importadores.importadores_ruc", "dg_importadores.razonsocial", "dg_importadores.regimen_2018", "dg_importadores.direccion")]
dg_imp$dg_importadores.estado <- is.character(dg_imp$dg_importadores.estado)
dg_imp$dg_importadores.estado <- ifelse(dg_imp$dg_importadores.estado == 0, "Activo", 
                                        ifelse(dg_imp$dg_importadores.estado == 1, "Baja provisional", 
                                               ifelse(dg_imp$dg_importadores.estado == 2, "Baja Provisional por Oficio", 
                                                      ifelse(dg_imp$dg_importadores.estado == 3, "Suspensión temporal", 
                                                             ifelse(dg_imp$dg_importadores.estado == 10, "Baja definitiva", 
                                                                    ifelse(dg_imp$dg_importadores.estado == 11, "Baja de oficio", 
                                                                           ifelse(dg_imp$dg_importadores.estado == 21, "Otros obligados")))))))

names(dg_imp) <- c("n_doc_declado", "nombre", "regimen", "direccion", "estado")
dg_imp$n_doc_declado <- as.character(dg_imp$n_doc_declado)
dg_imp <- unique(dg_imp)


pdt_2015_res_imp <- merge(aux2, dg_imp, by = "n_doc_declado")



# Para el 2016
poliza_16 <- poliza %>% filter(ano_prese == 16 & tipo_docum == 4)

aux <- poliza_16 %>% group_by(libr_tribu) %>% summarise(cant_imp = n(),
                                                        CIF_soles = sum(CIF_soles), 
                                                        FOB_soles = sum(fob_soles))
aux2 <- merge(pdt_2016_res, aux, by.x = "n_doc_declado", by.y = "libr_tribu", all = T)



dg_imp <- fread("dg_importadores.csv")
dg_imp <- dg_imp[,c("dg_importadores.importadores_ruc", "dg_importadores.razonsocial", "dg_importadores.regimen_2018", "dg_importadores.direccion")]
dg_imp$dg_importadores.estado <- is.character(dg_imp$dg_importadores.estado)
dg_imp$dg_importadores.estado <- ifelse(dg_imp$dg_importadores.estado == 0, "Activo", 
                                        ifelse(dg_imp$dg_importadores.estado == 1, "Baja provisional", 
                                               ifelse(dg_imp$dg_importadores.estado == 2, "Baja Provisional por Oficio", 
                                                      ifelse(dg_imp$dg_importadores.estado == 3, "Suspensión temporal", 
                                                             ifelse(dg_imp$dg_importadores.estado == 10, "Baja definitiva", 
                                                                    ifelse(dg_imp$dg_importadores.estado == 11, "Baja de oficio", 
                                                                           ifelse(dg_imp$dg_importadores.estado == 21, "Otros obligados")))))))

names(dg_imp) <- c("n_doc_declado", "nombre", "regimen", "direccion", "estado")
dg_imp$n_doc_declado <- as.character(dg_imp$n_doc_declado)
dg_imp <- unique(dg_imp)


pdt_2016_res_imp <- merge(aux2, dg_imp, by = "n_doc_declado")

# Para el 2017
poliza_17 <- poliza %>% filter(ano_prese == 17 & tipo_docum == 4)

aux <- poliza_17 %>% group_by(libr_tribu) %>% summarise(cant_imp = n(),
                                                        CIF_soles = sum(CIF_soles), 
                                                        FOB_soles = sum(fob_soles))
aux2 <- merge(pdt_2017_res, aux, by.x = "n_doc_declado", by.y = "libr_tribu", all = T)



dg_imp <- fread("dg_importadores.csv")
dg_imp <- dg_imp[,c("dg_importadores.importadores_ruc", "dg_importadores.razonsocial", "dg_importadores.regimen_2018", "dg_importadores.direccion")]
dg_imp$dg_importadores.estado <- is.character(dg_imp$dg_importadores.estado)
dg_imp$dg_importadores.estado <- ifelse(dg_imp$dg_importadores.estado == 0, "Activo", 
                                        ifelse(dg_imp$dg_importadores.estado == 1, "Baja provisional", 
                                               ifelse(dg_imp$dg_importadores.estado == 2, "Baja Provisional por Oficio", 
                                                      ifelse(dg_imp$dg_importadores.estado == 3, "Suspensión temporal", 
                                                             ifelse(dg_imp$dg_importadores.estado == 10, "Baja definitiva", 
                                                                    ifelse(dg_imp$dg_importadores.estado == 11, "Baja de oficio", 
                                                                           ifelse(dg_imp$dg_importadores.estado == 21, "Otros obligados")))))))

names(dg_imp) <- c("n_doc_declado", "nombre", "regimen", "direccion", "estado")
dg_imp$n_doc_declado <- as.character(dg_imp$n_doc_declado)
dg_imp <- unique(dg_imp)


pdt_2017_res_imp <- merge(aux2, dg_imp, by = "n_doc_declado")

## merge de los pdt_res_imp
nombres <- names(pdt_2015_res)
nombres <- paste(nombres, 2015, sep = "_")
nombres[1] <- "n_doc_declado"
names(pdt_2015_res) <- nombres

nombres <- names(pdt_2016_res)
nombres <- paste(nombres, 2016, sep = "_")
nombres[1] <- "n_doc_declado"
names(pdt_2016_res) <- nombres

nombres <- names(pdt_2017_res)
nombres <- paste(nombres, 2017, sep = "_")
nombres[1] <- "n_doc_declado"
names(pdt_2017_res) <- nombres

resumen_pdt <- merge(pdt_2015_res, pdt_2016_res, by = "n_doc_declado", all = T)
resumen_pdt <- merge(resumen_pdt, pdt_2017_res, by = "n_doc_declado", all = T)


write.csv(resumen_pdt, "resumen_pdt.csv", row.names = F)
## merge de los pdt_res_imp
pdt_res_imp <- merge(pdt_2016_res_imp, pdt_2017_res_imp, by = "n_doc_declado", all = T)
nombres <- names(pdt_res_imp)
nombres <- gsub(".x", "_2016", nombres)
nombres <- gsub(".y", "_2017", nombres)
names(pdt_res_imp) <- nombres




















#-------Resumen compras con tarjeta-----
rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)

vts_tc <- fread("vtas_tarj_16_17.csv")
nombres <- names(vts_tc)
nombres <- gsub("dg_tc_total1.", "", nombres)
names(vts_tc) <- nombres

options(scipen = 999)

# Cargamos tio de cambio 
tipo <- fread("tipo_cambio_periodo.csv")

vts_tc <- merge(vts_tc, tipo, by = "periodo")

vts_tc$mto_soles <- round(ifelse(vts_tc$tipo_moneda == "D", vts_tc$total_monto*vts_tc$`tipo cambio`, vts_tc$total_monto),2)

# aÑO 2016
vts_16 <- vts_tc %>% filter(substr(periodo, 1, 4)==2016)
aux <- vts_16 %>% group_by(ruc_establecimiento, tipo_tarjeta) %>% summarise(monto = sum(mto_soles))
aux1 <- aux %>% spread(tipo_tarjeta, monto)
names(aux1) <- c("ruc", "mto_C_16", "mto_D_16")
aux1$mto_C_16 <- ifelse(is.na(aux1$mto_C_16), 0, aux1$mto_C_16)
aux1$mto_D_16 <- ifelse(is.na(aux1$mto_D_16), 0, aux1$mto_D_16)
aux1$mto_total_16 <- aux1$mto_C_16 + aux1$mto_D_16
aux1 <- as.data.frame(aux1)

# aÑO 2017
vts_17 <- vts_tc %>% filter(substr(periodo, 1, 4)==2017)
aux <- vts_17 %>% group_by(ruc_establecimiento, tipo_tarjeta) %>% summarise(monto = sum(mto_soles))
aux2 <- aux %>% spread(tipo_tarjeta, monto)
names(aux2) <- c("ruc", "mto_C_17", "mto_D_17")
aux2$mto_C_17 <- ifelse(is.na(aux2$mto_C_17), 0, aux2$mto_C_17)
aux2$mto_D_17 <- ifelse(is.na(aux2$mto_D_17), 0, aux2$mto_D_17)
aux2$mto_total_17 <- aux2$mto_C_17 + aux2$mto_D_17

resumen_tarjetas <- merge(aux1, aux2, by = "ruc", all = T)
resumen_tarjetas$mto_C_16 <- ifelse(is.na(resumen_tarjetas$mto_C_16), 0, resumen_tarjetas$mto_C_16)
resumen_tarjetas$mto_D_16 <- ifelse(is.na(resumen_tarjetas$mto_D_16), 0, resumen_tarjetas$mto_D_16)
resumen_tarjetas$mto_total_16 <- ifelse(is.na(resumen_tarjetas$mto_total_16), 0, resumen_tarjetas$mto_total_16)
resumen_tarjetas$mto_C_17 <- ifelse(is.na(resumen_tarjetas$mto_C_17), 0, resumen_tarjetas$mto_C_17)
resumen_tarjetas$mto_D_17 <- ifelse(is.na(resumen_tarjetas$mto_D_17), 0, resumen_tarjetas$mto_D_17)
resumen_tarjetas$mto_total_17 <- ifelse(is.na(resumen_tarjetas$mto_total_17), 0, resumen_tarjetas$mto_total_17)

write.csv(resumen_tarjetas, "resumen_tarjetas.csv", row.names = F)

#-------Resumen RCC (revisar)------
rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)

rcc_cic <- fread("rcc_48k.csv")
rcc_cic$anio <- substr(rcc_cic$num_periodoinf, 1, 4)

rcc_2017 <- rcc_cic %>% filter(anio == 2017) %>% group_by(ruc) %>% summarise(prestamo = mean(prestamos), 
                                                                             automotriz = mean(automotriz),
                                                                             tarjeta_credito = mean(tarjeta_credito),
                                                                             hipotecario = mean(hipotecario),
                                                                             otros = mean(otros),
                                                                             Normal = sum(clasificacionSBS=="Normal"),
                                                                             CPP = sum(clasificacionSBS=="CPP"),
                                                                             Deficiente = sum(clasificacionSBS=="Deficiente"),
                                                                             Dudoso = sum(clasificacionSBS=="Dudoso"),
                                                                             Perdida = sum(clasificacionSBS=="Perdida"),
                                                                             total = n())

rcc_2017$Normal <- (rcc_2017$Normal)/(rcc_2017$total)
rcc_2017$CPP <- (rcc_2017$CPP)/(rcc_2017$total)
rcc_2017$Deficiente <- (rcc_2017$Deficiente)/(rcc_2017$total)
rcc_2017$Dudoso <- (rcc_2017$Dudoso)/(rcc_2017$total)
rcc_2017$Perdida <- (rcc_2017$Perdida)/(rcc_2017$total)
rcc_2017$ruc <- as.character(rcc_2017$ruc)

rcc_2017 <- rcc_2017[,-ncol(rcc_2017)]

write.csv(rcc_2017, "rcc_2017.csv", row.names = F)

#-------Construcción de Ratios----
#-------Consolidado de informacion---------
rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)

resumen_liquida <- fread("resumen_liquida.csv") #OK
# Ficha_Nacional <- read.csv("~/bases/Ficha_Nacional.csv", sep=";")

dg_importadores <- fread("dg_importadores.csv") 
nombres <- names(dg_importadores)
nombres <- gsub("dg_importadoresc.", "", nombres)
names(dg_importadores) <- nombres
dg_importadores$importadores_ruc <- as.character(dg_importadores$importadores_ruc)
dg_importadores$correo_1 <- gsub(" ", "", dg_importadores$correo_1)
dg_importadores$correo_2 <- gsub(" ", "", dg_importadores$correo_2)
dg_importadores$razonsocial <- gsub("  ", "", dg_importadores$razonsocial)

Importadores_Doc <- fread("importadores_doc.csv") #OK
nombres <- names(Importadores_Doc)
nombres <- gsub("importadores_doc.", "", nombres)
names(Importadores_Doc) <- nombres

regimen <- fread("vw_regimen201801.csv") 
names(regimen) <- c("num_ruc", "regimen")
regimen$num_ruc <- as.character(regimen$num_ruc)

resumen_importadores_1 <- fread("resumen_poliza.csv") #OK
# resumen_importadores_1.csv este es resumen_poliza.csv

resumen_pdt <- fread("resumen_pdt.csv") #OK
resumen_pdt$compras_imp_2015 <- as.numeric(resumen_pdt$compras_imp_2015)
resumen_pdt$compras_nac_2015 <- as.numeric(resumen_pdt$compras_nac_2015)
resumen_pdt$compras_2015 <- as.numeric(resumen_pdt$compras_2015)
resumen_pdt$compras_imp_2016 <- as.numeric(resumen_pdt$compras_imp_2016)
resumen_pdt$igv_credito_2016 <- as.numeric(resumen_pdt$igv_credito_2016)
resumen_pdt$compras_imp_2017 <- as.numeric(resumen_pdt$compras_imp_2017)
resumen_pdt$compras_nac_2017 <- as.numeric(resumen_pdt$compras_nac_2017)
resumen_pdt$compras_2017 <- as.numeric(resumen_pdt$compras_2017)
resumen_pdt$igv_debito_2017 <- as.numeric(resumen_pdt$igv_debito_2017)
resumen_pdt$igv_credito_2017 <- as.numeric(resumen_pdt$igv_credito_2017)

res_anual_itf <- fread("res_anual_itf.csv") #Ok
res_anual_itf$n_dcto_declado <- as.numeric(res_anual_itf$n_dcto_declado)
res_anual_itf$cod_14_2015 <- as.numeric(res_anual_itf$cod_14_2015)
res_anual_itf$cod_14_2016 <- as.numeric(res_anual_itf$cod_14_2016)
res_anual_itf$cod_14_2017 <- as.numeric(res_anual_itf$cod_14_2017)


resumen_tarjetas <- fread("resumen_tarjetas.csv") #OK 
resumen_tarjetas$ruc <- as.character(resumen_tarjetas$ruc)

indicadores_1 <- fread("indicadores_1.csv") # Falta Script 

#convirtiendo a data_frame
resumen_liquida <- as.data.frame(resumen_liquida)
# Ficha_Nacional <- as.data.frame(Ficha_Nacional)
dg_importadores <- as.data.frame(dg_importadores)
Importadores_Doc <- as.data.frame(Importadores_Doc)
regimen <- as.data.frame(regimen)
resumen_importadores_1<- as.data.frame(resumen_importadores_1)
resumen_pdt<- as.data.frame(resumen_pdt)
resumen_tarjetas<- as.data.frame(resumen_tarjetas)
res_anual_itf<-as.data.frame(res_anual_itf)
res_anual_itf$n_dcto_declado <- as.character(res_anual_itf$n_dcto_declado)
indicadores_1 <- as.data.frame(indicadores_1)
indicadores_1 <- indicadores_1[,-1]
indicadores_1 <- unique(indicadores_1)

#Utilizando merge para tener la tabla_consolidado en funcion a las tablas cargadas
data1 <- resumen_liquida
# data1<- merge (resumen_liquida,Ficha_Nacional, by.x = "libr_tribu", by.y= "b.libr_tribu", all.x = TRUE)
data2 <- merge (Importadores_Doc,dg_importadores, by.x ="libr_tribu", by.y = "importadores_ruc",all.x = TRUE)

data3 <- merge (data1,regimen, by.x = "libr_tribu", by.y = "num_ruc", all.x = TRUE)
data4 <- merge (data3,data2, by ="libr_tribu", all.y = TRUE)

data5 <- merge(data4,resumen_importadores_1, by= "libr_tribu", all.y = TRUE)
data6 <- merge(resumen_pdt,data5, by.x = "n_doc_declado", by.y = "libr_tribu",all= TRUE)
data7 <- merge(data6, res_anual_itf, by.x = "n_doc_declado", by.y = "n_dcto_declado",all= TRUE)

resumen_tarjetas$ruc <- as.character(resumen_tarjetas$ruc)
data8 <- merge(data7, resumen_tarjetas, by.x ="n_doc_declado", by.y = "ruc", all = TRUE )
data8 <- unique(data8)


data8 <- as.data.frame(data8)
data_consolidado <- merge(indicadores_1, data8, by.x ="importadores", by.y = "n_doc_declado")

View(data_consolidado)

# Limpiamos el entorno de trabajo
borrar <- ls()
borrar <- borrar[-which(borrar %in% "data_consolidado")]
rm(list = borrar)
rm(borrar)

#convitiendola a data frame
data_consolidado<- as.data.frame(data_consolidado)


#cambiando los  nombres de las cabeceras
nombres <- names(data_consolidado)
nombres[1] <- "ruc"
names(data_consolidado) <- nombres

#realización de los Porcentaje de Ajuste por periodo desde el 2015 al 2017
data_consolidado$Porce_ajuste2015 <- ifelse(is.na(data_consolidado$imp_15) | data_consolidado$imp_15==0, 0, 
                                            ifelse(is.na(data_consolidado$tiene_av_15),0, 
                                                   ifelse((data_consolidado$tiene_av_15/data_consolidado$imp_15)>1,1,data_consolidado$tiene_av_15/data_consolidado$imp_15)))

data_consolidado$Porce_ajuste2016 <- ifelse(is.na(data_consolidado$imp_16) | data_consolidado$imp_16==0, 0, 
                                            ifelse(is.na(data_consolidado$tiene_av_16),0, 
                                                   ifelse((data_consolidado$tiene_av_16/data_consolidado$imp_16)>1,1,data_consolidado$tiene_av_16/data_consolidado$imp_16)))

data_consolidado$Porce_ajuste2017 <- ifelse(is.na(data_consolidado$imp_17) | data_consolidado$imp_17==0, 0, 
                                            ifelse(is.na(data_consolidado$tiene_av_17),0, 
                                                   ifelse((data_consolidado$tiene_av_17/data_consolidado$imp_17)>1,1,data_consolidado$tiene_av_17/data_consolidado$imp_17)))


# extraccion de los que cumplen ser mayor a 0.30 en los 3 porcentajes_ajustados o en alguno de 2 años
condicion1 <- ifelse(data_consolidado$Porce_ajuste2015 >=0.3 & 
                       data_consolidado$Porce_ajuste2016 >=0.3 & 
                       data_consolidado$Porce_ajuste2017 >=0.3, 1,0 )

condicion2 <- ifelse(data_consolidado$Porce_ajuste2015 ==0 &
                       data_consolidado$Porce_ajuste2016 >=0.3 &
                       data_consolidado$Porce_ajuste2017 >=0.3, 1, 0)



condicion3 <- ifelse(data_consolidado$Porce_ajuste2015 >=0.3 &
                       data_consolidado$Porce_ajuste2016 ==0 &
                       data_consolidado$Porce_ajuste2017 >=0.3, 1, 0)

condicion4<- ifelse(data_consolidado$Porce_ajuste2015 >=0.3 &
                      data_consolidado$Porce_ajuste2016 >=0.3 &
                      data_consolidado$Porce_ajuste2017 ==0, 1, 0)

data_consolidado$sub <- condicion1+condicion2+condicion3+condicion4
data_consolidado$sub <- ifelse(data_consolidado$sub == 1 & (data_consolidado$liq__dol_15_prom_av > 1500 | 
                                                              data_consolidado$liq__dol_16_prom_av > 1500 | 
                                                              data_consolidado$liq__dol_17_prom_av > 1500),1,0)

data_consolidado$nosub <- ifelse(data_consolidado$Porce_ajuste2015==0 & data_consolidado$Porce_ajuste2016==0 & data_consolidado$Porce_ajuste2017==0, 1, 0)
data_consolidado$nosub <- ifelse(data_consolidado$nosub == 1 & (data_consolidado$CIF_dolar_media_15 > 1500 & data_consolidado$CIF_dolar_media_16 > 1500 & data_consolidado$CIF_dolar_media_17 > 1500), 1, 0)

table(data_consolidado$nosub)
table(data_consolidado$sub)

data_consolidado$condicion <- ifelse(data_consolidado$sub == 1, 1, 
                                     ifelse(data_consolidado$nosub ==1 , 0, NA))

write.csv(data_consolidado, "data_consolidado.csv", row.names = F)

#---------Clusterización------------
# Limpieza de consola
rm(list=ls())

library(data.table)
library(dplyr)
library(tidyr)

datos <- fread("data_consolidado.csv")
datos <- datos[,-1]
datos <- as.data.frame(datos)

variables1 <- c("nro_pdt_2015", "nro_compras_2015", "nro_ventas_2015", "compras_2015", "ventas_2015", "igv_debito_2015", "igv_credito_2015", 
                "nro_pdt_2016", "nro_compras_2016", "nro_ventas_2016", "compras_2016", "ventas_2016", "igv_debito_2016", "igv_credito_2016", 
                "nro_pdt_2017", "nro_compras_2017", "nro_ventas_2017", "compras_2017", "ventas_2017", "igv_debito_2017", "igv_credito_2017")

variables2 <- c("FOB_dolar_media_15", "FOB_dolar_media_16", "FOB_dolar_media_17",
                "ADV_media_15", "ADV_media_16", "ADV_media_17")#, 
# "tr_ne_mto_2015", "tr_ne_mto_2016", "tr_ne_mto_2017")

datos1 <- datos[,variables1]
datos2 <- datos[,variables2]

imputa_na <- function(x) {ifelse(is.na(x), 0, x)}

datos1 <- sapply(datos1, imputa_na)
datos1 <- as.data.frame(datos1)

# datos2$compras_prom_2015 <- ifelse(datos2$nro_compras_2015==0, 0, datos2$compras_2015/datos2$nro_compras_2015)
# datos2$compras_prom_2016 <- ifelse(datos2$nro_compras_2016==0, 0, datos2$compras_2016/datos2$nro_compras_2016)
# datos2$compras_prom_2017 <- ifelse(datos2$nro_compras_2017==0, 0, datos2$compras_2016/datos2$nro_compras_2017)
# 
# datos2$ventas_prom_2015 <- ifelse(datos2$nro_ventas_2015==0, 0, datos2$ventas_2015/datos2$nro_ventas_2015)
# datos2$ventas_prom_2016 <- ifelse(datos2$nro_ventas_2016==0, 0, datos2$ventas_2016/datos2$nro_ventas_2016)
# datos2$ventas_prom_2017 <- ifelse(datos2$nro_ventas_2017==0, 0, datos2$ventas_2016/datos2$nro_ventas_2017)

datos1$compras_prom_2015 <- (datos1$compras_2015/datos1$nro_compras_2015)
datos1$compras_prom_2016 <- (datos1$compras_2016/datos1$nro_compras_2016)
datos1$compras_prom_2017 <- (datos1$compras_2017/datos1$nro_compras_2017)

datos1$ventas_prom_2015 <- (datos1$ventas_2015/datos1$nro_ventas_2015)
datos1$ventas_prom_2016 <- (datos1$ventas_2016/datos1$nro_ventas_2016)
datos1$ventas_prom_2017 <- (datos1$ventas_2017/datos1$nro_ventas_2017)

prom_sin_na <- function(x) { mean(x, na.rm = T)}

quitar <- c("compras_2015", "nro_compras_2015", "ventas_2015", "nro_ventas_2015","igv_debito_2015", "igv_credito_2015", 
            "compras_2016", "nro_compras_2016", "ventas_2016", "nro_ventas_2016","igv_debito_2016", "igv_credito_2016", 
            "compras_2017", "nro_compras_2017", "ventas_2017", "nro_ventas_2017","igv_debito_2017", "igv_credito_2017")

datos1 <- datos1[, -which(names(datos1) %in% quitar)]

datos1$nro_pdt_prom <- apply(datos1[, c("nro_pdt_2015", "nro_pdt_2016", "nro_pdt_2017")], 1, mean)
datos1$compras_prom <- apply(datos1[, c("compras_prom_2015", "compras_prom_2016", "compras_prom_2017")], 1, prom_sin_na)
datos1$compras_prom <- ifelse(is.nan(datos1$compras_prom), 0, datos1$compras_prom)
datos1$ventas_prom <- apply(datos1[, c("ventas_prom_2015", "ventas_prom_2016", "ventas_prom_2017")], 1, prom_sin_na)
datos1$ventas_prom <- ifelse(is.nan(datos1$ventas_prom), 0, datos1$ventas_prom)
datos1$ventas_prom <- ifelse(is.infinite(datos1$ventas_prom), 0, datos1$ventas_prom)
datos1[29580,3] <- 0

datos1 <- datos1[,10:12]

datos2$fob_prom <- apply(datos2[, c("FOB_dolar_media_15", "FOB_dolar_media_16", "FOB_dolar_media_17")], 1, prom_sin_na)
datos2$fob_prom <- ifelse(is.na(datos2$fob_prom), 0, datos2$fob_prom)
datos2$adv_prom <- apply(datos2[, c("ADV_media_15", "ADV_media_16", "ADV_media_17")], 1, prom_sin_na)
datos2$adv_prom <- ifelse(is.na(datos2$adv_prom), 0, datos2$adv_prom)
# datos2$tn_prom <- apply(datos2[, c("tr_ne_mto_2015", "tr_ne_mto_2016", "tr_ne_mto_2017")], 1, prom_sin_na)
# datos2$tn_prom <- ifelse(is.na(datos2$tn_prom), 0, datos2$tn_prom)
# datos2$tn_prom <- ifelse(datos2$tn_prom<0, 0, datos2$tn_prom)

datos2 <- datos2[,7:8]

datos_seg <- cbind(datos1, datos2)

summary(datos_seg)

par(mfrow = c(2,3))
hist(datos_seg$nro_pdt_prom)
hist(datos_seg$compras_prom)
hist(datos_seg$ventas_prom)
hist(datos_seg$fob_prom)
hist(datos_seg$adv_prom)
# hist(datos_seg$tn_prom)
par(mfrow = c(1,1))

aux <- datos_seg

aux$compras_prom <- log(aux$compras_prom+1)
aux$ventas_prom <- log(aux$ventas_prom+1)
aux$fob_prom <- log(aux$fob_prom+1)
aux$adv_prom <- log(aux$adv_prom+1)
# aux$tn_prom <- log(aux$tn_prom+1)

par(mfrow = c(2,3))
hist(aux$nro_pdt_prom)
hist(aux$compras_prom)
hist(aux$ventas_prom)
hist(aux$fob_prom)
hist(aux$adv_prom)
# hist(aux$tn_prom)
par(mfrow = c(1,1))

maximo <- apply(aux, 2, max)
minimo <- apply(aux, 2, min)
aux_esc <- scale(aux,center = minimo, scale = maximo-minimo)

library(corrplot)
library(ggplot2)
corrplot(cor(aux_esc))

wss <- 0
for (i in 1:30){
  set.seed(2018)
  wss[i] <- sum(kmeans(aux_esc[,-1], centers=i, iter.max = 50000)$withinss)
}
plot(1:30, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

set.seed(666)
cluster <- kmeans(aux_esc[,-1], 7)
cluster$size

aux$cluster <- cluster$cluster
ggplot(data = filter(aux,cluster!=10), aes(x=fob_prom, y=ventas_prom, col=as.factor(cluster)))+
  geom_point()


set.seed(666)
nclust <- 5
model <- m.kms <- kmeans(aux_esc[,-1], nclust)
model$size
dscm <- melt(model$centers)
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

datos_seg$cluster <- model$cluster
resumen <- aggregate(.~cluster, datos_seg, mean)
resumen <- round(resumen, 2)
resumen$size <- model$size
resumen

datos$cluster <- model$cluster

write.csv(datos, "datos_consolidado_cluster.csv", row.names = F)
write.csv(datos_seg, "datos_segm_cluster.csv", row.names = F)
write.csv(resumen, "centroides_cluster_media.csv", row.names = F)

#---------Solo Variables importantes-------
rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)

memory.limit(size = 12000)

datos <- fread("datos_consolidado_cluster.csv",stringsAsFactors = T)
datos$ruc <- as.character(datos$ruc)

variables <- c("ruc", "CIF_dolar_min_17","CIF_dolar_mediana_17","ADV_media_15","ADV_media_16","ADV_media_17",
               "nac_en_mto_2015","nac_en_mto_2016","nac_en_mto_2017",
               "tr_ne_mto_2017", "ventas_2017", "compras_2017","tr_ne_mto_2016", "ventas_2016", "compras_2016","tr_ne_mto_2015", "ventas_2015", "compras_2015",
               "I_M_Ventas_PDT_2015","I_M_Ventas_PDT_2016","I_M_compras_tot_2015","I_M_compras_tot_2016", "I_IGVAdu_IGDDeb2015", 
               "mto_total_16","mto_total_17",
               "igv_credito_2015", "igv_credito_2016", "igv_credito_2017", "igv_debito_2015", "igv_debito_2016", "igv_debito_2017",
               "dependencia","segmento","antiguedadempresa","tipoempresa_tipo_cod","ubigeo","pricos_mepecos",
               "cant_trab4ta_2016", "cant_trab4ta_2017", "cant_trab5ta_2016", "cant_trab5ta_2017",
               "tipoactividade_cod", 
               "regimen_2018", "condicion", "cluster")
datos <- as.data.frame(datos)
datos <- datos[,variables]

datos$compras_2017 <- as.numeric(datos$compras_2017)
datos$compras_2015 <- as.numeric(datos$compras_2015)
datos$igv_credito_2016 <- as.numeric(datos$igv_credito_2016)
datos$igv_credito_2017 <- as.numeric(datos$igv_credito_2017)
datos$igv_debito_2017 <- as.numeric(datos$igv_debito_2017)

datos$CIF_dolar_min_17<-ifelse(is.na(datos$CIF_dolar_min_17),0,datos$CIF_dolar_min_17)
datos$CIF_dolar_mediana_17<-ifelse(is.na(datos$CIF_dolar_mediana_17),0,datos$CIF_dolar_mediana_17)
datos$ADV_media_15<-ifelse(is.na(datos$ADV_media_15),0,datos$ADV_media_15)
datos$ADV_media_16<-ifelse(is.na(datos$ADV_media_16),0,datos$ADV_media_16)
datos$ADV_media_17<-ifelse(is.na(datos$ADV_media_17),0,datos$ADV_media_17)
datos$nac_en_mto_2015<-ifelse(is.na(datos$nac_en_mto_2015),0,datos$nac_en_mto_2015)
datos$nac_en_mto_2016<-ifelse(is.na(datos$nac_en_mto_2016),0,datos$nac_en_mto_2016)
datos$nac_en_mto_2017<-ifelse(is.na(datos$nac_en_mto_2017),0,datos$nac_en_mto_2017)
datos$tr_ne_mto_2017<-ifelse(is.na(datos$tr_ne_mto_2017),0,datos$tr_ne_mto_2017)
datos$ventas_2017<-ifelse(is.na(datos$ventas_2017),0,datos$ventas_2017)
datos$compras_2017<-ifelse(is.na(datos$compras_2017),0,datos$compras_2017)
datos$tr_ne_mto_2016<-ifelse(is.na(datos$tr_ne_mto_2016),0,datos$tr_ne_mto_2016)
datos$ventas_2016<-ifelse(is.na(datos$ventas_2016),0,datos$ventas_2016)
datos$compras_2016<-ifelse(is.na(datos$compras_2016),0,datos$compras_2016)
datos$tr_ne_mto_2015<-ifelse(is.na(datos$tr_ne_mto_2015),0,datos$tr_ne_mto_2015)
datos$ventas_2015<-ifelse(is.na(datos$ventas_2015),0,datos$ventas_2015)
datos$compras_2015<-ifelse(is.na(datos$compras_2015),0,datos$compras_2015)
datos$mto_total_16<-ifelse(is.na(datos$mto_total_16),0,datos$mto_total_16)
datos$mto_total_17<-ifelse(is.na(datos$mto_total_17),0,datos$mto_total_17)
datos$igv_credito_2015<-ifelse(is.na(datos$igv_credito_2015),0,datos$igv_credito_2015)
datos$igv_credito_2016<-ifelse(is.na(datos$igv_credito_2016),0,datos$igv_credito_2016)
datos$igv_credito_2017<-ifelse(is.na(datos$igv_credito_2017),0,datos$igv_credito_2017)
datos$igv_debito_2015<-ifelse(is.na(datos$igv_debito_2015),0,datos$igv_debito_2015)
datos$igv_debito_2016<-ifelse(is.na(datos$igv_debito_2016),0,datos$igv_debito_2016)
datos$igv_debito_2017<-ifelse(is.na(datos$igv_debito_2017),0,datos$igv_debito_2017)
datos$cant_trab4ta_2016<-ifelse(is.na(datos$cant_trab4ta_2016),0,1)
datos$cant_trab4ta_2017<-ifelse(is.na(datos$cant_trab4ta_2017),0,1)
datos$cant_trab5ta_2016<-ifelse(is.na(datos$cant_trab5ta_2016),0,1)
datos$cant_trab5ta_2017<-ifelse(is.na(datos$cant_trab5ta_2017),0,1)

datos$Itn_pc2015 <- datos$tr_ne_mto_2015/(datos$ventas_2015-datos$compras_2015)
datos$Itn_pc2015_cat <- ifelse(datos$tr_ne_mto_2015 ==0 & datos$ventas_2015 ==0 & datos$compras_2015==0, "zeros",
                               ifelse(datos$tr_ne_mto_2015 !=0 & datos$ventas_2015 ==0 & datos$compras_2015==0, "itf_nvnc",
                                      ifelse(datos$tr_ne_mto_2015 ==0 & datos$ventas_2015 !=0 & datos$compras_2015!=0, "nitf_cv","ind")))
datos$Itn_pc2016 <- datos$tr_ne_mto_2016/(datos$ventas_2016-datos$compras_2016)
datos$Itn_pc2016_cat <- ifelse(datos$tr_ne_mto_2016 ==0 & datos$ventas_2016 ==0 & datos$compras_2016==0, "zeros",
                               ifelse(datos$tr_ne_mto_2016 !=0 & datos$ventas_2016 ==0 & datos$compras_2016==0, "itf_nvnc",
                                      ifelse(datos$tr_ne_mto_2016 ==0 & datos$ventas_2016 !=0 & datos$compras_2016!=0, "nitf_cv","ind")))
datos$Itn_pc2017 <- datos$tr_ne_mto_2017/(datos$ventas_2017-datos$compras_2017)
datos$Itn_pc2017_cat <- ifelse(datos$tr_ne_mto_2017 ==0 & datos$ventas_2017 ==0 & datos$compras_2017==0, "zeros",
                               ifelse(datos$tr_ne_mto_2017 !=0 & datos$ventas_2017 ==0 & datos$compras_2017==0, "itf_nvnc",
                                      ifelse(datos$tr_ne_mto_2017 ==0 & datos$ventas_2017 !=0 & datos$compras_2017!=0, "nitf_cv","ind")))


datos$ratio_igvcred_igvdeb_2015 <- ifelse(datos$igv_credito_2015==0 & datos$igv_debito_2015==0, 0, datos$igv_credito_2015/datos$igv_debito_2015)
valor <- max((datos %>% filter(!is.infinite(ratio_igvcred_igvdeb_2015)))$ratio_igvcred_igvdeb_2015)
datos$ratio_igvcred_igvdeb_2015 <- ifelse(datos$ratio_igvcred_igvdeb_2015==Inf, valor, datos$ratio_igvcred_igvdeb_2015)

datos$ratio_igvcred_igvdeb_2016 <- ifelse(datos$igv_credito_2016==0 & datos$igv_debito_2016==0, 0, datos$igv_credito_2016/datos$igv_debito_2016)
valor <- max((datos %>% filter(!is.infinite(ratio_igvcred_igvdeb_2016)))$ratio_igvcred_igvdeb_2016)
datos$ratio_igvcred_igvdeb_2016 <- ifelse(datos$ratio_igvcred_igvdeb_2016==Inf, valor, datos$ratio_igvcred_igvdeb_2016)

datos$ratio_igvcred_igvdeb_2017 <- ifelse(datos$igv_credito_2017==0 & datos$igv_debito_2017==0, 0, datos$igv_credito_2017/datos$igv_debito_2017)
valor <- max((datos %>% filter(!is.infinite(ratio_igvcred_igvdeb_2017)))$ratio_igvcred_igvdeb_2017)
datos$ratio_igvcred_igvdeb_2017 <- ifelse(datos$ratio_igvcred_igvdeb_2017==Inf, valor, datos$ratio_igvcred_igvdeb_2017)

quitar <- c("dependencia", "antiguedadempresa", "ubigeo", "tipoactividade_cod")
datos <- datos[,-which(names(datos) %in% quitar)]

# Cargamos parche
parche_dg_imp <- fread("dg_imp_reemplazo.csv", colClasses = c("Character", "factor", "factor", 
                                                              "factor", "numeric", "factor", "factor"))
datos <- merge(datos, parche_dg_imp, by = "ruc")

datos1 <- datos %>% filter(condicion == 1 | condicion == 0)
datos2 <- datos %>% filter(is.na(condicion))

datos$dec_trab_16 <- ifelse(datos$cant_trab4ta_2016 == 1 | datos$cant_trab5ta_2016 == 1, 1, 0)
datos$dec_trab_17 <- ifelse(datos$cant_trab4ta_2017 == 1 | datos$cant_trab5ta_2017 == 1, 1, 0)

quitar <- c("cant_trab4ta_2016", "cant_trab4ta_2017", "cant_trab5ta_2016", "cant_trab5ta_2017")
datos <- datos[,-which(names(datos) %in% quitar)]

write.csv(datos, "datos_importadores_25_04.csv", row.names = F)

#------Extrapolamos a todo los demás--------
rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)
library(dummies)
library(rpart)
library(randomForest)

datos <- fread("datos_importadores_25_04.csv")
datos <- as.data.frame(datos)
datos$condicion <- as.factor(datos$condicion)

datos1 <- datos %>% filter(condicion == 1 | condicion == 0)
datos2 <- datos %>% filter(is.na(condicion))

# Tratamos datos1
quitar <- c("desc_ubigeo", "act_principal", "Itn_pc2015", "Itn_pc2016", "Itn_pc2017", 
            "tr_ne_mto_2015", "tr_ne_mto_2016", "tr_ne_mto_2017")
datos1 <- datos1[,-which(names(datos) %in% quitar)]

# Análisis de Ubigeo 
aux1 <- as.data.frame(dummy(datos1$ubigeo))
nombres <- names(aux1)
nombres <- gsub(")", "", nombres)
names(aux1) <- nombres

aux1 <- cbind(aux1, condicion=datos1$condicion)

bosque <- randomForest(condicion ~ ., data=aux1)
varImpPlot(bosque)
variables_1 <- bosque$importance
variables_1 <- as.data.frame(cbind(row.names(variables_1), variables_1))
variables_1$MeanDecreaseGini <- as.numeric(as.character((variables_1$MeanDecreaseGini)))
variables_1 <- arrange(variables_1, -MeanDecreaseGini)
variables_ubi <- as.character(variables_1$V1[1:5])

# Análisis de actividad principal 
aux2 <- as.data.frame(dummy(datos1$cod_act_princ))
nombres <- names(aux2)
nombres <- gsub(")", "", nombres)
names(aux2) <- nombres

aux2 <- cbind(aux2, condicion=datos1$condicion)

bosque <- randomForest(condicion ~ ., data=aux2)
varImpPlot(bosque)
variables_2 <- bosque$importance
variables_2 <- as.data.frame(cbind(row.names(variables_2), variables_2))
variables_2$MeanDecreaseGini <- as.numeric(as.character((variables_2$MeanDecreaseGini)))
variables_2 <- arrange(variables_2, -MeanDecreaseGini)
variables_act <- as.character(variables_2$V1[1:5])

# Solo ns quedamos con los ubigeos importantes
datos$ubigeo_150101 <- ifelse(datos$ubigeo == 150101,1,0)
datos$ubigeo_150115 <- ifelse(datos$ubigeo == 150115,1,0)
datos$ubigeo_230101 <- ifelse(datos$ubigeo == 230101,1,0)
datos$ubigeo_211101 <- ifelse(datos$ubigeo == 211101,1,0)
datos$ubigeo_150140 <- ifelse(datos$ubigeo == 150140,1,0)

datos$act_51313 <- ifelse(datos$cod_act_princ == 51313,1,0)
datos$act_52322 <- ifelse(datos$cod_act_princ == 52322,1,0)
datos$act_51906 <- ifelse(datos$cod_act_princ == 51906,1,0)
datos$act_51395 <- ifelse(datos$cod_act_princ == 51395,1,0)
datos$act_18100 <- ifelse(datos$cod_act_princ == 18100,1,0)

quitar <- c("ubigeo", "cod_act_princ")
datos <- datos[,-which(names(datos) %in% quitar)]

# Guardamos las variables
write.csv(datos, "datos_impor_ubact_cat.csv", row.names = F)

#------Pegamos RCC-------
rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(rpart.plot)
library(rpart)
library(nnet)
library(corrplot)
library(ROCR)
library(caret)

datos <- fread("datos_impor_ubact_cat.csv", stringsAsFactors = T)
datos <- as.data.frame(datos)
aux <- as.data.frame(sapply(datos, class))
(row.names(aux))[which(aux=="integer64")]
datos$compras_2017 <- as.numeric(datos$compras_2017)
datos$compras_2015 <- as.numeric(datos$compras_2015)
datos$igv_credito_2016 <- as.numeric(datos$igv_credito_2016)
datos$igv_credito_2017 <- as.numeric(datos$igv_credito_2017)
datos$igv_debito_2017 <- as.numeric(datos$igv_debito_2017)

datos <- datos[,-which(names(datos) %in% c("igv_credito_2015", "igv_debito_2015",
                                           "igv_credito_2016", "igv_debito_2016",
                                           "igv_credito_2017", "igv_debito_2017",
                                           "Itn_pc2015", "Itn_pc2016", "Itn_pc2017"))]
rcc <- fread("rcc_2017.csv")

datos1 <- merge(datos, rcc, by = "ruc", all.x = T)
datos1$prestamo <- ifelse(is.na(datos1$prestamo), 0, datos1$prestamo)
datos1$automotriz <- ifelse(is.na(datos1$automotriz), 0, datos1$automotriz)
datos1$tarjeta_credito <- ifelse(is.na(datos1$tarjeta_credito), 0, datos1$tarjeta_credito)
datos1$hipotecario <- ifelse(is.na(datos1$hipotecario), 0, datos1$hipotecario)
datos1$otros <- ifelse(is.na(datos1$otros), 0, datos1$otros)
datos1$sin_registros <- ifelse(is.na(datos1$Normal) | is.na(datos1$CPP) | is.na(datos1$Deficiente) | is.na(datos1$Dudoso) | is.na(datos1$Perdida), 1,0)
datos1$Normal <- ifelse(is.na(datos1$Normal), 0, datos1$Normal)
datos1$CPP <- ifelse(is.na(datos1$CPP), 0, datos1$CPP)
datos1$Deficiente <- ifelse(is.na(datos1$Deficiente), 0, datos1$Deficiente)
datos1$Dudoso <- ifelse(is.na(datos1$Dudoso), 0, datos1$Dudoso)
datos1$Perdida <- ifelse(is.na(datos1$Perdida), 0, datos1$Perdida)
datos1$segmento <- ifelse(datos1$segmento=="PEQUEÃ‘O", "PEQUENIO", datos1$segmento)

valor <- as.numeric(datos1 %>% filter(!is.infinite(ratio_igvcred_igvdeb_2015)) %>% summarise(max(ratio_igvcred_igvdeb_2015)))
datos1$ratio_igvcred_igvdeb_2015 <- ifelse(is.infinite(datos1$ratio_igvcred_igvdeb_2015), valor, datos1$ratio_igvcred_igvdeb_2015)

valor <- as.numeric(datos1 %>% filter(!is.infinite(ratio_igvcred_igvdeb_2016)) %>% summarise(max(ratio_igvcred_igvdeb_2016)))
datos1$ratio_igvcred_igvdeb_2016 <- ifelse(is.infinite(datos1$ratio_igvcred_igvdeb_2016), valor, datos1$ratio_igvcred_igvdeb_2016)

valor <- as.numeric(datos1 %>% filter(!is.infinite(ratio_igvcred_igvdeb_2017)) %>% summarise(max(ratio_igvcred_igvdeb_2017)))
datos1$ratio_igvcred_igvdeb_2017 <- ifelse(is.infinite(datos1$ratio_igvcred_igvdeb_2017), valor, datos1$ratio_igvcred_igvdeb_2017)

datos1 <- datos1[,-which(names(datos1) %in% c("desc_ubigeo", "act_principal"))]
datos1$condicion <- as.factor(datos1$condicion)


df_modelar <- datos1 %>% filter(condicion == 1 | condicion == 0)
df_evaluar <- datos1 %>% filter(is.na(condicion))

arbol <- rpart(condicion ~ ., data=df_modelar[,-1])
variables <- row.names(as.data.frame(arbol$variable.importance))[1:30]

bosque <- randomForest(condicion ~ ., data=df_modelar[,-1])
variables_bosque <- row.names(as.data.frame(varImp(bosque)))[1:40]
varImpPlot(bosque)

#preparamos variables 

# ADV promedio de los ultimos 3 años 
datos1$adv_prom <- (datos1$ADV_media_15 + datos1$ADV_media_16 + datos1$ADV_media_17)/3
# entradas nacionales 
datos1$nac_en_prom <- (datos1$nac_en_mto_2015 + datos1$nac_en_mto_2016 + datos1$nac_en_mto_2017)/3
# entradas nacionales 
datos1$tr_ne_prom <- (datos1$tr_ne_mto_2015 + datos1$tr_ne_mto_2016 + datos1$tr_ne_mto_2017)/3
# Ventas promedio
datos1$ventas_Prom <- (datos1$ventas_2015 + datos1$ventas_2016 + datos1$ventas_2017)/3
# Ventas promedio
datos1$compras_Prom <- (datos1$compras_2015 + datos1$compras_2016 + datos1$compras_2017)/3
# Ratio promedio
datos1$ratio_igvcred_igvdeb_prom <- (datos1$ratio_igvcred_igvdeb_2015 + datos1$ratio_igvcred_igvdeb_2016 + datos1$ratio_igvcred_igvdeb_2017)/3
# Montos promedio (ventas con tarjeta)
datos1$mto_total_prom <- (datos1$mto_total_16 + datos1$mto_total_17)/2
# variabilidad mediana - min
datos1$rango_med_min <- (datos1$CIF_dolar_mediana_17 - datos1$CIF_dolar_min_17)
# Declara trab
datos1$decla_trab <- ifelse(datos1$dec_trab_16 == 1 | datos1$dec_trab_17 == 1, 1, 0)
# Ratio M / compras
datos1$I_M_compras <- (datos1$I_M_compras_tot_2015 + datos1$I_M_compras_tot_2016)/2
# Ratio M / ventas
datos1$I_M_ventas <- (datos1$I_M_Ventas_PDT_2015 + datos1$I_M_Ventas_PDT_2016)/2


quitar <- c("ADV_media_15", "ADV_media_16", "ADV_media_17", "nac_en_mto_2015", "nac_en_mto_2016", "nac_en_mto_2017",
            "tr_ne_mto_2015", "tr_ne_mto_2016", "tr_ne_mto_2017", "ventas_2015", "ventas_2016", "ventas_2017",
            "compras_2015", "compras_2016", "compras_2017", "ratio_igvcred_igvdeb_2015", "ratio_igvcred_igvdeb_2016",
            "ratio_igvcred_igvdeb_2017", "mto_total_16", "mto_total_17", "CIF_dolar_mediana_17", "CIF_dolar_min_17", 
            "dec_trab_17", "dec_trab_16", "I_M_compras_tot_2015", "I_M_compras_tot_2016", "I_M_Ventas_PDT_2015",
            "I_M_Ventas_PDT_2016")
datos1 <- datos1[, -which(names(datos1) %in% quitar)]

df_modelar <- datos1 %>% filter(condicion == 1 | condicion == 0)
df_evaluar <- datos1 %>% filter(is.na(condicion))
bosque <- randomForest(condicion ~ ., data=df_modelar[,-1])
variables_bosque <- as.data.frame(varImp(bosque))
variables_bosque$var <- row.names(variables_bosque)
variables_bosque <- arrange(variables_bosque, -Overall)

varImpPlot(bosque)

variables_final <- variables_bosque[1:30,2]
write.csv(variables_bosque, "variables importantes.csv", row.names = F)

datos2 <- datos1[, c(variables_final, "condicion")]
datos2 <- datos2[,-which(names(datos2) %in% c("Itn_pc2015_cat", "Itn_pc2016_cat", "dependencia", "tipoempresa_tipo_cod",
                                              "Itn_pc2017_cat", "regimen_2018", "segmento"))]
datos2$condicion <- as.numeric(as.character(datos2$condicion))
datos2 <- datos2 %>% filter(!is.na(condicion))


library(corrplot)
corrplot(cor(datos2))

#---------Posibles subvauadores------
library(rpart)
library(randomForest)
library(caret)
library(nnet)

# variables_final <- variables_final[4:30]

variables_final <- variables_final[-which(variables_final %in% c("adv_prom", "act_51313", "ubigeo_150101", "act_52322", "act_51906", "ubigeo_150115"))]


datos2 <- datos1[, c("ruc", variables_final, "condicion")]
nivel_tipoemp <- levels(datos2$tipoempresa_tipo_cod)

df_modelar <- datos2 %>% filter(condicion == 1 | condicion == 0)
df_evaluar <- datos2 %>% filter(is.na(condicion))

# VALIDACIÓN CRUZADA
# Creamos los grupos(K-Folds), separa a la data en 10 grupos diferentes
# set.seed(1) # Prefijamos una semilla para tener resultados reproducibles
folds <- 10 # 10 Grupos
df_modelar$kfold <- sample(1:folds, nrow(df_modelar), replace = T)
niveles <- levels(datos)
especificidad <- data.frame(NULL)
sensibilidad <- data.frame(NULL)
gini <- data.frame(NULL)
for (i in 1:folds){
  #i=3
  test <- subset(df_modelar[,-1], kfold == i)
  train <- subset(df_modelar[,-1], !kfold == i)
  t1 <- train %>% filter(condicion==1)
  t2 <- train %>% filter(condicion==0)
  indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
  t1 <- rbind(t1, t1[indice,])
  train <- rbind(t1,t2)
  
  # BOSQUE ALEATORIO
  bosque <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("kfold"))], ntree=10, mtry= 3)
  bosque_pred <- predict(bosque, test, type = "response") # Predicción sobre test
  # Matriz de confusión
  table(bosque_pred,test$condicion)
  confusionMatrix(bosque_pred,test$condicion,positive="1")
  Espec_BA <- confusionMatrix(bosque_pred,test$condicion,positive="1")$byClass[2]
  Sens_BA <- confusionMatrix(bosque_pred,test$condicion,positive="1")$byClass[1]
  auc_BA <- confusionMatrix(bosque_pred,test$condicion,positive="1")$overall[1]
  gini_BA <- 2*auc_BA-1
  
  # ARBOL DE CLASIFICACION
  arbol <- rpart(condicion ~ ., data=train[,-which(names(train) %in% c("kfold"))], method = "class")
  arb_pred <- predict(arbol,test, type = "class")
  # Matriz de confusión
  table(arb_pred,test$condicion)
  confusionMatrix(arb_pred,test$condicion,positive="1")
  Espec_AC <- confusionMatrix(arb_pred,test$condicion,positive="1")$byClass[2]
  Sens_AC <- confusionMatrix(arb_pred,test$condicion,positive="1")$byClass[1]
  auc_AC <- confusionMatrix(arb_pred,test$condicion,positive="1")$overall[1]
  gini_AC <- 2*auc_AC-1
  
  
  # REDES NEURONALES
  red_neu <- nnet(condicion~., data = train[,-which(names(train) %in% c("kfold"))],size = 7, rang = 0.1,decay = 5e-10, maxit = 5000)
  red_pred <- predict(red_neu,test)
  red_pred_clase <- factor(ifelse(red_pred>0.5,1,0))
  levels(red_pred_clase) <- c(0,1)
  # Matriz de confusión
  table(red_pred_clase,test$condicion)
  confusionMatrix(red_pred_clase,test$condicion,positive="1")
  Espec_RN <- confusionMatrix(red_pred_clase,test$condicion,positive="1")$byClass[2]
  Sens_RN <- confusionMatrix(red_pred_clase,test$condicion,positive="1")$byClass[1]
  auc_RN <- confusionMatrix(red_pred_clase,test$condicion,positive="1")$overall[1]
  gini_RN <- 2*auc_RN-1
  especificidad <- rbind(especificidad, data.frame(iteracion = i, BosqueAleatorio = Espec_BA,ArbolClasificacion = Espec_AC, RedNeuronal = Espec_RN))
  sensibilidad <- rbind(sensibilidad, data.frame(iteracion = i, BosqueAleatorio = Sens_BA,ArbolClasificacion = Sens_AC, RedNeuronal = Sens_RN))
  gini <- rbind(gini, data.frame(iteracion = i, BosqueAleatorio = auc_BA,ArbolClasificacion = auc_AC, RedNeuronal = auc_RN))
  print(paste("Fin de Iteración",i))
  
  # # NAIVE BAYES 
  # na_ba <- nnet(condicion~., data = train,size = 7, rang = 0.1,decay = 5e-10, maxit = 5000)
  # na_ba_pna_ba <- pna_baict(na_ba,test)
  # na_ba_pna_ba_clase <- factor(ifelse(na_ba_pna_ba>0.5,1,0))
  # levels(na_ba_pna_ba_clase) <- c(0,1)
  # # Matriz de confusión
  # table(na_ba_pna_ba_clase,test$condicion)
  # confusionMatrix(na_ba_pna_ba_clase,test$condicion,positive="1")
  # Espec_RN <- confusionMatrix(na_ba_pna_ba_clase,test$condicion,positive="1")$byClass[2]
  # auc_RN <- confusionMatrix(na_ba_pna_ba_clase,test$condicion,positive="1")$overall[1]
  # gini_RN <- 2*auc_RN-1
  # especificidad <- rbind(especificidad, data.frame(iteracion = i, BosqueAleatorio = Espec_BA,ArbolClasificacion = Espec_AC, na_baNeuronal = Espec_RN))
  # gini <- rbind(gini, data.frame(iteracion = i, BosqueAleatorio = auc_BA,ArbolClasificacion = auc_AC, na_baNeuronal = auc_RN))
  # print(paste("Fin de Iteración",i))
}
especificidad <- round(especificidad,2)
row.names(especificidad) <- NULL
sensibilidad <- round(sensibilidad,2)
row.names(sensibilidad) <- NULL
gini <- round(gini,2)
row.names(gini) <- NULL
# Medida de precisión sensibilidad
sensibilidad_final <- apply(sensibilidad[,-1],2,mean)
windows()
plot(sensibilidad[,c(1,2)], type = "l", ylim = c(0,1), main = "Evolución de la sensibilidad para
     todos los modelos",
     cex.axis = .7,cex.lab = .7,cex.main = .8,
     xlab ="No. de Iteraciones", ylab="sensibilidad")
lines(sensibilidad$BosqueAleatorio, col="green",pch=22)
lines(sensibilidad$ArbolClasificacion, col="red")
lines(sensibilidad$RedNeuronal, col="blue")
abline(h = sensibilidad_final[1], col = "black", lty = 2)
abline(h = sensibilidad_final[2], col = "green", lty = 2)
abline(h = sensibilidad_final[3], col = "red", lty = 2)
abline(h = sensibilidad_final[4], col = "blue", lty = 2)
legend("bottomright", ncol=4,c("Logistica","BosqueAleatorio","Arbol de clasificacion", "Red
                               Neuronal"),
       cex=0.6,bty="n",fill=c("black","green","red","blue"))
# Medida de precisión Gini
gini_final <- apply(gini[,-1],2,mean)
windows()
plot(gini[,c(1,2)], type = "l", ylim = c(0,1), main = "Evolución de Gini para todos los modelos",
     cex.axis = .7,cex.lab = .7,cex.main = .8,
     xlab ="No. de Iteraciones", ylab="Indicador Gini")
lines(gini$BosqueAleatorio, col="green",pch=22)
lines(gini$ArbolClasificacion, col="red")
lines(gini$RedNeuronal, col="blue")
abline(h = gini_final[1], col = "black", lty = 2)
abline(h = gini_final[2], col = "green", lty = 2)
abline(h = gini_final[3], col = "red", lty = 2)
abline(h = gini_final[4], col = "blue", lty = 2)
legend("bottomright", ncol=4,c("Logistica","BosqueAleatorio","Arbol de clasificacion", "Red
                               Neuronal"),
       cex=0.6,bty="n",fill=c("black","green","red","blue"))


# Solo especificidad
especificidad_final <- apply(especificidad[,-1],2,mean)
windows()
plot(especificidad[,c(1,2)], type = "l", ylim = c(0,1), main = "Evolución de la especificidad para
     todos los modelos",
     cex.axis = .7,cex.lab = .7,cex.main = .8,
     xlab ="No. de Iteraciones", ylab="especificidad")
lines(especificidad$BosqueAleatorio, col="green",pch=22)
lines(especificidad$ArbolClasificacion, col="red")
lines(especificidad$RedNeuronal, col="blue")
abline(h = especificidad_final[1], col = "black", lty = 2)
abline(h = especificidad_final[2], col = "green", lty = 2)
abline(h = especificidad_final[3], col = "red", lty = 2)
abline(h = especificidad_final[4], col = "blue", lty = 2)
legend("bottomright", ncol=4,c("Logistica","BosqueAleatorio","Arbol de clasificacion", "Red
                               Neuronal"),
       cex=0.6,bty="n",fill=c("black","green","red","blue"))


# Bosque prueba para cada cluster 
# Cluster 1
df1 <- df_modelar %>% filter(cluster == 1)
df1_ev <- df_evaluar %>% filter(cluster == 1)

indice <- sample(1:nrow(df1), 0.8*nrow(df1), replace = F)
test <- df1[-indice,]
train <- df1[indice,]
t1 <- train %>% filter(condicion==1)
t2 <- train %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
train <- rbind(t1,t2)
table(train$condicion)

bosque1 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold"))], ntree=10, mtry= 5)
varImpPlot(bosque1)
variables1 <- as.data.frame(varImp(bosque1))
variables1 <- cbind(variables1, var = row.names(variables1))
variables1 <- arrange(variables1, -Overall)
variables1 <- as.character(variables1[1:5,2])
bosque_pred1 <- predict(bosque1, test, type = "response") # Predicción sobre test

confusionMatrix(bosque_pred1,test$condicion,positive="1")




# Cluster 2
df2 <- df_modelar %>% filter(cluster == 2)
df2_ev <- df_evaluar %>% filter(cluster == 2)

indice <- sample(1:nrow(df2), 0.8*nrow(df2), replace = F)
test <- df2[-indice,]
train <- df2[indice,]
t1 <- train %>% filter(condicion==1)
t2 <- train %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
train <- rbind(t1,t2)
table(train$condicion)

bosque2 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold"))], ntree=10, mtry= 5)
varImpPlot(bosque2)
variables2 <- as.data.frame(varImp(bosque2))
variables2 <- cbind(variables2, var = row.names(variables2))
variables2 <- arrange(variables2, -Overall)
variables2 <- as.character(variables2[1:5,2])
bosque_pred2 <- predict(bosque2, test, type = "response") # Predicción sobre test

confusionMatrix(bosque_pred2,test$condicion,positive="1")

# Cluster 3
df3 <- df_modelar %>% filter(cluster == 3)
df3_ev <- df_evaluar %>% filter(cluster == 3)

indice <- sample(1:nrow(df3), 0.8*nrow(df3), replace = F)
test <- df3[-indice,]
train <- df3[indice,]
t1 <- train %>% filter(condicion==1)
t2 <- train %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
train <- rbind(t1,t2)
table(train$condicion)

bosque3 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold"))], ntree=10, mtry= 5)
varImpPlot(bosque3)
variables3 <- as.data.frame(varImp(bosque3))
variables3 <- cbind(variables3, var = row.names(variables3))
variables3 <- arrange(variables3, -Overall)
variables3 <- as.character(variables3[1:5,2])
bosque_pred3 <- predict(bosque3, test, type = "response") # Predicción sobre test

confusionMatrix(bosque_pred3,test$condicion,positive="1")


# Cluster 4
df4 <- df_modelar %>% filter(cluster == 4)
df4_ev <- df_evaluar %>% filter(cluster == 4)

indice <- sample(1:nrow(df4), 0.8*nrow(df4), replace = F)
test <- df4[-indice,]
train <- df4[indice,]
t1 <- train %>% filter(condicion==1)
t2 <- train %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
train <- rbind(t1,t2)
table(train$condicion)

bosque4 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold"))], ntree=10, mtry= 5)
varImpPlot(bosque4)
variables4 <- as.data.frame(varImp(bosque4))
variables4 <- cbind(variables4, var = row.names(variables4))
variables4 <- arrange(variables4, -Overall)
variables4 <- as.character(variables4[1:5,2])
bosque_pred4 <- predict(bosque4, test, type = "response") # Predicción sobre test

confusionMatrix(bosque_pred4,test$condicion,positive="1")

# Cluster 5
df5 <- df_modelar %>% filter(cluster == 5)
df5_ev <- df_evaluar %>% filter(cluster == 5)

indice <- sample(1:nrow(df5), 0.8*nrow(df5), replace = F)
test <- df5[-indice,]
train <- df5[indice,]
t1 <- train %>% filter(condicion==1)
t2 <- train %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
train <- rbind(t1,t2)
table(train$condicion)

bosque5 <- randomForest(condicion ~ ., data=train[,-which(names(train) %in% c("ruc","kfold"))], ntree=10, mtry= 5)
varImpPlot(bosque5)
variables5 <- as.data.frame(varImp(bosque5))
variables5 <- cbind(variables5, var = row.names(variables5))
variables5 <- arrange(variables5, -Overall)
variables5 <- as.character(variables5[1:5,2])
bosque_pred5 <- predict(bosque5, test, type = "response") # Predicción sobre test

confusionMatrix(bosque_pred5,test$condicion,positive="1")
str(df_modelar)

# Bosque final para cada cluster
# cluster 1
df1 <- df_modelar %>% filter(cluster == 1)
df1 <- df1[,c(variables1, "condicion")]
df1_ev <- df_evaluar %>% filter(cluster == 1)

t1 <- df1 %>% filter(condicion==1)
t2 <- df1 %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
df1 <- rbind(t1,t2)

bosque <- randomForest(condicion ~ ., data=df1, ntree=10, mtry= 3)
bosque_pred1 <- predict(bosque, df1_ev, type = "response") # Predicción sobre test
bosque_pred2 <- as.data.frame(predict(bosque, df1_ev, type = "prob")) # Predicción sobre test
names(bosque_pred2) <- c("No_Sub", "Sub")

df1_ev$condicion <- bosque_pred1
df1_ev <- cbind(df1_ev, bosque_pred2)

df1_ev$condicion2 <- ifelse(df1_ev$Sub>=0.8,1,0)
table(df1_ev$condicion2)
varImpPlot(bosque)

# cluster 2
df2 <- df_modelar %>% filter(cluster == 2)
df2 <- df2[,c(variables2, "condicion")]
df2_ev <- df_evaluar %>% filter(cluster == 2)

t1 <- df2 %>% filter(condicion==1)
t2 <- df2 %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
df2 <- rbind(t1,t2)

bosque <- randomForest(condicion ~ ., data=df2, ntree=10, mtry= 3)
bosque_pred1 <- predict(bosque, df2_ev, type = "response") # Predicción sobre test
bosque_pred2 <- as.data.frame(predict(bosque, df2_ev, type = "prob")) # Predicción sobre test
names(bosque_pred2) <- c("No_Sub", "Sub")

df2_ev$condicion <- bosque_pred1
df2_ev <- cbind(df2_ev, bosque_pred2)

df2_ev$condicion2 <- ifelse(df2_ev$Sub>=0.8,1,0)
table(df2_ev$condicion2)


# cluster 3
df3 <- df_modelar %>% filter(cluster == 3)
df3 <- df3[,c(variables3, "condicion")]
df3_ev <- df_evaluar %>% filter(cluster == 3)

t1 <- df3 %>% filter(condicion==1)
t2 <- df3 %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
df3 <- rbind(t1,t2)

bosque <- randomForest(condicion ~ ., data=df3, ntree=10, mtry= 3)
bosque_pred1 <- predict(bosque, df3_ev, type = "response") # Predicción sobre test
bosque_pred2 <- as.data.frame(predict(bosque, df3_ev, type = "prob")) # Predicción sobre test
names(bosque_pred2) <- c("No_Sub", "Sub")

df3_ev$condicion <- bosque_pred1
df3_ev <- cbind(df3_ev, bosque_pred2)

df3_ev$condicion2 <- ifelse(df3_ev$Sub>=0.8,1,0)
table(df3_ev$condicion2)

# cluster 4
df4 <- df_modelar %>% filter(cluster == 4)
df4 <- df4[,c(variables4, "condicion")]
df4_ev <- df_evaluar %>% filter(cluster == 4)

t1 <- df4 %>% filter(condicion==1)
t2 <- df4 %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
df4 <- rbind(t1,t2)

bosque <- randomForest(condicion ~ ., data=df4, ntree=10, mtry= 3)
bosque_pred1 <- predict(bosque, df4_ev, type = "response") # Predicción sobre test
bosque_pred2 <- as.data.frame(predict(bosque, df4_ev, type = "prob")) # Predicción sobre test
names(bosque_pred2) <- c("No_Sub", "Sub")

df4_ev$condicion <- bosque_pred1
df4_ev <- cbind(df4_ev, bosque_pred2)

df4_ev$condicion2 <- ifelse(df4_ev$Sub>=0.8,1,0)
table(df4_ev$condicion2)

# cluster 5
df5 <- df_modelar %>% filter(cluster == 5)
df5 <- df5[,c(variables5, "condicion")]
df5_ev <- df_evaluar %>% filter(cluster == 5)

t1 <- df5 %>% filter(condicion==1)
t2 <- df5 %>% filter(condicion==0)
indice <- sample(1:nrow(t1), nrow(t2)-nrow(t1), replace = T)
t1 <- rbind(t1, t1[indice,])
df5 <- rbind(t1,t2)

bosque <- randomForest(condicion ~ ., data=df5, ntree=10, mtry= 3)
bosque_pred1 <- predict(bosque, df5_ev, type = "response") # Predicción sobre test
bosque_pred2 <- as.data.frame(predict(bosque, df5_ev, type = "prob")) # Predicción sobre test
names(bosque_pred2) <- c("No_Sub", "Sub")

df5_ev$condicion <- bosque_pred1
df5_ev <- cbind(df5_ev, bosque_pred2)

df5_ev$condicion2 <- ifelse(df5_ev$Sub>=0.8,1,0)
table(df5_ev$condicion2)

df_evaluar <- rbind(df1_ev, df2_ev, df3_ev, df4_ev, df5_ev)
# df_evaluar$condicion <- df_evaluar$condicion2
importadores <- rbind(df_modelar, df_evaluar[,-c(27,28,29)])
importadores <- importadores[, c("ruc", "condicion")]
write.csv(importadores, "Importadores_con_probSUb.csv", row.names = F)
write.csv(df_evaluar, "df_evaluar.csv", row.names = F)
write.csv(df_modelar, "df_modelar.csv", row.names = F)

variables <- rbind(cbind(variables1,1), cbind(variables2,2), cbind(variables3,3), cbind(variables4,4), cbind(variables5,5))
write.csv(variables, "import_var_clus.csv", row.names = F)

