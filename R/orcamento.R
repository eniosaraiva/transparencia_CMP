library(magrittr)
library(dplyr)
library(readr)
# Orçamento ---------------------------------------------------------------
receita_2021 <- read_delim("data-raw/receita/RECEITA_ATE_DEZEMBRO_2021.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", encoding = "latin1"), trim_ws = TRUE) %>% 
  select(3:5)
receita_2020 <- read_delim("data-raw/receita/RECEITA_ATE_DEZEMBRO_2020.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", encoding = "latin1"), trim_ws = TRUE) %>% 
  select(3:5)
receita_2019 <- read_delim("data-raw/receita/RECEITA_ATE_DEZEMBRO_2019.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", encoding = "latin1"), trim_ws = TRUE) %>% 
  select(3:5)
receita_2018 <- read_delim("data-raw/receita/RECEITA_ATE_DEZEMBRO_2018.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", encoding = "latin1"), trim_ws = TRUE) %>% 
  select(3:5)
receita_2017 <- read_delim("data-raw/receita/RECEITA_ATE_DEZEMBRO_2017.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", encoding = "latin1"), trim_ws = TRUE) %>% 
  select(3:5)
receita_2016 <- read_delim("data-raw/receita/RECEITA_ATE_DEZEMBRO_2016.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", encoding = "latin1"), trim_ws = TRUE) %>% 
  select(3:5)
# 2016: Ausente a receita realizada. Retirado do Portal da Transparência do Executivo.
receita_2016_real <- read_csv("data-raw/Receita/realizado2016.csv") %>% 
  select(4)
colnames(receita_2016_real)[1] <- "Valor"
# -------------------------------------------------------------------------
receita_2015 <- read_delim("data-raw/receita/RECEITA_ATE_DEZEMBRO_2015.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", encoding = "latin1"), trim_ws = TRUE) %>% 
  select(3:5)
# -------------------------------------------------------------------------
receita_2021["Ano"]<-2021
receita_2020["Ano"]<-2020
receita_2019["Ano"]<-2019
receita_2018["Ano"]<-2018
receita_2017["Ano"]<-2017
receita_2016["Ano"]<-2016
receita_2015["Ano"]<-2015
receita <- bind_rows(receita_2015, receita_2016, receita_2017, receita_2018, receita_2019, receita_2020, receita_2021)
orc_2013 <- c(10849995, 11431710, 105.36, 2013)
orc_2014 <- c(12458000, 11661306, 93.60, 2014)
receita <- rbind(orc_2013, orc_2014, receita)
receita <- as.data.frame(receita)
colnames(receita)[1] <- "Receita Prevista"
colnames(receita)[2] <- "Receita Realizada"
colnames(receita)[3] <- "% Realizado"
colnames(receita)[4] <- "Ano"
pop <- c(229414, 235983, 242384, 248623, 254709, 255793, 261469, 267036, 272490)
receita <- bind_cols(receita, pop)
colnames(receita)[5] <- "Populacao_estimada"
rec_2022 <- c(23000000, 0, 0, 2022, 0)
receita <- rbind(receita, rec_2022)
orc_pref_est <- c(363352670, 414040800, 641763400, 505000000, 448781200, 468720000, 492500000, 544534256, 507924700, 810214367)
orc_pref_rel <- c(332258717.49, 363802869.54, 402623146.60, 417112623.99, 407905401.28, 395756234.07, 528287163.56, 570366384.32,  633115371.81, 0)
receita <- bind_cols(receita, orc_pref_est, orc_pref_rel)
colnames(receita)[6] <- "Orcamento_Mun_Estimado"
colnames(receita)[7] <- "Orcamento_Mun_Realizado"
receita <- dplyr::select(receita, 4, 5, 1, 2, 3, 6, 7)
colnames(receita)[5] <- "%_Realizado_CM"
receita <- receita %>% 
  mutate(freq_relat=Orcamento_Mun_Realizado/Orcamento_Mun_Estimado) %>%  
  mutate(freq_relat=round(freq_relat*100)) 
colnames(receita)[8] <- "%_Realizado_Pref"
receita[10,2] <- "272490"
receita[10,4] <- "25246521.86"
receita[10,5] <- "109.76"
receita[10,7] <- "712724926.61"  
receita[10,8] <- "87.97" 
#
inflacao <- c(5.91, 6.41, 10.67, 6.29, 2.95, 3.75, 4.31, 4.52, 10.06, 5.79)
#Dados extraídos de: https://www.inflation.eu/pt/taxas-de-inflacao/brasil/inflacao-historica/ipc-inflacao-brasil.aspx
receita <- cbind (receita, inflacao)
colnames(receita)[colnames(receita)=="inflacao"] <- "Inflacao_IPC"
saveRDS(receita, file = "data/receita.rds")
rm(receita_2015)
rm(rec_2022)
rm(receita_2016)
rm(receita_2016_real)
rm(receita_2017)
rm(receita_2018)
rm(receita_2019)
rm(receita_2020)
rm(inflacao)
rm(orc_2013)
rm(orc_2014)
rm(orc_pref_est)
rm(orc_pref_rel)
rm(receita_2021)
rm(pop)
rm(df)
