library(readr)
library(tidyverse)
#
caminho <- "Data-Raw/folha"
arquivos_csv2 <- list.files(caminho, pattern = ".CSV", full.names = TRUE)
folha <- data.frame()
for (arquivo in arquivos_csv2) {
  dados_folha <- read_delim(arquivo, 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = ".", encoding = "ISO-8859-2"), 
                      na = "NA", trim_ws = TRUE)
  dados_folha$nome_arquivo <- gsub(".CSV", "", basename(arquivo))
  dados_folha$nome_arquivo <- gsub("folha_", "", basename(dados_folha$nome_arquivo))
  folha <- rbind(folha, dados_folha)
}
folha["remuneracao"] <- folha$`Remuneraçăo Fixa` + folha$`Vantagens de Natureza Pessoal` + folha$`Funçăo ou Cargo em Comissăo` + folha$`Gratificaçăo Natalina` + folha$`Férias (1/3 Constitucional)` + folha$`Abono Permanęncia` - folha$Faltas
folha <- apply(folha, 2, trimws)
folha <- as.data.frame(folha)
folha$...20 <- NULL
folha$`Vantagens Indenizatórias` <- NULL
folha$`Remuneraçăo menos Descontos` <- NULL
folha$`Imposto de Renda` <- NULL
folha$`Carga Horária` <- NULL
folha$`Contribuiçăo Previdenciária` <- NULL
folha$`Redutor Constitucional` <- NULL
folha$`Abono Permanęncia` <- NULL
folha <- rename(folha, "Ano_Ingresso" = "Exercício")
folha <- rename(folha, "Vinculo" = "Vínculo")
folha <- rename(folha, "Venc_Basico" = "Remuneraçăo Fixa")
folha <- rename(folha, "Grat_Natalina" = "Gratificaçăo Natalina")
folha <- rename(folha, "Ad_Ferias" = "Férias (1/3 Constitucional)")
folha <- rename(folha, "Funcao" = "Funçăo ou Cargo em Comissăo")
folha <- rename(folha, "Data" = "nome_arquivo")
folha <- rename(folha, "Vantagens" = "Vantagens de Natureza Pessoal")
folha$Data <- gsub("_", "/", folha$Data)
folha$Data <- as.Date(paste("01", folha$Data, sep = "/"), format = "%d/%m/%Y")
folha <- folha %>%
  mutate_all(toupper)
folha <- data.frame(lapply(folha, function(x) gsub("É", "E", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Ú", "U", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Â", "A", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Ç", "C", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Ã", "A", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Á", "A", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Í", "I", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Ó", "O", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Ô", "O", x)))
folha <- data.frame(lapply(folha, function(x) gsub("  ", " ", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Ă", "A", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Ę", "E", x)))
folha <- data.frame(lapply(folha, function(x) gsub("Ő", "O", x)))
folha$Venc_Basico <- as.numeric(folha$Venc_Basico)
folha$Vantagens <- as.numeric(folha$Vantagens)
folha$Funcao <- as.numeric(folha$Funcao)
folha$Grat_Natalina <- as.numeric(folha$Grat_Natalina)
folha$Ad_Ferias <- as.numeric(folha$Ad_Ferias)
folha$Faltas <- as.numeric(folha$Faltas)
folha$Cargo <- paste(folha$Cargo, folha$Funçăo)
folha$Funçăo <- NULL
folha <- rename(folha, "Favorecido" = "Nome")
folha$Venc_Basico <- NULL
folha$Vantagens <- NULL
folha$Funcao <- NULL
folha$Ad_Ferias <- NULL
folha$Faltas <- NULL
folha$Grat_Natalina <- NULL
folha$remuneracao <- as.numeric(folha$remuneracao)
soma_remuneracao <- folha %>%
  group_by(Favorecido, Data) %>%
  mutate(remuneracao_total = sum(remuneracao))
dados_unicos <- soma_remuneracao %>%
  distinct(Favorecido, Data, .keep_all = TRUE)
dados_unicos$remuneracao <- NULL
####
arquivos_rds <- list.files(caminho, pattern = "\\.rds$", full.names = TRUE)
folha_velha <- vector()
for (arquivo in arquivos_rds) {
  dados <- readRDS(arquivo)
  dados$Data <- gsub(".rds", "", basename(arquivo))
  dados$Data <- gsub("sal_", "", basename(dados$Data))
  dados <- apply(dados, 2, trimws)
  dados <- as.data.frame(dados)
  dados$carga_horaria <- NULL
  dados <- rename(dados, "remuneracao_total" = "remuneracao")
  dados <- rename(dados, "Favorecido" = "nome")
  dados <- rename(dados, "Cargo" = "cargo")
  dados$lotacao <- NULL
  dados <- data.frame(lapply(dados, function(x) gsub("Estagiarios", "Estagiario", x)))
  dados <- data.frame(lapply(dados, function(x) gsub("'", "", x)))
  dados <- data.frame(lapply(dados, function(x) gsub("´", "", x)))
  dados <- data.frame(lapply(dados, function(x) gsub("  ", " ", x)))
  dados$Data <- gsub("_", "/", dados$Data)
  dados$Data <- paste0(dados$Data, "/01")
  dados$Data <- as.Date(dados$Data)
  folha_velha <- rbind(folha_velha, dados)
}
folha_velha <- folha_velha %>% 
  mutate_all(toupper)
folha_velha$remuneracao_total <- gsub("\\.", "", folha_velha$remuneracao_total)
folha_velha$remuneracao_total <- gsub(",", ".", folha_velha$remuneracao_total)
folha_velha$remuneracao_total <- as.numeric(folha_velha$remuneracao_total)
folha_velha <- folha_velha[folha_velha$remuneracao_total != 0, ]
folha_velha$CPF <- NA
folha_velha$Ano_Ingresso <- NA
#
folha <- rbind(folha_velha, dados_unicos)
rm(folha_velha)
rm(dados)
rm(dados_folha)
rm(dados_unicos)
rm(soma_remuneracao)
rm(arquivo)
rm(arquivos_csv2)
rm(caminho)
rm(arquivos_rds)
#
lista_geral <- readRDS("Data/lista_geral.rds")
folha <- left_join(folha, lista_geral, by = "Favorecido")
colnames(folha)[which(names(folha) == "id.y")] <- "id"
folha$CPF[is.na(folha$CPF)] <- folha$id[is.na(folha$CPF)]
#
folha$Cargo <- gsub("ASSISTENTE DE GABINETE VER. - AGV", "ASSISTENTE DE GABINETE", folha$Cargo)
folha$Cargo <- gsub("DIRETOR DE RH E TECNOLOGIA DA INFORMACAO DIRETOR DE RECURSOS HUMANOS", "DIRETOR DE RH E TI", folha$Cargo)
folha$Cargo <- gsub("DIRETOR DE RH E TECNOLOGIA DA INFORMACAO", "DIRETOR DE RH", folha$Cargo)
folha$Cargo <- gsub("DIRETOR DE RECURSOS HUMANOS", "DIRETOR DE RH", folha$Cargo)
folha$Cargo <- gsub("AGENTE LEGISLATIVO DIRETOR DE ADMINISTRACAO E FINANCAS", "DIRETOR DE ADMINISTRACAO E FINANCAS", folha$Cargo)
folha$Cargo <- gsub("OPERADOR DE MICROCOMPUTADOR DIRETOR DE ADMINISTRACAO E FINANCAS", "DIRETOR DE ADMINISTRACAO E FINANCAS", folha$Cargo)
folha$Cargo <- gsub("ASSESSOR PARLAMENTAR DE GAB. DE VER. - APV", "ASSESSOR PARLAMENTAR", folha$Cargo)
folha$Cargo <- gsub("ASSESSOR ESPECIAL DO PARLAMENTAR -AEP", "ASSESSOR PARLAMENTAR", folha$Cargo)
folha$Cargo <- gsub("ASSESSOR ESPECIAL", "ASSESSOR PARLAMENTAR", folha$Cargo)
folha$Cargo <- gsub("ASSESSOR NIVEL MEDIO DO PARLAMENTAR", "ASSESSOR PARLAMENTAR", folha$Cargo)
folha$Cargo <- gsub("ASSESSOR DE NIVEL MEDIO DE GAB. DE VER. - ANV", "ASSESSOR PARLAMENTAR", folha$Cargo)
folha$Cargo <- gsub("ASSESSOR PARLAMENTAR DE PROCESSO LEGISLATIVO -AEL", "ASSESSOR PROCESSO LEGISLATIVO", folha$Cargo)
folha$Cargo <- gsub("ASSESSOR JURIDICO DE GAB. DE VER. - AJV", "ASSESSOR JURIDICO", folha$Cargo)
folha$Cargo <- gsub("CHEFE DE GABINETE DO VER. - CGV", "CHEFE DE GABINETE", folha$Cargo)
folha$Cargo <- gsub("CONSULTOR LEGISLATIVO DE GAB. DE VER. - CLG", "CONSULTOR LEGISLATIVO", folha$Cargo)
folha$Cargo <- gsub("SECRETARIO (A) DE GABINETE DE VER. - SGV", "SECRETARIO DE GABINETE", folha$Cargo)
folha$Cargo <- gsub("VIGILANTE FUNCAO GRATIFICADA III", "VIGILANTE/SEGURANCA", folha$Cargo)
folha$Cargo <- gsub("VIGILANTE FUNCAO GRATIFICADA II", "VIGILANTE/SEGURANCA", folha$Cargo)
folha$Cargo <- gsub("VIGILANTE FUNCAO GRATIFICADA I", "VIGILANTE/SEGURANCA", folha$Cargo)
folha$Cargo <- gsub("AUXILIAR LEGISLATIVO DE VIGILACIA- (AUL I)", "VIGILANTE/SEGURANCA", folha$Cargo)
folha$Cargo <- gsub("ASSESSOR PARLAMENTAR DE PROCESSO LEGISLATIVO", "ASSESSOR PROCESSO LEGISLATIVO", folha$Cargo)
folha$Cargo <- gsub("SECRETARIO (A) DE GABINETE DE VER. - SGV", "SECRETARIO DE GABINETE", folha$Cargo)
folha$Cargo <- gsub("AUXILIAR LEGISLATIVO DE VIGILACIA- \\(AUL I\\)", "VIGILANTE/SEGURANCA", folha$Cargo)
folha$Cargo <- gsub("AUXILIAR LEGISLATIVO DE LIMPEZA- \\(AUL II\\)", "ASG", folha$Cargo)
folha$Cargo <- gsub("ANALISTA LEGISLATIVO \\(DECISAO JUDICIAL/ DISPON.\\)", "ANALISTA LEGISLATIVO", folha$Cargo)
folha$Cargo <- gsub("TEC DE NIVEL SUPERIOR \\(DECISAO JUDICIAL / DISPON.\\)", "TECNICO DE NIVEL SUPERIOR", folha$Cargo)
folha$Cargo <- gsub("SECRETARIO \\(A\\) DE GABINETE DE VER. - SGV", "SECRETARIO DE GABINETE", folha$Cargo)
folha$Cargo <- gsub("GERENTE DE EXPIDIENTE LEGISLATIVO E APOIO AS COMIS", "GERENTE DE EXPEDIENTE LEGISLATIVO", folha$Cargo)
folha$Cargo <- gsub("SEGURANCA DE PLENARIO", "VIGILANTE/SEGURANCA", folha$Cargo)
folha$Cargo <- gsub("REDATOR DE ATAS FUNCAO GRATIFICADA II", "REDATOR DE ATAS", folha$Cargo)
#
folha <- folha %>% group_by(id) %>% mutate(Primeiro_Pgto = min(as.Date(Data)))
folha <- folha %>% group_by(id) %>% mutate(Ultimo_Pgto = max(as.Date(Data)))
folha$Meses <- round(as.numeric(difftime(as.Date(folha$Ultimo_Pgto), as.Date(folha$Primeiro_Pgto), units = "days")) / 30.44)
folha$Meses <- pmax(folha$Meses, 1)
folha$Primeiro_Pgto <- as.Date(folha$Primeiro_Pgto, "%Y")
folha$Ultimo_Pgto <- as.Date(folha$Ultimo_Pgto, "%Y")
folha$Primeiro_Pgto <- format(folha$Primeiro_Pgto, "%Y")
folha$Ultimo_Pgto <- format(folha$Ultimo_Pgto, "%Y")
#
grupo_favorecido <- split(folha, f = folha$Favorecido)
preencher_ano_ingresso <- function(grupo) {
  grupo$Ano_Ingresso[is.na(grupo$Ano_Ingresso)] <- max(grupo$Ano_Ingresso, na.rm = TRUE)
  return(grupo)
}
grupo_favorecido <- lapply(grupo_favorecido, preencher_ano_ingresso)
folha <- do.call(rbind, grupo_favorecido)
#
folha <- folha %>%
  mutate(Ano_Ingresso = ifelse(is.na(Ano_Ingresso), Primeiro_Pgto, Ano_Ingresso))
#
folha$Ano_Ingresso <- as.numeric(folha$Ano_Ingresso)
folha$Anos_antes_de_2014 <- ifelse(folha$Ano_Ingresso < 2014, 2014 - folha$Ano_Ingresso, 0)
folha$N_Meses <- folha$Meses + folha$Anos_antes_de_2014*12
folha$Meses <- NULL
folha$Anos_antes_de_2014 <- NULL
#
saveRDS(folha, file = "Data/folha.rds")
#
rm(grupo_favorecido)
rm(preencher_ano_ingresso)
rm(lista_geral)
#
tempo_permanencia <- folha 
tempo_permanencia$Ano_Ingresso <- as.numeric(tempo_permanencia$Ano_Ingresso)
tempo_permanencia$Ultimo_Pgto <- as.numeric(tempo_permanencia$Ultimo_Pgto)
tempo_permanencia <- tempo_permanencia %>%
  group_by(Vinculo, id) %>%
  mutate(Tempo_Permanencia = Ultimo_Pgto - Ano_Ingresso)
tempo_medio_permanencia <- tempo_permanencia %>%
  group_by(Vinculo) %>%
  summarize(Tempo_Medio_Permanencia = mean(Tempo_Permanencia, na.rm = TRUE), 
            Total_Obs = n_distinct(id))
#
folha$Ano <- format(as.Date(folha$Data), "%Y")
remuneracao_media <- aggregate(folha$remuneracao_total, by = list(folha$Vinculo, folha$Ano), mean)
names(remuneracao_media) <- c("Vinculo", "Ano", "Remuneracao_media")
remuneracao_media <- spread(remuneracao_media, Ano, Remuneracao_media)
valor_medio_total <- aggregate(folha$remuneracao_total, by = list(folha$Vinculo), mean)
names(valor_medio_total) <- c("Vinculo", "Valor_medio_total")
resultados <- merge(remuneracao_media, valor_medio_total)
resultados$num_medio_id_ate_2019 <- aggregate(folha[folha$Ano < "2020",]$id, by = list(folha[folha$Ano < "2020",]$Vinculo), FUN = function(x) length(unique(x)))$x
resultados$num_medio_id_desde_2020 <- aggregate(folha[folha$Ano >= "2020",]$id, by = list(folha[folha$Ano >= "2020",]$Vinculo), FUN = function(x) length(unique(x)))$x
##
# Adicionar coluna com o grau de instrução por cargo.
# Apontar o número de cargos em 2019, 2020 e 2022.
# Apontar a remuneração de 2019, 2020 e 2022 e indicar se existe associação entre cargo comissionado e salário mais elevado.
# Na média salarial, corrigir, pois está sendo calculado a média, mas contabilizando férias e 13.