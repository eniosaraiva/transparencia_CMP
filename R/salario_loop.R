#### DESATUALIZADO POIS O PORTAL DA TRANSPARÊNCIA FOI ALTERADO NO SEGUNDO SEMESTRE DE 2022
library(magrittr)
library(stringr)
# Falha detectada ---------------------------------------------------------
# Mês 11/2018 não está no Portal.
# -------------------------------------------------------------------------
mes <- 11
ano <- 2018
data_pesq <- glue::glue("01/{mes}/{ano}")
path <- "output/"
url <- "http://186.209.101.218:8080/transparencia/leiacessoinf.aspx"
# Acesso da Pagina Inicial ------------------------------------------------
r0 <- httr::GET(url)
# coleta os dados da sessao -----------------------------------------------
vs <- r0 |>
  xml2::read_html() |>
  xml2::xml_find_all("//*[@name='__VIEWSTATE']") |>
  xml2::xml_attr("value")
ev <- r0 |>
  xml2::read_html() |>
  xml2::xml_find_all("//*[@name='__EVENTVALIDATION']") |>
  xml2::xml_attr("value")
# Criterio de Pesquisa ----------------------------------------------------
body <- list(
  `__EVENTTARGET` = "",
  `__EVENTARGUMENT` = "",
  `__LASTFOCUS` = "",
  `__VIEWSTATE` = vs,
  `__VIEWSTATEGENERATOR` = "E0486018",
  `__VIEWSTATEENCRYPTED` = "",
  `__EVENTVALIDATION` = ev,
  `ctl00$ContentPlaceHolder1$ddlMesReferencia` = data_pesq,
  `ctl00$ContentPlaceHolder1$TipoPesq` = "rbPesqPorNome",
  `ctl00$ContentPlaceHolder1$txtPesqNome` = "",
  `ctl00$ContentPlaceHolder1$btnPesquisar` = "Pesquisar"
)
# Realizando a 1a pesquisa ------------------------------------------------
inicio <- httr::POST(
  url, body = body,
  encode = "form", as = "text"
)
vs <- inicio |>
  xml2::read_html() |>
  xml2::xml_find_all("//*[@name='__VIEWSTATE']") |>
  xml2::xml_attr("value")
ev <- inicio |>
  xml2::read_html() |>
  xml2::xml_find_all("//*[@name='__EVENTVALIDATION']") |>
  xml2::xml_attr("value")
# Coletando o numero de paginas -------------------------------------------
pag <- "Last"
body_pag <- list(
  `__EVENTTARGET` = "ctl00$ContentPlaceHolder1$gvFuncionarios",
  `__EVENTARGUMENT` = paste0("Page$", pag),
  `__VIEWSTATE` = vs,
  `__VIEWSTATEGENERATOR` = "E0486018",
  `__VIEWSTATEENCRYPTED` = "",
  `__EVENTVALIDATION` = ev
)
fim <- httr::POST(
  url,
  body = body_pag,
  encode = "form",
)
n_pags <- httr::content(fim) %>% 
  xml2::xml_find_all("//a[contains(@href,'Page$')]/@href")
n_pags <- n_pags[11]
n_pags <- as.character(n_pags)
n_pags <- as.data.frame(n_pags)
n_pags <- str_remove(n_pags, ".{86}")
n_pags <- str_extract(n_pags, "[[:digit:]]{2}")
n_pags <- as.numeric(n_pags)
pag_fim <- n_pags + 1
fim <- httr::POST(
  url,
  body = body_pag,
  encode = "form"
)
# Baixando as demais páginas ----------------------------------------------
r_anterior <- httr::POST(
  url, body = body,
  encode = "form")
for(pag in 1:pag_fim) {
  vs <- r_anterior |>
    xml2::read_html() |>
    xml2::xml_find_all("//*[@name='__VIEWSTATE']") |>
    xml2::xml_attr("value")
  ev <- r_anterior |>
    xml2::read_html() |>
    xml2::xml_find_all("//*[@name='__EVENTVALIDATION']") |>
    xml2::xml_attr("value")
  body_pag <- list(
    `__EVENTTARGET` = "ctl00$ContentPlaceHolder1$gvFuncionarios",
    `__EVENTARGUMENT` = paste0("Page$", pag),
    `__VIEWSTATE` = vs,
    `__VIEWSTATEGENERATOR` = "E0486018",
    `__VIEWSTATEENCRYPTED` = "",
    `__EVENTVALIDATION` = ev
  )
  r_anterior <- httr::POST(
    url,
    body = body_pag,
    encode = "form",
    httr::write_disk(glue::glue("{path}{ano}_{mes}_pag{pag}.html"), TRUE)
  )}
# CRIANDO E TRATANDO O DATAFRAME -------------------------------------------------------
ano <- 2014
mes <- 9
local <- glue("data-raw/salarios_{ano}_{mes}")
arquivo <- paste0(local, ".csv")
library(glue)
#tratamento <- function (ano, mes) {
arquivos = list.files(pattern = paste0(ano,"_", mes), recursive = TRUE)
for(pagina in 1:arquivos) {
temp_table <- readHTMLTable(pagina)[[12]]
final_table <- temp_table %>%
  select(V1:V5) %>%
  na.omit() %>%
  `colnames<-` (c("Nome","Lotacao","Cargo","Carga_Horaria", "Remuneracao")) %>%
  `rownames<-` (seq_len(nrow(.)))
final_table <- final_table[-16, ] %>% 
  janitor::clean_names() %>% 
  tibble() 
sem_acento <- rm_accent(final_table$nome)
final_table[1] <- sem_acento
sem_acento <- rm_accent(final_table$lotacao)
final_table[2] <- sem_acento
sem_acento <- rm_accent(final_table$cargo)
final_table[3] <- sem_acento
purrr:map(arquivos)
write.csv2(final_table, paste0(arquivol), col.names = TRUE)}
#}}
tratamento(2014, 09)
final_table
#