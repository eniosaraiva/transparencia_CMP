A <- as.data.frame(folha$Favorecido)
colnames(A)[1] <- "nome"
A <- distinct(A)
B <- as.data.frame(pagamentos$Favorecido)
B <- distinct(B)
colnames(B)[1] <- "nome"
nomes_c <- union(A$nome, B$nome)
C <- data.frame(nome = nomes_c)
id <- seq(1, nrow(C))
lista_geral <- cbind(id, C)
colnames(lista_geral)[2] <- "Favorecido"
#
rm(A)
rm(B)
rm(C)
rm(id)
rm(nomes_c)
saveRDS(lista_geral, file = "Data/lista_geral.rds")
