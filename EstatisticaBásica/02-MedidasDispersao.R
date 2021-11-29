# Parte 2 - Medidas de Dispersão

#Definindo Pasta de Trabalho
setwd("C:/Users/david/OneDrive/Documentos/PowerBI/Cap12")
getwd()

#Carregando Dataset
vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252")

#Resumo Dataset
View(vendas)
str(Vendas)
summary(Vendas$valor)
summary(vendas$Custo)

#Variância
var(vendas$Valor)

#Desvio Padrão
sd(vendas$Valor)

#Coeficiente de Variação
cv <- function(x){coef <- sd(x)/mean(x)*100
return(coef)}

cv(vendas$Valor)
