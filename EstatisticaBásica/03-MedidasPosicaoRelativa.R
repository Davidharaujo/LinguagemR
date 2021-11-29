# Parte 3 - Medidas de Posição

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

# Medidas de Tendência Central
summary(vendas)
summary(vendas[c("Valor","Custo")])

#Explorando Variáveis Numéricas
mean(vendas$Valor)
median(vendas$Valor)
quantile(vendas$Valor)
quantile(vendas$Valor, probs = c(0.01,0.99))
quantile(vendas$Valor, seq(from = 0, to = 1, by = 0.20))
IQR(vendas$Valor) #Diferença entre Q3 e Q1
range(vendas$Valor)
summary(vendas$Valor)
diff(range(vendas$Valor))
