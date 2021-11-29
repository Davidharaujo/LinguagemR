# Parte 1 - Medidas de Posição

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

#Média
?mean
mean(vendas$Valor)
mean(vendas$Custo)

#Média Ponderada
weighted.mean(vendas$Valor)
weighted.mean(vendas$Custo)

#Mediana
median(vendas$Valor)
median(vendas$Custo)

#Moda
#Criando a Função
moda <- function(v) {
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v,valor_unico)))]
}

#Obtendo a Moda
resultado <- moda(vendas$Valor)
resultado_custo <- moda(vendas$Custo)

#Criando Gráfico de Média de Valor por Estado com ggplot2
install.packages("ggplot2")
library(ggplot2)

#Cria Gráfico
ggplot(vendas) +
  stat_summary(aes(x=Estado,
                   y=Valor),
               fun = mean,
               geom = "bar",
               fill = "lightgreen",
               col = "gray50") +
  labs(title = "Média de Valor por Estado")

