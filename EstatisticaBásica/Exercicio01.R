# Exercicio Estatística Básica na Linguagem R

#Definindo Pasta de Trabalho
setwd("C:/Users/david/OneDrive/Documentos/PowerBI/Cap12")
getwd()

#Carregando Dataset
notas <- read.csv("Notas.csv", fileEncoding = "windows-1252")

#Questão 1 - Apresente um Resumo de Dados e Estatisticas do Dataset

summary(notas)
View(notas)
str(notas)

#Questão 2 - Qual a Média de Cada turma
mean(notas$TurmaA)
mean(notas$TurmaB)

#Questão 3 - Qual Turma apresentou maior variabilidade de notas? Justifique sua resposta.

var(notas$TurmaA)
sd(notas$TurmaA)
var(notas$TurmaB)
sd(notas$TurmaB)

#R.: Turma A apresentou maior variabilidade de notas, dado os resultados de variância e desvio padrão

#Questão 4 - Calcule o Coeficiente de Variação das 2 turmas

cv <- function(x){coef <- sd(x)/mean(x)*100
return(coef)}

cv(notas$TurmaA)
cv(notas$TurmaB)

#Questão 5 - Qual nota apareceu mais vezes em cada turma?

moda <- function(v) {
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v,valor_unico)))]
}
moda(notas$TurmaA)
moda(notas$TurmaB)

