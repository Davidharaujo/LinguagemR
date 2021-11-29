# Parte 4 - Tabela de Frequencia

#Definindo Pasta de Trabalho
setwd("C:/Users/david/OneDrive/Documentos/PowerBI/Cap12")
getwd()

#Carregando Dataset
dados <- read.table("Usuarios.csv",
                    dec = ".",
                    sep = ",",
                    h = T,
                    fileEncoding = "windows-1252")

#Visualizando e Sumarizando Dados
View(dados)
names(dados)
str(dados)
summary(dados$salario)
summary(dados$grau_instrucao)
mean(dados$salario)
mean(dados$grau_instrucao)

#Tabela de Frequências Absolutas
freq <- table(dados$grau_instrucao)
View(freq)

#Tabela de Frequências Relativas
freq_rel <- prop.table(freq)
View(freq_rel)

#Porcentagem (100 * freq_rel)
p_freq_rel <- prop.table(freq_rel)*100
View(p_freq_rel)

#Adiciona Linhas de Total
View(freq)
freq <- c(freq,sum(freq))
names(freq)[4] <- "Total"
View(freq)

# Tabela Final com Todos os Valores

#Calculamos Frequencia Relativa e frequencia proporcional
freq_rel <- c(freq_rel, sum(freq_rel))
p_freq_rel <- c(p_freq_rel,sum(p_freq_rel))

#Tabela Final com todos os valores
tabela_final <- cbind(freq,
                      freq_rel = round(freq_rel, digits=2),
                      p_freq_rel = round(p_freq_rel, digits = 2))
View(tabela_final)





