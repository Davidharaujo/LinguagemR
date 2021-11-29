#******************************************************************************
#*
#*               Mirosoft Power BI para Data Science, Versao 2.0
#*                                   DSA
#*
#*                               Mini-Projeto 4
#*
#*     Prevendo a Inadimplencia de Clientes com Machine Learning e Power BI
#*
#*
#******************************************************************************

#Definindo pasta de trabalho
setwd("C:/Users/david/OneDrive/Documentos/PowerBI/Cap15")
getwd()

#Definicao do problema em PDF na pasta do arquivo

#Instalando pacotes do projeto
install.packages("Amelia")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape")
install.packages("randomForest")
install.packages("e1071")
install.packages("Rcpp")
#Carregando Pacotes
library(Amelia)
library(caret)
library(ggplot2)
library(dplyr)
library(reshape)
library(randomForest)
library(e1071)
library(Rcpp)

#Correção erro de versão R
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)

#Carregando Dataset
#Fonte: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
dados_clientes <- read.csv("dados/dataset.csv")

#Visualizando o dataset
View(dados_clientes)
str(dados_clientes)
summary(dados_clientes)


#################Análise Exploratória, Limpeza e Transofrmação##################

#Removendo a primeira coluna ID
dados_clientes$ID <- NULL
dim(dados_clientes)
View(dados_clientes)

#Renomeando a coluna de classe
colnames(dados_clientes)
colnames(dados_clientes)[24] <- "Inadimplente"
colnames(dados_clientes)
View(dados_clientes)

#Verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
?missmap
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)

#Convertendo os atributos genero, escolaridade, estado civil e idade para
#fatores (caterogrias)

#Renomeando colunas categoricas
colnames(dados_clientes)
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"
colnames(dados_clientes)
View(dados_clientes)

#Genero
View(dados_clientes$Genero)
srt(dados_clientes$Genero)
dados_clientes$Genero <- cut(dados_clientes$Genero,
                             c(0,1,2),
                             labels = c("Masculino",
                                        "Feminino"))
View(dados_clientes)

#Escolaridade
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade,
                                   c(0,1,2,3,4),
                                   labels = c("P�s Graduado",
                                              "Superior",
                                              "Medio",
                                              "Outros"))
summary(dados_clientes$Escolaridade)
View(dados_clientes)

#Estado_Civil
dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil,
                                   c(0,1,2,3),
                                   labels = c("Casado",
                                              "Solteiro",
                                              "Outros"))
View(dados_clientes)
summary(dados_clientes$Estado_Civil)

#Idade - Mudando para faixas etárias
dados_clientes$Idade <- cut(dados_clientes$Idade,
                                   c(0,30,50,100),
                                   labels = c("Jovem",
                                              "Adulto",
                                              "Idoso"))
View(dados_clientes)
summary(dados_clientes$Idade)

#Convertendo colunas PAY para categóricas
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_1 <- as.factor(dados_clientes$PAY_1)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

#Convertendo Coluna Alvo
dados_clientes$Inadimplente <- as.factor(dados_clientes$Inadimplente)
str(dados_clientes$Inadimplente)

#Verificando a tabela e ETL
str(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main="Valores Missing Encontrados")
dados_clientes <- na.omit(dados_clientes)
missmap(dados_clientes, main="Valores Missing Encontrados")
dim(dados_clientes)

#Inadimplentes versus nao-inadimplentes
table(dados_clientes$Inadimplente)

#Proporcao de Inadimplentes x Nao-Inadimplentes
prop.table(table(dados_clientes$Inadimplente))

#Plot de Distribuicao
qplot(Inadimplente, data=dados_clientes, geom = "bar")+
  theme(axis.text.x =element_text(angle = 90,hjust = 1) )

#Set Sedd
set.seed(12345)

#Amostragem Estratificada
#Seleciona as linhas de acordo com a variavel inadimplente como strata
?createDataPartition
indice <- createDataPartition(dados_clientes$Inadimplente, p=0.75, list=FALSE)
dim(indice)

#Definimos os dados de treinamento como subconjunto do conjunto de dados original
#com numeros de indices de linha (conforme identificado acima) e todas as colunas
dados_treino <- dados_clientes[indice,]
dim(dados_treino)
table(dados_treino$Inadimplente)

#Porcentagem das classes
prop.table(table(dados_treino$Inadimplente))

#Numero de registros no dataset de treinamento
dim(dados_treino)

#Comparando Treino versus Original
compara_dados <- cbind(prop.table(table(dados_treino$Inadimplente)),
                       prop.table(table(dados_clientes$Inadimplente)))
compara_dados <- c("Treinamento","Original")
compara_dados

#Melt Data - Converter colunas em linhas
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

#Plot Treinamento versus Original
ggplot(melt_compara_dados, aes(x = X1, y = value))+
  geom_bar(aes(fill= X2), stat ="identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

#Tudo que nao esta� no dataset de treino esta em teste.
dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)

######################## Modelos de Machine Learning ##########################

#Construindo a primeira versao do modelo
modelo_v1 <- randomForest(Inadimplente ~ .,data = dados_treino)
modelo_v1

#Avaliando o modelo
plot(modelo_v1)

#Previsoes com dados de teste
previsoes_v1 <- predict(modelo_v1, dados_teste)
previsoes_v1
#Confusion Matrix
cm_v1 <- caret::confusionMatrix(previsoes_v1, dados_teste$Inadimplente,positive="1")
cm_v1

#Calculando precision, recall e F1-Score, metricas de avaliacao do modelo preditivo

y <- dados_teste$Inadimplente
y_pred_v1 <- previsoes_v1

precision <- posPredValue(y_pred_v1, y)
precision

recall <- sensitivity(y_pred_v1,y)
recall

F1 <- (2*precision*recall)/(precision+recall)
F1


#Balanceamento de classe

install.packages("DMwR")
library(DMwR)

#O pacote DMwR foi excluido do CRAN, logo a instalacao do pacote ocorreu pelo github
install.packages("remotes")
remotes::install_github("cran/DMwR")

#Aplicando SMOTE: Syntetic Minority Over-Sampling Technique

table(dados_treino$Inadimplente)
prop.table(table(dados_treino$Inadimplente))
set.seed(9560)
dados_treino_bal <- SMOTE(Inadimplente ~ ., data=dados_treino)
table(dados_treino_bal$Inadimplente)
prop.table(table(dados_treino_bal$Inadimplente))

#Construindo segunda versao do modelo
modelo_v2 <- randomForest(Inadimplente ~ ., data=dados_treino_bal)
modelo_v2

#Avaliando o modelo 2
plot(modelo_v2)

#Previsoes com dados de teste
previsoes_v2 <- predict(modelo_v2, dados_teste)

#Confusion Matrix
cm_v2 <- caret::confusionMatrix(previsoes_v2, dados_teste$Inadimplente,positive="1")
cm_v2

#Calculando precision, recall e F1-Score, metricas de avaliacao do modelo preditivo

y <- dados_teste$Inadimplente
y_pred_v2 <- previsoes_v2

precision <- posPredValue(y_pred_v2, y)
precision

recall <- sensitivity(y_pred_v2,y)
recall

F1 <- (2*precision*recall)/(precision+recall)
F1

#Importancia das variaveis preditoras para a previsao
View(dados_treino_bal)
varImpPlot(modelo_v2)

#Obtendo as variaveis mais importantes

imp_var <- importance(modelo_v2)
varImportance <- data.frame(Variables = row.names(imp_var),
                            Importance = round(imp_var[ ,'MeanDecreaseGini'],2))

#Criando o rank de variaveis baseado na importancia

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

#Usando ggplot2 para visualizar a importancia relativa  das variaveis
ggplot(rankImportance,
       aes(x = reorder(Variables, Importance),
           y = Importance,
           fill=Importance))+
  geom_bar(stat = "identity")+
  geom_text(aes(x=Variables, y=0.5, label = Rank),
            hjust=0,
            vjust = 0.55,
            size = 4,
            colour = 'red')+
  labs(x ='Variables')+
  coord_flip()

#Construindo a terceira versao do modelo preditivo
colnames(dados_treino_bal)
modelo_v3 <- randomForest(Inadimplente ~ PAY_0+PAY_2+PAY_3+PAY_AMT1+PAY_AMT2+PAY_5+BILL_AMT1,
             data = dados_treino_bal)
modelo_v3

#Avaliando o modelo 3
plot(modelo_v3)

#Previsoes com dados de teste
previsoes_v3 <- predict(modelo_v3, dados_teste)

#Confusion Matrix
cm_v3 <- caret::confusionMatrix(previsoes_v3, dados_teste$Inadimplente,positive="1")
cm_v3

#Calculando precision, recall e F1-Score, metricas de avaliacao do modelo preditivo

y <- dados_teste$Inadimplente
y_pred_v3 <- previsoes_v3

precision <- posPredValue(y_pred_v3, y)
precision

recall <- sensitivity(y_pred_v3,y)
recall

F1 <- (2*precision*recall)/(precision+recall)
F1

#Salvando modelo 3

saveRDS(modelo_v3, file="modelo/modelo_v3.rds")

#Carregando modelo

modelo_final <- readRDS("modelo/modelo_v3.rds")
modelo_final

#Previsoes com novos dados de 3 clientes

#Dados dos clientes
PAY_0 <- c(0,0,0)
PAY_2 <- c(0,0,0)
PAY_3 <- c(1,0,0)
PAY_AMT1 <- c(1100,1000,1200)
PAY_AMT2 <- c(1500,1300,1150)
PAY_5 <- c(0,0,0)
BILL_AMT1 <- c(350,420,280)

#Concatenar em um dataframe
novos_clientes <- data.frame(PAY_0,PAY_2,PAY_3,PAY_AMT1,PAY_AMT2,PAY_5,BILL_AMT1)
View(novos_clientes)

#Transformando em fator
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treino_bal$PAY_2))
novos_clientes$PAY_3 <- factor(novos_clientes$PAY_3, levels = levels(dados_treino_bal$PAY_3))
novos_clientes$PAY_5 <- factor(novos_clientes$PAY_5, levels = levels(dados_treino_bal$PAY_5))
str(novos_clientes)

#Previsoes

previsoes_final <- predict(modelo_final, novos_clientes)
View(previsoes_final)