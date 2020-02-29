## Projeto - AdTracking Fraud Detection Challenge

setwd("C:/FCD/BigDataRAzure/Projetos/Projeto01-fraud")
getwd()

library(caret)
library(readr)
library(gmodels)
library(randomForest)
library(ROSE)

# Lendo o arquivo csv
df <- read_csv("train_sample.csv")

# Transformando os dados
i <- c("ip", "app","device","os","channel")
df[i] <- lapply(df[i] , as.character)
df$is_attributed <- as.factor(df$is_attributed)

# Verificar quantos registros vazios(NA) por coluna
sapply(df, function(x) sum(is.na(x)))


####### Análise Exploratória ####### 
str(df)
summary(df)

length(unique(df$ip))
length(unique(df$app))
length(unique(df$device))
length(unique(df$os))

barplot(table(df$ip),main = "IP ID") #concentracao em alguns IP's (possivel bots?)
barplot(table(df$app),main = "App ID")
barplot(table(df$device), main = "Device ID")
barplot(table(df$os), main = "OS ID")
barplot(table(df$is_attributed), main = "Download efetuado?")

table(df$device)
prop.table(table(df$device))*100

####### SMOTE ####### 

# Retirando variavel atributted_time
dados_treino <- df[,-7]
head(dados_treino)

data_balanced <- ovun.sample(is_attributed ~ ., data = dados_treino, method = "over",
                             N = 199773)$data

table(data_balanced$is_attributed)


####### Construindo o modelo ####### 

## Falhei miseravelmente
# importance <- randomForest(is_attributed ~ ., data= dados_treino, importance=TRUE, proximity=TRUE)
# varImp(importance)

# Feature Selection - Avaliando importancia das variaveis 
importancia <- randomForest(is_attributed ~ ., 
                       data = dados_treino, 
                       ntree = 100, 
                       nodesize = 10, importance = TRUE)

varImpPlot(importancia)


# Criando o modelo
modelo_v1 <- randomForest(is_attributed ~ ., 
                       data = data_balanced, 
                       ntree = 100, 
                       nodesize = 10)

print(modelo_v1)
summary(modelo_v1)

# Outro modelo, sem a Variável 'device'
data_balanced2 <- data_balanced[,-3]
head(data_balanced2)

modelo_v2 <- randomForest(is_attributed ~ ., data = data_balanced2, 
                          ntree = 100, nodesize = 10)

print(modelo_v2)
plot(modelo_v2)

####### Previsoes ####### 
dados_teste <- read_csv("test.csv")
head(dados_teste)

valores_previstos <- predict(modelo_v2, newdata = dados_teste)

table(valores_previstos)

#criando dataframe no formato de entrega final
resultado <- cbind(dados_teste[,1], valores_previstos)
View(resultado)

#Salvando trab.
write_csv(resultado, "Resultado_final.csv")




