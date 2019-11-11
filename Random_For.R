#setar#

setwd('C:/Users/Admin/Desktop/Teste R')
## random forest #


##O algoritmo funciona agregando as predicoes feitas arvores de decisao multiplas##

#pacote random forest#

install.packages('randomForest')
library(randomForest)
require(caTools)
library(dplyr)

# baixar os dados do exemplo #

data <- read.csv("processed.cleveland.data",
                  header = FALSE)
dim(data)


# como os dados do exemplo nao tem cabecalho, necessario nomea-los #

names(data) <- c("age",
                 "sex",
                 "cp",
                 "trestbps",
                 "choi",
                 "fbs",
                 "restecg",
                 "thalach",
                 "exang",
                 "oldpeak",
                 "slope",
                 "ca",
                 "thai",
                 "num")


head(data)


# o exemplo segue um resuldado diatico, (sim ou nao) - nao usa a variavel ordinal #
# portanto, melhor criar uma variavel 0-1 #

data$num[data$num > 1] <- 1


summary(data)


#A base de dados interpreta fatores como numero, tal qual a variavel sex#

sapply(data,
       class)


# mudar a classe das variaveis #

data <- data %>% 
        transform(age = as.integer(age),
                  sex = as.factor(sex),
                  cp = as.factor(cp),
                  trestbps = as.integer(trestbps),
                  choi = as.integer(choi),
                  fbs = as.factor(fbs),
                  restecg = as.factor(restecg),
                  thalach = as.integer(thalach),
                  exang = as.factor(exang),
                  oldpeak = as.numeric(oldpeak),
                  slope = as.numeric(slope),
                  ca = as.factor(thai),
                  num = as.factor(num))


data %>% sapply(class)

#alterar o ? para na#

data[ data == '?'] <- NA


# substituir os valores NA de thai por normal = 3 #


data$thai[which(is.na(data$thai))] <- as.factor("3.0")

# retirar as colunas de ca que sao NA #

data <- data[!(data$ca %in% c(NA)),]

colSums(is.na(data))


# ca e thai para fatores #

data$ca <- factor(data$ca)
data$thai <- factor(data$thai)


# separar a amostra de treino e teste #

sample = sample.split(data$num, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)


dim(train)
dim(test)


## Depois da amostra preparada, o RF pode ser aplicada ##

rf <- randomForest(num ~.,
                   data = train)

rf


# predicao na amostra de teste #

pred = predict(rf,
               newdata = test[-14])


# matriz para avaliar as predicoes corretas [confusion matrix] #

cm = table(test[,14],
           pred)
cm
