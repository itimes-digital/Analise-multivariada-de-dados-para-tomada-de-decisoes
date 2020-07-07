# cluster analysis
# import dataset
install.packages('pvclust')
library(cluster)
library(readxl)

base <- read_excel("D:/cursos/Análise multivariada de dados para tomada de decisões/Desempenho UN4.xlsx")

# Retirada de todas as linhas NA
base <- base[complete.cases(base),]

# values of data set (mydata)
mydata <- base[, 2:5]

# Elimina variável não esperada no escopo da análise
mydata$Lideranca <- NULL

# Houve um comportamento não esperado que a função
# scale não estava funcionando, tive que transformar
# em número para poder funcionar a padronização.
mydata$Pontualidade <- as.numeric(mydata$Pontualidade)
mydata$Produtividade <- as.numeric(mydata$Produtividade)
mydata$Relacionamento <- as.numeric(mydata$Relacionamento)

#scaled matrix (zscores)
mydata <- scale(mydata)

# Distância calculada para todas as variáveis
# Distance matrix by Euclidean
d <- dist(mydata, method = 'euclidean');d

# Hierarchial Method - Single
# Calcula os clusters hierárquico de acordo com a distância definida
fit <- hclust(d, method = 'single');fit

# plotando um gráfico dendrogram
plot(fit)

# Definindo visualmente os grupos
rect.hclust(fit, k = 3, border = 'red')
rect.hclust(fit, k = 4, border = 'orange')
rect.hclust(fit, k = 5, border = 'blue')
rect.hclust(fit, k = 6, border = 'yellow')


# fraw rectugular red boards to separate groups 
library(pvclust)

# calcular o valor-p de cada cluster, calcula como os cluster podem ser divididos.

# automated selection of groups
mydata_modif <- t(mydata)

result <- pvclust(mydata_modif, method.dist = 'cor', method.hclust='average', nboot = 1000)
plot(result)
pvrect(result, alpha = 0.95)#percentual de confiança

# k-means com 3 clusters - validation and interpretation
# K-means realiza cluster não hierárquicos
fit <- kmeans(mydata, 3);fit

fit <- kmeans(mydata, 4);fit

fit <- kmeans(mydata, 5);fit

# resultado de agrupamento/cluster pelo kmeans
fit

