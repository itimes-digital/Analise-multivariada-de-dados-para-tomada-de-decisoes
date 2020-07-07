# cluster analysis
# import dataset
install.packages('pvclust')
library(cluster)

#varejo_UN4_ <- read_excel("D:/cursos/Análise multivariada de dados para tomada de decisões/varejo  UN4 .xlsx")
#Desempenho_UN4 <- read_delim("D:/cursos/Análise multivariada de dados para tomada de decisões/Desempenho UN4.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Siderur_clusterteste_UN4 <- read_delim("D:/cursos/Análise multivariada de dados para tomada de decisões/Siderur_clusterteste UN4.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Retirada de todas as linhas NA
#Siderur_clusterteste_UN4 <- Siderur_clusterteste_UN4[complete.cases(Desempenho_UN4),]

# values of data set (mydata)
mydata <- Siderur_clusterteste_UN4[, 2:5]

#scaled matrix (zscores)
mydata <- scale(mydata)

# Distance matrix by Euclidean
d <- dist(mydata, method = 'euclidean')

# Distância calculada para todas as variáveis
d

# Hierarchial Method - Single
# Calcula os clusters hierárquico de acordo com a distância definida
fit <- hclust(d, method = 'single')

fit

# plotando um gráfico dendrogram
plot(fit)

# k_clusters
group <- cutree(fit, k = 3)

# Definindo visualmente os grupos
rect.hclust(fit, k = 3, border = 'red')
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

