library(psych)
library(factoextra)
library(dplyr)

summary(wine)

set.seed(1)
k_1 <- kmeans(scale(wine[, -1]), centers = 1, nstart = 10)
k_2 <- kmeans(scale(wine[, -1]), centers = 2, nstart = 10)
k_3 <- kmeans(scale(wine[, -1]), centers = 3, nstart = 10)
k_4 <- kmeans(scale(wine[, -1]), centers = 4, nstart = 10)
k_5 <- kmeans(scale(wine[, -1]), centers = 5, nstart = 10)
k_6 <- kmeans(scale(wine[, -1]), centers = 6, nstart = 10)

plot(1:6,  
     c(k_1$tot.withinss,
       k_2$tot.withinss, 
       k_3$tot.withinss, 
       k_4$tot.withinss, 
       k_5$tot.withinss, 
       k_6$tot.withinss),
     type = "b", ylab = "within clusters ss", xlab = "k")

table(obs = wine$Cultivar, est = k_3$cluster)



#Animal

rownames(animals)
summary(animals)

plot(hclust(dist(animals), method = "complete"), cex = .8)
plot(hclust(dist(animals), method = "ward.D2"),  cex = .8)

plot(hclust(dist(scale(animals))), cex = .8)
plot(hclust(dist(scale(animals)), method = "ward.D2"), cex = .8)

boxplot(scale(animals))

animals <- animals %>% mutate_at(vars(bw, brw), log)

plot(hclust(dist(scale(animals))), cex = .8)
plot(hclust(dist(scale(animals)), method = "ward.D2"), cex = .8)

