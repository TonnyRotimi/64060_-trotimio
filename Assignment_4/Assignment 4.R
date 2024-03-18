library(ISLR)
library(caret)
library(class)
library(gmodels)
library(dplyr)
library(e1071)
library(cluster)
library(factoextra)
library(kernlab)
library(ggcorrplot)
library(broom)




pharma <- read.csv('C:/Users/rotim/Documents/R/Pharmaceuticals.csv')
summary(pharma)

#To remove the character variables
pharma <-pharma[,-c(1,2,12,13,14)]

#To scale  the data which is important when clustering
pharma <- scale(pharma)

#First we observe the correlation between our features.
pharma_corr<- cor(pharma)

ggcorrplot(pharma_corr,
           outline.color = "grey50",
           lab = TRUE,
           hc.order = TRUE,
           type = "full")

# From the correlation matrix, we see most of the features are moderately correlated, with the exception of the following variables which exhibited a high correlation of 0.7 and above between each other; (ROA-Market_cap,RoE-ROA, Net_profit_margin-ROA). Due to the significance of the variables in this analysis, we do not omit or replace them.

#To determine the optimal no of clusters using the fviz_nbclust() 
fviz_nbclust(pharma, kmeans, method = "wss")
#From the graph below we see the elbow point at 2, hence we use 2 clusters.
set.seed(123)
cluster <- kmeans(pharma, centers = 2, nstart = 25)

#To see the centers of each cluster, from this we can explain different characteristics of the dataset
cluster$centers

#From the first cluster, we see the pharmaceuticals are mostly have a high Return on Equity (ROE)
# and Return on Assets (ROA), indicating they make decent Net income/earnings, visible in the fact
#that they also have high Net profit margins. We also see these companies are not heavily leveraged
#hence reducing their interest expense burden and allowing for a higher net profit margin

cluster_table <- table(cluster$cluster)
cluster_table


fviz_cluster(cluster, data = pharma)

#From the output below we see the optimal no of clusters to use is 10