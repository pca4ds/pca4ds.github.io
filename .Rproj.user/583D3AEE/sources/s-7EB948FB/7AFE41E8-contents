# PCA of dataset cities94.csv
# This is a second PCA with rescaled variables: divided by the 
# mean salary of a city.
# Results for sections 3.8 "PCA and Clustering"

# packages
library(FactoMineR)
library(ggplot2)
library(plotrix)

# working directory is the top-level directory
cities <- read.csv('data/cities94.csv', stringsAsFactors = FALSE)

# names of active variables
active <- c(
  "teacher", "bus_driver", "mechanic", "construction_worker",
  "metalworker", "cook_chef", "factory_manager", "engineer",
  "bank_clerk", "executive_secretary", "salesperson", "textile_worker")

# removing missing values
dat2 <- na.omit(
  cities[ ,c('city', 'net_hourly_salary', 'work_hours_year', active)]
)


# ========================================================
# 2.3) Beyond the first factor
# ========================================================

# mean salary as product of 'net_hourly_salary' x 'work_hours_year'
mean_salary <- dat2[ ,2] * dat2[ ,3]

# data of ratios
# transformed data: dividing by mean salary
dat_trans <- sweep(dat2[ ,active], 1, mean_salary, FUN = "/")
colnames(dat_trans) <- paste(active, '2', sep='')


# PCA on Data of Ratios
pca2 <- PCA(dat_trans, graph = FALSE, ncp = 6)

#load("data/pca2.RData")

pca_matrix <- pca2$ind$coord
rownames(pca_matrix) <- dat2$city
dist_matrix <- dist(pca_matrix, method = "euclidean")^2
clus <- hclust(dist_matrix, method = "ward.D")
plot(clus, hang = -1, cex = 0.8, las = 1, 
     axes = FALSE, xlab = "", ylab = "", 
     main = "", sub = "")
abline(h = 60, col = "red")
cutree(clus, k = 4)

# plot(clus, hang = -1, cex = 0.8, las = 1, 
#      xlab = "", main = "")
# abline(h = 60, col = "red")


# pca_matrix <- dat_trans
# rownames(pca_matrix) <- dat2$city
# dist_matrix <- dist(pca_matrix, method = "minkowski", p = 2)
# clus <- hclust(dist_matrix, method = "ward.D2")
# plot(clus, hang = -1, cex = 0.8, las = 1)
# abline(h = 4, col = "red")


# PCA plot
col_ind <- c("black", "red", "green", "blue")[cutree(clus, k = 4)]
plot(pca2, choix = "ind", col.ind = col_ind)
plot(pca2$ind$coord[,1], pca2$ind$coord[,2])


hcpc2 <- HCPC(pca2, nb.clust = 4, graph = FALSE)
plot(hcpc2, choice = "map", las = 1)


# output object from clustering analysis
save(hcpc2, file = 'data/clustering.RData')


