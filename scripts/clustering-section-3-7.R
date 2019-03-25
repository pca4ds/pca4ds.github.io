# PCA of dataset cities94.csv
# This is a second PCA with rescaled variables: divided by the 
# mean salary of a city.
# Results for sections 2.3 "Beyond the First Factor"

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


# min_salary <- apply(dat2[ ,-1], 1, min)
# max_salary <- apply(dat2[ ,-1], 1, max)
# mean_salary <- rowMeans(dat2[ ,-1])
# 
# # adding salary inequality of cities
# rdat$salary_ineq <- (max_salary - min_salary) / mean_salary
# 
# # adding qualified manual jobs
# rdat$qual_jobs <- (rdat$mechanic + rdat$metalworker) / (2 * mean_salary)

# transformed data
dat_trans <- sweep(dat2[ ,active], 1, mean_salary, FUN = "/")
colnames(dat_trans) <- paste(active, '2', sep='')


# table 2.6: correlations of all variables
var2_labels <- c(
  'tea2', 'bus2', 'mec2', 'con2', 'met2', 'coo2', 'dep2',
  'eng2', 'ban2', 'exe2', 'sal2', 'tex2'
)


# PCA of Transformed Data
pca2 <- PCA(dat_trans, graph = FALSE, ncp = 5)

#load("data/pca2.RData")

pca_matrix <- pca2$ind$coord
rownames(pca_matrix) <- dat2$city
dist_matrix <- dist(pca_matrix, method = "euclidean")
clus <- hclust(dist_matrix, method = "ward.D2")
plot(clus, hang = -1, cex = 0.8, las = 1)
abline(h = 4, col = "red")


pca_matrix <- dat_trans
rownames(pca_matrix) <- dat2$city
dist_matrix <- dist(pca_matrix, method = "minkowski", p = 2)
clus <- hclust(dist_matrix, method = "ward.D2")
plot(clus, hang = -1, cex = 0.8, las = 1)
abline(h = 4, col = "red")
