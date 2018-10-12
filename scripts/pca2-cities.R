# PCA of dataset cities94.csv
# This is a second PCA with rescaled variables: divided by the 
# mean salary of a city.
# Results for sections 2.3 "Beyond the First Factor"

# packages
library(FactoMineR)
library(ggplot2)

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


# min_salary <- apply(dat[ ,-1], 1, min)
# max_salary <- apply(dat[ ,-1], 1, max)
# mean_salary <- rowMeans(dat[ ,-1])
# 
# # adding salary inequality of cities
# rdat$salary_ineq <- (max_salary - min_salary) / mean_salary
# 
# # adding qualified manual jobs
# rdat$qual_jobs <- (rdat$mechanic + rdat$metalworker) / (2 * mean_salary)

# transformed data
dat_trans <- sweep(dat2[ ,active], 1, mean_salary, FUN = "/")
colnames(dat_trans) <- paste(active, '2', sep='')


# table 2.5: summary statistics of second analysis
trans_vars_statistics <- cbind.data.frame(
  #variable = active,
  weight = nrow(dat2),
  mean = round(colMeans(dat_trans), 2),
  stdev = round(apply(dat_trans, 2, sd), 2),
  min = round(apply(dat_trans, 2, min), 2),
  max = round(apply(dat_trans, 2, max), 2)
)
trans_vars_statistics

save(trans_vars_statistics, file = 'data/table-2-5.RData')


# table 2.6: correlations of all variables
var2_labels <- c(
  'tea2', 'bus2', 'mec2', 'con2', 'met2', 'coo2', 'dep2',
  'eng2', 'ban2', 'exe2', 'sal2', 'tex2'
)

var_trans_corrs <- round(cor(dat_trans), 2)
var_trans_corrs[upper.tri(var_trans_corrs, diag = FALSE)] <- NA
rownames(var_trans_corrs) <- var2_labels
colnames(var_trans_corrs) <- var2_labels

save(var_trans_corrs, file = 'data/table-2-6.RData')


# PCA of Transformed Data
pca2 <- PCA(dat_trans, graph = FALSE)


# eigenvalues (table 2.7) of 2nd analysis
eigs2 <- data.frame(
  num = 1:nrow(pca2$eig),
  eigenvalues = round(pca2$eig[,1], 4),
  percentage = round(pca2$eig[,2], 2),
  cumulative = round(pca2$eig[,3], 2)
)
rownames(eigs2) = 1:nrow(eigs2)
write.csv(eigs2, file = 'data/table-2-7.csv')


# table 2.8: results for variables of 2nd analysis
pca2_vars <- data.frame(
  coord1 = round(pca2$var$coord[,1], 2),
  coord2 = round(pca2$var$coord[,2], 2),
  coord3 = round(pca2$var$coord[,3], 2),
  cor1 = round(pca2$var$cor[,1], 2),
  cor2 = round(pca2$var$cor[,2], 2),
  cor3 = round(pca2$var$cor[,3], 2)
)

save(pca2_vars, file = 'data/table-2-8.RData')


# figure 2.9 (circle of correlations)
png("images/figure-2-9.png", 
    height = 1000, width = 1000, res = 200)
plot(pca2, choix = 'var', las = 1, title = '')
dev.off()



# table 2.9: results for individuals of 2nd analysis
n <- nrow(dat2)
dat_scaled2 <- sqrt(n/(n-1)) * scale(dat_trans)

pca2_inds <- data.frame(
  city = dat2$city,
  wgt = round(100/nrow(dat2), 2),
  disto = round(apply(dat_scaled2, 1, function(x) sum(x^2)), 2),
  coord1 = round(pca2$ind$coord[,1], 2),
  coord2 = round(pca2$ind$coord[,2], 2),
  contr1 = round(pca2$ind$contrib[,1], 2),
  contr2 = round(pca2$ind$contrib[,2], 2),
  cosqr1 = round(pca2$ind$cos2[,1], 2),
  cosqr2 = round(pca2$ind$cos2[,2], 2)
)

write.csv(pca2_inds, file = 'data/table-2-9.csv')



# figure 2.10 (first factorial plane)
pca2_ind <- as.data.frame(pca2$ind$coord)
pca2_ind$city <- dat2$city

fig_2_10 <- ggplot(data = pca2_ind, aes(x = Dim.1, y = Dim.2)) + 
  geom_vline(xintercept = 0, color = 'gray60') +
  geom_hline(yintercept = 0, color = 'gray60') +
  geom_point(color = 'gray50') + 
  geom_text(aes(label = city), alpha = 0.6) +
  theme_minimal()

ggsave(filename = 'images/figure-2-10.png', plot = fig_2_10, 
       width = 7, height = 5)

