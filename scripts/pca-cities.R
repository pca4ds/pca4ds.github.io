
library(FactoMineR)
library(ggplot2)

# working directory is the top-level directory
cities <- read.csv('data/cities94.csv', stringsAsFactors = FALSE)

active <- c(
  "teacher", "bus_driver", "mechanic", "construction_worker",
  "metalworker", "cook_chef", "factory_manager", "engineer",
  "bank_clerk", "executive_secretary", "salesperson", "textile_worker")

dat <- na.omit(cities[ ,c('city', active)])

# table 1.4: summary statistics
summary(dat)

active_vars_statistics <- cbind.data.frame(
  #variable = active,
  weight = nrow(dat),
  mean = colMeans(dat[ ,-1]),
  stdev = apply(dat[ ,-1], 2, sd),
  min = apply(dat[ ,-1], 2, min),
  max = apply(dat[, -1], 2, max)
)
active_vars_statistics


# PCA
pca <- PCA(dat[ ,active], graph = FALSE)


# eigenvalues (table 2.1)
eigs <- data.frame(
  num = 1:nrow(pca$eig),
  eigenvalues = round(pca$eig[,1], 4),
  percentage = round(pca$eig[,2], 2),
  cumulative = round(pca$eig[,3], 2)
)
rownames(eigs) = 1:nrow(eigs)
write.csv(eigs, file = 'data/eigenvalues.csv')


# first factorial plane
pca_ind <- as.data.frame(pca$ind$coord)
pca_ind$city <- dat$city
head(pca_ind)


# figure 1.9 (first factorial plane)
fig_1_9 <- ggplot(data = pca_ind, aes(x = Dim.1, y = Dim.2)) + 
  geom_vline(xintercept = 0, color = 'gray60') +
  geom_hline(yintercept = 0, color = 'gray60') +
  geom_point() + 
  geom_text(aes(label = city)) +
  theme_minimal()

ggsave(filename = 'images/figure-1-9.png', plot = fig_1_9, 
       width = 7, height = 5)


# circle of correlations
png("../images/figure-1-11.png", 
    height = 1000, width = 1000, res = 200)
plot(pca, choix = 'var', las = 1, title = '')
dev.off()



# table 2.2: results for individuals
n <- nrow(dat)
dat_scaled <- sqrt(n/(n-1)) * scale(dat[,-1])

pca_inds <- data.frame(
  city = dat$city,
  wgt = round(100/nrow(dat), 2),
  disto = round(apply(dat_scaled, 1, function(x) sum(x^2)), 2),
  coord1 = round(pca$ind$coord[,1], 2),
  coord2 = round(pca$ind$coord[,2], 2),
  contr1 = round(pca$ind$contrib[,1], 2),
  contr2 = round(pca$ind$contrib[,2], 2),
  cosqr1 = round(pca$ind$cos2[,1], 2),
  cosqr2 = round(pca$ind$cos2[,2], 2)
)

sink('data/table-2-2.txt')
pca_inds
sink()


# figure 2.3 (factorial plane, size of cities with contributions)
fig_2_3 <- ggplot(data = pca_inds, aes(x = coord1, y = coord2)) + 
  geom_vline(xintercept = 0, color = 'gray60') +
  geom_hline(yintercept = 0, color = 'gray60') +
  geom_point(size = pca_inds$contr1, color = 'gray50', alpha = 0.5) +   
  geom_text(aes(label = city), size = 3, color = 'gray20') +
  xlab('Factor 1') +
  ylab('Factor 2') +
  theme_minimal()

ggsave(filename = 'images/figure-2-3.png', plot = fig_2_3, 
       width = 7, height = 5)


# figure 2.5 (factorial plane, squared cosines)
fig_2_5 <- ggplot(data = pca_inds, aes(x = coord1, y = coord2)) + 
  geom_vline(xintercept = 0, color = 'gray60') +
  geom_hline(yintercept = 0, color = 'gray60') +
  geom_point(size = 7*rowSums(pca_inds[ ,c('cosqr1', 'cosqr2')]), 
             color = 'gray50', alpha = 0.5) +   
 # geom_text(aes(label = city), size = 3, color = 'gray20') +
  xlab('Factor 1') +
  ylab('Factor 2') +
  theme_minimal()

ggsave(filename = 'images/figure-2-5.png', plot = fig_2_5, 
       width = 7, height = 5)


# table 2.3: results for variables
pca_vars <- data.frame(
  coord1 = round(pca$var$coord[,1], 2),
  coord2 = round(pca$var$coord[,2], 2),
  coord3 = round(pca$var$coord[,3], 2),
  cor1 = round(pca$var$cor[,1], 2),
  cor2 = round(pca$var$cor[,2], 2),
  cor3 = round(pca$var$cor[,3], 2)
)

save(pca_vars, file = 'data/table-2-3.RData')


# table 2.4: correlations of all variables
var_labels <- c(
  'tea', 'bus', 'mec', 'con', 'met', 'coo', 'dep',
  'eng', 'ban', 'exe', 'sal', 'tex'
)

aux <- order(pca$var$cor[ ,1], decreasing = TRUE)
var_corrs <- round(cor(dat[, active])[aux, aux], 2)
var_corrs[upper.tri(var_corrs, diag = TRUE)] <- NA
rownames(var_corrs) <- var_labels[aux]
colnames(var_corrs) <- var_labels[aux]

save(var_corrs, file = 'data/table-2-4.RData')
