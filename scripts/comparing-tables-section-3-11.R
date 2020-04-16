# packages
library(FactoMineR)
library(ggplot2)

# working directory is the top-level directory
cities94 <- read.csv('data/cities94.csv', stringsAsFactors = FALSE)
cities91 <- read.csv('data/cities91.csv', stringsAsFactors = FALSE)

# names of active variables
active <- c(
  "teacher", "bus_driver", "mechanic", "construction_worker",
  "metalworker", "cook_chef", "factory_manager", "engineer",
  "bank_clerk", "executive_secretary", "salesperson", "textile_worker")

# removing missing values
dat2 <- na.omit(
  cities94[ ,c('city', 'net_hourly_salary', 'work_hours_year', active)]
)


dat91 <- cities91[ ,c('city', active)]

to_exclude <- c("Manama", "Prague", "Budapest", "AbuDhabi", "Bangkok")
                #"Bangkok", "Cairo", "KualaLumpur")
to_include <- !(cities94$city %in% to_exclude)


# ========================================================
# 2.3) Beyond the first factor
# ========================================================

# mean salary as product of 'net_hourly_salary' x 'work_hours_year'
mean_salary <- dat2[ ,2] * dat2[ ,3]

# transformed data
dat_trans <- sweep(dat2[ ,active], 1, mean_salary, FUN = "/")
colnames(dat_trans) <- paste(active, '2', sep='')

# PCA of Transformed Data
pca2 <- PCA(dat_trans, graph = FALSE)

# first factorial plane
pca_ind <- as.data.frame(pca2$ind$coord[ ,1:2])
pca_ind$city <- dat2$city
pca_ind$year <- 1994
pca_ind <- pca_ind[!(pca_ind$city %in% to_exclude), ]


# Cities 1991
dat91_trans <- sweep(dat91[ ,active], 1, mean_salary[to_include], FUN = "/")
dat91_supp <- sweep(dat91_trans, 2, colMeans(dat_trans), "-")
dat91_supp <- sweep(dat91_supp, 2, apply(dat_trans, 2, sd), "/")

diag(1/pca2$svd$vs) %*% pca2$svd$V[ ,1:2]

more <- as.data.frame(as.matrix(dat91_supp) %*% pca2$svd$V[ ,1:2])
more <- as.data.frame(as.matrix(dat91_supp) %*% diag(1/pca2$svd$vs) %*% pca2$svd$V[ ,1:2])
more$city <- dat91$city
more$year <- 1991
colnames(more) <- c("Dim.1", "Dim.2", "city", "year")

dat <- rbind.data.frame(pca_ind, more)
dat$year <- as.factor(dat$year)

# figure 1.9 (first factorial plane)
ggplot(data = dat, aes(x = Dim.1, y = Dim.2)) + 
  geom_vline(xintercept = 0, color = 'gray60') +
  geom_hline(yintercept = 0, color = 'gray60') +
  geom_point(aes(color = year)) + 
  geom_text(aes(label = city, color = year)) +
  theme_minimal()

