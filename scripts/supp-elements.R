# Second PCA of dataset cities94.csv
# Results for sections 2.4 "Using Supplementary Elements"
# We use the results from the second PCA with rescaled variables
# in order to project the supplementary variables
# (working directory is the top-level directory)

# packages
library(FactoMineR)
library(ggplot2)
library(dplyr)

# omit cities 10 and 23 (due to NAs)
cities <- read.csv('data/cities94.csv', stringsAsFactors = FALSE)
cities2 <- cities[-c(10,23), ]

# results from 1st PCA
load('data/pca1.RData')
pca1 <- pca$ind$coord
  
# results from 2nd PCA
load('data/pca2.RData')
pca2 <- pca2$ind$coord



# ========================================================
# Deriving new variables: 
# 'salary_inequality' and 'manual_jobs'
# ========================================================

# names of active variables
active <- c(
  "teacher", "bus_driver", "mechanic", "construction_worker",
  "metalworker", "cook_chef", "factory_manager", "engineer",
  "bank_clerk", "executive_secretary", "salesperson", "textile_worker")

# removing missing values
dat2 <- na.omit(
  cities[ ,c('city', 'net_hourly_salary', 'work_hours_year', active)]
)

# mean salary as product of 'net_hourly_salary' x 'work_hours_year'
mean_salary <- dat2[ ,2] * dat2[ ,3]

min_salary <- apply(dat2[ ,-1], 1, min)
max_salary <- apply(dat2[ ,-1], 1, max)
#mean_salary <- rowMeans(dat2[ ,-1])

# adding salary inequality of cities
salary_inequality <- (max_salary - min_salary) / mean_salary

# adding qualified manual jobs
manual_jobs <- (dat2$mechanic + dat2$metalworker) / (2 * mean_salary)


# ========================================================
# Table 2.10: 
# ========================================================
# Quantitative supplementary variables for 2nd PCA

# projection of quant supplementary variables
quant_supp_table <- rbind(
  cor(cities2[3:40], pca2[ ,1:3]),
  cor(pca1, pca2[ ,1:3]),
  cor(salary_inequality, pca2[ ,1:3]),
  cor(manual_jobs, pca2[ ,1:3])
)


# fix correlation with public transportation
# (has a missing value)
public_trans <- as.vector(
  cor(cities2$public_transportation[-1], pca2[-1,1:3], use = 'na.or.complete')
)
quant_supp_table['public_transportation', ] <- public_trans

rownames(quant_supp_table) <- c(
  colnames(cities2[3:40]), 
  paste0('Axis', 1:5), 
  'salary_inequality', 'manual_qualified')

quant_supp_table <- round(quant_supp_table, 2)

# export table 2-10
save(quant_supp_table, file = 'data/table-2-10.RData')


# ========================================================
# Figure 2.13: 
# ========================================================
# "World regions" as nominal supplementary variable


pca2_region <- cbind.data.frame(
  pca2,
  region = cities$region[-c(10,23)]
)

pca2_region %>% 
  group_by(region) %>%
  summarise(
    dim1 = mean(Dim.1),
    dim2 = mean(Dim.2)
  )

fig_2_13 <- ggplot() + 
  geom_point(data = as.data.frame(pca2), aes(x = Dim.1, y = Dim.2),
             color = 'white') + 
  geom_vline(xintercept = 0, color = 'gray60') +
  geom_hline(yintercept = 0, color = 'gray60') +
  theme_minimal()

ggsave(filename = 'images/figure-2-13.png', plot = fig_2_10, 
       width = 7, height = 5)
