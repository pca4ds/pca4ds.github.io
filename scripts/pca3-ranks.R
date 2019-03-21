# PCA of dataset cities94.csv
# This is one of the PCAs discussed in chapter 3 
# performed on the table of ranks
# Results for sections 3.4 "Analysis of a table of ranks"

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

# mean salary as product of 'net_hourly_salary' x 'work_hours_year'
mean_salary <- dat2[ ,2] * dat2[ ,3]

var2_labels <- c(
  'tea2', 'bus2', 'mec2', 'con2', 'met2', 'coo2', 'dep2',
  'eng2', 'ban2', 'exe2', 'sal2', 'tex2'
)

# transformed data
dat_trans <- sweep(dat2[ ,active], 1, mean_salary, FUN = "/")
colnames(dat_trans) <- paste(active, '2', sep='')


# ===================================================================
# table 3.6: Spearman correlations of all variables
# ===================================================================
# Table of Ranks (Spearman)
dat_rank <- cor(dat_trans, method = "spearman")
cor_rank <- round(dat_rank, 2)
table_3_6 <- cor_rank

table_3_6[upper.tri(table_3_6, diag = FALSE)] <- NA
rownames(table_3_6) <- var2_labels
colnames(table_3_6) <- var2_labels
table_3_6

save(table_3_6, file = 'data/table-3-6.RData')


# ===================================================================
# PCA of table of ranks
# ===================================================================

# table 3.7: eigenvalues of PCA on ranks table
evd <- eigen(dat_rank)

table_3_7 <- data.frame(
  num = 1:length(evd$values),
  eigenvalue = round(evd$values, 4),
  percentage = round(100 * evd$values / sum(evd$values), 2),
  cumulative = round(100 * cumsum(evd$values / sum(evd$values)), 2)
)
rownames(table_3_7) = 1:nrow(table_3_7)

save(table_3_7, file = 'data/table-3-7.RData')


# table 3.8: results for active variables
table_3_8 <- round(
  cbind(
  evd$vectors[,1:3] %*% diag(sqrt(evd$values[1:3])),
  evd$vectors[,1:3] %*% diag(sqrt(evd$values[1:3]))),
  digits = 2
)
colnames(table_3_8) <- paste0(rep(c("coord", "cor"), each = 3), 1:3)
rownames(table_3_8) <- paste0(active, '2')

save(table_3_8, file = 'data/table-3-8.RData')


# ===================================================================
# PCA plot
# ===================================================================

# function to create a circle
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

corcir <- circle(c(0, 0), npoints = 100)

# PCA plot
png("images/figure-3-2.png", width = 800, height = 800, res = 150)
op = par(mar = c(4.5, 4.5, 1, 1))
plot(table_3_8[ ,1:2], xlim = c(-1.1,1.1), ylim = c(-1.1, 1.1), las = 1,
     xlab = "Factor 1", ylab = "Factor 2", type = "n")
lines(corcir[,1], corcir[,2], col = "gray50")
abline(h = 0, v = 0, col = "gray50", lty = 2)
arrows(x0 = rep(0, nrow(table_3_8)), x1 = table_3_8[ ,1],
       y0 = rep(0, nrow(table_3_8)), y1 = table_3_8[ ,2],
       length = 0.1, col = "gray50")
text(table_3_8[ ,1], table_3_8[ ,2], labels = active, xpd = TRUE)
par(op)
dev.off()

