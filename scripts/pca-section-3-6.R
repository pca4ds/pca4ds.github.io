# PCA of dataset cities94.csv
# Results for section 3.6

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
dat <- na.omit(cities[ ,c('city', active)])

# PCA
pc1 <- prcomp(dat[ ,active], scale. = TRUE)


# figure 3.4
png("images/figure-3-4.png", width = 800, height = 800, res = 150)
op = par(mar = c(4.5, 4.5, 1, 1))
plot(-1 * pc1$rotation[ ,1], pc$rotation[ ,2], 
     xlim = c(-0.1, 0.5), ylim = c(-0.45, 0.55), type = "n", las = 1,
     xlab = "Factor 1", ylab = "Factor 2")
abline(h = 0, v = 0, lty = 2, col = "gray50")
arrows(x0 = rep(0, length(active)), y0 = rep(0, length(active)),
       x1 = -1 * pc1$rotation[ ,1], y1 = pc$rotation[ ,2],
       length = 0.1, col = "gray70")
text(-1 * pc1$rotation[ ,1], pc$rotation[ ,2], labels = abbreviate(active, 18), 
     offset = 0.1, cex = 0.9, pos = 4, xpd = TRUE)
par(op)
dev.off()
