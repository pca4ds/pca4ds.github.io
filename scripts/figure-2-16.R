
# Figure 2.16
# V-test associated to a critical probability

# normal function 
normal <- function(x, mu = 0, sigma = 1) {
  1 / (sigma * sqrt(2*pi)) * exp(-0.5 * ((x - mu)/ sigma)^2)
}

# polygon coordinates of area under the curve
normal_auc <- function(x0, x1, mu = 0, sigma = 1, n = 50) {
  stopifnot(n > 0)
  if (x0 > x1) {
    stop("x0 must be lower than x1")
  }
  xs <- c(
    rep(x0, n),
    seq(x0, x1, length.out = n),
    rep(x1, n),
    rep(x1, x0, length.out = n)
  )
  ys <- c(
    seq(0, normal(x0, mu = mu, sigma = sigma), length.out = n),
    normal(seq(x0, x1, length.out = n), mu = mu, sigma = sigma),
    seq(0, normal(x1, mu = mu, sigma = sigma), length.out = n),
    rep(0, n)
  )
  list(xs = xs, ys = ys)
}


x <- seq(-4, 4, length.out = 500)
y  <- normal(x, mu = 0)
auc1 <- normal_auc(-4, 4, mu = 0)
auc2 <- normal_auc(2.5, 4, mu = 0)

png("images/figure-2-16-bis.png", 
    pointsize = 30, width = 700, height = 500)
op = par(mar = c(2.5,1,1,1))
plot(x, y, type = 'n', col = 'gray50', lwd = 4,
     axes = FALSE, xlab = '', ylab = '')
axis(side = 1, at = seq(-4, 4, 2), line = -0.35, 
     labels = rep("", 5))
polygon(auc1$xs, auc1$ys, col = 'gray85', border = NA)
polygon(auc2$xs, auc2$ys, col = 'blue', border = NA)
lines(x, y, col = 'gray50', lwd = 3)
par(op)
dev.off()
