library("glmnet")

set.seed(20)
load("Leukemia.RData")
x <- Leukemia$x
y <- Leukemia$y

system.time(rlas<-glmnet(x, y,  family = "binomial", alpha = 1,  lambda.min = 1e-4))
system.time(rrid<-glmnet(x, y,  family = "binomial", alpha = 0,  lambda.min = 1e-4))
system.time(renet<-glmnet(x, y, family = "binomial", alpha = .2, lambda.min = 1e-4))

nsteps <- 10
b1 <- coef(rlas)[-1, 1:nsteps]
w <- nonzeroCoef(b1)
b1 <- as.matrix(b1[w, ])

b2 <- coef(rrid)[-1, 1:nsteps]
w <- nonzeroCoef(b2)
b2 <- as.matrix(b2[w, ])

b3 <- coef(renet)[-1, 1:nsteps]
w <- nonzeroCoef(b3)
b3 <- as.matrix(b3[w, ])

ylim <- range(b1, b2, b3)

png(file = "leuk2.png", height = 1100, width = 800, res = 144)
par(mfrow = c(1, 3))

matplot(1:nsteps, t(b1), type = "o", pch = 19, col = "blue", xlab = "Step", ylab = "Coefficients", ylim = ylim, lty = 1)
title("Lasso")
abline(h = 0, lty = 2)

matplot(1:nsteps, t(b3), type = "o", pch = 19, col = "blue", xlab = "Step", ylab = "Coefficients", ylim = ylim, lty = 1)
title("Elastic Net")

matplot(1:nsteps, t(b2), type = "o", pch = 19, col = "blue", xlab = "Step", ylab = "Coefficients", ylim = ylim, lty = 1)
title("Ridge Regression")

dev.off()


set.seed(1010)
n <- 1000
p <- 100
nzc <- trunc(p/10)
x <- matrix(rnorm(n*p), n, p)
beta <- rnorm(nzc)
fx <- x[, seq(nzc)]%*%beta
eps <- rnorm(n)*5
y <- drop(fx+eps)
px <- exp(fx)
px <- px/(1+px)
ly <- rbinom(n = length(px), prob = px, size = 1)


pdf(file = "cveg.pdf", width = 8, height = 8, pointsize = 12)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 4, 1))
# fit1 <- glmnet(x, y)
junk1 <- cv.glmnet(x, y)
plot(junk1)
title("Gaussian Family", line = 2.5)
frame()
# fit2 <- glmnet(x, ly, family = "binomial")
set.seed(1011)
junk2 <- cv.glmnet(x, ly, family = "binomial")
plot(junk2)
title("Binomial Family", line = 2.5)

set.seed(1011)
junk3 <- cv.glmnet(x, ly, family = "binomial", type = "class")
plot(junk3)
title("Binomial Family", line = 2.5)

dev.off()

