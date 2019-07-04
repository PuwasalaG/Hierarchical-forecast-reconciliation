library(tsibble)
library(matrixcalc)
library(reshape2)

Prison_bot.ts <- read.csv("Prison.bottom.TS.csv")[,-1]

Prison.gts <- gts(Prison_bot.ts/1e3, characters = c(3,1,9),
                  gnames = c("State", "Gender", "Legal",
                             "State*Gender", "State*Legal",
                             "Gender*Legal"))


Prison_allts <- allts(Prison.gts)

m <- ncol(Prison_bot.ts)
n <- ncol(Prison_allts)
j=1
Train <- Prison_allts[j:(40+j-1),]
Test <- Prison_allts[(40+j):56,]
Bts_tr <- Train[,(n-m+1):n]


##-- Using glmnet package --##

library(glmnet)


y <- Train[,1]

y_diff1 <- diff(y, lag = 1)
B_diff <- diff(Bts_tr, lag = 1)

y_diff1_1 <- lag(y_diff1, 1)
y_diff1_2 <- lag(y_diff1, 2)
y_diff1_3 <- lag(y_diff1, 3)
y_diff1_4 <- lag(y_diff1, 4)

B_diff_1 <- matrix(0, nrow = length(y_diff1), ncol = m)

for (i in 1:m) {
  
  B_diff_1[,i] <- lag(B_diff[,i], 1)
  
}

colnames(B_diff_1) <- colnames(Bts_tr)

y1 <- y_diff1_1[-(1:4)]
y2 <- y_diff1_2[-(1:4)]
y3 <- y_diff1_3[-(1:4)]
y4 <- y_diff1_4[-(1:4)]
X <- data.frame(y1, y2, y3, y4, B_diff_1[-(1:4),]) %>% as.matrix()

Y <- y_diff1[-(1:4)]

y_1 <- lag(y, 1)[-1]


##--LASSO--##

Pars <- list()
lambda <- seq(0, 0.25, 0.01)
y_fc <- numeric(length(lambda))
mean.resid <- numeric(length(lambda))
MSE <- numeric(length(lambda))
Non_zero_Coef <- character(length(lambda))

Lasso_fit <- glmnet(x = X, y = Y, family = c("gaussian"), alpha = 1, lambda = lambda, 
                    thresh = 1e-14)

for (j in 1:length(lambda)) {
  
  Pars[[j]] <- coef(Lasso_fit, s=lambda[j])
  
  par <- Pars[[j]] %>% as.matrix()

  NonzeroCoef <- rownames(Pars[[j]])[nonzeroCoef(Pars[[j]])]
  Non_zero_Coef[j] <- paste(NonzeroCoef, collapse = ",")
  
  Intercept <- rep(1, nrow(B_diff_1))
  X1 <- data.frame(Intercept, y_diff1_1, y_diff1_2, y_diff1_3, y_diff1_4, B_diff_1) %>% 
    as.matrix()
  X1[is.na(X1)] <- 0
  
  y_hat_lasso <- t(par) %*% t(X1) + y_1
  
  mean.resid[j] <- mean(y[-1] - y_hat_lasso)
  MSE[j] <- mean((y[-1] - y_hat_lasso)^2)
  
  t <- length(y)
  const_lasso <- par[1]
  alpha_lasso <- par[2:5]
  beta_lasso <- par[6:37]
  
  
  y_fc[j] <- const_lasso + y[t] + alpha_lasso[1]*(y[t]-y[t-1]) + 
    alpha_lasso[2]*(y[t-1] - y[t-2]) + alpha_lasso[3]*(y[t-2] - y[t-3]) + 
    alpha_lasso[4]*(y[t-3] - y[t-4]) + beta_lasso %*% (Bts_tr[(t),] - Bts_tr[(t-1),])
  
}


data.frame(lambda, y_fc, mean.resid, Non_zero_Coef) %>% View()

# y_hat_lasso <- numeric(length(y))
# 
# y_hat_lasso[1] <- const_lasso 
# y_hat_lasso[2] <- const_lasso + y[1] + alpha_lasso[1]*y[1] + beta_lasso %*% Bts_tr[1,]
# y_hat_lasso[3] <- const_lasso + y[2] + alpha_lasso[1]*(y[2]-y[1]) + alpha_lasso[2]*y[1] + beta_lasso %*% (Bts_tr[2,] - Bts_tr[1,])
# y_hat_lasso[4] <- const_lasso + y[3] + alpha_lasso[1]*(y[3]-y[2]) + alpha_lasso[2]*(y[2] - y[1]) + alpha_lasso[3]*y[1] +
#   beta_lasso %*% (Bts_tr[3,] - Bts_tr[2,])
# y_hat_lasso[5] <- const_lasso + y[4] + alpha_lasso[1]*(y[4]-y[3]) + alpha_lasso[2]*(y[3] - y[2]) + 
#   alpha_lasso[3]*(y[2] - y[1]) + alpha_lasso[4]*y[1] + beta_lasso %*% (Bts_tr[4,] - Bts_tr[3,])
# y_hat_lasso[6] <- const_lasso + y[5] + alpha_lasso[1]*(y[5]-y[4]) + alpha_lasso[2]*(y[4] - y[3]) + 
#   alpha_lasso[3]*(y[3] - y[2]) + alpha_lasso[4]*(y[2] - y[1]) + beta_lasso %*% (Bts_tr[5,] - Bts_tr[4,])
# 
# 
# for (i in 7:length(y)) {
#   
#   y_hat_lasso[i] <- const_lasso + y[i-1] + alpha_lasso[1]*(y[i-1]-y[i-2]) + 
#     alpha_lasso[2]*(y[i-2] - y[i-3]) + alpha_lasso[3]*(y[i-3] - y[i-4]) + 
#     alpha_lasso[4]*(y[i-4] - y[i-5]) + beta_lasso %*% (Bts_tr[(i-1),] - Bts_tr[(i-2),])
#   
# }

CV_lasso <- cv.glmnet(x = X, y = Y, family = "gaussian", lambda = seq(0,1,0.05))
opt.lambda <- CV_lasso$lambda.min

