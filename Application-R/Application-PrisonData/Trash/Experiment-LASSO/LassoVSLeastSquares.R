library(tsibble)
library(matrixcalc)

# Y[,1] %>% as.ts() %>% autoplot()

Train <- Prison_allts[j:(48+j-1),]
Test <- Prison_allts[(48+j):56,]
Bts_tr <- Train[,(n-m+1):n]


##-- Using glmnet package --##

library(glmnet)


y <- Train[,2]

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

X <- data.frame(y_diff1_1[-(1:4)], y_diff1_2[-(1:4)], y_diff1_3[-(1:4)], y_diff1_4[-(1:4)], 
                B_diff_1[-(1:4),]) %>% as.matrix()

Y <- y_diff1[-(1:4)]
y_1 <- lag(y, 1)[-1]

## To get fitted values
Intercept <- rep(1, nrow(B_diff_1))
X1 <- data.frame(Intercept, y_diff1_1, y_diff1_2, y_diff1_3, y_diff1_4, B_diff_1) %>% 
  as.matrix()
X1[is.na(X1)] <- 0


##--LASSO--##

Lasso_fit <- glmnet(x = X, y = Y, family = c("gaussian"), alpha = 1)
CV_lasso <- cv.glmnet(x = X, y = Y, family = "gaussian", lambda = seq(0,0.25,0.01))
opt.lambda <- CV_lasso$lambda.min

Pars <- coef(Lasso_fit, s=opt.lambda) %>% as.matrix()


y_hat_lasso <- t(Pars) %*% t(X1) + y_1

t <- length(y)
const_lasso <- Pars[1]
alpha_lasso <- Pars[2:5]
beta_lasso <- Pars[6:37]


y_lasso_fc <- const_lasso + y[t] + alpha_lasso[1]*(y[t]-y[t-1]) + 
  alpha_lasso[2]*(y[t-1] - y[t-2]) + alpha_lasso[3]*(y[t-2] - y[t-3]) + 
  alpha_lasso[4]*(y[t-3] - y[t-4]) + beta_lasso %*% (Bts_tr[(t),] - Bts_tr[(t-1),])

y_lasso_fc -  mean(y[-1] - y_hat_lasso)

# const_lasso <- Pars[1]
# alpha_lasso <- Pars[2:5]
# beta_lasso <- Pars[6:37]
# 
# y <- Train[,1]
# y_1 <- lag(y, 1)[-(1:5)]
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
# 
# t <- length(y)
# y_lasso_fc <- y[t] + alpha_lasso[1]*(y[t]-y[t-1]) + 
#   alpha_lasso[2]*(y[t-1] - y[t-2]) + alpha_lasso[3]*(y[t-2] - y[t-3]) + 
#   alpha_lasso[4]*(y[t-3] - y[t-4]) + beta_lasso %*% (Bts_tr[(t),] - Bts_tr[(t-1),])





##--Least Sqaures--##

# Intercept <- rep(1, nrow(B_diff_1))
# X <- data.frame(Intercept, y_diff1_1, y_diff1_2, y_diff1_3, y_diff1_4, B_diff_1) %>% as.matrix()

# Pars_LS <- solve(t(X) %*% X) %*% t(X) %*% Y

Data <- data.frame(Y, X) %>% as.ts()
xnam <- colnames(Data)[2:37]
fmla <- as.formula(paste("Y ~ ", paste(xnam, collapse= "+")))
LS_fit <- tslm(formula = fmla, data = Data)
Pars_LS <- coefficients(LS_fit) 
Pars_LS[is.na(Pars_LS)] <- 0

y_hat_LS <- t(Pars_LS) %*% t(X1) + y_1

const_LS <- Pars_LS[1]
alpha_LS <- Pars_LS[2:5]
beta_LS <- Pars_LS[6:37]

y_LS_fc <- const_LS + y[t] + alpha_LS[1]*(y[t]-y[t-1]) + 
  alpha_LS[2]*(y[t-1] - y[t-2]) + alpha_LS[3]*(y[t-2] - y[t-3]) + 
  alpha_LS[4]*(y[t-3] - y[t-4]) + beta_LS %*% (Bts_tr[(t),] - Bts_tr[(t-1),])


y_LS_fc -  mean(y[-1] - y_hat_LS)


###---Using BigVAR package ---###

# Y <- cbind(Train[,1], Bts_tr) %>% as.matrix()
# Y_diff1 <- diff(Y, lag = 1)
# 
# VARX <- list()
# VARX$k = 1
# VARX$s = 1
# #
# #
# #VARX with LASSO for aggregate levels
# Var_agg_lasso.ft <- constructModel(as.matrix(Y_diff1), p = 4, struct = "Basic", gran = c(1),
#                                    verbose = F, VARX = VARX, h=1, ownlambdas = T)
# Var_agg_lasso.Results <- cv.BigVAR(Var_agg_lasso.ft)
# 
# Var_agg_lasso_estim <- BigVAR.est(Var_agg_lasso.ft)
# 
# 
# 
# par_hat <- Var_agg_lasso_estim$B[,,1]
# const <- par_hat[1]
# alpha <- par_hat[2:5]
# beta <- par_hat[6:37]
# 
# 
# y <- Train[,1]
# y_hat <- numeric(nrow(Y))
# 
# y_hat[1] <- const 
# y_hat[2] <- const + y[1] + alpha[1]*y[1] + beta %*% Bts_tr[1,]
# y_hat[3] <- const + y[2] + alpha[1]*(y[2]-y[1]) + alpha[2]*y[1] + beta %*% (Bts_tr[2,] - Bts_tr[1,])
# y_hat[4] <- const + y[3] + alpha[1]*(y[3]-y[2]) + alpha[2]*(y[2] - y[1]) + alpha[3]*y[1] +
#   beta %*% (Bts_tr[3,] - Bts_tr[2,])
# y_hat[5] <- const + y[4] + alpha[1]*(y[4]-y[3]) + alpha[2]*(y[3] - y[2]) + 
#   alpha[3]*(y[2] - y[1]) + alpha[4]*y[1] + beta %*% (Bts_tr[4,] - Bts_tr[3,])
# y_hat[6] <- const + y[5] + alpha[1]*(y[5]-y[4]) + alpha[2]*(y[4] - y[3]) + 
#   alpha[3]*(y[3] - y[2]) + alpha[4]*(y[2] - y[1]) + beta %*% (Bts_tr[5,] - Bts_tr[4,])
# 
# 
# for (i in 7:nrow(Y)) {
#   
#   y_hat[i] <- const + y[i-1] + alpha[1]*(y[i-1]-y[i-2]) + 
#     alpha[2]*(y[i-2] - y[i-3]) + alpha[3]*(y[i-3] - y[i-4]) + 
#     alpha[4]*(y[i-4] - y[i-5]) + beta %*% (Bts_tr[(i-1),] - Bts_tr[(i-2),])
#   
# }
# 
# t <- length(y)
# y_fc <- const + y[t] + alpha[1]*(y[t]-y[t-1]) + 
#   alpha[2]*(y[t-1] - y[t-2]) + alpha[3]*(y[t-2] - y[t-3]) + 
#   alpha[4]*(y[t-3] - y[t-4]) + beta %*% (Bts_tr[(t),] - Bts_tr[(t-1),])
# 
# 
# y_fc -  mean(y - y_hat)
# 
