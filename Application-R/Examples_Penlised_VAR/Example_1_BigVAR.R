library(devtools)

if (!require(Quandl)) {
  install_github("quandl/R-package")
}

# to install BigVAR from Github
if (!require(BigVAR)) {
  install_github("wbnicholson/BigVAR/BigVAR")
}

suppressPackageStartupMessages(library(Quandl))

# Gross Domestic Product (Relative to 2000)
GDP = Quandl("FRED/GDP", type = "xts")
GDP <- GDP/mean(GDP["2000"]) * 100
# Transformation Code: First Difference of Logged Variables
GDP <- diff(log(GDP))
# Federal Funds Rate
FFR = Quandl("FRED/FEDFUNDS", type = "xts", collapse = "quarterly")
# Transformation Code: First Difference
FFR <- diff(FFR)
# CPI ALL URBAN CONSUMERS, relative to 1983
CPI = Quandl("FRED/CPIAUCSL", type = "xts", collapse = "quarterly")
CPI <- CPI/mean(CPI["1983"]) * 100
# Transformation code: Second difference of logged variables
CPI <- diff(log(CPI), 2)

#Standardising each series

k = 3
Y1 <- cbind(GDP, CPI, FFR)
Y1 <- na.omit(Y1)
# Demean
Y1 <- Y1 - (c(rep(1, nrow(Y1)))) %*% t(c(apply(Y1, 2, mean)))
# Standarize Variance
for (i in 1:k) {
  Y1[, i] <- Y1[, i]/apply(Y1, 2, sd)[i]
}

plot(as.zoo(Y1), main = "", xlab = "", ylab = c("GDP", "FFR", "CPI"))

library(BigVAR)

#Concidering GDP as the endogeneous and CPI, FFR as exogeneous
VARX <- list()
VARX$k <- 1
VARX$s <- 1

Model1 = constructModel(as.matrix(Y1), p = 4, struct = "Basic", gran = c(25, 10),
                        verbose = F, h=1)
# Model1 = constructModel(as.matrix(Y), p = 4, struct = "Lag", gran = c(25, 10),
#                         verbose = F, VARX = list(), h=5)

Model1Results = cv.BigVAR(Model1)
Model1Results

# Coefficient matrix at end of evaluation period
Model1Results@betaPred

# Residuals at end of evaluation period
Model1Results@resids

# Fitted values at end of evaluation period
Model1Results@resids

# Lagged Values at end of evaluation period
Model1Results@Zvals

#sparsity pattern of the final coefficient matrix in the evaluation period
SparsityPlot.BigVAR.results(Model1Results)

#N-step ahead prediction

predict(Model1Results, 1)




#VARXFit() - Fit a VAR or VARX model by least squares
VAR_LS <- VARXFit(Y, p=1, IC="AIC", VARX = VARX)

PredictVARX(VAR_LS)
