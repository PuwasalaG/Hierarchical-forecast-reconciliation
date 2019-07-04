#library(devtools)
#install_github("gabrielrvsc/HDeconometrics")
library(HDeconometrics)
data("BRinf")
data=embed(BRinf,2)
y=data[,1]; x=data[,-c(1:ncol(BRinf))]

## == Break the data into in-sample and out-of-sample
y.in=y[1:100]; y.out=y[-c(1:100)]
x.in=x[1:100,]; x.out=x[-c(1:100),]

## == LASSO == ##
lasso=ic.glmnet(x.in,y.in,crit = "bic")
plot(lasso$glmnet,"lambda",ylim=c(-2,2))