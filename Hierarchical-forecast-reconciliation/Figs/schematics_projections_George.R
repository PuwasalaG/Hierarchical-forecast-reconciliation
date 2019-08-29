library(tidyverse)
library(gridExtra)
library(latex2exp)
library(tikzDevice)
library(scatterplot3d)
library(tidyverse)
options(tikzDefaultEngine = 'pdftex', tikzLatexPackages=c(getOption("tikzLatexPackages"),"\\usepackage{amsfonts}","\\usepackage{bm}"),
        tikzFooter = "\\caption{a caption}")


# Generating errors
set.seed(2002)
rmat<-matrix(c(2,4,-0.5,0.25),2,2,byrow = FALSE)
e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
e_sample <- e + matrix(rep(c(0.8,0.7), 100), nrow = 100, ncol = 2, byrow = T)

# In sample direction

tikz('InsampDir_1_George.tex',height=5, width = 5, standAlone = TRUE)

# pdf(NULL)
# dev.control(displaylist="enable")

plot.new()
plot.window(xlim = c(-1,4),ylim = c(-3,5), asp = 1)
lines(c(0,0),c(-3,5))
lines(c(-2,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,4.6,1.25,lwd=2)
text(4.6,1.25,"{$\\large \\mathfrak{s}$}",pos=4, cex = 1)

arrows(0,0,2.4,4.8,lwd=2)
text(2.4,4.8,"{$\\large \\bm{R}$}",pos=3, cex = 1)

#rmat<-matrix(c(1,4,-0.5,0.25),2,2,byrow = FALSE)
# e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
points(e[,1],e[,2],pch=19,col='orange')

dev.off()
tools::texi2dvi('InsampDir_1_George.tex',pdf=T)



####################

tikz('InsampDir_2_George.tex',height=5, width = 5, standAlone = TRUE)

# pdf(family = "serif")
# dev.control(displaylist="enable")

plot.new()
plot.window(xlim = c(-1,4),ylim = c(-3,5), asp = 1)
lines(c(0,0),c(-3,5))
lines(c(-2,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,4.6,1.25,lwd=2)
text(4.6,1.25,"{$\\large \\mathfrak{s}$}",pos=4, cex = 1)


arrows(0,0,2.4,4.8,lwd=2)
text(2.4,4.8,"{$\\large \\bm{R}$}",pos=3, cex = 1)

#rmat<-matrix(c(1,4,-0.5,0.25),2,2,byrow = FALSE)
# e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
# e_sample <- e + matrix(rep(c(0.6,0.3), 100), nrow = 100, ncol = 2, byrow = T)
points(e_sample[,1],e_sample[,2],pch=19,col='gray')

points(2.15,2.6,pch=20,cex=2,col='blue')
text(2.15,2.6,"{$\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,cex = 1)

points(0.4,0.12,pch=20,cex=2,col='black')
text(0.4,0.12,"{$\\color{black}{\\bm{y}}$}",pos = 1, cex = 1)

# Plot_InsampDir_2 <- recordPlot()
# invisible(dev.off())
dev.off()
tools::texi2dvi('InsampDir_2_George.tex',pdf=T)


########
tikz('InsampDir_3_George.tex',height=5, width = 5, standAlone = TRUE)

# pdf(NULL)
# dev.control(displaylist="enable")

plot.new()
plot.window(xlim = c(-1,4),ylim = c(-3,5), asp = 1)
lines(c(0,0),c(-3,5))
lines(c(-2,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,4.6,1.25,lwd=2)
text(4.6,1.25,"{$\\large \\mathfrak{s}$}",pos=4, cex = 1)


arrows(0,0,2.4,4.8,lwd=2)
text(2.4,4.8,"{$\\large \\bm{R}$}",pos=3, cex = 1)

#rmat<-matrix(c(1,4,-0.5,0.25),2,2,byrow = FALSE)
# e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
points(e_sample[,1],e_sample[,2],pch=19,col='gray')

points(0.4,0.12,pch=20,cex=2,col='black')
text(0.4,0.12,"{$\\color{black}{\\bm{y}}$}",pos = 1, cex = 1)

points(2.15,2.6,pch=20,cex=2,col='blue')
text(2.15,2.6,"{$\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3, cex = 1)

points(0.95,0.25,pch=20,cex=2,col='red')
text(0.95,0.15,"{$\\color{red}{\\tilde{\\bm{y}}}$}",pos = 1, cex = 1)

lines(c(0.15,3.2),c(-1.4,4.7),lty=2)
arrows(2.15,2.6,0.95,0.25,lwd=2,col = 'blue')


#text(1.25,2,"{\\huge ${\\color{blue} s\\circ g}$}",pos=4)

dev.off()
tools::texi2dvi('InsampDir_3_George.tex',pdf=T)



#--Orthogonal projection of the points

tikz('OrthProj_George.tex',height=5, width = 5, standAlone = TRUE)
# pdf(family = "serif")
# dev.control(displaylist="enable")

plot.new()
plot.window(xlim = c(-1,4),ylim = c(-3,5), asp = 1)
lines(c(0,0),c(-3,5))
lines(c(-2,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,4.6,1.25,lwd=2)
text(4.6,1.25,"{$\\large \\mathfrak{s}$}",pos=4, cex = 1)


arrows(0,0,2.4,4.8,lwd=2)
text(2.4,4.8,"{$\\large \\bm{R}$}",pos=3, cex = 1)

#rmat<-matrix(c(1,4,-0.5,0.25),2,2,byrow = FALSE)
# e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
points(e_sample[,1],e_sample[,2],pch=19,col='gray')

S <- c(4.6,1.25)
SP = S %*% solve(t(S) %*% S) %*% t(S)
# SP<-matrix(c(0.8,0.4,0.4,0.2),2,2)

e_OLS <- t(SP%*%t(e_sample))
points(e_OLS[,1],e_OLS[,2],pch=19,col='red')

arrows(x0 = e_sample[,1], y0 = e_sample[,2],
       x1 = e_OLS[,1], y1 = e_OLS[,2], code = 0,
       lty = 2, col = "blue")

points(0.4,0.12,pch=20,cex=2,col='black')
text(0.4,0.12,"{$\\color{black}{\\bm{y}}$}",pos = 1, cex = 1)

# points(1.5,3,pch=20,cex=2,col='gray')
# text(1.5,3,"{\\huge $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)

# Plot_OrthProj <- recordPlot()
# invisible(dev.off())
dev.off()
tools::texi2dvi('OrthProj_George.tex',pdf=T)



#--Oblique projection of the points

tikz('ObliqProj_George.tex',height=5, width = 5, standAlone = TRUE)

# pdf(NULL)
# dev.control(displaylist="enable")


plot.new()
plot.window(xlim = c(-1,4),ylim = c(-3,5), asp = 1)
lines(c(0,0),c(-3,5))
lines(c(-2,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,4.6,1.25,lwd=2)
text(4.6,1.25,"{$\\large \\mathfrak{s}$}",pos=4, cex = 1)


arrows(0,0,2.4,4.8,lwd=2)
text(2.4,4.8,"{$\\large \\bm{R}$}",pos=3, cex = 1)

#rmat<-matrix(c(1,4,-0.5,0.25),2,2,byrow = FALSE)
# e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
points(e_sample[,1],e_sample[,2],pch=19,col='gray')


S <- c(4.6,1.25)
W <- cov(e_sample)
W_inv <- solve(W)
SP <- S %*% solve(t(S) %*% W_inv %*% S) %*% t(S) %*% W_inv

e_MinT <- t(SP%*%t(e_sample))
points(e_MinT[,1],e_MinT[,2],pch=19,col='red')


arrows(x0 = e_sample[,1], y0 = e_sample[,2],
       x1 = e_MinT[,1], y1 = e_MinT[,2], code = 0,
       lty = 2, col = "blue")


# points(1.5,3,pch=20,cex=2,col='gray')
# text(1.5,3,"{\\huge $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)

points(0.4,0.12,pch=20,cex=2,col='black')
text(0.4,0.12,"{$\\color{black}{\\bm{y}}$}",pos = 1, cex = 1)


# Plot_ObliqProj <- recordPlot()
# invisible(dev.off())

dev.off()
tools::texi2dvi('ObliqProj_George.tex',pdf=T)



par(mfrow=c(2,2))
Plot_InsampDir_1
Plot_InsampDir_2
Plot_OrthProj
Plot_ObliqProj



