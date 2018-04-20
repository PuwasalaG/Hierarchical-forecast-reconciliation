##A script to create a plot that is a schematic describing probabilistic forecast reconciliation.

library(latex2exp)
library(tikzDevice)
options(tikzLatexPackages=c(getOption("tikzLatexPackages"),"\\usepackage{amsfonts}","\\usepackage{bm}"),
        tikzFooter = "\\caption{a caption}")
tikz('probforerec_schematic.tex',height=5)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\Large ${\\bm S}$}",pos=4)
text(2,1,"{\\huge $\\mathfrak{s}$}",pos=1)
#lines(c(-0.25,1),c(-1,4),lwd=2)
arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

lines(c(0,1.5),c(-1,5),lty=2)

lines(c(1,2.5),c(-1,5),lty=2)

lines(c(2/7,10/7),c(1/7,5/7),lwd=4,col='red')
text(1.5,-0.5,"{\\huge $\\color{red}{s(\\mathcal{B})}$}")
arrows(1.5,-0.5,6/7,3/7,col='red')

cord.x<-c(-0.25, 0.75, 2.75, 1.75)
cord.y<-c(-2, -2, 6, 6)

polygon(cord.x,cord.y,col=adjustcolor('blue',alpha=0.1),border = NA)

text(3,3,"{\\huge $\\color{blue}{g^{-1}(\\mathcal{B}})$}",col='blue')
arrows(3,3,1.5,2,col='blue')

dev.off()
