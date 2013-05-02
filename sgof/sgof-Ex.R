pkgname <- "sgof"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('sgof')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BBSGoF")
### * BBSGoF

flush(stderr()); flush(stdout())

### Name: BBSGoF
### Title: BBSGoF multitesting procedure.
### Aliases: BBSGoF
### Keywords: htest correlation BB-SGoF multitesting SGOF

### ** Examples


data(Hedenfalk)

res<-BBSGoF(Hedenfalk$x)
summary(res)   #automatic number of blocks, number of rejected nulls, 
				 #estimated FDR, beta and beta-binomial parameters, Tarone test of no correlation 
par(mfrow=c(2,2))
plot(res)   #Tarone test, within-block correlation, beta density (for automatic k),
			  #and decision plot (number of rejected nulls)





graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("BH")
### * BH

flush(stderr()); flush(stdout())

### Name: BH
### Title: Benjamini-Hochberg (BH) multitesting procedure
### Aliases: BH Benjamini-Hochberg
### Keywords: htest Benjamini-Hochberg FDR multitesting

### ** Examples


data(Hedenfalk)

res<-BH(Hedenfalk$x)
summary(res)   #number of rejected nulls, estimated FDR
plot(res)   #adjusted p-values



cleanEx()
nameEx("Hedenfalk")
### * Hedenfalk

flush(stderr()); flush(stdout())

### Name: Hedenfalk
### Title: Hedenfalk data
### Aliases: Hedenfalk
### Keywords: datasets

### ** Examples

data(Hedenfalk)
hist(Hedenfalk$x)



cleanEx()
nameEx("SGoF")
### * SGoF

flush(stderr()); flush(stdout())

### Name: SGoF
### Title: SGoF multitesting procedure
### Aliases: SGoF
### Keywords: htest BB-SGoF multitesting SGOF

### ** Examples

p<-runif(387)^2  #387 p-values, intersection null violated
res<-SGoF(p,alpha=0.05,gamma=0.05)
plot(res)  #original versus adjusted p-values
summary(res) #number of rejected nulls, estimated FDR



cleanEx()
nameEx("plot.BBSGoF")
### * plot.BBSGoF

flush(stderr()); flush(stdout())

### Name: plot.BBSGoF
### Title: Plot BBSGoG
### Aliases: plot.BBSGoF
### Keywords: htest correlation BB-SGoF multitesting

### ** Examples


data(Hedenfalk)

res<-BBSGoF(Hedenfalk$x)
plot(res)  



cleanEx()
nameEx("plot.BH")
### * plot.BH

flush(stderr()); flush(stdout())

### Name: plot.BH
### Title: Plot of a BH object
### Aliases: plot.BH
### Keywords: htest Benjamini-Hochberg FDR multitesting

### ** Examples


data(Hedenfalk)

res<-BH(Hedenfalk$x)
plot(res)  



cleanEx()
nameEx("plot.SGoF")
### * plot.SGoF

flush(stderr()); flush(stdout())

### Name: plot.SGoF
### Title: Plot of a SGoF object
### Aliases: plot.SGoF
### Keywords: htest multitesting SGOF

### ** Examples


data(Hedenfalk)

res<-SGoF(Hedenfalk$x)
plot(res)  



cleanEx()
nameEx("print.BBSGoF")
### * print.BBSGoF

flush(stderr()); flush(stdout())

### Name: print.BBSGoF
### Title: Print of a BBSGoF object
### Aliases: print.BBSGoF
### Keywords: htest correlation BB-SGoF multitesting

### ** Examples


data(Hedenfalk)

res<-BBSGoF(Hedenfalk$x)
plot(res)  



cleanEx()
nameEx("print.BH")
### * print.BH

flush(stderr()); flush(stdout())

### Name: print.BH
### Title: Print of a BH obejct
### Aliases: print.BH
### Keywords: htest Benjamini-Hochberg FDR multitesting

### ** Examples


data(Hedenfalk)

res<-BH(Hedenfalk$x)
plot(res)  



cleanEx()
nameEx("print.SGoF")
### * print.SGoF

flush(stderr()); flush(stdout())

### Name: print.SGoF
### Title: Print of a SGoF object
### Aliases: print.SGoF
### Keywords: htest multitesting SGOF

### ** Examples


p<-runif(387)^2  #387 p-values, intersection null violated
res<-SGoF(p,alpha=0.05,gamma=0.05)
summary(res) #number of rejected nulls, estimated FDR




cleanEx()
nameEx("summary.BBSGoF")
### * summary.BBSGoF

flush(stderr()); flush(stdout())

### Name: summary.BBSGoF
### Title: Summary of a BBSGoF object
### Aliases: summary.BBSGoF
### Keywords: htest correlation BB-SGoF multitesting

### ** Examples


data(Hedenfalk)

res<-BBSGoF(Hedenfalk$x)
summary(res)




cleanEx()
nameEx("summary.BH")
### * summary.BH

flush(stderr()); flush(stdout())

### Name: summary.BH
### Title: Summary of a BH object
### Aliases: summary.BH
### Keywords: htest Benjamini-Hochberg FDR multitesting

### ** Examples

data(Hedenfalk)

res<-BH(Hedenfalk$x)
summary(res) 



cleanEx()
nameEx("summary.SGoF")
### * summary.SGoF

flush(stderr()); flush(stdout())

### Name: summary.SGoF
### Title: Summary of a SGoF object
### Aliases: summary.SGoF
### Keywords: htest multitesting SGOF

### ** Examples


p<-runif(387)^2  #387 p-values, intersection null violated
res<-SGoF(p,alpha=0.05,gamma=0.05)
summary(res) #number of rejected nulls, estimated FDR




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
