rm(list=ls()); graphics.off(); cat("\014")

png("distribution_target_vars.png",width=800,height=600)

par(mar=c(5.1,5.1,4.1,2.1))
par(mfrow=c(2,2))

ds <- read.csv2("DATA_REGRESSION/boston.csv",sep=",")
par(lwd=2)
hist(as.numeric(ds$MEDV),breaks=30,probability=F,xlab="target feature (scaled in [0,1])",main="Boston dataset",lwd=3,
     cex.main=2, cex.axis=2, cex.lab=2, col=adjustcolor("deepskyblue",alpha.f=0.6), border="white" )
#lines(density(as.numeric(ds$MEDV),n=300,from=0,to=1),col="blue3")

# ds <- read.csv2("DATA_REGRESSION/origin_of_music.csv",sep=",")

ds <- read.csv2("DATA_REGRESSION/space_ga.csv",sep=",")
par(lwd=2)
hist(as.numeric(ds$ln_votes_pop),breaks=30,probability=F,xlab="target feature (scaled in [0,1])",main="Space-ga dataset",lwd=3,
     cex.main=2, cex.axis=2, cex.lab=2, col=adjustcolor("deepskyblue",alpha.f=0.6), border="white")#, ylim=c(0,7) )
#lines(density(as.numeric(ds$ln_votes_pop),n=300,from=0,to=1),col="blue3")


ds <- read.csv2("DATA_REGRESSION/sulfur.csv",sep=",")
par(lwd=2)
hist(as.numeric(ds$y1),breaks=30,probability=F,xlab="target feature (scaled in [0,1])",main="Sulfur dataset",lwd=3,
     cex.main=2, cex.axis=2, cex.lab=2, col=adjustcolor("deepskyblue",alpha.f=0.6), border="white")#, ylim=c(0,17) )
#lines(density(as.numeric(ds$y1),n=300,from=0,to=1),col="blue3")

ds <- read.csv2("DATA_REGRESSION/wind.csv",sep=",")
par(lwd=2)
hist(as.numeric(ds$MAL),breaks=30,probability=F,xlab="target feature (scaled in [0,1])",main="Wind dataset",lwd=3,
     cex.main=2, cex.axis=2, cex.lab=2, col=adjustcolor("deepskyblue",alpha.f=0.6), border="white")#, ylim=c(0,17) )


dev.off()