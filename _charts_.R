rm(list=ls()) ; graphics.off(); cat("\014")

library(plot3D)


cat("> Creating figures for classification:\n")

datasets <- c("banknote_authentication","blood_transfusion","heloc","phoneme","wdbc")
algos <- c("mlp","svm","rf")

if( !dir.exists("FIGURES_CLASS") )
  dir.create("FIGURES_CLASS")


corr_class <- NULL
cat("[")
for( ml in algos ) {
  for( ds in datasets ) {
    
    cat("=")
    
    png(paste0("FIGURES_CLASS/",ml,"_grid_",ds,".png"), width=900, height=300 )
    
    par(mfrow=c(1,3))
    res <- readRDS( paste0("RESULTS_CLASS/",ml,"_grid_",ds,".rds") )
    
    n <- length(sort(unique(res[,1])))
    
    mar0 <- par("mar")
    par(mar=mar0+c(0,1,0,0))
    
    plot( 1-res$accuracy, 1000*res$energy, pch=21, cex=2,
          bg=adjustcolor("deepskyblue",alpha.f=0.2), col="deepskyblue",
          main=paste(ml,"on",ds), xlab="mce", ylab="energy", 
          cex.main=2, cex.axis=2, cex.lab=2 )
    
    spearmanCorr <- cor.test( 1-res$accuracy, res$energy, method="spearman", exact=F )
    corr_class <- rbind( corr_class, data.frame(algo=ml,
                                                dataset=ds,
                                                rho=spearmanCorr$estimate,
                                                pValue=spearmanCorr$p.value,
                                                row.names=NULL) )
    
    par(mar=c(2,2,2,2))
    persp3D( x=sort(unique(res[,1])), y=sort(unique(res[,2])), z=matrix(1-res$accuracy,n,n),
             colkey=F, resfac=10, xlab=names(res)[1], ylab=names(res)[2], zlab="mce", cex.lab=2 )
    
    persp3D( x=sort(unique(res[,1])), y=sort(unique(res[,2])), z=matrix(res$energy,n,n),
             colkey=F, resfac=10, xlab=names(res)[1], ylab=names(res)[2], zlab="energy", cex.lab=2 )
    
    par(mar=mar0)
    
    dev.off()
  }
}
cat("]\n\n")





cat("> Creating figures for regression:\n")

datasets <- c("boston","space_ga","sulfur","wind")
algos <- c("mlp","svm","rf")

if( !dir.exists("FIGURES_REGR") )
  dir.create("FIGURES_REGR")


corr_regr <- NULL
cat("[")
for( ml in algos ) {
  for( ds in datasets ) {
    
    cat("=")
    
    png(paste0("FIGURES_REGR/",ml,"_grid_",ds,".png"), width=900, height=300 )
    
    par(mfrow=c(1,3))
    res <- readRDS( paste0("RESULTS_REGR/",ml,"_grid_",ds,".rds") )
    
    n <- length(sort(unique(res[,1])))
    
    mar0 <- par("mar")
    par(mar=mar0+c(0,1,0,0))
    
    plot( res$rmse, 1000*res$energy, pch=21, cex=2,
          bg=adjustcolor("deepskyblue",alpha.f=0.2), col="deepskyblue",
          main=paste(ml,"on",ds), xlab="rmse", ylab="energy", 
          cex.main=2, cex.axis=2, cex.lab=2 )
    
    spearmanCorr <- cor.test( res$rmse, res$energy, method="spearman", exact=F )
    corr_regr <- rbind( corr_regr, data.frame( algo=ml,
                                               dataset=ds,
                                               rho=spearmanCorr$estimate,
                                               pValue=spearmanCorr$p.value,
                                               row.names=NULL ) )
    
    par(mar=c(2,2,2,2))
    persp3D( x=sort(unique(res[,1])), y=sort(unique(res[,2])), z=matrix(res$rmse,n,n),
             colkey=F, resfac=10, xlab=names(res)[1], ylab=names(res)[2], zlab="rmse", cex.lab=2 )
    
    persp3D( x=sort(unique(res[,1])), y=sort(unique(res[,2])), z=matrix(res$energy,n,n),
             colkey=F, resfac=10, xlab=names(res)[1], ylab=names(res)[2], zlab="energy", cex.lab=2 )
    
    par(mar=mar0)
    
    dev.off()
  }
}
cat("]\n\n")

cat("\n[Classification tasks: correlations]\n\n")
corr_class$rho <- round(corr_class$rho,4)
corr_class$pValue <- round(corr_class$pValue,3)
print( corr_class )

cat("\n[Regressions tasks: correlations]\n\n")
corr_regr$rho <- round(corr_regr$rho,4)
corr_regr$pValue <- round(corr_regr$pValue,3)
print( corr_regr )
