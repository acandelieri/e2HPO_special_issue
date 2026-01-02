rm(list=ls()) ; graphics.off(); cat("\014")

library(plot3D)


cat("> Creating table for classification:\n")

datasets <- c("banknote_authentication","blood_transfusion","heloc","phoneme","wdbc")
algos <- c("mlp","svm","rf")

gridTable_class <- boTable_class <- e2hpoTable_class <- NULL
addInfo <- NULL

for( ml in algos ) {
  for( ds in datasets ) {
    
    gridRes <- readRDS( paste0("RESULTS_CLASS/",ml,"_grid_",ds,".rds") )
    boRes <- readRDS( paste0("RESULTS_CLASS/",ml,"_bo_",ds,".rds") )
    e2hpoRes <- readRDS( paste0("RESULTS_CLASS/",ml,"_miso_",ds,".rds") )
    
    # for grid
    gridTable_class <- rbind( gridTable_class, data.frame( algorithm=ml,
                                                           dataset=ds,
                                                           mce_grid=min(1-gridRes$accuracy),
                                                           energy_grid=sum(1000*gridRes$energy),
                                                           stringsAsFactors=F ) )
    
    
    # for BO
    boTable_class <- rbind( boTable_class, data.frame( algorithm=ml,
                                                       dataset=ds,
                                                       mce_bo=min(1-boRes$accuracy),
                                                       energy_bo=sum(1000*boRes$energy),
                                                       stringsAsFactors=F ) )
    
    
    # for e2HPO
    e2hpoTable_class <- rbind( e2hpoTable_class, data.frame( algorithm=ml,
                                                             dataset=ds,
                                                             mce_e2hpo=min(1-e2hpoRes$accuracy),
                                                             energy_e2hpo=sum(1000*e2hpoRes$energy),
                                                             stringsAsFactors=F ) )    
    tmp0 <- aggregate( e2hpoRes$source, by=list(e2hpoRes$seed), length )
    tmp1 <- aggregate( e2hpoRes$source, by=list(e2hpoRes$seed,e2hpoRes$source), length )
    addInfo <- rbind( addInfo, data.frame( algorithm=ml,
                                           dataset=ds,
                                           avgUsageOfGroundTruth=format(round(100*mean(tmp1$x[tmp1$Group.2=="full"] / tmp0$x),2),nsmall=2),
                                           sdUsageOfGroundTruth=format(round(100*sd(tmp1$x[tmp1$Group.2=="full"] / tmp0$x),2),nsmall=2),
                                           stringsAsFactors=F) )
    
  }
}

table_class <- data.frame( algorithm=e2hpoTable_class$algorithm,
                           dataset=strtrim(e2hpoTable_class$dataset,10),
                           # mce
                           mce_grid=format(round(100*gridTable_class$mce_grid,2),nsmall=2),
                           mce_bo=format(round(100*boTable_class$mce_bo,2),nsmall=2),
                           mce_e2hpo=format(round(100*e2hpoTable_class$mce_e2hpo,2),nsmall=2),
                           # energy
                           energy_grid=format(round(gridTable_class$energy_grid,3),nsmall=3),
                           energy_bo=format(round(boTable_class$energy_bo,3),nsmall=3),
                           energy_e2hpo=format(round(e2hpoTable_class$energy_e2hpo,3),nsmall=3),
                           gt_usage=paste0( addInfo$avgUsageOfGroundTruth," (",addInfo$sdUsageOfGroundTruth,")" ) )


cat("\n\n[Table 1]\n")
print( table_class[,c(1:3,6,4,7,5,8,9)], row.names=F )


cat("\n> Creating table for regression:\n")

datasets <- c("boston","space_ga","sulfur","wind")
algos <- c("mlp","svm","rf")

gridTable_regr <- boTable_regr <- e2hpoTable_regr <- NULL
addInfo <- NULL

for( ml in algos ) {
  for( ds in datasets ) {
    
    gridRes <- readRDS( paste0("RESULTS_REGR/",ml,"_grid_",ds,".rds") )
    boRes <- readRDS( paste0("RESULTS_REGR/",ml,"_bo_",ds,".rds") )
    e2hpoRes <- readRDS( paste0("RESULTS_REGR/",ml,"_miso_",ds,".rds") )
    
    # for grid
    gridTable_regr <- rbind( gridTable_regr, data.frame( algorithm=ml,
                                                         dataset=ds,
                                                         rmse_grid=min(gridRes$rmse),
                                                         energy_grid=sum(1000*gridRes$energy),
                                                         stringsAsFactors=F ) )
    
    
    # for BO
    boTable_regr <- rbind( boTable_regr, data.frame( algorithm=ml,
                                                     dataset=ds,
                                                     rmse_bo=min(boRes$rmse),
                                                     energy_bo=sum(1000*boRes$energy),
                                                     stringsAsFactors=F ) )
    
    
    # for e2HPO
    e2hpoTable_regr <- rbind( e2hpoTable_regr, data.frame( algorithm=ml,
                                                           dataset=ds,
                                                           rmse_e2hpo=min(e2hpoRes$rmse),
                                                           energy_e2hpo=sum(1000*e2hpoRes$energy),
                                                           stringsAsFactors=F ) )    
    tmp0 <- aggregate( e2hpoRes$source, by=list(e2hpoRes$seed), length )
    tmp1 <- aggregate( e2hpoRes$source, by=list(e2hpoRes$seed,e2hpoRes$source), length )
    addInfo <- rbind( addInfo, data.frame( algorithm=ml,
                                           dataset=ds,
                                           avgUsageOfGroundTruth=format(round(100*mean(tmp1$x[tmp1$Group.2=="full"] / tmp0$x),2),nsmall=2),
                                           sdUsageOfGroundTruth=format(round(100*sd(tmp1$x[tmp1$Group.2=="full"] / tmp0$x),2),nsmall=2),
                                           stringsAsFactors=F) )
    
  }
}

table_regr <- data.frame( algorithm=e2hpoTable_regr$algorithm,
                          dataset=strtrim(e2hpoTable_regr$dataset,10),
                          # rmse
                          rmse_grid=format(round(100*gridTable_regr$rmse_grid,2),nsmall=2),
                          rmse_bo=format(round(100*boTable_regr$rmse_bo,2),nsmall=2),
                          rmse_e2hpo=format(round(100*e2hpoTable_regr$rmse_e2hpo,2),nsmall=2),
                          # energy
                          energy_grid=format(round(gridTable_regr$energy_grid,3),nsmall=3),
                          energy_bo=format(round(boTable_regr$energy_bo,3),nsmall=3),
                          energy_e2hpo=format(round(e2hpoTable_regr$energy_e2hpo,3),nsmall=3),
                          gt_usage=paste0( addInfo$avgUsageOfGroundTruth," (",addInfo$sdUsageOfGroundTruth,")" ) )

cat("\n\n[Table 2]\n")
print( table_regr[,c(1:3,6,4,7,5,8,9)], row.names=F )






cat("> Creating table for classification (with limited energy!):\n")

datasets <- c("banknote_authentication","blood_transfusion","heloc","phoneme","wdbc")
algos <- c("mlp","svm","rf")

boTable_class <- e2hpoTable_class <- NULL
addInfo <- NULL

for( ml in algos ) {
  for( ds in datasets ) {

    boRes <- readRDS( paste0("RESULTS_CLASS/",ml,"_bo_",ds,".rds") )
    e2hpoRes <- readRDS( paste0("RESULTS_CLASS/",ml,"_miso_",ds,".rds") )
    
    energyLim <- aggregate( e2hpoRes$energy, by=list(e2hpoRes$seed), sum )
    names(energyLim) <- c("seed","maxEnergyAllowed")

    # for BO
    ixs <- NULL
    for( i in 1:nrow(energyLim) ) {
      aux <- which(cumsum(boRes$energy[boRes$seed==energyLim$seed[i]])<=energyLim$maxEnergyAllowed[i])
      ixs <- c( ixs, which(boRes$seed==energyLim$seed[i])[aux] )
    }
    boTable_class <- rbind( boTable_class, data.frame( algorithm=ml,
                                                       dataset=ds,
                                                       mce_bo=min(1-boRes$accuracy[ixs]),
                                                       energy_bo=sum(1000*boRes$energy[ixs]),
                                                       stringsAsFactors=F ) )
    
    
    # for e2HPO
    e2hpoTable_class <- rbind( e2hpoTable_class, data.frame( algorithm=ml,
                                                             dataset=ds,
                                                             mce_e2hpo=min(1-e2hpoRes$accuracy),
                                                             energy_e2hpo=sum(1000*e2hpoRes$energy),
                                                             stringsAsFactors=F ) )    
    tmp0 <- aggregate( e2hpoRes$source, by=list(e2hpoRes$seed), length )
    tmp1 <- aggregate( e2hpoRes$source, by=list(e2hpoRes$seed,e2hpoRes$source), length )
    addInfo <- rbind( addInfo, data.frame( algorithm=ml,
                                           dataset=ds,
                                           avgUsageOfGroundTruth=format(round(100*mean(tmp1$x[tmp1$Group.2=="full"] / tmp0$x),2),nsmall=2),
                                           sdUsageOfGroundTruth=format(round(100*sd(tmp1$x[tmp1$Group.2=="full"] / tmp0$x),2),nsmall=2),
                                           stringsAsFactors=F) )
    
  }
}

table_class <- data.frame( algorithm=e2hpoTable_class$algorithm,
                           dataset=strtrim(e2hpoTable_class$dataset,10),
                           # mce
                           mce_bo=format(round(100*boTable_class$mce_bo,2),nsmall=2),
                           mce_e2hpo=format(round(100*e2hpoTable_class$mce_e2hpo,2),nsmall=2),
                           # energy
                           energy_bo=format(round(boTable_class$energy_bo,3),nsmall=3),
                           energy_e2hpo=format(round(e2hpoTable_class$energy_e2hpo,3),nsmall=3),
                           gt_usage=paste0( addInfo$avgUsageOfGroundTruth," (",addInfo$sdUsageOfGroundTruth,")" ),
                           delta_mce=format(round(100*(e2hpoTable_class$mce_e2hpo-boTable_class$mce_bo),2),nsmall=2) )


cat("\n\n[Table 3]\n")
print( table_class[,c(1:3,5,4,6:8)], row.names=F )





cat("> Creating table for regression (with limited energy!):\n")

datasets <- c("boston","space_ga","sulfur","wind")
algos <- c("mlp","svm","rf")

boTable_regr <- e2hpoTable_regr <- NULL
addInfo <- NULL

for( ml in algos ) {
  for( ds in datasets ) {
    
    boRes <- readRDS( paste0("RESULTS_REGR/",ml,"_bo_",ds,".rds") )
    e2hpoRes <- readRDS( paste0("RESULTS_REGR/",ml,"_miso_",ds,".rds") )
    
    energyLim <- aggregate( e2hpoRes$energy, by=list(e2hpoRes$seed), sum )
    names(energyLim) <- c("seed","maxEnergyAllowed")
    
    # for BO
    ixs <- NULL
    for( i in 1:nrow(energyLim) ) {
      aux <- which(cumsum(boRes$energy[boRes$seed==energyLim$seed[i]])<=energyLim$maxEnergyAllowed[i])
      ixs <- c( ixs, which(boRes$seed==energyLim$seed[i])[aux] )
    }
    boTable_regr <- rbind( boTable_regr, data.frame( algorithm=ml,dataset=ds,
                                                     rmse_bo=min(boRes$rmse[ixs]),
                                                     energy_bo=sum(1000*boRes$energy[ixs]),
                                                     stringsAsFactors=F ) )
    
    
    # for e2HPO
    e2hpoTable_regr <- rbind( e2hpoTable_regr, data.frame( algorithm=ml,
                                                           dataset=ds,
                                                           rmse_e2hpo=min(e2hpoRes$rmse),
                                                           energy_e2hpo=sum(1000*e2hpoRes$energy),
                                                           stringsAsFactors=F ) )    
    tmp0 <- aggregate( e2hpoRes$source, by=list(e2hpoRes$seed), length )
    tmp1 <- aggregate( e2hpoRes$source, by=list(e2hpoRes$seed,e2hpoRes$source), length )
    addInfo <- rbind( addInfo, data.frame( algorithm=ml,
                                           dataset=ds,
                                           avgUsageOfGroundTruth=format(round(100*mean(tmp1$x[tmp1$Group.2=="full"] / tmp0$x),2),nsmall=2),
                                           sdUsageOfGroundTruth=format(round(100*sd(tmp1$x[tmp1$Group.2=="full"] / tmp0$x),2),nsmall=2),
                                           stringsAsFactors=F) )
    
  }
}

table_regr <- data.frame( algorithm=e2hpoTable_regr$algorithm,
                          dataset=strtrim(e2hpoTable_regr$dataset,10),
                          # rmse
                          rmse_bo=format(round(100*boTable_regr$rmse_bo,2),nsmall=2),
                          rmse_e2hpo=format(round(100*e2hpoTable_regr$rmse_e2hpo,2),nsmall=2),
                          # energy
                          energy_bo=format(round(boTable_regr$energy_bo,3),nsmall=3),
                          energy_e2hpo=format(round(e2hpoTable_regr$energy_e2hpo,3),nsmall=3),
                          gt_usage=paste0( addInfo$avgUsageOfGroundTruth," (",addInfo$sdUsageOfGroundTruth,")" ),
                          delta_rmse=format(round(100*(e2hpoTable_regr$rmse_e2hpo-boTable_regr$rmse_bo),2),nsmall=2) )


cat("\n\n[Table 4]\n")
print( table_regr[,c(1:3,5,4,6:8)], row.names=F )


dmce <- as.numeric(table_class$delta_mce)
drmse <- as.numeric(table_regr$delta_rmse)
cat("Classification delta-mce: mean = ",mean(dmce),"; sd = ",sd(dmce),"\n",sep="")
cat("Regression delta-rmse: mean = ",mean(drmse),"; sd = ",sd(drmse),"\n",sep="")
