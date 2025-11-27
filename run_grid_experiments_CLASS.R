rm(list=ls()) ; graphics.off(); cat("\014")

datasets <- c("banknote_authentication", "blood_transfusion", "heloc", "phoneme", "wdbc")
mod <- "svm" # possible values: mlp, rf, svm

path <- getwd()

n_feat <- c(4, 4, 22, 5, 30)


if(mod=="svm"){
  lower <- c(-2, -2)
  upper <- c(2, 2)
  par1.name <- "C"
  par2.name <- "gamma"
}else if(mod=="rf"){
  lower <- c(0, 0)
  upper <- c(1, 1)
  par1.name <- "n_est"
  par2.name <- "max_feat"
}else if(mod=="mlp"){
  lower <- c(0, 0)
  upper <- c(1, 1)
  par1.name <- "size"
  par2.name <- "iters"
}

ng <- 15
grid <- expand.grid(seq(lower[1], upper[1], length.out=ng), seq(lower[2], upper[2], length.out=ng))


for(d in datasets){
  
  cat(">", d, "\n[")
  energy <- c()
  accuracy <- c()
  
  for(i in 1:nrow(grid)){
    
    if(file.exists(paste0(path, "/emissions.csv"))){
      file.remove(paste0(path, "/emissions.csv"))
    }
    
    if(mod=="svm"){
      par1 <- 10^grid[i,1]
      par2 <- 10^grid[i,2]
    }else if(mod=="rf"){
      par1 <- round(grid[i,1]*(700-300) + 300,0)
      par2 <- grid[i,2]*(0.75-0.25) + 0.25
    }else if(mod=="mlp"){
      par1 <- round(grid[i,1]*(1.5*max(8, n_feat[datasets==d])-max(8, n_feat[datasets==d])) + max(8, n_feat[datasets==d]), 0)
      par2 <- round((grid[i,2]*(300-100) + 100), 0)
    }
    
    if(.Platform$OS.type=="windows"){
      out <- system2(command="python",
                      args=paste0(shQuote(paste0(path, "/", mod, "_CLASS.py")), " ",par1," ",par2, " ", d),
                      wait=TRUE,
                      invisible=F,
                      stdout = T)
      acc <- as.numeric(gsub("\f", "", out[length(out)]))
    }else{
      stop("at line 67 specify the path to your python3 environment and comment line 66")
      out <- system(command=paste0("/home/pyndaryus/.virtualenvs/.venv/bin/python3", " ",path,"/",mod, "_CLASS.py ",par1," ",par2, " ", d),
                     intern=T,
                     ignore.stderr = T,
                     ignore.stdout = F)
      acc <- as.numeric(out)
    }
    stopifnot(!is.na(acc))
    
    res <- read.csv(paste0(path, "/emissions.csv"), header=T)
    
    energy <- c(energy, round(res$energy_consumed, 9))
    accuracy <- c(accuracy, acc)
    
    file.remove(paste0(path, "/emissions.csv"))
    cat("=")
  }
  
  # save results
  grid.res <- cbind(grid, accuracy, energy)
  colnames(grid.res) <- c(par1.name, par2.name, "accuracy", "energy")
  
  if(!dir.exists(paste0(path,"/RESULTS_CLASS"))){
    dir.create(paste0(path, "/RESULTS_CLASS"))
  }
  saveRDS(grid.res, paste0(path, "/RESULTS_CLASS/",mod,"_grid_", d,".rds"))
  
  cat("]\n")
  
}


