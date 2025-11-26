rm(list=ls()) ; graphics.off(); cat("\014")

library(mlrMBO)
library(lhs)
library(mlr)
library(readr)


mod <- "rf" # possible values "mlp", rf", "svm"

path <- getwd()

datasets <- c("boston","origin_of_music","space_ga","sulfur","wind")
n_feat <- c(14, 118, 7, 7, 15)
target_col <- c("MEDV","latitude","ln_votes_pop","y1","MAL")
n.init <- 10
iters <- 30
nRuns <- 5


if(mod=="svm"){
  par1.name <- "C"
  par2.name <- "gamma"
  lower <- c(-2, -2)
  upper <- c(2, 2)
  parset <- makeParamSet(
    makeNumericParam(par1.name, lower=lower[1], upper=upper[1]),
    makeNumericParam(par2.name, lower=lower[2], upper=upper[2]))
}else if(mod=="rf"){
  par1.name <- "n_est"
  par2.name <- "max_feat"
  lower <- c(0, 0)
  upper <- c(1, 1) 
  parset <- makeParamSet(
    makeNumericParam(par1.name, lower=lower[1], upper=upper[1]),
    makeNumericParam(par2.name, lower=lower[2], upper=upper[2]))
}else if(mod=="mlp"){
  par1.name <- "size"
  par2.name <- "iters"
  lower <- c(0, 0)
  upper <- c(1, 1)
  parset <- makeParamSet(
    makeNumericParam(par1.name, lower=lower[1], upper=upper[1]),
    makeNumericParam(par2.name, lower=lower[2], upper=upper[2]))
}


for(d in datasets){

  tcol <- target_col[which(datasets==d)]
  cat("> dataset:", d, "target var:", tcol, "\n")

  # main
  obj.fun = makeSingleObjectiveFunction(
    name = "energy",
    fn = function(x) {
      
      if(mod=="svm"){
        par1 <- 10^x[1]
        par2 <- 10^x[2]
      }else if(mod=="rf"){
        par1 <- round(x[1]*(700-300) + 300, 0)
        par2 <- x[2]*(0.75-0.25) + 0.25
      }else if(mod=="mlp"){
        par1 <- round(x[1]*(1.5*max(8, n_feat[datasets==d])-max(8, n_feat[datasets==d])) + max(8, n_feat[datasets==d]), 0)
        par2 <- round((x[2]*(300-100) + 100), 0)
      }
      
      if(file.exists(paste0(path, "/emissions.csv"))){
        file.remove(paste0(path, "/emissions.csv"))
      }
      
      if(.Platform$OS.type=="windows"){
        out <- system2(command="python", 
                       args=paste0(shQuote(paste0(path, "/", mod, "_REGR.py")), " ",par1," ",par2, " ", d,  " ",tcol),
                       wait=TRUE,
                       invisible=F,
                       stdout = T,
                       stderr = T
                      )
        
        last_line <- gsub("\f", "", out[length(out)])
        rmse <- suppressWarnings(as.numeric(last_line))
        #rmse <- as.numeric(gsub("\f", "", out[length(out)]))
        if (is.na(rmse)) {
          stop("ERROR: rmse not available!")
        }
      }else{
        stop("at line 78 specify the path to your python3 environment and comment line 77")
        out <- system(command=paste0("/home/pyndaryus/.virtualenvs/.venv/bin/python3"," ",path,"/",mod,"_CLASS.py ",par1," ",par2, " ", d),
                      intern=T, 
                      ignore.stderr = T,
                      ignore.stdout = F)
        rmse <- suppressWarnings(as.numeric(last_line))
      }
      
      res <- read.csv(paste0(path, "/emissions.csv"), header=T)
      
      energy <<- c(energy, round(res$energy_consumed, 9))
      
      file.remove(paste0(path, "/emissions.csv"))
      cat("=")
      
      stopifnot(!is.na(rmse))
      
      return(rmse)
    },
    par.set = parset,
    minimize = TRUE
  )
  
  for(s in 1:nRuns){
    cat("\n>",s,"[")
    energy <<- c()
    
    des = generateDesign(n = n.init, par.set = getParamSet(obj.fun), fun = lhs::randomLHS)
    des$y = apply(des, 1, obj.fun)
    cat("]\n")
    
    surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))
    
    control = makeMBOControl()
    control = setMBOControlTermination(control, iters = iters)
    control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())
    
    res.mbo = mbo(obj.fun, design = des, learner = surr.km, control = control, show.info = FALSE)
    
    df <- cbind(rep(s,n.init+iters), getOptPathX(res.mbo$opt.path), getOptPathY(res.mbo$opt.path), energy)
    if(s==1){
      final.df <- df
    }else{final.df <- rbind(final.df, df)}
    
  }
  cat("\n")
  
  #save results
  colnames(final.df) <- c("seed", par1.name, par2.name, "rmse", "energy")
  if(!dir.exists(paste0(path,"/RESULTS_REGR"))){
    dir.create(paste0(path, "/RESULTS_REGR"))
  }
  saveRDS(final.df, file=paste0(path, "/RESULTS_REGR/",mod, "_bo_", d, ".rds" ))
}

