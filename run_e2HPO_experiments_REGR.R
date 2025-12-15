rm(list=ls()); graphics.off(); cat("\014")

library(lhs)
library(DiceKriging)
source("MISO-AGP_3_patched.R")

#************************************************************************************************
# Running machine configuration
#************************************************************************************************
path <- "." # if running on Ubuntu machine
# path <- getwd() # if running on Windows machine
#************************************************************************************************

mod <- "rf" # possible values "mlp", rf", "svm"

n.initial <- 5+5 # 5 for each source
max.evals <- 25 # including initial
nRuns <- 5

datasets <- c("boston","origin_of_music","space_ga","sulfur","wind")
n_feat <- c(14, 118, 7, 7, 15)
target_col <- c("MEDV","latitude","ln_votes_pop","y1","MAL")

kernel.type <- "matern3_2"


# Objective function
train_validate <- function( dataset, par1, par2) {
  
  if(file.exists(paste0(path, "/emissions.csv"))){
    file.remove(paste0(path, "/emissions.csv"))
  }
  
  if(mod=="svm"){
    par1 <- 10^par1
    par2 <- 10^par2
  }else if(mod=="rf"){
    par1 <- round(par1*(700-300) + 300,0)
    par2 <- par2*(0.75-0.25) + 0.25
  }else if(mod=="mlp"){
    par1 <- round(par1*(1.5*max(8, n_feat)-max(8, n_feat)) + max(8, n_feat), 0)
    par2 <- round((par2*(300-100) + 100), 0)
  }
  
  if(.Platform$OS.type=="windows"){
    out <- system2(command="python",
                    args=paste0(shQuote(paste0(path, "/", mod, "_REGR.py")), " ",par1," ",par2, " ", dataset, " ", tcol),
                    wait=TRUE,
                    invisible=F,
                    stdout = T)
    #rmse <- as.numeric(gsub(",", "", out[length(out)]))
    rmse <- suppressWarnings(as.numeric(gsub(",", "", out[length(out)])))
    
  }else{
    # stop("at line 50 specify the path to your python3 environment and comment line 49")
    out <- system(command=paste0("/home/pyndaryus/.virtualenvs/.venv/bin/python3", " ",path,"/",mod, "_REGR.py ",par1," ",par2, " ", dataset," ",tcol),
                   intern=T,
                   ignore.stderr = T,
                   ignore.stdout = F)
    rmse <- as.numeric(out)
  }
  stopifnot(!is.na(rmse))
  
  res <- read.csv(paste0(path, "/emissions.csv"), header=T)
  energy <- round(res$energy_consumed,9)
  file.remove(paste0(path, "/emissions.csv"))
  
  return(c(rmse, energy))
}


# Defining the two sources:
# source #1 (entire dataset)
fs1 <- function( x, dataset ) {
  train_validate(dataset=dataset, par1=x[2], par2=x[1] )
}

# source #2 (10% of the dataset)
fs2 <- function( x, dataset ) {
  train_validate( dataset=paste0(dataset,"_redux"), par1=x[2], par2=x[1] )
}



# Setting the search space
if(mod=="svm"){
  # C in [10^-2, 10^2] --> [-2,2] in log-scale
  # gamma in [10^-2, 10^2] --> [-2,2] in log-scale
  x.min <- c(-2, -2)
  x.max <- c(2, 2)
}else{
  x.min <- c(0,0)
  x.max <- c(1,1)
}


# MISO-wiLDCosts problem:
problem.setup = list( search.space = cbind(x.min,x.max), min.problem=T, fs=list(fs1,fs2) )

for(dataset.name in datasets){
  
  tcol <- target_col[which(datasets==dataset.name)]
  cat("\014*****", dataset.name,"*****\n\n")
  
  final.RES <- NULL
  for(j in 1:nRuns){
    
    cat("> Seed =", j, ":\n  * Initializing: [")
    set.seed(j)
  
    RES <- maximinLHS(n.initial, 2)
  
    if(mod=="svm"){
      RES[,1] <- RES[,1]*4 - 2
      RES[,2] <- RES[,2]*4 - 2
    }
  
    fe <- NULL
    for(i in 1:n.initial){
      cat("=")
      if(i<=n.initial/2){
        fe <- rbind(fe, fs1(RES[i,], dataset.name))
      }else{
        fe <- rbind(fe, fs2(RES[i,], dataset.name))
      }
    }
    cat("]\n")
  
    RES <- data.frame( source=c(rep("full", floor(n.initial/2)), rep("redux", ceiling(n.initial/2))), 
                RES, rmse = fe[,1], energy = fe[,2])
  
  
    cum.energy <- sum(RES$energy)
  
    f.GPs <- list()
    c.GPs <- list()
  
    s_ <- c("full", "redux")
    
    cat("  * Sequential queries [")
    while( nrow(RES)<max.evals ) {
      cat("=")
      # update GPs for sources
      for( s in s_ ) {
        if( length(unique(RES$rmse[which(RES$source==s)]))==1 ) {
          f.GPs[[s]] <- km( design=RES[which(RES$source==s),2:3],
                            response=RES$rmse[which(RES$source==s)]+rnorm(sum(RES$source==s),0,10^-8),
                            covtype=kernel.type, nugget.estim=T, control=list(trace=F) )   
        } else {
          f.GPs[[s]] <- km( design=RES[which(RES$source==s),2:3],
                          response=RES$rmse[which(RES$source==s)],
                          covtype=kernel.type, nugget.estim=T, control=list(trace=F) ) 
        }
      }
      
      # generating AGP for sources
      AGP <- augmented.gp( f.GPs, covtype=kernel.type )
    
      # update GPs for costs
      for( s in s_ ) {
        if( length(unique(RES$energy[which(RES$source==s)]))==1 ) {
          c.GPs[[s]] <- km( design=RES[which(RES$source==s),2:3],
                            response=RES$energy[which(RES$source==s)]+rnorm(sum(RES$source==s),0,0.01*sd(RES$energy)),
                            covtype=kernel.type, nugget.estim=T, control=list(trace=F) ) 
        } else {
          c.GPs[[s]] <- km( design=RES[which(RES$source==s),2:3],
                            response=RES$energy[which(RES$source==s)],
                            covtype=kernel.type, nugget.estim=T, control=list(trace=F) ) 
        }
      }
    
    
      # choose (s_,x_)
      min.problem <- T
      if(min.problem ) {
        y.abs <- min(AGP@y)
      } else {
        y.abs <- max(AGP@y)
      }
      next.query <- next_pair( GPs=f.GPs, GPs.costs=c.GPs, min.problem=T,
                             scale.costs=F, f.best=y.abs, search.space=cbind(x.min, x.max),
                             m=1, beta_=NULL, tot.evals=300, delta_=0.1 )
    
  
      # evaluate (s_,x_)
      if(next.query$s_==1){
        fe <- fs1(next.query$x_, dataset.name)
      }else{
        fe <- fs2(next.query$x_, dataset.name)
      }
  
  
      # update RES  
      RES <- rbind( RES,
                  data.frame( source=c("full", "redux")[next.query$s_],
                              X1 = next.query$x_[1],
                              X2 = next.query$x_[2],
                              rmse = fe[1],
                              energy = fe[2]
                              ))
  
    
      # update cumulated cost
      cum.energy <- cum.energy + fe[2]
  
    
    }
    cat("]\n")
    RES <- cbind(seed=rep(j, nrow(RES)), RES)
    final.RES <- rbind(final.RES, RES)

  }

  if(!dir.exists(paste0(path,"/RESULTS_REGR"))){
    dir.create(paste0(path, "/RESULTS_REGR"))
  }
  saveRDS(final.RES, paste0(path,"/RESULTS_REGR/", mod, "_miso_", dataset.name, ".rds"))
}

