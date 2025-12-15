# version 1.0
# Date: 2020-09-20
# by: Antonio Candelieri



#  Function for measuring the discrepancy between 2 GPs
#  - x: points where measure the discrepancy
#  - gp: first GP
#  - gp_: second GP
gp.discrepancy <- function( x, gp, gp_ ) {
  p <- predict( gp, newdata=data.frame(x), type="UK", checkNames=F )
  p_ <- predict( gp_, newdata=data.frame(x), type="UK", checkNames=F)
  gp.discrepancy <- abs(p$mean-p_$mean)
}


# function to genrate the Augmented GP (AGP)
# - GPs: a list of GPs, one for each information source
# TODO - m: technical parameter of the MISO-AGP algorithm (see Candelieri et al.............)
augmented.gp <- function( GPs, m=1, covtype="gauss" ) {
  
  D.aug <- data.frame(source=rep(1,nrow(GPs[[1]]@X)),
                      GPs[[1]]@X,
                      y=GPs[[1]]@y,
                      stringsAsFactors=F) # design of the GP associated to the hifi source
  
  if( length(GPs)>1 ) {
    
    # Generate the augmeted design
    for( s in 2:length(GPs) ) {
      D.s <- GPs[[s]]@X
      d <- gp.discrepancy(D.s,GPs[[1]],GPs[[s]])
      p0 <- predict( GPs[[1]], newdata=data.frame(D.s), "UK", checkNames=F )
      ixs <- which(d<m*p0$sd)
      
      if( length(ixs)>0 ) {
        pts <- D.s[ixs,]
        if( !is.matrix( pts ) ) {
          pts <- matrix(pts,nrow=length(ixs))
          pts <- as.data.frame( pts )
          colnames(pts) <- colnames(GPs[[s]]@X)
        }
        
        # print(data.frame( source=rep(s,length(ixs)),
        #                   pts,
        #                   y=GPs[[s]]@y[ixs],
        #                   stringsAsFactors=F) )
        
        D.aug <- rbind(D.aug, data.frame( source=rep(s,length(ixs)),
                                          pts,
                                          y=GPs[[s]]@y[ixs],
                                          stringsAsFactors=F) )
      }
    }
    
    # Fitting the augmented GP
    GP.aug <- km( design=data.frame(D.aug[,2:(ncol(D.aug)-1)]),
                  response=D.aug$y,
                  covtype=covtype,
                  nugget.estim=T,
                  control=list(trace=0) )

  } else {
    GP.aug <- GPs[[1]]
  }
  
  return( GP.aug )
}



# This function provides the scheduling for beta in the Confidence Bound, as described in
# Srinivas et al. 2012: Information-theoretic regret bounds for gaussian process optimization
# in the bandit setting. IEEE Transactions on Information Theory, 58(5), 3250-3265.
# - tot.evals: overall number of available function evaluations
# - delta: probability for convergence
# - UCB: TRUE=Upper Confidence Bound, FALSE=Lower Confidence Bound
CB.Srinivas <- function( x, GP, tot.evals, delta_=0.1, UCB=T ) {
  
  if( !is.matrix(x) )
    x <- t(x)
  pred <- predict( GP, x, "UK", checkNames=F )
  pi_t <- (pi^2)*( ((tot.evals)*nrow(GP@X))^2)/(6*delta_)
  beta_t <- 2*log(pi_t)
  if( UCB )
    res <- pred$mean + sqrt(beta_t)*pred$sd
  else
    res <- pred$mean - sqrt(beta_t)*pred$sd

  return(res)
}


# Modified from MISO-AGP project ---------

# MISO-AGP acquisition function:
# the confidence bound from Srinivas can be used or a given value/schedule of beta can be provided
# if beta != NULL --> tot.evals and delta are not used! They are args for Srinivas' scheduling for beta
# - GPs: list of GPs, one for each information source
# - GP.aug: augmented GP
# - GPs.costs: GPs approximating the location-dependent query costs, one for each information source
# - min.problem: TRUE=minimization, FALSE=maximization
# - beta: parameter to deal with the exploration-exploitaion trade-off in CB. beta == NULL --> Srinivas's scheduling for beta is used
# - tot.evals, delta: args of the CB.Srinivas function 

# MODIFIED for MISO-wiLDCosts
misoagp.acquisition <- function( x, GPs, GP.aug, GP.cost, f.best, min.problem = T, beta_=NULL, tot.evals, delta_=0.1 ) {
  
  if( length(x)>1 )
    x <- t(x)
  p.aug <- predict( GP.aug, newdata=data.frame(x), type="UK", checkNames=F )
  d <- gp.discrepancy( x, GPs, GP.aug )
  
  p.cost <- predict( GP.cost, newdata=data.frame(x), type="UK", checkNames=F )
  worst.cost <- p.cost$mean + p.cost$sd
  
  
  if( is.null(beta_) )
    # Srinivas' scheduling for beta
    cb <- CB.Srinivas( x=x, GP=GP.aug, tot.evals=tot.evals, delta_=delta_, UCB=!min.problem ) 
  else {
    if( min.problem )
      cb <- p.aug$mean - sqrt(beta_)*p.aug$sd # lower confidence bound
    else
      cb <- p.aug$mean + sqrt(beta_)*p.aug$sd # upper confidence bound
  }
  
  if( min.problem )
    optimistic.improvement <- f.best - cb
  else
    optimistic.improvement <- cb - f.best
  
  # penalizing the optimistic improvement
  utility.value <- optimistic.improvement/(1 + worst.cost * d )
  
  return( utility.value )
}


# # MISO-AGP corrective acquisition function:
# # the max-explorative point is selected (function learning)
# misoagp.corrective.acquisition <- function( x, GP.aug ) {
#   
#   if( length(x)>1 )
#     x <- t(x)
#   p <- predict( GP.aug, newdata=data.frame(x), "UK", checkNames=F )
#   return( p$sd )
# }


# Modified from MISO-AGP project ---------

# MISO-AGP auxiliary problem: next pair is selected according to MISO-AGP acquisition function or,
# if needed, it is corrected as reported in the paper:
# TODO Candelieri et al........
# args are thoe used by the 'misoagp.acquisition' function

# MODIFIED for MISO-wiLDCosts
next_pair <- function( GPs, GPs.costs, min.problem=T, scale.costs=T, f.best, search.space, m=1, #epsilon=NULL,
                       beta_=NULL, tot.evals, delta_=0.1 ) {
  
  library(lhs)
  
  # if( is.null(epsilon) ) {
  #   epsilon = min(search.space[,2]-search.space[,1])/10000
  #   message("epsilon has been set to:", epsilon)
  # }
  
  # Computing the AGP
  GP.aug <- augmented.gp( GPs, m )
  
  # computing the augmented best seen
  if( is.null(f.best) ) {
    if( min.problem )
      f.best <- min(GP.aug@y)
    else
      f.best <- max(GP.aug@y)
  }
  
  
  # find next pair: MISO-AGP acquisition ----------------------------------------------
  BEST.x <- BEST.value <- BEST.s <- NULL
  

  for( s in 1:length(GPs)) {
    
    # ***********************************************************************
    # Patch (January 2025)
    # ***********************************************************************
    
    if( s==1 || nrow(GPs[[1]]@X) > nrow(GPs[[s]]@X) ) {

    # ***********************************************************************      
    
      set.seed(123)
      pars <- maximinLHS( 10 * nrow(search.space), nrow(search.space) ) 
    
      best.value <- best.x <- NULL
      
      for( i.par in 1:nrow(pars) ) {
        # the acquisition is always maximized (optimistic improvement is positive also in the case of a minimization
        # problem (i.e. LCB)
        res <- optim( par=pars[i.par,], fn=misoagp.acquisition, method="L-BFGS-B",
                      lower=search.space[,1], upper=search.space[,2], control=list(fnscale=-1,trace=0), 
                      GPs=GPs[[s]], GP.aug=GP.aug, GP.cost=GPs.costs[[s]], f.best=f.best,
                      min.problem=min.problem, beta_=beta_, tot.evals=tot.evals, delta_=delta_ )
        
        if( is.null(best.value) || res$value>best.value ) { # acquisition is always maximized (optimistic improvement)
          best.value <- res$value
          best.x <- res$par
        }
      }
      
      if( is.null(BEST.value) || best.value>BEST.value ) {
        BEST.x <- best.x
        BEST.value <- best.value
        BEST.s <- s
      }
      
    }
    
  }
  
  # # correct selected pair, if needed -------------------------------------------------------------
  # to.correct <- F
  # 
  # # for( s in 1:length(GPs) ) {
  # #   if( min(sqrt(apply( (GPs[[s]]@X - matrix(rep(BEST.X,nrow(GPs[[s]]@X)),nrow=nrow(GPs[[s]]@X),byrow=T))^2,1,sum))) < epsilon )
  # #     to.correct <- T
  # # }
  # 
  # 
  # if( min(sqrt(apply( (GPs[[BEST.s]]@X - matrix(rep(BEST.x,nrow(GPs[[BEST.s]]@X)),nrow=nrow(GPs[[BEST.s]]@X),byrow=T))^2,1,sum))) < epsilon )
  #   to.correct <- T
  # 
  # 
  # if( to.correct ) {
  #   
  #   best.value <- best.x <- NULL
  #   
  #   for( i.par in 1:nrow(pars) ) {
  #     # the corrective acquisition is always maximized (max variance = max uncertainty = max exploration)
  #     res <- optim( par=pars[i.par,], fn=misoagp.corrective.acquisition, method="L-BFGS-B",
  #                   lower=search.space[,1], upper=search.space[,2], control=list(fnscale=-1, trace=0),
  #                   GP.aug=GP.aug )
  #     
  #     if( is.null(best.value) || res$value>best.value ) { # the acquisition function is always maximized!
  #       best.value <- res$value
  #       best.x <- res$par
  #     }
  #   }
  #   
  #   BEST.x <- best.x
  #   BEST.value <- best.value # not relevant, just for completeness
  #   BEST.s <- 1 # always hi-fi source
  # }
  

  return( list(s_=BEST.s, x_=BEST.x, GP.aug=GP.aug))#, correction.applied=to.correct) )
}
