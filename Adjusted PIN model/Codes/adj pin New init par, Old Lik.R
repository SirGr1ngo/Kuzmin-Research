#Specification with new init param algo, but Duarte Lik
library(parallel)
library(doParallel)

pin_adj_inp_Duarte <- function(data){ 
  
  # cl <- makeCluster(8)
  # registerDoParallel(cl)
  
  if (sum(data[,1],data[,2])==0) {
    output <- cbind(c("alpha","delta", "theta", "mu_b", "mu_s","epsilon_b","epsilon_s",
                      "Delta_b", "Delta_s", "PIN_value"),
                    c(0,0,0,0,0,0,0,0,0,0))
    return(output)
  }else{
    
    inpar_df <- initpar(data)
    sort_mat <- data.frame(matrix(nrow=nrow(inpar_df), ncol=11))
    colnames(sort_mat) <- c("alpha","delta", "theta", "mu_b", "mu_s","epsilon_b","epsilon_s",
                            "Delta_b", "Delta_s", "fn value", "PIN_value")
    
    k <- 1
    
   # foreach (i=1:nrow(inpar_df)) %dopar% {
   #   source("C:/Users/Grigorii/Desktop/R codes for 2022/Adjusted PIN/Init par and Lik/duarte lik for adj pin.R")
    
    for (i in 1:nrow(inpar_df)) {
          in_param <- unlist(inpar_df[i,])
          
          #solving the max problem 
          library(nloptr)
          lower <- c(0.01,0.01,0.01, 0.0001,0.0001, 0.0001,0.0001,0.0001, 0.0001)
          upper <- c(1, 1, 1, Inf, Inf, Inf, Inf, Inf, Inf)
          est = neldermead(in_param, Duarte_lik(data,) , lower, upper) 
          #est = optim(par=c(0.1,0.1,0.1, 1,1, 1,1,1, 1), Duarte_fun(data,),method = "L-BFGS-B", lower = c(0.1,0.1,0.1, 1,1, 1,1,1, 1), upper = c(1,1,1,Inf, Inf, Inf,Inf, Inf, Inf))
          #est <- optim(par=in_param, LK_fun(data,), method = "L-BFGS-B", lower = lower, upper = upper)
          #continuation of Yan Zhang, sorting objective function values:
          #sort_mat <- cbind(nrow=125, ncol=6)
          
          sort_mat[k,1] <- est$par[1] #aplha
          sort_mat[k,2] <- est$par[2] #delta
          sort_mat[k,3] <- est$par[3] #theta
          sort_mat[k,4] <- est$par[4] #mu_b
          sort_mat[k,5] <- est$par[5] #mu_s
          sort_mat[k,6] <- est$par[6] #epsilon_buy
          sort_mat[k,7] <- est$par[7] #epsilon_sell
          sort_mat[k,8] <- est$par[8] #Delta_b
          sort_mat[k,9] <- est$par[9] #Delta_s
          sort_mat[k,10] <- est$value #fn_value
          sort_mat[k,11] <- (est$par[1]*(est$par[2]*est$par[4]+(1-est$par[2])*est$par[5]))/(est$par[1]*(est$par[2]*est$par[4]+(1-est$par[2])*est$par[5])+
                                                                                              (est$par[8]+est$par[9])*est$par[3]+est$par[6]+est$par[7])
          
          k <- k+1
        }

    sort_mat <- sort_mat[order(sort_mat[,10],na.last = NA, decreasing = FALSE),]
    sort_mat[1,]
    
    # stopCluster(cl)
  }  
}    


