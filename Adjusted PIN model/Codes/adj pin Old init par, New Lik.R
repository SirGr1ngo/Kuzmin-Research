dy_pin <- function(data) {
  
  inpar_df <- simple_init_par(10)
  sort_mat <- data.frame(matrix(nrow=nrow(inpar_df), ncol=11))
  colnames(sort_mat) <- c("alpha","delta", "theta", "mu_b", "mu_s","epsilon_b","epsilon_s",
                          "Delta_b", "Delta_s", "fn value", "PIN_value")
  
  k <- 1
  for (i in 1:nrow(inpar_df)) {
    in_param <- unlist(inpar_df[i,])
    
    library(nloptr)
    est = neldermead(in_param, Kuzmin_fun(data,) , lower = c(0.01,0.01,0.01, 0.0001,0.0001, 0.0001,0.0001,0.0001, 0.0001), upper = c(1,1,1,Inf, Inf, Inf,Inf, Inf, Inf))
    
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
  # optim(initial, Kuzmin_fun(data,), method = "L-BFGS-B", lower = c(0.01,0.01,0.01, 0.01,0.01, 0.01,0.01,0.01, 0.01), upper = c(1,1,1,Inf, Inf, Inf,Inf, Inf, Inf))-> xx
  # param <- xx$par
  # neglogl <- xx$value
  # PIN <- (param[1] * (param[2]* param[4] + (1 -param[2])*param[5]))/(param[1] * (param[2]* param[4] + (1 -param[2])*param[5]) + (param[6] + param[7]) * (param[1]* param[3] + (1-param[1])*param[3]) + param[8] + param[9] ) # PIN
  # 
  # 
  #   return(list(PIN = PIN, param = param, neglogl = neglogl))
}