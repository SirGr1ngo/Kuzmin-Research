Duarte_lik <- function(data=NULL,in_param=c(FALSE,FALSE,FALSE,FALSE,FALSE,
                                            FALSE,FALSE,FALSE,FALSE)){ 
  function(x){
    in_param <- x
    a <- in_param[1] #alpha
    d <- in_param[2] #delta
    theta <- in_param[3]
    u_b <- in_param[4] #mu_b
    u_s <- in_param[5] #mu_s
    e_b <- in_param[6] #epsilon_buy
    e_s <- in_param[7] #epsilon_sell
    Delta_b <- in_param[8] #Delta_b
    Delta_s <- in_param[9] #Delta_s
    Buy <- data[,1] # vector of Buy operations
    Sell <- data[,2] # vector of Sell operations
    #need to account for each day for which data is considered: 
    days <- min(length(data[,1]),length(data[,2]))
    L <- 0 
    for (i in (1:days)) { 
      B <- Buy[i] 
      S <- Sell[i]
      
      ln_b <- 0
      for (i in 1:B) {
        ln_b <- ln_b + log(i)}
      ln_s <- 0
      for (i in 1:S) {
        ln_s <- ln_s + log(i)}
      
      L_1 <- (1-a)*(1-theta)*exp(-e_b+B*log(e_b)-ln_b)*exp(-e_s+S*log(e_s)-ln_s)
      if (is.nan(L_1) | is.infinite(L_1)){L_1 = 0}
      L_2 <- (1-a)*theta*exp(-e_b-Delta_b+B*log(e_b+Delta_b)-ln_b)*exp(-e_s-Delta_s+
                                                                         S*log(e_s+Delta_s)-ln_s)
      if (is.nan(L_2) | is.infinite(L_2)){L_2 = 0}
      L_3 <- a*(1-theta)*(1-d)*exp(-e_b+B*log(e_b)-ln_b)*exp(-u_s-e_s+S*log(e_s+u_s)-ln_s)
      if (is.nan(L_3) | is.infinite(L_3)){L_3 = 0}
      L_4 <- a*theta*(1-d)*exp(-e_b-Delta_b+B*log(e_b+Delta_b)-ln_b)*exp(-u_s-e_s-Delta_s+S*log(u_s+e_s+Delta_s)-ln_s)
      if (is.nan(L_4) | is.infinite(L_4)){L_4 = 0}
      L_5 <- a*(1-theta)*d*exp(-u_b-e_b+B*log(u_b+e_b)-ln_b)*exp(-e_s+S*log(e_s)-ln_s)
      if (is.nan(L_5) | is.infinite(L_5)){L_5 = 0}
      L_6 <- a*theta*d*exp(-u_b-e_b-Delta_b+B*log(u_b+e_b+Delta_b)-ln_b)*exp(-e_s-Delta_s+S*log(e_s+Delta_s)-ln_s)
      if (is.nan(L_6) | is.infinite(L_6)){L_6 = 0}
      
      L <- L + log(sum(L_1,L_2,L_3,L_4,L_5,L_6))
    }
    return(-L)
  }
}