Kuzmin_fun <- function(data=NULL,in_param=c(FALSE,FALSE,FALSE,FALSE,FALSE,
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
      
      e1 <- -Delta_b-Delta_s+B*log(1+(Delta_b/e_b))+S*log(1+(Delta_s/e_s))
      e2 <- -u_s+S*log(1+(u_s/e_s))
      e3 <- -u_s-Delta_b-Delta_s+B*log(1+(Delta_b/e_b))+S*log(1+(u_s+Delta_s)/e_s)
      e4 <- -u_b+B*log(1+(u_b/e_b))
      e5 <- -u_b-Delta_b-Delta_s+B*log(1+(u_b+Delta_b)/e_b)+S*log(1+(Delta_s/e_s))
      e_max <- max(e1,e2,e3,e4,e5)
      
      L <- log((1-a)*(1-theta)*exp(-e_max)+(1-a)*theta*exp(e1-e_max)+ a*(1-theta)*(1-d)*exp(e2-e_max) +
                   a*theta*(1-d)*exp(e3-e_max) +a*(1-theta)*d*exp(e4-e_max)+
                   a*theta*d*exp(e5-e_max))-(e_b+e_s)+S*log(e_s)+B*log(e_b) +e_max
      if (is.nan(L)){L = 0}
    }
    return(-L)
  }
}


