pin_fun <- function(data, factor=c("EHO","LK")){ 
  if (sum(data[,1],data[,2])==0) {
    output <- cbind(c("alpha","delta","mu","epsilon_b","epsilon_s","PIN_value"),
                  c(0,0,0,0,0,0))
    return(output)
  }else{
  #Yan Zhang algorithm:
  a_i <- c(0.1,0.3,0.5,0.7,0.9) #vector alpha of initial values
  d_j <- c(0.1,0.3,0.5,0.7,0.9) #vector delta of initial values
  g_n <- c(0.1,0.3,0.5,0.7,0.9) #vector gamma of initial values
  B_bar <- mean(data[,1])
  S_bar <- mean(data[,2])
  #matrix for further sorting procedure:
  sort_mat <- data.frame(matrix(nrow=125, ncol=6))
  #checking conditions:
  k <- 1
  for (i in (1:5)) {
    for (j in (1:5)) {
      for (n in (1:5)) {
        a <- a_i[i]
        d <- d_j[j]
        g <- g_n[n]
        e_b <- g*B_bar #epsilon_buy
        u <- (B_bar-e_b)/(a*(1-d)) #mu
        e_s <- S_bar- (a*d*u) #epsilon_sell
        if(e_s<0){ 
          k <- k+1 #move to the next as condition is violated
          next
        }
        in_param <-c(a,d,u,e_b,e_s)
        
        #EHO Liklihood eq-n
  if(factor=="EHO"){
  EHO_fun <- function(data=NULL,in_param=c(FALSE,FALSE,FALSE,FALSE,FALSE)){ 
    function(x){
      in_param <- x
      a <- in_param[1] #alpha
      d <- in_param[2] #delta
      u <- in_param[3] #mu
      e_b <- in_param[4] #epsilon_buy
      e_s <- in_param[5] #epsilon_sell
      Buy <- data[,1] # vector of Buy operations
      Sell <- data[,2] # vector of Sell operations
      
      #need to account for each day for which data is considered: 
      days <- min(length(data[,1]),length(data[,2]))
      L <- 0 
      for (i in (1:days)) { 
        B <- Buy[i] 
        S <- Sell[i] 
        M <- min(B,S)+max(B,S)/2 
        X_b <- in_param[4]/(in_param[3]+in_param[4]) 
        X_s <- in_param[5]/(in_param[3]+in_param[5]) 
        #let us divide the Likelihood function into two subparts: 
        L_1 <- log(a*d*exp(-u)*X_b^(B-M)*X_s^(-M)+ 
                     a*(1-d)*exp(-u)*X_b^(-M)*X_s^(S-M)+ 
                     (1-a)*X_b^(B-M)*X_s^(S-M)) 
        L_2 <- B*log(e_b+u)+S*log(e_s+u)-(e_b+e_s)+M*(log(X_b)+log(X_s)) 
        #after several calculation sometimes L_1 appears to be NA, so:
        if (is.nan(L_1)){L_1 = 0}
        L <- (L +sum(L_1,L_2))
      }
      return(-L)
    }
  }
  #solving the max problem 
  library(nloptr)
  lower <- c(0, 0, 0, 0, 0)
  upper <- c(1, 1, Inf, Inf, Inf)
  est = neldermead(in_param, EHO_fun(data,) , lower, upper) 
  #continuation of Yan Zhang, sorting objective function values:
  #sort_mat <- cbind(nrow=125, ncol=6)
  sort_mat[k,1] <- est$par[1] #aplha
  sort_mat[k,2] <- est$par[2] #delta
  sort_mat[k,3] <- est$par[3] #mu
  sort_mat[k,4] <- est$par[4] #epsilon_buy
  sort_mat[k,5] <- est$par[5] #epsilon_sell
  sort_mat[k,6] <- est$value*(-1) #fn_value
  k <- k+1
  
  #LK liklihood eq-n
  } else if(factor=="LK"){
    LK_fun <- function(data=NULL,in_param=c(FALSE,FALSE,FALSE,FALSE,FALSE)){ 
      function(x){
        in_param <- x
        a <- in_param[1] #alpha
        d <- in_param[2] #delta
        u <- in_param[3] #mu
        e_b <- in_param[4] #epsilon_buy
        e_s <- in_param[5] #epsilon_sell
        Buy <- data[,1] # vector of Buy operations
        Sell <- data[,2] # vector of Sell operations
        #need to account for each day for which data is considered: 
        days <- min(length(data[,1]),length(data[,2]))
        L <- 0 
        for (i in (1:days)) { 
          B <- Buy[i] 
          S <- Sell[i] 
          M <- min(B,S)+max(B,S)/2 
          e_1t <- -u-(B*log(1+(u/e_b)))
          e_2t <- -u-(S*log(1+(u/e_s)))
          e_3t <- -B*log(1+(u/e_b))- S*log(1+(u/e_s))
          e_maxt <- max(e_1t,e_2t,e_3t)
          #let us divide the Likelihood function into two subin_paramts: 
          L_1 <- log(a*d*exp(e_1t-e_maxt)+ 
                       a*(1-d)*exp(e_2t-e_maxt)+ 
                       (1-a)*exp(e_3t-e_maxt)) 
          L_2 <- B*log(e_b+u)+S*log(e_s+u)-(e_b+e_s)+e_maxt 
          L <- (L +sum(L_1,L_2))
        }
        return(-L)
      }
    }
    #solving the max problem 
    library(nloptr)
    lower <- c(0, 0, 0, 0, 0)
    upper <- c(1, 1, Inf, Inf, Inf)
    est = neldermead(in_param, LK_fun(data,) , lower, upper) 
    #continuation of Yan Zhang, sorting objective function values:
    #sort_mat <- cbind(nrow=125, ncol=6)
    sort_mat[k,1] <- est$par[1] #aplha
    sort_mat[k,2] <- est$par[2] #delta
    sort_mat[k,3] <- est$par[3] #mu
    sort_mat[k,4] <- est$par[4] #epsilon_buy
    sort_mat[k,5] <- est$par[5] #epsilon_sell
    sort_mat[k,6] <- est$value*(-1) #fn_value
    k <- k+1
  }
      }      
    }
  }
  #taking the last (largest) ones and eliminating those with boundary values of alpha
  #sort_mat <- sort_mat[with(sort_mat[,1]>0.0001 & sort_mat[,1]<0.9999),]
  # sort_mat <- sort_mat[sort_mat[,1]!=0 & sort_mat[,1]!=1,]
  sort_mat <- sort_mat[order(sort_mat[,6],na.last = NA, decreasing = FALSE),]
  sort_mat <- sort_mat[with(df, sort_mat[,1]>0.0001 & sort_mat[,1]<0.9999),]
  output <- cbind(c("alpha","delta","mu","epsilon_b","epsilon_s","PIN_value"),
                  c(sort_mat[nrow(sort_mat),1],sort_mat[nrow(sort_mat),2],
                    sort_mat[nrow(sort_mat),3],sort_mat[nrow(sort_mat),4],
                    sort_mat[nrow(sort_mat),5],
                    (sort_mat[nrow(sort_mat),1]*sort_mat[nrow(sort_mat),3])/((sort_mat[nrow(sort_mat),1]*sort_mat[nrow(sort_mat),3])
                                               + sort_mat[nrow(sort_mat),4] + sort_mat[nrow(sort_mat),5])))
  return(output)
  }
}
