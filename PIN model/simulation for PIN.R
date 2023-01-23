#rm(list = ls())

set.seed(1)
library(ggplot2)
#Generating porabilities of parameters mu, a, d
p_mu <- runif(100, min=0, max=1)
p_a <- runif(100, min=0, max=1)
p_d <- runif(100, min=0, max=1)
p_eb <- runif(100, min=0, max=1)

#Trade intensity
I <- 2500

#Theoretical trading rates
mu <- p_mu*I
e_s <- (1-p_mu)*p_eb*I
e_b <- (1-p_mu)*(1-p_eb)*I

#Theoretical PIN-s
PIN <- p_a*mu/(p_a*mu+e_s+e_b)

#alpha and delta for Order flow generation
a <- rbinom(100, 1, p_a)
d <- rbinom(100, 1, p_d)

#Forming data with theoretical values
df <- data.frame(p_a, p_d, p_mu, a, d, mu, e_s, e_b, PIN)

#Generating Buy and Sell orders
f.genorderflow <- function(N, p_a, p_d, mu, e_b, e_s) {
  buySells <- matrix(0,N,3)
  for (i in 1:N) {
    if (rbinom(1,1,p_a) == 1) {
      if(rbinom(1,1,p_d) == 1) {
        #negative signal
        buySells[i,1] <- rpois(1,e_b) # buy
        buySells[i,2] <- rpois(1, e_s + mu) # sell  
        buySells[i,3] <- -1 #marking
      } else {
        #positive signal
        buySells[i,1] <- rpois(1,e_b + mu) # buy
        buySells[i,2] <- rpois(1, e_s) # sell  
        buySells[i,3] <- +1 #marking
      }
    } else {
      #no signal
      buySells[i,1] <- rpois(1,e_b) # buy
      buySells[i,2] <- rpois(1, e_s) # sell 		
    }	
  }
  return(buySells)
}


order <- list()
pin_est <- vector()
pin_est1 <- vector()
for (i in 1:100) {
  order[[i]] <- f.genorderflow(60, p_a[i], p_d[i], mu[i], e_b[i], e_s[i])
  pin_est[i] <- tryCatch(print(as.numeric(pin_fun(order[[i]], "LK")[6,2])),
                          error = function(e) print(0))
}

pin_e <- df$PIN

pin_diff <- pin_e - pin_est
mse_lk <- sum((pin_e - pin_est)^2)/length(pin_est)

pin_comp <- data.frame(pin_e=pin_e, pin_est = pin_est, diff=pin_diff)

ggplot(pin_comp, aes(x=pin_e, y=pin_est)) + 
  geom_point()

