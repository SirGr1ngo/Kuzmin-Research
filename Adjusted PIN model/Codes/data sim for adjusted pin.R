set.seed(1)
library(ggplot2)
#Generating probabilities of parameters mu, a, d
p_mu <- runif(1000, min=0, max=1)
p_mu_b <- runif(1000, min=0, max=1)
p_a <- runif(1000, min=0, max=1)
p_d <- runif(1000, min=0, max=1)
p_theta <- runif(1000, min=0, max=1)
p_e <- runif(1000, min=0, max=1)
p_eb <- runif(1000, min=0, max=1)
p_Delta_b <- runif(1000, min=0, max=1)

#Trade intensity (2500)
I <- 2500

#Theoretical trading rates
u_b <- p_mu*p_mu_b*I
u_s <- p_mu*(1-p_mu_b)*I
e_s <- (1-p_mu)*p_e*p_eb*I
e_b <- (1-p_mu)*p_e*(1-p_eb)*I
Delta_b <- (1-p_mu)*(1-p_e)*p_Delta_b*I
Delta_s <- (1-p_mu)*(1-p_e)*(1-p_Delta_b)*I

#Theoretical PIN-s
PIN <- p_a*(p_d*u_b+(1-p_d)*u_s)/(p_a*(p_d*u_b+(1-p_d)*u_s)+(Delta_b+Delta_s)*p_theta+e_s+e_b)





f.genorderflow2 <- function(N, a, d, theta, u_b, u_s, Delta_b, Delta_s, e_b, e_s) {
  buySells <- matrix(0,N,3)
  for (i in 1:N) {
    if (rbinom(1,1,a) == 1) {
      # private information has occured
      if(rbinom(1,1,d) == 1) {
        # positive news
        if(rbinom(1,1, theta) == 1) {
          # case one
          buySells[i,1] <- rpois(1, e_b + u_b + Delta_b)
          buySells[i,2] <- rpois(1, e_s + Delta_s)
          buySells[i,3] <- 1
        } else {
          # case two
          buySells[i,1] <- rpois(1, e_b + u_b)
          buySells[i,2] <- rpois(1, e_s)
          buySells[i,3] <- 2				
        }
      } else {
        # negative news
        if(rbinom(1,1, theta) == 1) {
          # case three
          buySells[i,1] <- rpois(1, e_b + Delta_b)
          buySells[i,2] <- rpois(1, e_s + u_s + Delta_s)
          buySells[i,3] <- 3
        } else {
          # case four
          buySells[i,1] <- rpois(1, e_b)
          buySells[i,2] <- rpois(1, e_s + u_s)
          buySells[i,3] <- 4				
        }
      }
    } else {
      # private information has not occured 
      if(rbinom(1,1, theta) == 1) {
        # case five
        buySells[i,1] <- rpois(1, e_b + Delta_b)
        buySells[i,2] <- rpois(1, e_s + Delta_s)
        buySells[i,3] <- 3
      } else {
        # case six
        buySells[i,1] <- rpois(1, e_b)
        buySells[i,2] <- rpois(1, e_s)
        buySells[i,3] <- 4				
      }
    }	
  }
  return(buySells)
}

#
# 1 - old Lik, Simple init par
# 2 - old Lik, New init par
# 3 - New Lik, Simple init par
# 4 - New Lik. New init par


order <- list()
 pin_est_1 <- list()
 pin_est_2 <- list()
 pin_est_3 <- list()
pin_est_4 <- list()

for (i in 1:10) {
  order[[i]] <- f.genorderflow2(24, p_a[i], p_d[i], p_theta[i], u_b[i], u_s[i],
                              Delta_b[i], Delta_s[i], e_b[i], e_s[i])
  pin_est_1[[i]] <- pin_adj(order[[i]])
  pin_est_2[[i]] <- pin_adj_inp_Duarte(order[[i]])
  pin_est_3[[i]] <- dy_pin(order[[i]])
  pin_est_4[[i]] <- dy_pin_inp_newlik(order[[i]])
  print(i)
}

#save.image(file = "pinest4more.RData")

pin_e <- PIN[1:1000]

pin_est_1 <- do.call(rbind, pin_est_1)
pin_est_2 <- do.call(rbind, pin_est_2)
pin_est_3 <- do.call(rbind, pin_est_3)
pin_est_4 <- do.call(rbind, pin_est_4)

pin_comp_1 <- data.frame(pin_e=pin_e, p_a=p_a, p_d=p_d, p_theta=p_theta, mu_b_e=u_b, mu_s_e=u_s,pin_est = pin_est_1$PIN_value, diff=pin_e - pin_est_1$PIN_value, 
                         a=pin_est_1$a, d=pin_est_1$d, theta=pin_est_1$theta, mu_b=pin_est_1$mu_b,
                         mu_s=pin_est_1$mu_s)

pin_comp_2 <- data.frame(pin_e=pin_e[1:375], p_a=p_a[1:375], p_d=p_d[1:375], p_theta=p_theta[1:375], mu_b_e=u_b[1:375], mu_s_e=u_s[1:375],pin_est = pin_est_2$PIN_value, diff=pin_e[1:375] - pin_est_2$PIN_value,
                         a=pin_est_2$a, d=pin_est_2$d, theta=pin_est_2$theta, mu_b=pin_est_2$mu_b,
                         mu_s=pin_est_2$mu_s)

pin_comp_3 <- data.frame(pin_e=pin_e, p_a=p_a, p_d=p_d, p_theta=p_theta, mu_b_e=u_b, mu_s_e=u_s, pin_est = pin_est_3$PIN_value, diff=pin_e - pin_est_3$PIN_value, 
                         a=pin_est_3$a, d=pin_est_3$d, theta=pin_est_3$theta, mu_b=pin_est_3$mu_b,
                         mu_s=pin_est_3$mu_s)

pin_comp_4 <- data.frame(pin_e=pin_e, p_a=p_a, p_d=p_d, p_theta=p_theta, mu_b_e=u_b, mu_s_e=u_s,pin_est = pin_est_4$PIN, diff=pin_e - pin_est_4$PIN, 
                         a=pin_est_4$a, d=pin_est_4$d, theta=pin_est_4$theta, mu_b=pin_est_4$mu_b,
                         mu_s=pin_est_4$mu_s)




