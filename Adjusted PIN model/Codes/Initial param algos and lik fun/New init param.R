library(nleqslv)

#Testing data
Buy<-c(350,250,500,552,163,345,847,923,123,349)
Sell<-c(382,500,463,550,200,323,456,342,578,455)
data<-cbind(Buy,Sell)

initpar <- function(data=NULL){
a_i <- c(0.1,0.3,0.5,0.7,0.9) #vector alpha of initial values
d_j <- c(0.1,0.3,0.5,0.7,0.9) #vector delta of initial values
theta_t <- c(0.1,0.3,0.5,0.7,0.9) #vector theta of initial values
g_n <- c(0.1,0.3,0.5,0.7,0.9) #vector gamma of initial values

B_bar <- mean(data[,1])
S_bar <- mean(data[,2])
B2_bar <- mean(data[,1]^2)
S2_bar <- mean(data[,2]^2)

init_mat <- matrix(nrow = 625, ncol = 9)

k <- 1
for (i in (1:5)) {
  for (j in (1:5)) {
    for (t in (1:5)) {
     for (n in (1:5)) {
        
      a <- a_i[i]
      d <- d_j[j]
      theta <- theta_t[t]
      g <- g_n[n]
      e_b <- g*B_bar #epsilon_buy
      e_s <- g*S_bar #epsilon_sell

# 
# x[1] - mu_b
# x[2] - mu_s
# x[3] - Delta_b
# x[4] - Delta_s

#System of moment conditions
fn <- function(x) {
  
  B1 <- e_b + theta*x[3] + a*d*x[1]-B_bar
  S1 <- e_s + theta*x[4] + a*(1-d)*x[2]-S_bar
  B2 <- e_b^2 + a*d*x[1]^2 + theta*(x[3]^2 + 2*e_b*x[3]) + 2*a*d*x[1]*(e_b+theta*x[3])-B2_bar
  S2 <- e_s^2 + a*(1-d)*x[2]^2 + theta*(x[4]^2 + 2*e_s*x[4]) + 2*a*(1-d)*x[2]*(e_s + theta*x[4])-S2_bar
  
  return(c(B1, S1, B2, S2) )
  
}

param <- nleqslv(c(100,100,100,100), fn)$x
if(sum(param>0)<4){param <- c(NA, NA, NA, NA)}

init_mat[k,] <- c(a, d, theta, e_b, e_s, param) 
k <- k+1

         }
     }
  }
} 

init_mat <- data.frame(init_mat)
colnames(init_mat) <- c("alpha","delta", "theta", "mu_b", "mu_s","epsilon_b","epsilon_s",
                        "Delta_b", "Delta_s")

#Eliminating cases with negative trading rates
init_mat <- init_mat[complete.cases(init_mat), ]
return(init_mat)
}
