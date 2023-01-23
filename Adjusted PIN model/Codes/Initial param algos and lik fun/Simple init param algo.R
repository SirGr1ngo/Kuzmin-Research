simple_init_par <- function(N=10){
a_i <- c(0.1,0.3,0.5,0.7,0.9) #vector alpha of initial values
d_j <- c(0.1,0.3,0.5,0.7,0.9) #vector delta of initial values
theta_n <- c(0.1,0.3,0.5,0.7,0.9) #vector gamma of initial values
init_mat <- matrix(nrow = 125, ncol = 9)

k <- 1
for (i in 1:5) {
  for (j in 1:5) {
    for (n in 1:5) {
      init_mat[k,] <- c(a_i[i],d_j[j],theta_n[n],10,10, 10,10,10, 10)
      k <- k+1
      }
    }
}
init_mat <- data.frame(init_mat)
colnames(init_mat) <- c("alpha","delta", "theta", "mu_b", "mu_s","epsilon_b","epsilon_s",
                        "Delta_b", "Delta_s")
x <- sample(1:125, N, replace = FALSE)
init_mat <- init_mat[x,]
return(init_mat)
}