rm(list =ls())
library(gtools)
library(tidyr)
set.seed(3894)



### Gibbs Sampler

#500 observations
n <- 500
#2 variables
p <- 2
#5 categories per variable
cats <- 5

#Fix true lambda
lambda.star <- sample(1:n, replace = TRUE)

#Simulated but now fixed probabilities
a12 <- c(.5,.6)
b12 <- c(1,1)
beta.sim <- rbeta(p,a12, b12)
mu1 <- c(.2,.2,.2,.2,.2)
mu2 <- c(.1,.1,.4,.3,.1)
theta.sim <- rbind(rdirichlet(n = 1, mu1 ),
                   rdirichlet(n = 1, mu2) )

#y and z are simulated but given truth
y.sim <- array(0, c(n, p, cats))
z.sim <- matrix(0, n, p)
x.sim <- array(0,c(n,p,cats))
for (l in 1:p){
  y.sim[,l,] <- t(rmultinom(n, 1, theta.sim[l,]))
  z.sim[,l] <- rbinom(n, 1, beta.sim[l])

}

#Initialize values for Gibbbs Sampler
mu <- rbind(mu1,mu2)
beta.init <- c(.5,.5)
theta.init <- matrix(.2, nrow = 2, ncol = 5)

z <- cbind(rbinom(n, 1, beta.init[1]), rbinom(n,1, beta.init[2]))
y <- y.sim
for (l in 1:p){
  y[,l,] <- t(rmultinom(n, 1, theta.init[l,]))
}




#Everything above is fixed for all simulations
#Everything below changes for each simulation
Sims <- 100
num.distortions.sims <- array(0, dim = c(n,gibbs.reps,Sims)) 
num.errors.sims <- array(0, dim= c(n,gibbs.reps, Sims))
num.error.star.sims <- array(0, dim = c(n,gibbs.reps, Sims))
ls.possible.sims <- matrix(0, gibbs.reps, Sims)

for(s in 1:Sims){
 
  for (l in 1:p){
  #Simulate data, X
  #X is same as (reorderd) Y when z=0
  #Reorder y's to match true lambda
  x.sim[,l,] <- y.sim[lambda.star,l,]
  x.sim[z.sim[,l]==1,l,] <- t(rmultinom(sum(z.sim[,l]), 1, theta.sim[l,]))
  #### Done with Simulation
  }

  x <- x.sim
  lambda <- rep(0, n)

  #Allocate space for samples
  gibbs.reps <- 1000
  beta.gibbs <-  matrix(0,nrow = p, ncol = gibbs.reps)
  theta.gibbs <- array(0, dim = c(p,cats,gibbs.reps))
  z.gibbs <- array(0,dim = c(n,p, gibbs.reps))
  y.gibbs <- array(0, dim = c(n,p,cats,gibbs.reps))
  lambda.gibbs <- matrix(0, n, gibbs.reps)
  N <- rep(0,gibbs.reps)
  num.distortions <- matrix(0, n, gibbs.reps)
  num.errors <- matrix(0, n , gibbs.reps)
  num.errors.star <- matrix(0, n, gibbs.reps)
  ls.possible <- rep(0,gibbs.reps)
  start <- Sys.time()
  #Start Gibbs Sampler
  for(r in 1:gibbs.reps){
    #Sample lambda
    #Here I assume independence between lambda_i and lambda_j, i!=j.
    for (j in 1:n){
      bad1 <- c()
      bad2 <- c()
      if (z[j,1] == 0 ){
        bad1 <- which(rowSums(t(apply(y[,1,], FUN= '==', y= x[j,1,], 1 ))) !=cats)
      }
      if (z[j,2] == 0){
        bad2 <- which(rowSums(t(apply(y[,2,], FUN= '==', y= x[j,2,], 1 ))) !=cats)
      }
      bad <- union(bad1,bad2)
      good <- setdiff(1:n, bad)
      ls.possible[r] <- ls.possible[r] + lambda.star[j] %in% good
      lambda[j] <- sample(rep(good,2),1)
    }
  
    #When  and where to use reduced y's?
  
    N[r] <- length(unique(lambda))
    lam.uni <- unique(lambda)
  
    #Sample beta
    a.gibbs1 <- a12[1] + sum(z[,1])
    a.gibbs2 <- a12[2] + sum(z[,2])
    b.gibbs1 <- b12[1] + sum(1-z[,1])
    b.gibbs2 <- b12[2] + sum(1-z[,2])
  
    beta <- rbeta(p, c(a.gibbs1,a.gibbs2), c(b.gibbs1, b.gibbs2))

    #Sample theta
    #Should theta_lm FC in paper have an 'm' with y? I think so
    dir <-matrix(0, p, cats)
    for(l in 1:p){
      dir[l,] <- mu[l,] + colSums(y[lam.uni,l,])  + colSums(z[,l]*x[,l,])+ 1
    }
    theta <- rbind(rdirichlet(1,dir[1,]),
                   rdirichlet(1, dir[2,]))
  
    #Sample y's
    for (j.prime in 1:N[r]){
      #The js are all j's in R_ij.prime
      js <- which(lambda == j.prime)
      len <- length(js)
      for (l in 1:p){
        if (len != 0 & sum(z[js,l]) != len){
          ind <- js[z[js,l]==0][1]
          y[j.prime,l,] <- x[ind, l,]
        } else { 
          y[j.prime,l,] <-  t(rmultinom(1,1,theta[l,]))
          }
      }
    }
  
    #Sample z's
    for (l in 1:p){
      good <- rowSums(x[,l,] == y[lambda,l,])==cats
      z[!good ,l] <- 1
      prodm <- matrix(0, n,cats)
      for (m in 1:cats){
        prodm[,m] <- theta[l,m]^x[,l,m]
      }
      prod <- apply(prodm, FUN= prod, 1)
      p_num <- beta[l]*prod
      p_den <- beta[l]*prod + (1-beta[l])
      z[good,l] <- rbinom(sum(good),1, p_num/p_den)
    }
  
    for ( j in 1: n){
      cj <- lambda[j]
      cj.star <-lambda.star[j]
      num.errors[j,r] <- sum(rowSums(x[j,,] == y[cj,,]) == cats)
      num.errors.star[j,r] <- sum(rowSums(x[j,,] == y[cj.star,,]) == cats)
    }
  
    #Save values
    beta.gibbs[,r] <- beta
    theta.gibbs[,,r] <- theta
    z.gibbs[,,r] <- z
    y.gibbs[,,,r] <- y
    lambda.gibbs[,r] <- lambda
    num.distortions[,r] <- rowSums(z)
  
    if(r %% 100 == 0){print(paste("Done with Gibbs Sample ", r, " of ", gibbs.reps, sep = ""))}
  }
  end <- Sys.time()
  print(paste("Done with Gibbs Sampler ", s, " of ", Sims, " in ", end - start, sep = ""))

  num.distortions.sims[,,s] <- num.distortions
  num.errors.sims[,,s] <- num.errors
  num.errors.star.sims[,,s] <- num.errors.star
  ls.possible.sims[,s] <- ls.possible
}

plot(beta.gibbs[1,], type = "l")
plot(beta.gibbs[2,], type = "l")

plot(N, type = "l")
abline(h = length(unique(lambda.star)), col = "red")
abline(h = mean(N), col = "blue")



