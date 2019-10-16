library(italy)
head(italy10)
apply(italy10, FUN = function(x)length(unique(x)), 2)
italy1 <- italy10[italy10$IREG == 1,c(3,6,7,11)]

cats <- apply(italy, FUN = function(x)length(unique(x)), 2)
p <- 4
cats
italy1$SEX <- italy1$SEX - 1
italy1$CIT <- italy1$CIT - 1
italy1$ACOM4C
italy1$SETT <- italy1$SETT - 1


x <- italy1




#-------------------------------------------------------------------------------------
library(gtools)
library(tidyr)
library(utils)
set.seed(2020)

#This function calculates the overall probability of correctly clustering a point with another.
#Lambda.star is the truth. Does not account for correctly not clustering a singleton.
post.prob.match <- function(lambda, lambda.star){
  n <- length(lambda)
  uni.lamb <- unique(lambda.star)
  u <- length(uni.lamb)
  c<-0
  prob <-0
  for (j in 1:u){
    same.inds <- which(lambda.star ==  uni.lamb[j])
    group <- lambda[same.inds]
    if (length(group) !=1){
      combs <- combn(group, 2)
      c <- c+ length(combs)/2
      prob <- prob + sum(apply(combs, FUN = function(x){x[1] == x[2]}, 2))}
  }
  prob/c
}



n <- dim(italy1)[1]

#Prior values
a <- rep(.1,p)
b <- rep(1, p)


#Initialize values for Gibbbs Sampler
theta <- list()
theta.init <- list()
theta.init[[1]] <- c(.5,.5) 
theta.init[[2]] <- c(.95,.05)
theta.init[[3]] <- c(.25,.25,.25,.25)
theta.init[[4]] <- c(.2,.2,.2,.2,.2)

mu <- theta.init
beta.init <- c(.1,.1, .1, .1)


z <- cbind(rbinom(n, 1, beta.init[1]), rbinom(n, 1, beta.init[2]),
           rbinom(n, 1, beta.init[3]), rbinom(n, 1, beta.init[4]))

y <- matrix(0, n, p)
lambda.init <- sample(1:n, replace = TRUE)
N.init <- length(unique(lambda.init))
uni.lam <- unique(lambda.init)
for (l in 1:p){
  y[uni.lam,l] <- rmultinom(N.init, 1, theta.init[[l]]) %>%
                  apply(FUN = function(x) which(x==1), 2) - 1
}


gibbs.reps <- 10000
lambda <- lambda.init

#Allocate space for samples
beta.gibbs <-  matrix(0,nrow = p, ncol = gibbs.reps)
z.gibbs <- array(0,dim = c(n,p, gibbs.reps))
y.gibbs <- array(0, dim = c(n,p,gibbs.reps))
lambda.gibbs <- matrix(0, n, gibbs.reps)
N <- rep(0,gibbs.reps)
num.distortions <- matrix(0, n, gibbs.reps)
num.errors <- matrix(0, n , gibbs.reps)
num.errors.star <- matrix(0, n, gibbs.reps)
num.clusters <- matrix(0, 4, gibbs.reps)
#ppm <- rep(0,gibbs.reps)
start <- Sys.time()
start1 <- Sys.time()
#Start Gibbs Sampler
for(r in 1:gibbs.reps){
  #Sample lambda
  #Here I assume independence between lambda_i and lambda_j, i!=j.
  for (j in 1:n){
    cj <- lambda[j]
    num.errors[j,r] <- rowSums(x[j,] != y[cj,])  
    bad <- list()
    for(l in 1:p){
      if (z[j,l] == 0 ){
        bad[[l]] <- which(y[,l] != x[j,l])
      }
    }
    bad.all <- unique(unlist(bad))
    good <- setdiff(1:n, bad)
    lambda[j] <- sample(rep(good,2),1)
  }
  
  num.clusters[,r] <-  table(table(lambda), useNA = "always")[1:4]
  #ppm[r] <- post.prob.match(lambda,lambda.star)
  #When and where to use reduced y's?
  
  N[r] <- length(unique(lambda))
  lam.uni <- unique(lambda)
  
  #Sample beta
  for (l in 1:p){  
    a.gibbs <- a[l] + sum(z[,l])
    b.gibbs <- b[l] + sum(1-z[,l])
    beta[l] <- rbeta(1, a.gibbs, b.gibbs)
  }

  
    
  #Sample theta
  #Should theta_lm FC in paper have an 'm' with y? I think so
  for(l in 1:p){
    y.sums <- colSums(sapply(1:cats[l], FUN = function(x){x == 1+ y[lam.uni,l]}))
    xz.sums <- colSums(sapply(1:cats[l], FUN = function(c){c == 1+ x[,l]})*z[,l])
    dir <- mu[[l]] + y.sums + xz.sums+ 1
    theta[[l]] <- rdirichlet(1,dir)
  }

  
  #Sample y's
  #I think the paper gets the indexing of j.prime incorrect (should go to n, not N)
  #So I did what I think the paper is trying to say.
  #Basically I only sample the y's that have lambdas pointing to it.
  #This approach does not lead to interpretting y, but I don't think we should care,
  #because it does not affect lambda, and we just want to know whether two x's point 
  #to the same y, not which exact label it is (which is arbitrary)
  for (j.prime in 1:N[r]){
    #The js are all j's in R_ij.prime
    which.y <- lam.uni[j.prime]
    js <- which(lambda == which.y)
    len <- length(js)
    for (l in 1:p){
      if (sum(z[js,l]) != len){
        ind <- js[z[js,l]==0][1]
        y[which.y,l] <- x[ind, l]
      } else { 
        y[which.y,l] <-  rmultinom(1, 1, theta[[l]]) %>%
          apply(FUN = function(x) which(x==1), 2) - 1
    }
  }
  }
  #Sample z's
  for (l in 1:p){
    good <- x[,l] == y[lambda,l]
    z[!good ,l] <- 1
    prodm <- matrix(0, n, cats[l])
    x.m <- sapply(1:cats[l], FUN = function(c){c == 1+ x[,l]})
    for (m in 1:cats[l]){
      prodm[,m] <- theta[[l]][m]^x.m[,m]
    }
    prod <- apply(prodm, FUN= prod, 1)
    p_num <- beta[l]*prod
    p_den <- beta[l]*prod + (1-beta[l])
    z[good,l] <- rbinom(sum(good),1, p_num/p_den)
  }
  
  
  #Save values
  beta.gibbs[,r] <- beta
  z.gibbs[,,r] <- z
  y.gibbs[,,r] <- y
  lambda.gibbs[,r] <- lambda
  num.distortions[,r] <- rowSums(z)
  
  if(r %% 10 == 0){
    end1 <- Sys.time()
    print(paste("Done with Gibbs Sample ", r, " of ", gibbs.reps, " in ", end1 - start1, sep = ""))
    start1<- Sys.time() 
    }
}
end <- Sys.time()
end-start  
save(beta.gibbs, z.gibbs, y.gibbs, lambda.gibbs, 
     num.distortions, num.errors, num.clusters, file = "./italy_gibbs_output.RData" )


load("./italy_gibbs_output.RData")
plot(beta.gibbs[1,], type = "l")
plot(beta.gibbs[2,], type = "l")
plot(beta.gibbs[3,], type = "l")
plot(beta.gibbs[4,], type = "l")


plot(num.clusters[1,], type = "l")
plot(num.clusters[2,], type = "l")
plot(num.clusters[3,], type = "l")
plot(num.clusters[4,], type = "l")


dim(num.errors)
dim(z.gibbs)


ncls <- table(table(lambda.star))[1:4]
plot(num.clusters[1,], type = "l")
abline(h= ncls[1], col = "red")
plot(num.clusters[2,], type = "l")
abline(h=ncls[2], col = "red")
plot(num.clusters[3,], type = "l")
abline(h=ncls[3], col = "red")
plot(num.clusters[4,], type = "l")
abline(h=ncls[4], col = "red")

sum(lambda.gibbs[1, ] == lambda.gibbs[70,])/gibbs.reps
plot(lambda)
plot(ppm, type = "l")



print(paste("Done with Gibbs Sampler ", s, " of ", Sims, " in ", end - start, sep = ""))

num.distortions.sims[,,s] <- num.distortions
num.errors.sims[,,s] <- num.errors
num.errors.star.sims[,,s] <- num.errors.star
ls.possible.sims[,s] <- ls.possible