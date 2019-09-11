library(MCMCpack)
rm(list=ls())

#Number of variables
p <-2
#Number of observations
n <- 3

mu1 <- c(1,1)
mu2 <- c(1,1)
lpp <- list()
lpp2 <- list()
ppp <- list()
ppp2 <- list()


Theta1 <- rdirichlet(1, mu1)
Theta2 <- rdirichlet(1, mu2)
Theta <- list(Theta1, Theta2)

z <- matrix(c(0,0,1,0,1,1), n, p)

x1 <- list(c(0,0), c(0,1), c(1,0), c(1,1))
x2 <- x1
x3 <- x2  
X <- expand.grid(x1,x2,x3)

lambda <- list(c(1,1,1), c(3,3,3), c(2,2,2),
               c(1,1,2), c(1,1,3), c(2,2,1), c(2,2,3), c(3,3,1), c(3,3,2),
               c(1,2,1), c(1,3,1), c(3,2,3), c(3,1,3), c(2,3,2), c(2,1,2),
               c(1,2,2), c(1,3,3), c(2,1,1), c(2,3,3), c(3,2,2), c(3,1,1), 
               c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))

# Simulate Y
Y.list <- list()
for (i in 1:dim(X)[1]){
  y <- matrix(2, n,p)
  for (j in 1:n){
    x <- unlist(X[i,][j])
    for (l in 1:p){
      if (z[j,l] == 0){
        y[j,l] <- x[l]
      }else{
        y[j,l] <- rbinom(1,1,Theta[[l]][1,1])
      }
    }
  }
  Y.list[[i]] <- y
}

lambda.sadinle.priors <- c(rep(1/15, 3), rep(1/30, 24))

for (i in 1:dim(X)[1]){
  lambda.post.probs <- rep(1, length(lambda))
  y <- Y.list[[i]]
  for (c in 1:length(lambda)){
    for (j in 1:n){
      x <- unlist(X[i,][j])
      cj <- lambda[[c]][j]
      for (l in 1:p){
        if (z[j,l] == 0 & x[l] != y[cj,l]){
          lambda.post.probs[c] <- 0
        } 
      }
    }
  }
  lpp[[i]] <- lambda.post.probs/sum(lambda.post.probs)
  lpp2[[i]] <- lambda.post.probs*lambda.sadinle.priors/sum(lambda.post.probs*lambda.sadinle.priors)
  ppp[[i]] <- c(sum(lpp[[i]][1:3]), sum(lpp[[i]][4:9]), sum(lpp[[i]][10:15]), sum(lpp[[i]][16:21]), sum(lpp[[i]][22:27])) 
  ppp2[[i]] <-  c(sum(lpp2[[i]][1:3]), sum(lpp2[[i]][4:9]), sum(lpp2[[i]][10:15]), sum(lpp2[[i]][16:21]), sum(lpp2[[i]][22:27])) 
}




par(mfrow=c(4,4))
par(mfrow=c(1,1))
for (i in 1:16){
  plot(ppp[[i]], col = "Red", type = "b", ylim = c(0, .3333), xlab = "Partition Number", ylab= "Prior Probability")  
  points(ppp2[[i]], col = "blue", type = "b")
}

for (i in 17:32){
  plot(ppp[[i]], col = "Red", type = "b", xlab = "Partition Number", ylab= "Prior Probability")
  points(ppp2[[i]], col = "blue", type = "b")
}

for (i in 33:48){
  plot(ppp[[i]], col = "Red", type = "b", xlab = "Partition Number", ylab= "Prior Probability")
  points(ppp2[[i]], col = "blue", type = "b")
}

for (i in 49:64){
  plot(ppp[[i]], col = "Red", type = "b", xlab = "Partition Number", ylab= "Prior Probability")
  points(ppp2[[i]], col = "blue", type = "b")
}

