library(dplyr)
library(tidyr)
library(gtools)
library(utils)
library(italy)


# data stuff -----

#PARENT is ???
#SEX is sex
#ANASC is (probably) year of birth
#NASCREG is region of birth
#CIT is (probably) Italian citizenship
#ACOM4C is town size
#STUDIO is ???
#Q is working status
#QUAL is employment status
#SETT is branch of activity
#IREG is region

#The only thing missing is highest education level obtained
# head(italy10)
# head(italy08)
italy10.1 <- italy10[italy10$IREG == 1,-c(1,8,12)]
italy08.1 <- italy08[italy08$IREG == 1, -c(1,8,12)]
italy1 <- rbind(italy08.1[1:250,], italy10.1[1:250,])
italy1 <- italy1[complete.cases(italy1),]

IDs <- c(italy08$id[italy08$IREG ==1][1:250], italy10$id[italy10$IREG == 1][1:250])
# head(IDs)
cats <- apply(italy1, FUN = function(x)length(unique(x)), 2)
p <- length(cats)
# cats
italy1 <- apply(italy1, 2, function(x) x - min(x)) #rescales

x <- as.matrix(italy1)
n <- nrow(x)
N <- nrow(x)

#Create sensible lambda vector based off IDs
lambda.star <- rep(0, n)
for (u in unique(IDs)){
  lambda.star[which(IDs == u)] <- max(lambda.star) + 1
}


# sampler ------------------------------------------------------------------------------------
set.seed(2020)


#Checks to see if x,y,z are valid combination for sampling of lambdas
# valid.xyz <- function(x,y,z){
#   if( is.null(dim(x))){
#     sum(x*(1-z) == y*(1-z))==p
#   }
#   else{
#   rowSums(x*(1-z) == y*(1-z))==p
#   }
# }

#This function calculates the overall probability of correctly clustering a point with another.
#Lambda.star is the truth. Does not account for correctly not clustering a singleton.
# post.prob.match <- function(lambda, lambda.star){
#   n <- length(lambda)
#   uni.lamb <- unique(lambda.star)
#   u <- length(uni.lamb)
#   c<-0
#   prob <-0
#   for (j in 1:u){
#     same.inds <- which(lambda.star ==  uni.lamb[j])
#     group <- lambda[same.inds]
#     if (length(group) !=1){
#       combs <- combn(group, 2)
#       c <- c+ length(combs)/2
#       prob <- prob + sum(apply(combs, FUN = function(x){x[1] == x[2]}, 2))}
#   }
#   prob/c
# }

#Prior values
a <- rep(.05, p)
b <- rep(.95, p)


#Initialize values for Gibbbs Sampler
mu <-lapply(cats, FUN = function(x){rep(1/x, x)})
theta <- mu 
beta <- rep(.05, p)
z <- matrix(rbinom(n*p, 1, beta), n, p)
lambda <- sample(1:N, n, replace = TRUE)
y <- matrix(0, nrow = N, ncol = p)
# 
# for (j.prime in 1:N){
#   #The js are all j's in R_ij.prime
#   js <- which(lambda == j.prime)
#   len <- length(js)
#   inds <- c()
#   for(j in 1:len){
#     which.z <- setdiff(which(z[js[j],]==0), inds)
#     y[j.prime, which.z] <- x[js[j], which.z]
#     inds <- union(inds, which.z)
#     if (setequal(1:p, inds)){break}
#   }
#   inds.no <- setdiff(1:p, inds)
#   y[j.prime, inds.no] <- lapply(inds.no, FUN = function(index){which(rmultinom(1, 1, theta[[index]]) == 1) - 1}) %>%
#     unlist()
# }


gibbs.reps <- 10000

#Allocate space for samples
beta.gibbs <-  matrix(0,nrow = p, ncol = gibbs.reps)
z.gibbs <- array(0,dim = c(n,p, gibbs.reps))
y.gibbs <- list()
lambda.gibbs <- matrix(0, n, gibbs.reps)
#N <- rep(0,gibbs.reps)
num.distortions <- matrix(0, n, gibbs.reps)
num.errors <- matrix(0, n , gibbs.reps)
num.errors.star <- matrix(0, n, gibbs.reps)
num.clusters <- matrix(0, 4, gibbs.reps)
#ppm <- rep(0,gibbs.reps)
start <- Sys.time()
start1 <- Sys.time()
#Start Gibbs Sampler
for(r in 1:gibbs.reps){
  #Sample y's
  #I think the paper gets the indexing of j.prime incorrect (should go to n, not N)
  #So I did what I think the paper is trying to say.
  #Basically I only sample the y's that have lambdas pointing to it.
  #This approach does not lead to interpretting y, but I don't think we should care,
  #because it does not affect lambda, and we just want to know whether two x's point 
  #to the same y, not which exact label it is (which is arbitrary)
  for (j.prime in 1:N){
    #The js are all j's in R_ij.prime
    js <- which(lambda == j.prime)
    len <- length(js)
    inds <- c()
    for(j in 1:len){
      which.z <- setdiff(which(z[js[j],]==0), inds)
      y[j.prime, which.z] <- x[js[j], which.z]
      inds <- union(inds, which.z)
      if (setequal(1:p, inds)){break}
    }
    inds.no <- setdiff(1:p, inds)
    y[j.prime, inds.no] <- lapply(inds.no, FUN = function(index){which(rmultinom(1, 1, theta[[index]]) == 1) - 1}) %>%
      unlist()
  }
  
  #Sample beta
  a.gibbs <- a + colSums(z)
  b.gibbs <- b + colSums(1-z)
  beta <- rbeta(p, a.gibbs, b.gibbs)
  
  #Sample theta
  #Should theta_lm FC in paper have an 'm' with y? I think so
  y_ordered <- y[lambda, ]
  xz <- x*z
  for(l in 1:p){
    y.sums <- as.data.frame(table(y_ordered[, l] + 1))
    xz.sums <- as.data.frame(table(xz[, l]))
    
    sums <- merge(y.sums, xz.sums, by = "Var1", all.x = TRUE)
    mu_sums <- merge(data.frame(Var1 = 1:length(mu[[l]]), mu = mu[[l]]),
                     sums, by = "Var1", all.x = TRUE)
    theta[[l]] <- rdirichlet(1, rowSums(sums[, -1], na.rm = TRUE) + 1)
  }
  
  #Sample z's
  #This one doesn't sit well with me. 
  #There should be an m for x, yes?
  for (l in 1:p){
    good <- x[,l] == y[lambda,l]
    z[!good, l] <- 1
    x.m <- sapply(1:cats[l], FUN = function(c){c == 1+ x[,l]})
    x.m <- table(x[, l])
    
    prodm <- t(apply(x.m, FUN = function(x){theta[[l]]^x},1))
    prod <- apply(prodm, FUN= prod, 1)
    p_num <- beta[l]*prod
    p_den <- beta[l]*prod + (1-beta[l])
    z[good,l] <- rbinom(sum(good),1, p_num/p_den)
  }
  
  
  #Sample lambda
  #Here I assume independence between lambda_i and lambda_j, i!=j.
  for (j in 1:n){
      y.comp <- matrix(unlist(t(t(y)*(1-z[j,]))), n, p, byrow= FALSE)
      x.comp <- as.numeric(x[j,]*(1-z[j,]))
      p_lambda <- rowSums(abs(y.comp - matrix(rep(x.comp, nrow(y.comp)), nrow(y.comp), byrow = TRUE))) == 0
      
      if(sum(p_lambda) == 1) {
        lambda[j] <- which(p_lambda) 
      } else {
        lambda[j] <- sample(which(p_lambda), 1)
      }
  }
    
  #Save values
  beta.gibbs[,r] <- beta
  z.gibbs[,,r] <- z
  y.gibbs[[r]] <- y
  lambda.gibbs[,r] <- lambda
  num.distortions[,r] <- rowSums(z)
  num.clusters[, r] <-  table(table(lambda), useNA = "always")[1:4]
  #ppm[r] <- post.prob.match(lambda,lambda.star)
  #When and where to use reduced y's?
  
  #lam.uni <- unique(lambda)
  #N[r] <- length(lam.uni)
  
  if(r %% 10 == 0){
    end1 <- Sys.time()
    print(paste("Done with Gibbs Sample ", r, " of ", gibbs.reps, " in ", end1 - start1, sep = ""))
    start1<- Sys.time() 
  }
  if (r %% 1000 == 0){
    print(paste("Saving first ", r, " samples.", sep = ""))
    save(beta.gibbs, z.gibbs, y.gibbs, lambda.gibbs, 
             num.distortions, num.errors, num.clusters, r,  file = "./italy_gibbs_output_take3.RData" )
  }
}
end <- Sys.time()
end-start  
save(beta.gibbs, z.gibbs, y.gibbs, lambda.gibbs, 
     num.distortions, num.errors, num.clusters, r, file = "./italy_gibbs_output_take3.RData" )


load("./italy_gibbs_output_take2.RData")
library(blink)
linked <- links(lambda.gibbs)
plot(beta.gibbs[1,], type = "l")
plot(beta.gibbs[2,], type = "l")
plot(beta.gibbs[3,], type = "l")
plot(beta.gibbs[4,], type = "l")


dim(z.gibbs)

z.prop <- apply(z.gibbs, FUN = mean, c(2,3))
plot(z.prop[6,], type = "l")


plot(num.clusters[1,], type = "l")
plot(num.clusters[2,], type = "l")
plot(num.clusters[3,], type = "l")
plot(num.clusters[4,], type = "l")


dim(num.errors)
dim(z.gibbs)
ppm.actual <- function(lambda.gibbs){
  gibbs.reps <- dim(lambda.gibbs)[2]
  n <- dim(lambda.gibbs)[1]
  ppm.real <- matrix(0,n,n)
  for (i in 1:n){
    lsi <- lambda.gibbs[i,]
    for (j in 1:n){
      lsj <- lambda.gibbs[j,]
      ppm.real[i,j] <- sum(lsi == lsj)/gibbs.reps
    }
  }
  return(ppm.real)
}

ppm.real <- ppm.actual(lambda.gibbs)

sdg <- sqrt(gibbs.reps*.01*.99*(1/gibbs.reps^2))
hist(as.vector(ppm.real[ppm.real < 1]))
abline(v= c(.01 + 2*sdg, .01 - 2*sdg), col = "red")
quant <- quantile(ppm.real, .95)
match.matrix <- ppm.real > quant

#Calculate the false negative rate
fnr <- function(lambda.star, mm){
  n <- dim(mm)[1]
  count.num <- 0
  count.den <- 0
  for (i in 1:(n-1)){
    lsi <- lambda.star[i]
    for (j in (i+1):n){
      lsj <- lambda.star[j]
      #If they match in truth, do they match in estimation?
      if (lsi == lsj){
        count.den <- count.den + 1
        count.num <- count.num + mm[i,j]
      }
    }
  }
  1- count.num/count.den
}

#Calculate the false positive rate
fpr <- function(lambda.star, mm){
  n <- dim(mm)[1]
  count.num <- 0
  count.den <- 0
  for (i in 1:(n-1)){
    lsi <- lambda.star[i]
    for (j in (i+1):n){
      lsj <- lambda.star[j]
      #If they match in truth, do they match in estimation?
      if (lsi != lsj){
        count.den <- count.den + 1
        count.num <- count.num + mm[i,j]
      }
    }
  }
  count.num/count.den
} 

fnr(lambda.star, match.matrix)
fpr(lambda.star, match.matrix)
library(blink)
?links

lam.gs <- matrix(c(1,1,2,2,3,3,5,6,4,3,4,5,3,2,4,1,2,3,4,2), ncol=20,  nrow=4)



linked <- links(lambda.gibbs)


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
