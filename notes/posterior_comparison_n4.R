rm(list=ls())
library(tidyr)

###
# Steorts et. al. (2015) and Sadinle (2014) papers give alternative ways to specify priors.
# Steorts's prior is uniform on Lambda, which specify the relationship between observed x and potential true y's.
# Sadinle's prior is uniform on partitions, which can be derived from Lambda.
# We want to determine difference between posterior on Lambda for each method.
# Two step procedure to find posterior probabilities for both Steorts and Sadinle methods.
# Part 1: Specify true lambda. For all possible combinations of y and z with given lambda,
# generate all possible datasets, X.
# Part 2: Using possible X's found in part 1, for each combination of y and z find the posterior
# probability of obtaining the true lambda specified in part 1.
# Plots of posterior probabilities for all 27 possible lambdas are included at end.

#------------------------------------------------------------------------------------------------
valid.lambda <- function(x,z,x.real){
  diffs <- sum(x[z == 0] != x.real[z == 0])
  return(diffs ==0)
}

get.x.real<- function(y, lambda){ 
  c(y[lambda[1],], y[lambda[2],], y[lambda[3],], y[lambda[4],]) %>%
    rbind()  %>%
    t() %>%
    as.vector()
}
#Number of observations
n <- 4
#Number of variables
p <-2

total.combs <- 46656
#All possible values of lambda, where lambda_j = j' => x_j = y_j' in truth, j, j' = 1,2,3
lambdas <- expand.grid(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4))
t <- length(lambdas[,1])

possible.lambdas <- list(c(1,1,1,1), 
                c(1,1,1,2), c(1,1,2,1),c(1,2,1,1), c(2,1,1,1),
                c(1,1,2,2), c(1,2,1,2), c(1,2,2,1), 
                c(1,1,2,3), c(1,2,1,3), c(1,2,3,1), c(2,1,1,3), c(2,1,3,1), c(2,3,1,1),
                c(1,2,3,4))



lambda.sadinle.priors <- rep(0, t)
for(c in 1:t){
  lambda <- unlist(lambdas[c,])
  numb <- lambda %>%
    unique() %>%
    length()
  if (numb == 1){
    lambda.sadinle.priors[c] <- 1/60
  } else if (numb == 2){
    lambda.sadinle.priors[c] <- 1/180
  } else {
    lambda.sadinle.priors[c] <- 1/360 
    }
}

usable.lambdas <- list(c(1,1,1,1), c(2,1,1,1), c(2,2,1,1), c(3,2,1,1), c(4,3,2,1))
usable.indices <- c(1,2,6,7, 28)
#Generate all combinations of y and z (YZ) and all combinations of x (X), the data.
y1 <- list(c(0,0), c(0,1), c(1,0), c(1,1))
y2 <- y1
y3 <- y2  
y4 <- y1
z1 <- y1
z2 <- y1
z3 <- y1
z4 <- y1
X <- expand.grid(y1,y2,y3, y4)
names(X) <- c("x1", "x2", "x3", "x4")
X_nice <- apply(X, FUN =unlist, 1) %>%
          t()

YZ <- expand.grid(y1,y2,y3, y4, z1, z2, z3, z4)
names(YZ) <- c("y1", "y2", "y3", "y4", "z1", "z2", "z3", "z4")



#Initiate lists to store posterior probabilities
lsp <- list()
lsp2 <- list()
lpp <- list()
lpp2 <- list()
ppp <- list()
ppp2 <- list()
all.possible.x <- list()
all.errors <- list()
#Loop over all possible lambdas. For one iteration we have one truth (lambda.star).
#Takes approx 15 minutes to run.
start1<- Sys.time()
for (r in 1:length(usable.lambdas)){
  lambda.star <- usable.lambdas[[r]]
  possible.x <- list()
  ###Part 1 of outer-most loop, generate all possible data, X, for a given lambda.star and all combintations of y and z.
  for (i in 1:length(YZ$y1)){
    y <- matrix(unlist(YZ[i,1:n]),n,p, byrow =TRUE)
    x.real <- as.vector(t(rbind(y[lambda.star[1],], y[lambda.star[2],], y[lambda.star[3],], y[lambda.star[4],])))
    z <- unlist(YZ[i,(n+1):(2*n)])
    good.x <- apply(X_nice, FUN = valid.lambda, 1, z= z, x.real = x.real)
    possible.x[[i]] <- X_nice[good.x,]
    if(i %% 500 == 0){
      end1 <- Sys.time()
      print(paste("Done generating possible x's for iteration ", i , " of 65536, for lambda ", r, sep = ""))
      print(end1 - start1)
      start1 <- Sys.time()
      }
  }
  all.possible.x[[r]] <- possible.x
  errs <- list()
  lambda.star.post <- list()
  lambda.star.post2 <- list()
  start1 <- Sys.time()
  which.lambda <- usable.indices[r]
    ###Part 2 of outer-most loop, determine posterior probabilities of all lambdas based off each valid x,y,z combination.
  for (i in 1:length(YZ$y1)){
    y <- matrix(unlist(YZ[i,1:n]),n,p, byrow = TRUE)
    x.real <- c(y[lambda.star[1],], y[lambda.star[2],], y[lambda.star[3],], y[lambda.star[4],]) %>%
              rbind() %>%
              t() %>%
              as.vector()
    z <- unlist(YZ[i,(n+1):(2*n)])
    
    X_i <- possible.x[[i]]
    #Rearrange y to get it in the correct order based off lambda
    all.x.real <- t(apply(lambdas,FUN= get.x.real, 1, y = y))
    
    if (is.null(dim(X_i))){
      errs[[i]] <- sum(abs(x.real - X_i))
      lambda.post.probs <- as.numeric(apply(all.x.real, FUN = valid.lambda, 1, x= X_i, z= z))
      lpp <- lambda.post.probs/sum(lambda.post.probs)
      lpp2 <- lambda.post.probs*lambda.sadinle.priors/sum(lambda.post.probs*lambda.sadinle.priors)
      lambda.star.post[[i]] <-  lpp[which.lambda]
      lambda.star.post2[[i]] <- lpp2[which.lambda]
    }else{
      errs[[i]] <- apply(X_i, FUN = function(x){sum(abs(x.real - x))}, 1)
      lambda.post.probs <- t(apply(X_i, FUN = function(x){as.numeric(apply(all.x.real, FUN = valid.lambda, 1, x= x, z= z))}, 1))
      lpp <- lambda.post.probs/rowSums(lambda.post.probs)
      lpp2 <- lambda.post.probs*lambda.sadinle.priors/rowSums(lambda.post.probs*lambda.sadinle.priors)
      lambda.star.post[[i]] <-  lpp[,which.lambda]
      lambda.star.post2[[i]] <- lpp2[,which.lambda]
    }
    if (i %% 500 == 0){
      end1 <- Sys.time()
      print(paste("Done finding posterior probabilities for iteration ", i , " of 65536, for lambda ", r,  sep = ""))
      print(end1 - start1)
      start1 <- Sys.time()
    }
  }

  #Store posterior probabilities of truth for each possible truth.
  lsp[[r]] <- lambda.star.post
  lsp2[[r]] <- lambda.star.post2
  all.errors[[r]] <- errs
  print(paste("Done with ", r, " of 5 lambdas!", sep = ""))
}
total.combs <- 65536
err.df4 <- data.frame(rep(paste(usable.lambdas[1,], collapse=","), total.combs), lsp[[1]], lsp2[[1]], unlist(all.errors[[1]]))
names(err.df4) <- c("Lambda", "Posterior_Truth_Steort", "Posterior_Truth_Sadinle", "Number_of_Errors")

for (r in 2:length(usable.lambdas)){
  new.df <- data.frame(rep(paste(lambdas[r,], collapse=","), total.combs), lsp[[r]], lsp2[[r]], unlist(all.errors[[r]]))
  names(new.df) <- names(err.df)
  err.df4 <- rbind(err.df4, new.df)   
}

head(err.df4)
save(err.df4, file = "./error_dataframe_n_4")

length(unlist(all.possible.x))



load("./error_dataframe")
library(ggplot2)
library(dplyr)
library(tidyr)

err.df %>% 
  gather(key = method, post_prob_truth, starts_with("Posterior")) %>%
  ggplot(aes(factor(Number_of_Errors), post_prob_truth, color = method))+
  geom_jitter()+ 
  facet_wrap(~Lambda)

ggplot(err.df, aes(Lambda, Posterior_Truth_Steort, color = factor(Number_of_Errors)))+
  boxplot()


#Make some plots
boxplot(lsp, main = "Posterior Probability of Truth for steorts",
        xlab = "Lambda", 
        ylab = "Posterior Probability",
        sub = "Red dashed line is mean posterior probability")
abline(h = mean(unlist(lsp)), col = "red", lty= 2)




boxplot(lsp2, main = "Posterior Probability of Truth for Sadinle",
        xlab = "Lambda", 
        ylab = "Posterior Probability",
        sub = "Blue dashed line is mean posterior probability")
abline(h = mean(unlist(lsp2)), col = "blue", lty= 2)

hist(unlist(lapply(lsp2, mean)), col = "blue", breaks = seq(.1,.22, .01), 
     main = "Histogram of Mean Posterior Probability of True Lambda",
     sub = "Red = steorts, Blue = Sadinle",
     xlab = "Mean Posterior Probability")
hist(unlist(lapply(lsp, mean)), add = TRUE, col = "red",  breaks = seq(.1,.22, .01))


### Old Code -----------------------------------------------------------------------------------

#par(mfrow=c(4,4))
#par(mfrow=c(1,1))
#for (i in 1:16){
# plot(ppp[[i]], col = "Red", type = "b", ylim = c(0, .3333), xlab = "Partition Number", ylab= "Prior Probability")  
#  points(ppp2[[i]], col = "blue", type = "b")
#}

#for (i in 17:32){
#  plot(ppp[[i]], col = "Red", type = "b", ylim = c(0, .3333), xlab = "Partition Number", ylab= "Prior Probability")
#  points(ppp2[[i]], col = "blue", type = "b")
#}

#for (i in 33:48){
# plot(ppp[[i]], col = "Red", type = "b", ylim = c(0, .3333), xlab = "Partition Number", ylab= "Prior Probability")
#  points(ppp2[[i]], col = "blue", type = "b")
#}

#for (i in 49:64){
# plot(ppp[[i]], col = "Red", type = "b", ylim = c(0, .3333), xlab = "Partition Number", ylab= "Prior Probability")
#points(ppp2[[i]], col = "blue", type = "b")
#}



#lpp[[i]] <- lambda.post.probs/sum(lambda.post.probs)
#lpp2[[i]] <- lambda.post.probs*lambda.sadinle.priors/sum(lambda.post.probs*lambda.sadinle.priors)

#ppp[[i]] <- c(sum(lpp[[i]][1:3]), sum(lpp[[i]][4:9]), sum(lpp[[i]][10:15]), sum(lpp[[i]][16:21]), sum(lpp[[i]][22:27])) 
#ppp2[[i]] <-  c(sum(lpp2[[i]][1:3]), sum(lpp2[[i]][4:9]), sum(lpp2[[i]][10:15]), sum(lpp2[[i]][16:21]), sum(lpp2[[i]][22:27])) 

#Y.list <- list()
#for (i in 1:dim(X)[1]){
#  y <- matrix(2, n,p)
#  for (j in 1:n){
#    x <- unlist(X[i,][j])
#    for (l in 1:p){
#      if (z[j,l] == 0){
#        y[j,l] <- x[l]
#      }else{
#        y[j,l] <- rbinom(1,1,Theta[[l]][1,1])
#      }
#    }
#  }
#  Y.list[[i]] <- y
#}
