rm(list=ls())


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

#Number of observations
n <- 3
#Number of variables
p <-2

#All possible values of lambda, where lambda_j = j' => x_j = y_j' in truth, j, j' = 1,2,3
lambdas <-list(c(1,1,1), c(2,2,2),c(3,3,3), 
               c(1,1,2), c(1,1,3), c(2,2,1), c(2,2,3), c(3,3,1), c(3,3,2),
               c(1,2,1), c(1,3,1), c(3,2,3), c(3,1,3), c(2,3,2), c(2,1,2),
               c(1,2,2), c(1,3,3), c(2,1,1), c(2,3,3), c(3,2,2), c(3,1,1), 
               c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))


#Generate all combinations of y and z (YZ) and all combinations of x (X), the data.
y1 <- list(c(0,0), c(0,1), c(1,0), c(1,1))
y2 <- y1
y3 <- y2  
z1 <- y1
z2 <- y1
z3 <- y1
X <- expand.grid(y1,y2,y3)
names(X) <- c("x1", "x2", "x3")

YZ <- expand.grid(y1,y2,y3, z1, z2, z3)
names(YZ) <- c("y1", "y2", "y3", "z1", "z2", "z3")

#Give priors to differentiate between Sadinle and Steorts methods
lambda.sadinle.priors <- c(rep(1/15, 3), rep(1/30, 24))

#Initiate lists to store posterior probabilities
lsp <- list()
lsp2 <- list()
lpp <- list()
lpp2 <- list()
ppp <- list()
ppp2 <- list()

#Loop over all possible lambdas. For one iteration we have one truth (lambda.star).
#Takes approx 15 minutes to run.
for (r in 1:length(lambdas)){
lambda.star <- lambdas[[r]]
possible.x <- list()
  ###Part 1 of outer-most loop, generate all possible data, X, for a given lambda.star and all combintations of y and z.
  for (i in 1:length(YZ$y1)){
    y <- matrix(unlist(YZ[i,1:3]),n,p, byrow =TRUE)
    z <- matrix(unlist(YZ[i,4:6]),n,p, byrow =TRUE)
    xs <- c()
    #Loop over all 2^2^3=64 potential X's and determine which ones are possible.
    for (k in 1:length(X$x1)){
      x <- matrix(unlist(X[k,]),n,p, byrow = TRUE)
      marker <- 0
      #Loop over each observation
      for (j in 1:n){
        cj <- lambda.star[j]
        #Loop over each variable
        for (l in 1:p){
          #Determine if [j,l] entries of x,y,z and lambda are valid, mark the entry by adding a 1 to marker.
          if (z[j,l] != 0 | x[j,l] == y[cj,l]){
            marker <- marker+1
          }
        }
      }
      #We need all p*n  = 6 possible [j,l] entries to be valid for that whole x to be possible,
      #thus marker needs to be 6, in which case we add to our ongoing list of possible x's.
      if (marker == 6){
        xs <- rbind(xs, X[k,])
      }
    }
    #Store all possibles x's for the y-z combination i.
    possible.x[[i]] <- xs
    if(i %% 500 == 0){
      print(paste("Done generating possible x's for iteration ", i , " of 4096, for lambda ", r, sep = ""))
    }
  }
  lambda.star.post <- c()
  lambda.star.post2 <- c()
  ###Part 2 of outer-most loop, determine posterior probabilities of all lambdas based off each valid x,y,z combination.
  for (i in 1:length(YZ$y1)){
    y <- matrix(unlist(YZ[i,1:3]),n,p, byrow = TRUE)
    z <- matrix(unlist(YZ[i,4:6]),n,p, byrow = TRUE)
    X_i <- possible.x[[i]] 
    #Loop over all the possible x's for each combination of y and z. 
    #The number of possible x's is a power of 2 determined by how many 1's (distortions) there are in z.
    for (k in 1:nrow(X_i)){
      x <- matrix(unlist(X_i[k,]),n,p, byrow = TRUE)
      lambda.post.probs <- rep(1, length(lambdas))
      #Loop over all possible lambdas, and calculate a posterior probability for each one.  
      for (c in 1:length(lambdas)){
        is.break <- FALSE
        #Loop over each observation
        for (j in 1:n){
          cj <- lambdas[[c]][j]
          #Loop over each variable
          for (l in 1:p){
            #For each [j,l] entry, determine if x,y,z and lambda are valid together.
            #Set posterior probability to 0 and break from loops if they are not.
            if (z[j,l] == 0 & x[j,l] != y[cj,l]){
              lambda.post.probs[c] <- 0
              is.break <- TRUE
              break
            } 
          }
          if(is.break){break}
        }
      }
      #Normalize lambda.post.probs to sum to 1.
      #lpp is for Steorts, lpp2 is for Sadinle
      lpp <- lambda.post.probs/sum(lambda.post.probs)
      lpp2 <- lambda.post.probs*lambda.sadinle.priors/sum(lambda.post.probs*lambda.sadinle.priors)
      #We are specifically interested in the posterior probability for the true lambda, lambda.star (the rth value in lpp).  
      lambda.star.post <- append(lambda.star.post, lpp[r])
      lambda.star.post2 <- append(lambda.star.post2, lpp2[r])
    }
    if (i %% 500 == 0){
      print(paste("Done finding posterior probabilities for iteration ", i , " of 4096, for lambda ", r,  sep = ""))
    }
  }
  #Store posterior probabilities of truth for each possible truth.
  lsp[[r]] <- lambda.star.post
  lsp2[[r]] <- lambda.star.post2
  print(paste("Done with ", r, " of 27 lambdas!", sep = ""))
}

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
