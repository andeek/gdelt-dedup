#Difference in priors between Sadinle and Stoert

#The nth bell number, B_n, is the ways to partition a set of size n.
#With Sadinle, each of these B_n partitions has 1/B_n chance of occurring a priori 
B_n<- c(1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597, 27644437, 190899322, 1382958545, 10480142147, 82864869804, 682076806159, 5832742205057)

# With Stoert, a record has equal probability of being from each of the n entities, resulting in n^n different possibilities of records.

#n = 2
n <- 2
Sadinle <- rep(1/B_n[n], B_n[n])
Stoert <- c(.5, .5)
#Plot of prior probabilities
plot(Stoert, col = "Red", type = "b", xlab = "Partition Number", ylab= "Prior Probability", main = "Stoert (Red) vs. Sadinle (Black)")
points(Sadinle)
#Do vectors sum to probability 1?
sum(Sadinle)
sum(Stoert)
#What's the difference between the smallest and largest values of Stoert to Sadinle?
Sadinle[1] - Stoert[1]
abs(Sadinle[B_n[n]] - Stoert[B_n[n]])

#n = 3
n <-3

Sadinle <- rep(1/B_n[n], B_n[n])
Stoert <- c(n, 
            rep(n*(n-1), n),
            factorial(n))/n^n
plot(Stoert, col = "Red", type = "b", xlab = "Partition Number", ylab= "Prior Probability", main = "Stoert (Red) vs. Sadinle (Black)")
points(Sadinle, type = "b")
sum(Sadinle)
sum(Stoert)
Sadinle[1] - Stoert[1]
abs(Sadinle[B_n[n]] - Stoert[B_n[n]])

#n = 4
n <- 4
Sadinle <- rep(1/B_n[n], B_n[n])
Stoert <- c(n, 
            rep(n*(n-1), n),
            rep(n*(n-1), choose(n,2)/2),
            rep(factorial(n),choose(n,2)),
            factorial(n))/n^n
plot(Stoert, col = "Red", type = "b", xlab = "Partition Number", ylab= "Prior Probability", main = "Stoert (Red) vs. Sadinle (Black)")
points(Sadinle, type = "b")
sum(Sadinle)
sum(Stoert)
Sadinle[1] - Stoert[1]
abs(Sadinle[B_n[n]] - Stoert[B_n[n]])

#n = 5
n <- 5
Sadinle <- rep(1/B_n[n], B_n[n])
Stoert <- c(n, 
            rep(n*(n-1), n), 
            rep(n*(n-1), choose(n,2)),
            rep(n*(n-1)*(n-2), choose(n,2)),
            rep(n*(n-1)*(n-2), n*(choose((n-1),2))/2),
            rep(n*(n-1)*(n-2)*(n-3), choose(n,2)),
            factorial(n))/n^n
plot(Stoert, col = "Red", ylim = c(0, max(Sadinle, Stoert)), type ="b", xlab = "Partition Number", ylab= "Prior Probability", main = "Stoert (Red) vs. Sadinle (Black)")
points(Sadinle)
sum(Sadinle)
sum(Stoert)                    

#This is on the way to being general, but is hard when number of "types" of partitions increases seemingly irregularly.
#General.Stoert <- c(n, rep(n*(n-1), n), 
#                  rep(n*(n-1), choose(n,2)),
#                  rep(n*(n-1)*(n-2), choose(n,2)),
#                  rep(n*(n-1)*(n-2), n*(choose((n-1),2))/2),
#                  rep(n*(n-1)*(n-2)*(n-3), choose(n,2)),
#                  .  
#                  .  
#                  .  
#                  factorial(n))/n^n
