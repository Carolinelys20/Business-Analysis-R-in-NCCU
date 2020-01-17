#1. Write a “divide” function
#a.
divide <- function(x){
  ifelse(x %% 3 == 0, "Divisible3",
         ifelse(x %% 5 == 0, "Divisible5",
                ifelse(x %% 15 == 0, "Divisible", x)))
}

#b.

divide <- c()
for(x in c(1:100)){
  divide[x] <- ifelse(x %% 3 == 0, "Divisible3",
                       ifelse(x %% 5 == 0, "Divisible5",
                              ifelse(x %% 15 == 0, "Divisible", x)))
}

divide

#2.
#a.

grades <- runif(40,1,100) + rnorm(40, 0, sqrt(2))
replace(grades, grades >100 || grades < 0, runif(40,1,100) + rnorm(40, 0, sqrt(2)))
grades

#b.
confidence_interval <- function(x){
  n <- length(x)
  m <- mean(x)
  std <- sd(x)
  r1 <- qnorm(0.025)
  r2 <- qnorm(0.975)
  low_spot <- m - (r2 * std / sqrt(n))
  up_spot <- m - (r1 * std / sqrt(n))
  return(c(low = low_spot, up = up_spot, mean = m))
  }

confidence_interval(grades)

#3.
#ℓ(θ)
theta <- as.factor(theta)

Ltheta <- function(theta){
  j = 0
  for (i in 1:length(x)){
    j <- j + log(1 + (theta - i)**2)
  }
  return(-length(x)*log(pi)-j)
  }

#ℓ′(θ) 
Lltheta <- function(theta){
  j = 0
  for (i in 1:length(x)){
    j <- j + (theta - i) / (1+(theta - i)**2)
  }
  return(-2*j)
  }



