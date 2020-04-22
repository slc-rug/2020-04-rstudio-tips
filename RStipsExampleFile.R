library(TeachingDemos)
library(fortunes)
library(R6)

## R6 Progress Bar ----

ProgBar <- R6Class("ProgBar",
                   public = list(
                     initialize = function(max,...) {
                       private$pb <- winProgressBar(max=max)
                     },
                     inc = function(amount=1) {
                       private$i <- private$i + amount
                       setWinProgressBar(private$pb, value=private$i)
                       invisible(self)
                     },
                     reset = function(value=0) {
                       private$i <- value
                       setWinProgressBar(private$pb, value=private$i)
                       invisible(self)
                     },
                     val=function() {
                       private$i
                     }
                   ),
                   private = list(
                     pb = NULL,
                     i = 0,
                     finalize = function(...) {
                       close(private$pb)
                     }
                   )
)

## Simulation Function Definition ----

# This function simulates dataset for logistic regression
# and analyzes it.

simfun1 <- function(x, n=100, beta=rep(0,ncol(x)), intercept=TRUE) {
  if( missing(x) ) { # Simulate x matrix
    x <- cbind(1, round(rnorm(n, 100, 10)))
  } else { # x matrix passed in
    n <- nrow(x)
    if( !any(apply(x,2,function(x){all(x==1)})) & intercept) { # add intercept
      x <- cbind(1, x)
    }
  }
  eta <- x %*% beta
  p <- binomial()$linkinv(eta)
  y <- rbinom(n=n, 1, p)
  mydat <- as.data.frame(cbind(y=y, x))
  summary(glm(y~.-1, data=mydat, family=binomial))
}


## Run Simulations under beta=0 ----

pb <- ProgBar$new(1000)

pb$reset()
out <- replicate(1000, {
  pb$inc()
  simfun1(n=250)
}, simplify = FALSE)

rm(pb)
gc()

hist(sapply(out, function(x) x$coefficients[2,4]))

## Run Simulations under beta=c(-5, 0.1) ----

pb <- ProgBar$new(1000)

pb$reset()
out2 <- replicate(1000, {
  pb$inc()
  simfun1(n=750, beta=c(-5, 0.1))
}, simplify = FALSE)

rm(pb)
gc()

pvals <- sapply(out2, function(x) x$coefficients[2,4])

hist(pvals)
mean(pvals <= 0.05)

## other ----

y <- 1:10

tmpfun <- function(x) {
  y <- any(x <= 0)
  if(y) {
    y <- abs(x)
  } else {
    y <- x
  }
  y2 <- "hello world"
  c(mean(log(y)), exp(mean(log(y))), 
    mean(y), 1/mean(1/y))
}

tmpfun(y)

## editing output for cut and paste ----

x <- runif(5, 0, 100)
x.ll <- x - runif(5, 0.1, 0.5)
x.ul <- x + runif(5, 0.2, 0.7)

cbind(mean=x, ll=x.ll, ul=x.ul)

# ran above, then copy/pasted to below

"
         mean       ll       ul
[1,] 28.71979 28.41000 28.94773
[2,] 36.52692 36.14478 37.17437
[3,] 78.41768 78.01877 78.84273
[4,] 66.93552 66.82571 67.60814
[5,] 33.67354 33.45449 34.05712

"


"

         mean   95% C.I.
Group 1 40.20 (39.78-40.48)
Group 2 85.48 (85.36-86.06)
Group 3 76.31 (76.01-76.79)
Group 4 78.82 (78.54-79.08)
Group 5 21.59 (21.24-21.86)

"

## extract ----

x <- c(1:10, 50)

exp(mean(log(x)))

gmean <- function(x) {
  logx <- log(x)
  mlx <- mean(logx)
  exp(mlx)
}






