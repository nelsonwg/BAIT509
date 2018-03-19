library(tidyverse)

#1
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

dat <- genreg(1000)

#2
dat <- mutate(dat, 
       yhat = 5,
       yhat1 = 5-x1,
       yhat2 = 5+2*x2,
       yhat12 = 5-x1+2*x2)

(mse <- mean((dat$yhat - dat$y)^2))
(mse1 <- mean((dat$yhat1 - dat$y)^2))
(mse2 <- mean((dat$yhat2 - dat$y)^2))
(mse12 <- mean((dat$yhat12 - dat$y)^2))

#Oracle Classification
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-0.2-x)))
  tibble(x=x, y=y)
}
dat2 <- gencla(1000)

0.8/(1+exp(-1))
1-0.2-0.8/(1+exp(-1))

dat2 <- mutate(dat2, 
               ProbA = 0.2,
               ProbB = 0.8/(1+exp(-dat2$x)),
               ProbC = 1 - 0.2 - 0.8/(1+exp(-dat2$x)))

ggplot2(dat2$ProbB ~ dat2$x)
plot(dat2$ProbC ~ dat2$x)

dat2 <- mutate(dat2, 
               yhat = ifelse(dat2$ProbB>=dat2$ProbC, "B", "C"))
1- mean(dat2$yhat == dat2$y)
