library(tidyverse)
library(knitr)
set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

dat$d <- abs(dat$x - 0)

#kNN
dat2 <- arrange(dat,d)[1:5, ]
mean(dat2$y)

#Loess
dat3 <- filter(dat, d <= 1)
mean(dat3$y)

scatter.smooth(dat$y ~ dat$x)

xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x){
  dat$d <- abs(dat$x - x)
  dat2 <- arrange(dat,d)[1:5, ]
  mean(dat2$y)
})

loess_estimates <- map_dbl(xgrid, function(x){
  dat$d <- abs(dat$x - x)
  dat3 <- filter(dat, d < 1)
  mean(dat3$y)
})

est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y)) +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()