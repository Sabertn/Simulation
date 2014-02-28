# function to generate standard Normal using the Box-Muller transformations
simnormbm <- function(n.gen){
  urandom1 <- runif(n.gen)
  urandom2 <- runif(n.gen)
  Rsqua <- -2*log(urandom1)
  theta <- 2*pi*urandom2
  x <- sqrt(Rsq)*cos(theta)
  y <- sqrt(Rsq)*sin(theta)
  # combine independent x and y
  z <- (x+y)/sqrt(2)
  # output
  z
}

out1 <- simnormbm(2000)
out1[1:20]
summary(out1)
sd(out1)
hist(out1,br=40,xlab="",ylab="",main="2,000 simulations from Normal using Box-Muller",freq=FALSE)
curve(dnorm(x),from=-4,to=4,col="blue",add=TRUE)
