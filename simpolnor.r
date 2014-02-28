# function to generate standard normal using the Polar method
simnormpolar <- function(n.gen){
  sim.vector <- rep(0,n.gen)
  for (i in 1:n.gen){
    urandom <- runif(2)
    v1 <- 2*urandom[1]-1; v2 <- 2*urandom[2]-1
    s <- v1^2 + v2^2
    while(s > 1){
      urandom <- runif(2)
      v1 <- 2*urandom[1]-1; v2 <- 2*urandom[2]-1
      s <- v1^2 + v2^2
    }
    insq <- -2*log(s)/s
    x <- sqrt(insq)*v1
    y <- sqrt(insq)*v2
    sim.vector[i] <- (x+y)/sqrt(2)
  }
  # output
  sim.vector
}
out1 <- simnormpolar(2000)
out1[1:20]
summary(out1)
sd(out1)
hist(out1,br=25,xlab="",ylab="",main="2,000 simulations from Normal using polar method",freq=FALSE)
curve(dnorm(x),from=-4,to=4,col="blue",add=TRUE)
