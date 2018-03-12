n <- 100
bootGLM <- function(x, y, B=1000){
  t<- dim(x)[2]+1
  fit = glm(y ~ x, family = binomial(link = "logit"))
  coe <- coef(fit)
  coe1 <- matrix(rep(0, B*t), B, t)
  for(i in 1: B){
    numb <- sample(c(1:n), size=n, replace=T)
    yy <- y[numb]
    xx <- x[numb,]
    fit1 = glm(yy ~ xx, family = binomial(link = "logit"))
    coe1[i,] <- coef(fit1)
  }
  pvalue <- rep(0, t)
  for(j in 1:t){
    pvalue[j] <- (sum(abs(coe1[, j]-mean(coe1[, j]))>abs(coe[j]))+1)/(B+1)
  }
  return(pvalue)
}
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x <- cbind(x1,x2,x3)
beta <- c(1,2,3)
p <- 1/(1+exp(-x%*%beta))
y <- rbinom(n,1,p)
##P-values returned by the summary function##
fit1 = glm(y ~ x, family = binomial(link = "logit"))
pp <- summary(fit1)$coefficients[,4]
ppvalue <- bootGLM(x, y, B=1000)

##In our example, beta0 is zero, and beta1, beta2, and beta3 are all nonzero, 
## so in the summary fuction, the P-value for beta0 is not small, and the P-values 
##for beta1, beta2, and beta3 are all very small, such as 10^(-05). However by 
##bootstrap, since B=1000, the P-values for beta1, beta2, and beta3 are about 
##10^(-2), and the P-value for beta0 is not small.

##Thus for beta0, the p-value of bootstrap method is comparable to that of summary 
##function, though there are some differences.

##For beta1, beta2, and beta3, the p-values of bootstrap method are
##much larger than those of summary function. So the p-values of bootstrap method
##are not comparable to those of the summary function.

