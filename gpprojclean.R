n<-4000
p<-800

X= matrix(rnorm(n*p,mean=0,sd=1/sqrt(n)), n, p)

betas1<-rep(10, 100)
betas1<- c(betas1,rep(-10, 100))
betas1<- c(betas1,rep(0, 600))

sim_logistic_data= function(sample_size,betvec)
{
  eta =X%*% betvec
  p = 1 / (1 + exp(-eta))
  y = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(y, X)
}

example_data1 = sim_logistic_data(sample_size =n,betvec=betas1)

head(example_data1)
fit1_glm = glm(y ~ ., data =example_data1 , family = binomial) 


plot(coefficients(fit1_glm),cex=0.2, col="blue")
points(betas1, cex=0.4)


betas2= matrix(rnorm(p,mean=3,sd=4), p, 1)

example_data2 = sim_logistic_data(sample_size =n,betvec=betas2)

fit2_glm = glm(y ~ ., data =example_data2, family = binomial) 
betas_with_0=c(0,betas2) 


fit_lm =lm(coefficients(fit2_glm)~betas_with_0)
linfit=as.numeric(coefficients(fit_lm))

plot(betas_with_0,coefficients(fit2_glm), cex=0.2)
lines(betas_with_0,betas_with_0, col="black")
lines(betas_with_0,linfit[1]+linfit[2]%*%betas_with_0, col="red")

eta =X%*% betas2
probs = 1 / (1 + exp(-eta))
true.data <- data.frame(tprob=probs, y=example_data2$y)
true.data$fits <-fit2_glm$fitted.values
true.data <- true.data[order(true.data$tprob, decreasing=FALSE),]
true.data$rank <-1:nrow(true.data)
plot(true.data$tprob, cex=0.4)
points(true.data$fits,cex=0.2, col="blue")


