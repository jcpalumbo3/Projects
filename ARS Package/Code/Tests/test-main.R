install.packages("PtProcess")
library(PtProcess)
mynorm <- function(x,mean=0,sd=1,log=FALSE){
  #unnormalized normal density function
  if(!log){
    return (exp(-(x-mean)^2/(2*sd^2)))
  }
  else{
    return(-(x-mean)^2/(2*sd^2))
  }
}

dlaplace <- function(x,mu=0,b=1,log=FALSE){
  if(!log){
    return ((1/(2*b))*exp(-abs(x-mu)/b))
  }
  
  else{
    return (log(1/(2*b))-(abs(x-mu)/b))
  }
  
}

dsubbotin <- function(x,r=1,log=FALSE){
  #r must be >=1 to be log concave
  C <- 2*gamma(1/r)*r^(1/r-1)
  if(!log){
    return (1/C*(exp(-abs(x^r)/r)))
  }
  else {
    return (log(1/C)+(-abs(x^r)/r))
  }
  
}

compare_pdf <- function(f,s,limit=0.001,...){

  hist(s, prob=TRUE,main = paste("Comparison with the real PDF",...), font.main=4,xlab='x',breaks=50)  # prob=TRUE for probabilities not counts
  lines(density(s, adjust=2), lty="dotted")

  x<-seq(min(s),max(s),limit)
  fx <- f(x,...)
  lines(x,fx,col='red')

  legend("topright",legend = c('real pdf','sample pdf'),
         lty=c(1,2),col=c('red','black'))
}

test_main <- function(){

  print("#######################################################################")
  print("This is the main test for the ars package.")
  print("#######################################################################")

  count <- 1000

  print("Demonstrate five cases of ars function.")

  print("==========**********__________dnorm_____________**********============")
  print("Case 1: dnorm; mean=2,sd=2,lowerbound=-Inf,upperbound=Inf(the entire domain)")
  s1 <- ars(dnorm,n=count,mean=2,sd=2)
  compare_pdf(dnorm,s1,mean=2,sd=2)

  print("-----------------------------------------------------------------------")
  print("Case 2: dnorm; mean=2,sd=2,lowerbound=4,upperbound=Inf(the given interval doesn't cover the mode)")
  s1 <- ars(dnorm,n=count,mean=2,sd=2,lb=4)
  compare_pdf(dnorm,s1,mean=2,sd=2)

  print("-----------------------------------------------------------------------")
  print("Case 3: dnorm; mean=2,sd=2,lowerbound=0,upperbound=Inf(the mode is on the boundary of given interval)")
  s1 <- ars(dnorm,n=count,mean=2,sd=2,lb=0)
  compare_pdf(dnorm,s1,mean=2,sd=2)

  print("-----------------------------------------------------------------------")
  print("Case 4: dnorm; mean=2,sd=2,lowerbound=-3,upperbound=Inf(the mode is covered)")
  s1 <- ars(dnorm,n=count,mean=2,sd=2,lb=-3)
  compare_pdf(dnorm,s1,mean=2,sd=2)


  print("-----------------------------------------------------------------------")
  print("Case 5: dnorm; mean=2,sd=2,lowerbound=-3,upperbound=1(the mode is covered and both bounds are finite)")
  s1 <- ars(dnorm,n=count,mean=2,sd=2,lb=-3,ub=1)
  compare_pdf(dnorm,s1,mean=2,sd=2)

  print("==========**********__________dgamma_____________**********============")
  print("Case 1: dgamma; shape=2,lowerbound=0,upperbound=Inf(the entire domain)")
  s1 <- ars(dgamma,n=count,shape=2,lb=0)
  compare_pdf(dgamma,s1,shape=2)

  print("-----------------------------------------------------------------------")
  print("Case 2: dgamma; shape=9,scale=2, lowerbound=5,upperbound=Inf(mode not covered)")
  s1 <- ars(dgamma,n=count,shape=9,lb=5,scale=2)
  compare_pdf(dgamma,s1,shape=9,scale=2)

  #print("-----------------------------------------------------------------------")
  #print("Case 3: dgamma; shape=9, lowerbound=-5,upperbound=Inf(mode covered, lowerbound is not in the legitimate domain)")
  s1 <- ars(dgamma,n=count,shape=9,lb=-5)
  compare_pdf(dgamma,s1,shape=9)


  print("==========**********__________dbeta_____________**********============")
  print("Case 1: dbeta; shape1=2,shape2=2, lowerbound=0,upperbound=1(the entire domain)")
  s1 <- ars(dbeta,n=count,shape1=9,shape2=2,lb=0,ub=1)
  compare_pdf(dbeta,s1,shape1=9,shape2=2)

  print("-----------------------------------------------------------------------")
  print("Case 2: dbeta; shape1=2,shape2=2, lowerbound=0,upperbound=0.4(mode not covered)")
  s1 <- ars(dbeta,n=count,shape1=2,shape2=2,lb=0,ub=0.4)
  compare_pdf(dbeta,s1,shape1=2,shape2=2)

  print("==========**********__________dlogis_____________**********============")
  print("Case 1: dlogis; scale=1,lowerbound=-Inf,upperbound=Inf(the entire domain)")
  s1 <- ars(dlogis,n=count)
  compare_pdf(dlogis,s1)

  print("-----------------------------------------------------------------------")
  print("Case 2: dlogis; scale=2,lowerbound=-Inf,upperbound=Inf(the entire domain)")
  s1 <- ars(dlogis,n=count,scale=2)
  compare_pdf(dlogis,s1,scale=2)

  print("-----------------------------------------------------------------------")
  print("Case 3: dlogis; scale=1, lowerbound=2,upperbound=Inf(mode not covered)")
  s1 <- ars(dlogis,n=count,lb=2)
  compare_pdf(dlogis,s1)

  print("==========**********__________dexp_____________**********============")
  print("Case 1: dexp; rate=1,lowerbound=0,upperbound=Inf(the entire domain)")
  s1 <- ars(dexp,n=count,lb=0)
  compare_pdf(dexp,s1)

  print("Case 2: dexp; rate=2,lowerbound=0,upperbound=Inf")
  s1 <- ars(dexp,n=count,lb=0,rate=2)
  compare_pdf(dexp,s1)
  
  print("==========**********__________dchisq_____________**********============")
  print("Case 1: dchisq(df=3); lowerbound=-Inf,upperbound=Inf(the entire domain)")
  s1 <- ars(dchisq,n=count,df=3)
  compare_pdf(dchisq,s1,df=3)
  
  print("-----------------------------------------------------------------------")
  print("Case 2: dchisq(df=3); lowerbound=1,upperbound=Inf(mode not covered)")
  s1 <- ars(dchisq,n=count,df=3,lb=1)
  compare_pdf(dchisq,s1,df=3)
  
  print("==========**********__________dunif_____________**********============")
  print("Case 1: dunif(0,1),lowerbound=0,upperbound=1(the entire domain)")
  s1 <- ars(dunif,n=1000,lb=0,ub=1)
  compare_pdf(dunif,s1)
  
  print("-----------------------------------------------------------------------")
  print("Case 2: dunif(0,2),lowerbound=0,upperbound=2(the entire domain)")
  s1 <- ars(dunif,n=1000,lb=0,ub=2)
  compare_pdf(dunif,s1)
  
  print("==========**********__________dweibull_____________**********============")
  print("Case 1: dweibull,shape=1,lowerbound=0,upperbound=Inf(the entire domain)")
  s1 <- ars(dweibull,n=1000,lb=0,shape=1)
  compare_pdf(dweibull,s1,shape=1)
  
  print("-----------------------------------------------------------------------")
  print("Case 2: dweibull,shape=2,lowerbound=0,upperbound=Inf(the entire domain)")
  s1 <- ars(dweibull,n=1000,lb=0,shape=2)
  compare_pdf(dweibull,s1,shape=2)
  
  print("==========*******____test unnormalized normal density____*******============")
  print("Case 1: mynorm,mean=0,sd=1,lowerbound=-Inf,upperbound=Inf(the entire domain)")
  s1 <- ars(mynorm,n=1000)
  compare_pdf(dnorm,s1)
  
  print("PASS!")
}

test_user_defined <- function(){
  print("==========**********__________dlaplace_____________**********============")
  print("Case 1: dlaplace; lowerbound=-Inf,upperbound=Inf(the entire domain)")
  s1 <- ars(dlaplace,n=1000)
  compare_pdf(dlaplace,s1)
  
  print("==========**********__________dsubbotin_____________**********============")
  print("Case 1: dsubbotin; lowerbound=-Inf,upperbound=Inf(the entire domain)")
  s1 <- ars(dsubbotin,n=count,r=2)
  compare_pdf(dsubbotin,s1)
}

test_error1 <- function(){
  print("-----------------------------------------------------------------------")
  print("Case 1: dbeta; shape1=9,shape2=2, lowerbound=0,upperbound=Inf(the upperbound is not legitimate, should give an error)")
  s1 <- ars(dbeta,n=100,shape1=9,shape2=2,lb=0)#give an error indicating the upper and lower bounds are not correct
  compare_pdf(dbeta,s1,shape1=9,shape2=2)
}

test_error2 <- function(){
  print("==========**********__________dpareto_____________**********============")
  print("Case 2: dpareto is not log-concave, and we catch the error!")
  s1 <- ars(dpareto,a=1,lambda=1,n=100,lb=1)
}

test_error3 <- function(){
  print("==========**********__________dt_____________**********============")
  print("t-distribution(df=3) is not log-concave, and we catch the error!")
  s1 <- ars(dt,n=count,df=3)
}

test_error4 <- function(){
  print("==========**********__________df_____________**********============")
  print("f-distribution(df1=3,df2=3) is not log-concave, and we catch the error!")
  s1 <- ars(df,n=count,df1=3,df2=3,l=0)
}

test_error5 <- function(){
  print("==========**********__________dcauchy_____________**********============")
  print("Cauchy distribution is not log-concave, and we catch the error!")
  s1 <- ars(dcauchy,n=count)
}

test_error6 <- function(){
  print("==========**********__________dlnorm_____________**********============")
  print("log normal distribution is not log-concave, and we catch the error!")
  s1 <- ars(dlnorm,n=count,lb=0)
}

suppressWarnings(try(test_main(),silent=TRUE))
suppressWarnings(try(test_user_defined(),silent = TRUE))

test_error1()
test_error2()
test_error3()
test_error4()
test_error5()
test_error6()






