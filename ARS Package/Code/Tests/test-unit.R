test_choose_init <- function(){
  print("#######################################################################")
  print("This is a unit test for the choose_init function.")
  print("#######################################################################")
  tolerance=.Machine$double.xmax^0.3
  NInf <- -(tolerance^0.25)#negative inf
  PInf <- tolerance^0.25#positive inf

  print("Demonstrate four cases of choose_init function.")

  print("-----------------------------------------------------------------------")
  print("Case 1: dnorm; interval is unbounded")
  f1<-dnorm
  bound <- c(NInf,PInf)
  maximum <- optimize(f1,maximum = TRUE,lower=bound[1],upper=bound[2], log=TRUE)$maximum
  init1 <- choose_init(f1,bound[1],bound[2],maximum)
  print("The initial values are:")
  print(init1)
  print("Are they inside the boundary?")
  print(init1<=bound[2]&init1>=bound[1])

  print("-----------------------------------------------------------------------")
  print("Case 2: dnorm; interval is (-2,2); covers the mode")
  bound <- c(-2,2)
  maximum <- optimize(f1,maximum = TRUE,lower=bound[1],upper=bound[2], log=TRUE)$maximum
  init1 <- choose_init(f1,bound[1],bound[2],maximum)
  print("The initial values are:")
  print(init1)
  print("Are they inside the boundary?")
  print(init1<=bound[2]&init1>=bound[1])

  print("-----------------------------------------------------------------------")
  print("Case 3: dnorm; interval is (-2,-1); does not cover the mode")
  #Interval (-2,-1)doesn't cover the mode
  bound <- c(-2,-1)
  maximum <- optimize(f1,maximum = TRUE,lower=bound[1],upper=bound[2], log=TRUE)$maximum
  init1 <- choose_init(f1,bound[1],bound[2],maximum)
  print("The initial values are:")
  print(init1)
  print("Are they inside the boundary?")
  print(init1<=bound[2]&init1>=bound[1])

  print("-----------------------------------------------------------------------")
  print("Case 4: dnorm; interval is (-2,0); does not cover the mode")
  #Interval with the mode on the boundary
  bound <- c(-2,0)
  maximum <- optimize(f1,maximum = TRUE,lower=bound[1],upper=bound[2], log=TRUE)$maximum
  init1 <- choose_init(f1,bound[1],bound[2],maximum)
  print("The initial values are:")
  print(init1)
  print("Are they inside the boundary?")
  print(init1<=bound[2]&init1>=bound[1])

}


test_check_logconcave <- function(){
  print("#######################################################################")
  print("This is a unit test for the check_logconcave function.")
  print("#######################################################################")
  tolerance=.Machine$double.xmax^0.3
  NInf <- -(tolerance^0.25)#negative inf
  PInf <- tolerance^0.25#positive inf

  print("Demonstrate four cases of check_logconcave function.")
  print("-----------------------------------------------------------------------")
  print("Case 1: dnorm; interval is (-Inf, Inf)")
  f1<-dnorm
  bound <- c(NInf,PInf)
  #for logconcavity at -Inf and Inf
  maximum <- optimize(f1,maximum = TRUE,lower=bound[1],upper=bound[2], log=TRUE)$maximum
  print("Check log-concavity at infinities")
  print(check_logconcave(f1,NInf,maximum,lb=bound[1],ub=bound[2]))
  print(check_logconcave(f1,PInf,maximum,lb=bound[1],ub=bound[2]))
  print("Check log-concavity at -1")
  print(check_logconcave(f1,-1,maximum,lb=bound[1],ub=bound[2]))

  print("-----------------------------------------------------------------------")
  print("Case 2: dnorm; interval is (-2, 2); covers the mode")
  #Interval (-2,2) covers the mode
  bound <- c(-2,2)
  maximum <- optimize(f1,maximum = TRUE,lower=bound[1],upper=bound[2], log=TRUE)$maximum
  print("Check log-concavity at boundaries")
  print(check_logconcave(f1,bound[1],lb=bound[1],ub=bound[2],maximum))
  print(check_logconcave(f1,bound[2],lb=bound[1],ub=bound[2],maximum))
  print("Check log-concavity close to the mode")
  print(check_logconcave(f1,maximum-1e-04,lb=bound[1],ub=bound[2],maximum))

  print("-----------------------------------------------------------------------")
  print("Case 3: dnorm; interval is (-2, -1); does not cover the mode")
  #Interval (-2,-1)doesn't cover the mode
  bound <- c(-2,-1)
  maximum <- optimize(f1,maximum = TRUE,lower=bound[1],upper=bound[2], log=TRUE)$maximum
  print("Check log-concavity at boundaries")
  print(check_logconcave(f1,bound[1],lb=bound[1],ub=bound[2],maximum))
  print(check_logconcave(f1,bound[2],lb=bound[1],ub=bound[2],maximum))
  print("Check log-concavity close to the mode")
  print(check_logconcave(f1,maximum-1e-04,lb=bound[1],ub=bound[2],maximum))

  print("-----------------------------------------------------------------------")
  print("Case 4: f2=exp(x^2); interval is (-2,2); not log-concave")
  f2 <- function(x) exp(x^2)
  bound <- c(-2,2)
  maximum <- optimize(f2,maximum = TRUE,lower=bound[1],upper=bound[2])$maximum
  print("Check log-concavity at -1")
  print(check_logconcave(f2,-1,lb=bound[1],ub=bound[2],maximum))
}

test_update <- function() {
  print("#######################################################################")
  print("This is a unit test for the update function.")
  print("#######################################################################")
  ## Example data
  Tk <- c(-1.000000e+00 , 1.164153e-10 , 1.000000e+00)
  hx <- c(-1.4189385,-0.9189385,-1.4189385)
  hpx <- c(1.000000e+00,-1.110223e-10,-1.000000e+00)
  z <- c(-1.315539e+23,-5.000000e-01 ,5.000000e-01 ,1.315539e+23)
  upper_m <- c(1.000000e+00,-1.110223e-10,-1.000000e+00)
  upper_b <- c(-0.4189385,-0.9189385,-0.4189385)
  upper <- list("m"=upper_m, "b"=upper_b)
  lower_m <- c(0.5, -0.5)
  lower_b <- c(-0.9189385,-0.9189385)
  lower <- list("m"=lower_m, "b"=lower_b)

  print("Demonstrate three cases of update function:")
  print("-----------------------------------------------------------------------")
  ## test1
  print("Case 1: placing the element at the beginning of the vector")
  index <- 1
  print(paste("Index equals", index))
  x_new <- -1.76103
  hx_new <- log(dnorm(x_new))
  hpx_new <- nDeriv(dnorm,x_new)
  test1 <- update(index, x_new, hx_new, hpx_new, Tk, hx, hpx, z, upper, lower)
  print(paste("The new element is placed at index",which(test1$Tk == setdiff(test1$Tk, Tk))))
  if (index == which(test1$Tk == setdiff(test1$Tk, Tk)) ) {
    print("Pass - the new element is placed in the right location.")
  } else{
    print("Does not pass - the new element is not in the right location.")
  }

  ## test2
  print("-----------------------------------------------------------------------")
  print("Case 2: placing the element at the end of the vector")
  index <- 4
  print(paste("Index equals", index))
  x_new <- 1.76103
  hx_new <- log(dnorm(x_new))
  hpx_new <- nDeriv(dnorm,x_new)
  test2 <- update(index, x_new, hx_new, hpx_new, Tk, hx, hpx, z, upper, lower)
  print(paste("The new element is placed at index",which(test2$Tk == setdiff(test2$Tk, Tk))))
  if (index == which(test2$Tk == setdiff(test2$Tk, Tk)) ) {
    print("Pass - the new element is placed in the right location.")
  } else{
    print("Does not pass - the new element is not in the right location.")
  }

  ## test3
  print("-----------------------------------------------------------------------")
  print("Case 3: placing the element in the middle of the vector")
  index <- 3
  print(paste("Index equals", index))
  x_new <- 0.5
  hx_new <- log(dnorm(x_new))
  hpx_new <- nDeriv(dnorm,x_new)
  test3 <- update(index, x_new, hx_new, hpx_new, Tk, hx, hpx, z, upper, lower)
  print(paste("The new element is placed at index",which(test3$Tk == setdiff(test3$Tk, Tk))))
  if (index == which(test3$Tk == setdiff(test3$Tk, Tk)) ) {
    print("Pass - the new element is placed in the right location.")
  } else{
    print("Does not pass - the new element is not in the right location.")
  }

}


test_hulls <- function() {
  print("#######################################################################")
  print("This is a unit test for the upperhull/lowerhull/intersection functions.")
  print("#######################################################################")
  print("Please see the plot.")
  # initial points
  Tk <- c(-2, 0.5, 2)
  hx <- log(dnorm(Tk))
  hpx <- nDeriv(dnorm,Tk)
  lb <- -1e10
  ub <- 1e10
  j=length(Tk)
  z <- c(lb, unlist(sapply(1:(length(Tk)-1),FUN=function(i) intersection(Tk[i],Tk[i+1],hx[i],hx[i+1], hpx[i], hpx[i+1]) )), ub)
  upper <- upperhull(hx, hpx, Tk)
  lower <- lowerhull(Tk[1:j-1],Tk[2:j],hx[1:j-1],hx[2:j])

  # Plot
  t <- seq(-5,5,by=0.01)
  dist <- dnorm(t)
  plot(t, log(dist))
  for (i in 1:length(upper$m)) {
    abline(b=upper$m[i], a=upper$b[i], col="blue")
  }
  for (i in 1:length(lower$b)) {
    abline(b=lower$m[i], a=lower$b[i], col="red")
  }
  for (i in 1:length(z)) {
    abline(v=z[i])
  }
  legend("bottomright",c("upperhull","lowerhull","intersection"), lty=c(1,1),
         col=c("blue","red","black"))
}

suppressWarnings(try(test_choose_init(),silent=TRUE))
suppressWarnings(try(test_check_logconcave(),silent=TRUE))
test_update()
test_hulls()
