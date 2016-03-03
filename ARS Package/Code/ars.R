choose_init <- function(f,lb,ub,maximum,tolerance=.Machine$double.eps^0.25,default = 0.1,...){
  #Chooses starting points: mode, one value to left, one value to right 
  #f is density, lb is lower bound, ub is upper bound, default is value used 
  #to choose left and right points
  
  if(maximum>1e10|maximum<(-1e10)){
    stop("The mode for the density within the given interval is not valid!")
  }
  left <- maximum - min(default,maximum-lb)
  right <- maximum + min(default,ub-maximum)
  
  init <- c(left,maximum,right)
  init  <- sort(init)
  init <- unique(init)
  
  if((left==lb)&(right!=ub)){
    left <- left+tolerance  
  }
  
  if((right==ub)&(left!=lb)){
    right <- right-tolerance
  }
  
  if((right==ub)&(left==lb)){
    left <- left+tolerance
    right <- right-tolerance
  }
  
  init <- c(left,maximum,right)
  init  <- sort(init)
  init <- unique(init)
  if ((!check_logconcave(f,left,maximum,lb,ub,...))|(!check_logconcave(f,right,maximum,lb,ub,...))){
    stop("The function is not log-concave!")
  }
  
  return (init)
}

check_logconcave <- function(f,x,maximum,lb,ub,tolerance=.Machine$double.eps^0.25,...){
  #Checks to ensure point achieves log concavity
  #If nDeriv returns an NaN value, return TRUE or raise warning indicating we 
  #cannot check log concavity 
  if(x<lb|x>ub){
    warnings("The point you want to check logconcavity is not within the given interval")
  }
  
  if(is.nan(nDeriv(f,x,...))|(is.nan(nDDeriv2(f,x,...)))){
    warning("Not able to check logconcavity at x because f'(x) or f''(x) is not available")
    return (TRUE)
  }
  
  if(abs(x-maximum)<tolerance){
    warning("Not able to check logconcavity at x because x is too close to the maximum of f")
    return (TRUE)
  }
  
  if(abs(x-lb)<tolerance){
    warning("Not able to check logconcavity at x because x is too close to the left boundary")
    return (TRUE)    
  }
  
  if(abs(x-ub)<tolerance){
    warning("Not able to check logconcavity at x because x is too close to the right boundary")
    return (TRUE)    
  }
  
  if(nDDeriv2(f,x,...)<=tolerance*10){
    return (TRUE)
  }
#  else if(x==maximum){
 #   return (TRUE)
  #}
  else{
    print(x)
    stop("The density function is not log-concave at the point shown above!")
  }
  
}

nDeriv <- function(f,x,e = .Machine$double.eps,...) {
  #Calculates the first numerical derivative 
  x <- as.numeric(x)
  tolerance <- 5e-6
  fp <- (log(f(x+tolerance,...)) - log(f(x-tolerance,...)))/(2*tolerance) # central difference
  return (fp)	
}

nDDeriv2 <- function(f,x,...){
  #Calculates the second numerical derivative 
  x<-as.numeric(x)
  tolerance <- 5e-6
  if (abs(tolerance/x) > 1e-8){
  fpp <- (log(f(x+tolerance,...))-2*log(f(x,...))+log(f(x-tolerance,...)))/(tolerance^2)
  return (fpp)}
  else{
    return (NaN)
  }
}



intersection <- function(x0, x1, hx0, hx1, hpx0, hpx1){
  #Calculates the intersection point between two line segments 
  #hx0 is the log f value of current x; hx1 is that of next x
  #hpx0 is the dlog f value of current x; hpx1 is that of next x
  if (hpx0 == hpx1) {
    return((x0 + x1)/2)
  } else {
    return((hx1 - hx0 - x1*hpx1 + x0*hpx0) / (hpx0 - hpx1))
  }
}

upperhull <- function(hx0, hpx0, x0){
  #Calculates slope/intercept for upperhull in standard linear equation: y=mx+b
  m <- hpx0
  b <- hx0 - hpx0*x0 
  return(list("m"=m,"b"=b))
}

lowerhull <- function(x0, x1, hx0, hx1) {
  #Calculates slope/intercept for lowerhull in standard linear equation: y=mx+b
  m <- (hx1-hx0) / (x1-x0)
  b <- (hx0*x1 - hx1*x0) / (x1-x0)
  return(list("m"=m, "b"=b))
}

expInteg <- function(m, b, z0, z1) {
  #Calculates integral of an exponential of a line segment 
  if (abs(m) < 1e-10) {
    return(exp(b)*(z1-z0))
  } else if ((exp(b)==Inf | exp(b)==-Inf) & ((exp(m*z1)-exp(m*z0))==0)) {
    return(0)
  } else {
  return(exp(b)/m*(exp(m*z1)-exp(m*z0)))
  }
}

sampleX <- function(m,b,z) {
  #Calculates a potential sample value x 
  
  #Integrates over each of the segment sections 
  s <- unlist(sapply(1:(length(z)-1),FUN=function(i) expInteg(m[i],b[i],z[i],z[i+1])))
  #Sums each area; divides each segment area by total to normalize
  total <- sum(s)
  s <- s/total
  #Random sample from a uniform[0,1]
  U <- runif(1)
  #Calculates the cdf using cumsum function 
  cdf <- cumsum(s)
  #Selects segment to sample from by determining where U would fall
  for(i in 1:length(s)) {
    if(U < cdf[i]) {break}
  }
  #Solves for x using inverse cdf method 
  if(i==1) cdf <- 0 else cdf <- cdf[i-1]
  if(m[i]==0){
    x <- (total*(U-cdf)/exp(b[i]))+z[i]
  }
  else{
    x <- log((m[i]*total*(U-cdf)/exp(b[i]))+exp(m[i]*z[i]))/m[i]
  }
  return(x)
}

update <- function(index, x_new, hx_new, hpx_new, Tk, hx, hpx, z, upper, lower) {
  #Updates vectors when new sample value is added
  uh <- upperhull(hx_new, hpx_new, x_new)
  last <- length(Tk)
  #Update depends on whether point is added to beginning, middle, or end
  if (index == 1) {
    z1 <- intersection(x_new, Tk[1], hx_new, hx[1], hpx_new, hpx[1])	
    z <- c(z[1], z1, z[2:length(z)])
    lh <- lowerhull(x_new, Tk[1], hx_new, hx[1])
    lower$m <- c(lh$m ,lower$m)
    lower$b <- c(lh$b, lower$b)
    Tk <- c(x_new, Tk)
    upper$m <- c(uh$m, upper$m)
    upper$b <- c(uh$b, upper$b)
    hx <- c(hx_new, hx)
    hpx <- c(hpx_new, hpx)
  } else if (index == last+1) {
    z0 <- intersection(Tk[last], x_new, hx[last], hx_new, hpx[last], hpx_new)	
    z <- c(z[1:length(z)-1],z0,z[length(z)])
    lh <- lowerhull(Tk[last], x_new, hx[last], hx_new)
    lower$m <- c(lower$m, lh$m)
    lower$b <- c(lower$b, lh$b)
    Tk <- c(Tk, x_new)
    upper$m <- c(upper$m, uh$m)
    upper$b <- c(upper$b, uh$b)
    hx <- c(hx, hx_new)
    hpx <- c(hpx, hpx_new)
  } else {
    z0 <- intersection(Tk[index-1], x_new, hx[index-1], hx_new, hpx[index-1], hpx_new)
    z1 <- intersection(x_new, Tk[index], hx_new, hx[index], hpx_new, hpx[index])
    z <- c(z[1:index-1], z0, z1, z[(index+1):length(z)])
    lh0 <- lowerhull(Tk[index-1], x_new, hx[index-1], hx_new)
    lh1 <- lowerhull(x_new, Tk[index], hx_new, hx[index])
    if (index == 2) {
      lower$m <- c(lh0$m, lh1$m, lower$m[index:(last-1)])
      lower$b <- c(lh0$b, lh1$b, lower$b[index:(last-1)])
    } else if (index == last) {
      lower$m <- c(lower$m[1:(index-2)], lh0$m, lh1$m)
      lower$b <- c(lower$b[1:(index-2)], lh0$b, lh1$b)
    } else {
      lower$m <- c(lower$m[1:(index-2)], lh0$m, lh1$m, lower$m[index:(last-1)])
      lower$b <- c(lower$b[1:(index-2)], lh0$b, lh1$b, lower$b[index:(last-1)])
    }
    Tk <- c(Tk[1:index-1], x_new, Tk[index:last])
    upper$m <- c(upper$m[1:index-1], uh$m, upper$m[index:last])
    upper$b <- c(upper$b[1:index-1], uh$b, upper$b[index:last])
    hx <- c(hx[1:index-1], hx_new, hx[index:last])
    hpx <- c(hpx[1:index-1], hpx_new, hpx[index:last])
  }
  return(list("Tk"=Tk, "z"=z, "hx"=hx, "hpx"=hpx, "upper"=upper, "lower"=lower))
}

bndeval <- function(mode, xs, index, m, b, min=-Inf){
  #Evaluates upperhull and lowerhull at sample value x 
  val = m[index-1]*xs+b[index-1] 
  #Sets lower bound to be zero if sample lies outside current samples
  if (mode == "lower" & (index == 1 || index == (length(m)+2) ) ){
      val <- min  
  }
  return(val)
}

srtest <- function(xs,l,u,f,...){
  #Performs squeeze and rejection test to determine if sample value is accepted
  w <- runif(1)
  stest <- FALSE
  rtest <- FALSE
  #Squeeze test
  st <- exp(l-u)
  if(st >= w){
    #True/False;add new sample point
    return(list("stest"=!stest,"rtest"=rtest)) 
  }
  #Rejection test
  else if(exp(log(f(xs,...))-u) >= w){   
    #False/True; add new sample point/abscissa
    return(list("stest"=stest,"rtest"=!rtest)) 
  } 
  else{
    #False/False; add new abscissa
    return(list("stest"=stest,"rtest"=rtest)) 
  }
  
}

## main ##
#' Adaptive Rejection Sampling from any log-concave probability density function. As sampling proceeds, the method updates 
#' a rejection envelope and squeezing function which converge to the true density function. This method is especially useful in 
#' situations where the density function is computationally expensive to evaluate. 
#' 
#' @param f  Log-concave probability density function
#' @param lowerbound  Leftmost point of the domain 
#' @param upperbound  Rightmost point of the domain 
#' @param n  Sample size 
#' @param tolerance  Re-evaluates lowerbound and upperbound if too large for calculations 
#' @param ...  Optional arguments for use in specifying parameters of input distribution 
#' 
#' @return A random sample from the probability density function specified. 
#' 
#' @authors Pi-Feng Chiu, Juanyan Li, Jamie Palumbo, Weiyan Shi 
#' @references Gilks, W.R., P. Wild. (1992) Adaptive Rejection Sampling for Gibbs Sampling, Applied Statistics 41:337???348.
#' 
#' @examples
ars <- function(f,lb=-Inf,ub=Inf,n,tolerance=.Machine$double.xmax^0.3,...) {
  #Sampling method for log-concave probability density functions 
  
  #Check validity of inputs
  if(!is.numeric(c(lb,ub,n,tolerance,...))) stop("All inputs must be numeric.")
  if(lb>=ub) stop("Lowerbound must be less than upperbound.")  
  if(n<=0) stop("Sample size n must be a positive number.")
  if(tolerance<0 || tolerance>.Machine$double.xmax) stop("Tolerance must be 
                                            between zero and machine epsilon.")  
  if(integrate(f,lb,ub,...)$value==Inf) stop("Function f must have finite integration.")
  
  #Initialization step: choose starting points; calculate envelope and squeeze
  if(lb < (-tolerance)){
    lb <- -(tolerance^0.25)
  }
  if(ub > (tolerance)){
    ub <- tolerance^0.25
  }
  maximum <- optimize(f,maximum = TRUE,lower=lb,upper=ub, log=TRUE,...)$maximum
  #Checks whether given user interval is within a legitimate domain
  if(maximum > tolerance^0.2){
    stop("not able to compute the mode! Please give the legitimate upper and 
         lower boundary for the density")
  }
  Tk <- choose_init(f,lb,ub,maximum,...)
  print("The inital values are:")
  print(Tk)
  #Tk <- c(0.49,0.5,0.51)
  hx <- log(f(Tk,...)) 
  hpx <- nDeriv(f,Tk,...)
  j=length(Tk)
  z <- c(lb, unlist(sapply(1:(length(Tk)-1),FUN=function(i) intersection(Tk[i],Tk[i+1],hx[i],hx[i+1], hpx[i], hpx[i+1]) )), ub)
 # z <- c(lb, intersection(Tk[1:j-1],Tk[2:j],hx[1:j-1],hx[2:j],hpx[1:j-1],hpx[2:j]), ub)
  upper <- upperhull(hx, hpx, Tk)
  lower <- lowerhull(Tk[1:j-1],Tk[2:j],hx[1:j-1],hx[2:j])
  
  #Sampling/updating step 
  x <- c() 
  count=0
  while(count<n){
    #Squeeze and rejection test 
    xs <- sampleX(upper$m,upper$b,z)
    if (f(xs,...)!=0) {
       z <- sort(z)
		 lInd <- findInterval(xs,Tk)+1
		 uInd <- findInterval(xs,z)+1
		 min <- log(f(lb,...))
		 u <- bndeval("upper",xs, uInd, upper$m, upper$b)
		 l <- bndeval("lower",xs, lInd, lower$m, lower$b, min)
		 test <- srtest(xs, l, u, f, ...)
		 if(test$stest & check_logconcave(f,xs,maximum,lb,ub,...)){  #xs is accepted; no need to be added to Tk
			count <- count + 1
			x <- c(x, xs)
		 } else {			#Otherwise, added to Tk; run rejection test
			hx_new <- log(f(xs,...))
			hpx_new <- nDeriv(f,xs,...)
			update_list <- update(lInd, xs, hx_new, hpx_new, Tk, hx, hpx, z, upper, lower) 
			Tk <- update_list$Tk
			z <- update_list$z
			hx <- update_list$hx
			hpx <- update_list$hpx
			upper <- update_list$upper
			lower <- update_list$lower
      }
      if(test$rtest & check_logconcave(f,xs,maximum,lb,ub,...)) {  
        count <- count + 1
        x <- c(x, xs)
      }
    }
  }
  return(x)
}

