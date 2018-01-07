LWS <- function(y, x, w, niter){
    
    n <- nrow(x)
    p <- ncol(x)
    
    
   rWr_LWS=Inf;
   # iterations = rep(0,times=n);
   # Outer Cycle
   for(i in 1:niter) {
   # Performing one iteration in the outer cycle
   #    [b_OneIter,S_OneIter,rWr_OneIter,perm_OneIter,iterations_OneIter]=OneIter(X,Z,Y,w);
   OneIter = LWS_OneIter(y,x,w)  
   
    if (rWr_LWS > OneIter$rWr){
    b_LWS      =  OneIter$b
    rWr_LWS    =  OneIter$rWr
    perm_LWS   =  OneIter$perm
    }
   }
    
    result <- list(beta=b_LWS, resid = rWr_LWS, rank=perm_LWS)
    return(result)
}

LWS_OneIter = function(y, x, w){

        n <- nrow(x)
    p <- ncol(x)
    W <- diag(w);
    tau         <- 0.00001;
    rWr_OneIter <- Inf;
    b_OneIter   <- matrix( rep(0,times=p),p,1);

    # Select the initial values
    index  = sample(n)
    SX     = x[(index[1:p+2]),]
    SY     = y[index[1:p+2]]
    while(kappa(SX) > 1e9 ){
        index  = sample(n)
        SX     = x[(index[1:(p+5)]),]
        SY     = y[index[1:(p+5)]]
    }
    #print(kappa(SX))
    b      = OLS_qr(SY,SX) 

    # Run the inner cycle
    while (sum(abs(b_OneIter - b) < tau) != p){
          #  rWr_OneIter > rWr && sum(abs(b_OneIter - b) < tau) ~= p;
          b_OneIter     = b
        
          r      = (Y-X%*%b)^2
          rI     = order(r)
          sX     = X[rI,]
          sY     = Y[rI,]  
          
          swX    = (sqrt(w)*sX)[which(w != 0),]
          swY    = (sqrt(w)*sY)[which(w != 0)]
          b      = OLS_qr(swY,swX) 
    }
    b_OneIter      = b
    r_OneIter      = (Y-X%*%b_OneIter)^2
    perm_OneIter   = order(r_OneIter)
    rWr_OneIter    = (r_OneIter[perm_OneIter]^2)%*%w   
    # S      = t((sY-sX%*%b)^2)%*%w
    result_OneIter <- list(b=b_OneIter, rWr = rWr_OneIter, perm=perm_OneIter)
    return(result_OneIter)
} 


OLS_qr = function(y, x, tol=1.e-07 ){
    qr_x <- qr(x, tol=tol, LAPACK=TRUE)
    b<-qr.coef(qr_x, y)
    return(b)
    
} 

OLS_lm <- function (y, x){
    b<-coef(lm(y ~ x - 1))
    return(b)
} 

OLS_def <- function (y, x) {
    b <- as.vector((solve(t(x)%*%x))%*%(t(x)%*%y))
    return(b)
} 



Weights1 <- function(n){
    # Generating weights
    # h      - number of trimmed observations (observations with zero weights)
    # g      - number of observations with weights equal to 1
    # h-g-1  - number of observations with rapidly decreasing weights
h <- ceiling(0.9*n)
g <- ceiling(0.8*n)
w <- rep(0,times=n)
w[1:g] <- 1
w[g:h] <- seq(1,0,length=h-g+1)
return(w)
}


