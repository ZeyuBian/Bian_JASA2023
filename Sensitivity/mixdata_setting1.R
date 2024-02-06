
## Use this to generate the data!!!

mix_data=function(n,t){
  
  iind=rep(c(T,F),n/2)
  X1=rnorm(n)
  theta=sin(1*(1:n))
  ## behavior policy
  pi1=.5
  a1=rbinom(n,1,pi1)
  
  ## reward function
  r1=2*X1+3*a1+theta+cos(1*3)
  
  X=r=a=list();X[[1]]=X1; r[[1]]=r1;a[[1]]=a1
  
  ## from time 2 to time t: loop
  
  for (j in 2:t) {
    Xprev=X[[j-1]];aprev=a[[j-1]]

    ## transition probability
    tran=-.25*Xprev+aprev
    
    Xnext=rep(0,n)
    
    if (j%%2==1) {
      Xnext[iind]=rnorm(n/2,tran)} else{
        Xnext[iind]=rnorm(n/2,tran,sd = 0.5)
      }
    
    if (j%%2==1) {
      Xnext[!iind]=rnorm(n/2,-tran)} else{
        Xnext[!iind]=rnorm(n/2,-tran,sd = 0.5)
      }

      
    anext=rbinom(n,1,pi1)
    rnext=2*Xnext+3*anext+theta+cos(3*j)
      
    X[[j]]=Xnext;r[[j]]=rnext;a[[j]]=anext}
  
  list(X,r,a)
}




