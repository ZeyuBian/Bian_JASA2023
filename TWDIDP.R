source('mixdata.R');library(latex2exp)
t0=5

TW1=TW2=TW3=TW4=TW5=TW6=TW7=TW8=c()

for (z in 1:500) {
  set.seed(z)
  data=mix_data(n,t)
  
  ## backward induction
  X=do.call(cbind,data[[1]]);R=do.call(cbind,data[[2]])
  
  pi1=.8;pi0=1-pi1
  
  A=do.call(cbind,data[[3]])
  x=c(X);r=c(R);a=c(A)
  H=I=diag(n)
  for (k in 1:(t-1)) {
    H=rbind(H,I)
  }
  
  G=L=matrix(0,nrow = n,ncol = t);G[,1]=1
  
  for (k in 2:(t)) {
    M=L
    M[,k]=1
    G=rbind(G,M)
  }
  H=H[,-1];G=G[,-1];D=cbind(H,G)
  
  m=lm(r~cbind(x,x^2,a,D))
  par=coef(m)
  
  Q1=cbind(1,x,x^2,1,D)%*%par
  Q0=cbind(1,x,x^2,0,D)%*%par
  
  res=matrix(nrow=n,ncol=t0)
  r=pi1*Q1+pi0*Q0
  r=matrix(r,nrow = n)
  res[,1]=r[,1]
  
  for (i in (2:t0)) {
    x=c(X[,1:(t-i+1)]);a=c(A[,1:(t-i+1)]);r=c(r[,-1])
    
    H=I=diag(n)
    for (k in 1:(t-i)) {
      if (i==t){
        break
      }
      H=rbind(H,I)
    }
    
    G=L=matrix(0,nrow = n,ncol = t-i+1);G[,1]=1
    
    for (k in 2:(t-i+1)) {
      if (i==t){
        break
      }
      M=L
      M[,k]=1
      G=rbind(G,M)
    }
    
    H=H[,-1];G=G[,-1]
    D=cbind(H,G)
    if (i==t0) {
      D=H
    }
    m=lm(r~cbind(x,x^2,a,D))
    
    par=coef(m)
    par[is.na(par)]=0
    Q1=cbind(1,x,x^2,1,D)%*%par
    Q0=cbind(1,x,x^2,0,D)%*%par
    r=pi1*Q1+pi0*Q0
    r=matrix(r,nrow = n)
    res[,i]=r[,1]
  }
  
  TW1[z]=(mean(res)-v)^2
  
  TW2[z]=abs(mean(res)-v)
  
  TW3[z]=crossprod(rowMeans(res)-vi)/n
  
  TW4[z]=mean(abs(rowMeans(res)-vi))
  
  TW5[z]=crossprod(colMeans(res)-vt)/t0
  
  TW6[z]=mean(abs(colMeans(res)-vt))
  
  TW7[z]=sum((res-vit)^2)/(n*t0)
  
  TW8[z]=sum(abs(res-vit))/(n*t0)
  
}


boxplot(TW1,TW3, TW5,TW7,ylab='MSE',
        xlab=TeX('$\\eta^{\\pi}$'))




