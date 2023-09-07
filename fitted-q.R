source('mixdata.R');library(rootSolve);library(latex2exp)
t0=5
fq1=fq2=fq3=fq4=fq5=fq6=fq7=fq8=c()

for (z in 1:200) {
  set.seed(z)
  
  data=mix_data(n,t)
  X=do.call(c,data[[1]]);R=do.call(c,data[[2]]);r=R[(1:((t-1)*n))]
  x=X[(n+1):(t*n)];xprev=X[(1:((t-1)*n))]
  
  pi1=.8;pi0=1-pi1
  
  a0=do.call(c,data[[3]]);a=a0[(1:((t-1)*n))]
  Q0=Q1=0
  
  y=(r+(pi1*Q1+pi0*Q0))
  m=lm(y~xprev+a)
  
  Q1=cbind(1,x,1)%*%coef(m)
  Q0=cbind(0,x,1)%*%coef(m)
  
  for (j in 1:5) {
    y=(r+(pi1*Q1+pi0*Q0))-coef(m)[1]
    m=lm(y~xprev+a)
    
    Q1=cbind(1,x,1)%*%coef(m)
    Q0=cbind(0,x,1)%*%coef(m)
  }
  
  par=coef(m)[1]
  
  fq1[z]=(par-v)^2
  
  fq2[z]=abs(par-v)
  
  fq3[z]=crossprod(par-vi)/n
  
  fq4[z]=mean(abs(par-vi))
  
  fq5[z]=crossprod(par-vt)/t0
    
  fq6[z]=mean(abs(par-vt))
    
  fq7[z]=crossprod(par-c(vit))/(n*t0)
    
  fq8[z]=mean(abs(par-c(vit)))
}
  


save.image('fqe.RData')


