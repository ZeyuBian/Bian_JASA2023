library(dplyr);library(readr)
ds=read_csv("sepsis_processed_state_action.csv")
d=as.matrix(table(ds$icustayid)[table(ds$icustayid)==20])
ind=as.integer(rownames(d))
ind=ind[1:500]
ds=ds[ds$icustayid%in%ind,]

ds=arrange(ds,bloc,icustayid)

ds=ds[,c(1,4,5,6,13,59,60,62)]
ds$iv_input[ds$iv_input==1]=0
ds$iv_input[ds$iv_input==2]=0
ds$iv_input[ds$iv_input!=0]=1

rm(d);rm(ind)
t=20;n=nrow(ds)/t

## backward induction

X=ds[,c(2:5,7)]

x=as.matrix(X);R=ds$SOFA;A=ds$iv_input
pi1=(R>=11);pi0=1-pi1

r=R;a=A
t0=t
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
x[,2:5]=scale(x[,2:5])
x=cbind(x,x[,2:5] ^2)

m=lm(r~cbind(x,a,D))
par=coef(m)
par[is.na(par)]=0
Q1=cbind(1,x,1,D)%*%par
Q0=cbind(1,x,0,D)%*%par

res4=matrix(nrow=n,ncol=t0)
r=pi1*Q1+pi0*Q0
r=matrix(r,nrow = n)
res4[,1]=r[,1]

for (i in (2:t0)) {
  x=as.matrix(X[1:((t-i+1)*n),]);a=c(A[1:((t-i+1)*n)]);r=c(r[,-1])
  pi1=(r>=11);pi0=1-pi1
  
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
  m=lm(r~cbind(x,a,D))
  
  par=coef(m)
  par[is.na(par)]=0
  Q1=cbind(1,x,1,D)%*%par
  Q0=cbind(1,x,0,D)%*%par
  r=pi1*Q1+pi0*Q0
  r=matrix(r,nrow = n)
  res4[,i]=r[,1]
}

save.image('pi4.RData')
library(latex2exp)

par(mfrow=c(1,2))
boxplot(rowMeans(res1),rowMeans(res2),rowMeans(res4),ylim=c(.8,14),
        names = c(TeX('$\\pi_1$'),TeX('$\\pi_2$'),TeX('$\\pi_3$')),
        ylab='Estimated Value',xlab=TeX('$\\eta^{\\pi}_i$'))
boxplot(colMeans(res1),colMeans(res2),colMeans(res4),ylim=c(6.6,7.15),
        names = c(TeX('$\\pi_1$'),TeX('$\\pi_2$'),TeX('$\\pi_3$')),
        xlab=TeX('$\\eta^{\\pi}_t$'))


mean(res1);mean(res2);mean(res3);mean(res4)





