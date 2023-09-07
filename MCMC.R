source('target.R')
n=80;t=20;t0=5
set.seed(24)
value=matrix(0,nrow = n,ncol = t)
for (e in 1:400) {
  v=target(n,t)
  v=do.call(cbind,v)
  value=value+v
}

vit=value[,(1:t0)]/(400)
vt=colMeans(value)[1:t0]/400 
vi=rowMeans(value)/400
v=mean(value)/400

