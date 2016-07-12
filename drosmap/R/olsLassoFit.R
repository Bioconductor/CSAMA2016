olsLassoFit <- function(X,D,alpha,intercept = TRUE, method = 'nnls'){
n = dim(X)[2];
if (intercept){
   alphaOLS = rbind(alpha,rep(1,dim(alpha)[2]));
   Dnew = cbind(D,rep(1,dim(X)[1]));
}else{
   alphaOLS = alpha;
   Dnew = D;
}		

rSq = rep(0,n);
SSE = rep(0,n);

for (i in 1:n){

    res = olsLassoFit0(X[,i],Dnew,alphaOLS[,i],method = method);
    
    alphaOLS[,i] = res$alphaOLS;
    rSq[i] = res$r.squared;
    SSE[i] = res$SSE;

}

return(list(alphaOLS = alphaOLS,rSq = rSq, D = Dnew, SSE = SSE));
}

olsLassoFit0<-function(x,D,a,method){
nonZeroInd = which(abs(a)>1e-6);
if (length(nonZeroInd)==0){
   alphaOLS = a*0;
   r.squared = 0;
   SSE = sum((x - mean(x))^2);
   
   return(list(alphaOLS=alphaOLS,r.squared = r.squared, SSE = SSE));
}

Dnew = D[,nonZeroInd];

if (method == 'nnls' && length(nonZeroInd) > 1){
   
   nnls.res = nnls(A = Dnew, b = x);
   coef = nnls.res$x;
   SST = sum((x - mean(x))^2);
   SSE = sum(nnls.res$residuals^2);
   r.squared = 1 - SSE/SST;
}else{
   lm.res = lm(x~Dnew-1);
   s = summary(lm.res);
   r.squared = s$r.squared;
   coef = lm.res$coefficients;
   SSE = sum(s$residuals^2);
}
alphaOLS = a;
alphaOLS[nonZeroInd] =coef;
return(list(alphaOLS=alphaOLS,r.squared = r.squared,SSE = SSE));
}