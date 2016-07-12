inverseImg<-function(X){
Y = X;
for (i in 1:ncol(X)){
    maxIntensity = max(X[,i]);
    Y[,i] = maxIntensity - X[,i];
}
return(Y);
}