patternCentroid<-function(D,width=32,height=16,template){
K = dim(as.matrix(D))[2];
ind = which(template[,,1]==1);

Dtemp = matrix(0,nrow = width*height,ncol = K);
Dtemp[ind,] = D;
vertLoc = horiLoc = rep(0,K);
for (i in 1:K){
    dtemp = Dtemp[,i];
    ptemp = matrix(dtemp,nrow = height,ncol = width);
    prob = colSums(ptemp);
    prob = prob/sum(prob);
    horiLoc[i] = sum(prob*(1:width));

    prob = rowSums(ptemp);
    prob = prob/sum(prob);
    vertLoc[i] = height - sum(prob*(1:height));

}
L = cbind(horiLoc,vertLoc);
return(L);

}