reorderDict<-function(D,width = 32, height =16,template){
# reorder the principal patterns from anterior to posterior
K = dim(as.matrix(D))[2];
if (is.null(template)){
   Dtemp = D;
}else{   
	 ind = which(template[,,1]==1);
	 Dtemp = matrix(0,nrow = width*height,ncol = K);
	 Dtemp[ind,] = D;
}

horiLoc = rep(0,K);
for (i in 1:K){
    dtemp = Dtemp[,i];
    ptemp = matrix(dtemp,nrow = height,ncol = width);
    prob = colSums(ptemp);
    prob = prob/sum(prob);
    horiLoc[i] = sum(prob*(1:width));
}
idx = sort.int(horiLoc,index.return = TRUE)$ix;
return(idx);
}


