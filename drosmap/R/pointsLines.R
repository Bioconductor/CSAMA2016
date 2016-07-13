pointsLines<-function(l,W,edgeLabel = NULL, col = '#FF000030', textcol = '#00000099',cex = 1){
n = nrow(W);
C = max(W);
for (i in 1:(n-1)){
    for (j in (i+1):n){
    	if (W[i,j]>0){
    	   lwd = W[i,j]/C;
    	   segments(l[i,1],l[i,2],l[j,1],l[j,2],col = col,lwd = lwd);
	   if (!is.null(edgeLabel)){
	      x = mean(c(l[i,1],l[j,1]));
	      y = mean(c(l[i,2],l[j,2]));
	   
	      text(x,y,edgeLabel[i,j],col = textcol,cex = cex);
	   }
	}
   }
}

}