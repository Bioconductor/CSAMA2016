imagesc <- function(I,colorScale = 'gray', margin = NULL, invert = TRUE,
	   	   printLabels = TRUE,zlim = NULL, lasX = 2, lasY = 1,cexX = 1){

# Display a heatmap using the image function.
# I: an image matrix.
# colorScale: the color scale of the heatmap. It can be 'blueRed','jet' or 'gray'.
# margin: the margin of the heatmap.
# invert: if TRUE, the image will be flipped in the vertical direction.
# printLabels: if TRUE, the labels on the horizontal axis will be printed.
# zlim: the minimum and maximum ‘z’ values for which colors should be plotted.
# lasX: the orientation of the labels on the horizontal axis. 1: vertical; 2: horizontal.
# lasY: the orientation of the labels on the vertical axis. 1: vertical; 2: horizontal.
# cexX: size of the labels on the horizontal axis.


imgTemp = t(I);
if (invert){
   indTemp = seq(from = dim(imgTemp)[2], to = 1, by = -1);
   imgTemp = imgTemp[,indTemp];
}
nR = dim(I)[1];
nC = dim(I)[2];


if (colorScale == 'gray'){
   colorScale = gray(seq(from = 1, to = 0, by = -0.05));
   }else if (colorScale == 'heatmap'){
   colorScale = rainbow(100);   
}else if (colorScale == 'blueRed'){
   colorScale = blue2red(100);
   colorScale = colorScale[length(colorScale):1];
}else if (colorScale == 'blue'){
      colorScale = blue2red(200)[100:1];
}else if (colorScale == 'jet'){
      colorScale = jet(100);
}	
	

if (printLabels){
	if (!is.null(margin)){
    	   par(mar = margin);
	}

    if (is.null(zlim)){
       image(imgTemp,col = colorScale, xaxt = 'n', yaxt = 'n');    
    }else{
       image(imgTemp,col = colorScale, xaxt = 'n', yaxt = 'n',zlim = zlim);    
    }

	temp = nrow(imgTemp);
	temp2 = (0:(temp-1))/(temp-1);    
	axis(1, at=temp2,
             labels=colnames(I), las = lasX,cex = cexX);
	temp = ncol(imgTemp);
	temp2 = (0:(temp-1))/(temp-1);    
	axis(2, at=temp2,
             labels=rownames(I)[temp:1], las = lasY);
}else{
    if (!is.null(margin)){
    	   par(mar = margin);
    }else {
    	  par(mar = c(.5,.5,.5,.5));
    }
    if (is.null(zlim)){
       image(imgTemp,col = colorScale, xaxt = 'n', yaxt = 'n');    
    }else{
       image(imgTemp,col = colorScale, xaxt = 'n', yaxt = 'n',zlim = zlim);    
    }

}




  
} 