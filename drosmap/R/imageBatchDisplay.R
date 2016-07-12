imageBatchDisplay<-function(D,width=32,height=16,nrow=7,ncol=7, colorScale = 'blueRed',template,
				imgNames = NULL, savePath = NULL, plotEllipse =TRUE, font = NULL,
				noNumber = FALSE, drawTableLine = FALSE, marginCol = 2){

# This function displays an array of spatial gene expression patterns of Drosophila embryos.
# D: a matrix containing the images to be displayed. Each column corresponds to an image.
# width/height: the width/height of an image.
# nrow/ncol: number of rows/columns in the layout.
# colorScale: the color scale used to display the images. It can be 'blueRed','jet' or 'gray'.
# template: a binary (entries being 0 or 1) matrix of size height * width. The number of 1's in
#           this matrix should be equal to the length of a column of D. It tells the function 
#	    how to arrange the entries of a column in D on a rectangle image of size height * width.   	    
# imgNames: the names to be printed under the images
# savePath: if provided, the plot will be saved as a file (in either pdf or png)
# plotEllipse: if TRUE, an ellipse resembling the embryo shape will be plotted within each image.
# font: the font size of the image labels.
# noNumber: if TRUE, no image number are printed under each image.
# drawTableLine: if TRUE, lines separating images will be drawn.
# marginCol: margin

  D = as.matrix(D);
  ind = which(template==1);	
  Dfull = matrix(0,nrow = height*width,ncol = dim(D)[2]);
  Dfull[ind,] = D;

  if (is.null(imgNames)){
      imgNames = colnames(D);
   }

   imageBatchDisplay0(Dfull, width, height, nrow,ncol,colorScale,imgNames,savePath,
			  noNumber = noNumber,paintBackground = FALSE, marginCol = marginCol,
 			  plotEllipse =  plotEllipse, drawTableLine = drawTableLine, font = font); 

}


imageBatchDisplay0<-function(D,width=32,height=16,nrow=7,ncol=7,colorScale = 'jet',imgNames = NULL, 
                               savePath = NULL, noNumber = FALSE,paintBackground = FALSE,textCol = 'black',
                      	       lineCol = '#00000050',drawTableLine = TRUE, plotEllipse = TRUE,
			       marginCol = 2, font = NULL){

   d = dim(D);
   p = d[1]; 
   n = d[2];
   
   if (is.null(imgNames)){
	imgNames = rep('',n);
    }
   if (paintBackground){
         nR = nrow*height;
 	 nC = ncol*width;

      	 I = matrix(max(D)/3,nrow = nR,ncol = nC);
	 marginTempRow = 0;
	 marginTempCol = 0;

   }else{
	if (noNumber & is.null(imgNames)){  
	   nR = nrow*height;
   	   nC = ncol*width;

      	   I = matrix(0,nrow = nR,ncol = nC);
	   marginTempRow = 0;
	   marginTempCol = 0;
	   
	}else{

	   marginTempRow = floor(height/2);
	   
	   marginTempCol = marginCol;
	   nR = nrow*height+marginTempRow*nrow;
   	   nC = ncol*width+marginTempCol*ncol;

  	   I = matrix(0,nrow = nR,ncol = nC);
	}	
   }
   rowStart = 1;
   colStart = 1;
   k = 1;
   for (i in 1:nrow){
       for (j in 1:ncol){
           indR = (rowStart + (i-1)*height):(i*height)+(i-1)*marginTempRow;
           indC = (colStart + (j-1)*width):(j*width)+(j-1)*marginTempCol;
           I[indR,indC] = matrix(D[,k],nrow = height,ncol = width);
           k = k+1;
           if (k>n){
               break;
	   }
        }
  
       if (k>n){
           break;
       }
   }
   
   imgTemp = t(I);
   indTemp = seq(from = dim(imgTemp)[2], to = 1, by = -1);
   imgTemp = imgTemp[,indTemp];
   if (colorScale == 'gray'){
      colorScale1 = gray(seq(from = 1, to = 0, by = -0.05));
      #textCol = 'red'
   }else if (colorScale == 'blueRed'){
   	 colorScale1 = blue2red(50,'ff');
	 colorScale1 = colorScale1[50:1];
	 #textCol = 'black';   
   }else if (colorScale == 'jet'){
   	 colorScale1 = jet(50);
	 #textCol = 'red';
   }
   #colorScale = gray(seq(from = 0, to = 1, by = 0.05));
   oldMargin = par('mar');
   if (is.null(savePath)){
#      X11(height = nR/30, width = nC/30);
      par(mar = c(0.5,0.5,0.5,0.5));
    }
   else{
     if (length(grep('*png',savePath))){
      	png(file = savePath,height = 3*nR,width = 3*nC);
      	par(mar = c(.5,.5,.5,.5));
     }
     if (length(grep('*pdf',savePath))){
      	pdf(file = savePath,height = 3*nR/50,width = 3*nC/50);
      	par(mar = c(.5,.5,.5,.5));
     }

  }
   if (colorScale == 'gray'|colorScale =='jet'){
  
      image(imgTemp,col = colorScale1, xaxt = 'n', yaxt = 'n',bty = 'n');
   }else if (colorScale == 'blueRed'){
        lowerV = min(D);
	upperV = max(D);
  	r = max(abs(lowerV),upperV);  
	
	#print(dim(imgTemp))
	#print(nC-marginTempCol)	
      	image(imgTemp[1:(nC-marginTempCol),],col = colorScale1, xaxt = 'n', yaxt = 'n',zlim =c(-r,r));
   }
   
   #return(I);   
   par(mar = oldMargin);
   
   
   nRow = dim(imgTemp)[1];
   nCol = dim(imgTemp)[2];
   
   horiLines = (seq(from = 1.2,to = nCol, by = height+marginTempRow)-1)/nCol;
   vertLines = (seq(from = 1-marginTempCol/2,to = nRow, by = width+marginTempCol)-1)/(nRow-marginTempCol);
   
   horiLines[1] = NA;
   vertLines[1] = NA;

   lwd = width/15;
   if (drawTableLine){
      abline(h = horiLines,col=lineCol,lwd = width/10);
      abline(v = vertLines,col=lineCol,lwd = width/10);

      #abline(h = horiLines[c(1,length(horiLines))],col=lineCol,lwd = width/5);
      #abline(v = vertLines[c(1,length(vertLines))],col=lineCol,lwd = width/5);
   }
    
   k = 1;
   xCoord = NULL;
   yCoord = NULL;
   charTemp = NULL;
   for (i in 1:nrow){
       for (j in 1:ncol){
           y =  1 - (i*height + 3 +(i-1)*marginTempRow)/nCol;
           x =  (j*width-width/2+(j-1)*marginTempCol)/(nRow-marginTempCol);
	   xCoord = c(xCoord,x);
	   yCoord = c(yCoord,y);
   	   if (!is.null(imgNames)){
	      if (noNumber){
	      charTemp = c(charTemp,imgNames[k]);
	      }else{
	      charTemp = c(charTemp, paste(k,': ',imgNames[k],sep = ''));		}
	   }else{
	      if (noNumber){
	      	 charTemp = c(charTemp,' ');
		}else{
	      charTemp = c(charTemp, k);}
	   }
           k = k+1;  
           if (k > n){
               break;
           }
       }
       if (k > n){
           break;
       }
       
   }
   
   if (is.null(font)){
      font = lwd/1.3;
   }
   text(xCoord,yCoord,charTemp,col=textCol,cex = font);
   
   if (plotEllipse){
      ellipseCoord = genEllipse(100);
      xCoordTemp = .5*(1.03*width*ellipseCoord$x+width)/nRow;
      yCoordTemp = 1.03*(.52*(width*ellipseCoord$y+height)/nCol);      
   

   xCoordCenter = yCoordCenter = NULL;
  
  k = 1;

  for (i in 1:nrow){
       for (j in 1:ncol){

       	   if (!noNumber | imgNames[k]!=''){
              y =  1-(i*height+(i-1)*marginTempRow+.5)/nCol # - (nrow-1)*0.01;
              x =  (j*width-width +(j-1)*marginTempCol)/(nRow-marginTempCol);
	      xCoordCenter = c(xCoordCenter,x);
	      yCoordCenter = c(yCoordCenter,y);
	   }
	   k = k+1;
	   if (k>n){
	      break;
	   
	    }
 	}
	   if (k>n){
	      break;
	   
	    }

   }

      for (i in 1:length(xCoordCenter)){
          points(xCoordTemp+xCoordCenter[i],(yCoordTemp+yCoordCenter[i]),type='l',lwd = 2);
      }
   }

   if (!is.null(savePath)){
      dev.off();
   }

}
