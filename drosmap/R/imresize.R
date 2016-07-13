imresize <- function(Iold, ratio){
# resizing the original image by nearest neighbor method

nrow = dim(Iold)[1];
ncol = dim(Iold)[2];

step = 1/ratio;
indRow = floor(seq(from = 1, to = nrow, by = step));
indCol = floor(seq(from = 1, to = ncol, by = step));
Inew = Iold[indRow,indCol];
return(Inew);
}