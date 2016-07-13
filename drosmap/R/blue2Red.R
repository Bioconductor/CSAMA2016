blue2red <- function(n,alphaInd = 'ff'){
  col.out = rep("NA",n)
  m = floor(n/2);
  redScale = blueScale = rep("NA",256);
  for (i in 0:255){
    hexTemp = digit2hex(i);
    blueScale[i+1] = paste('#',hexTemp,hexTemp,'ff',alphaInd,sep='');
    redScale[i+1] = paste('#','ff',hexTemp,hexTemp,alphaInd,sep='');
     
  }
  subInd = floor(seq(from=1,to=256,length.out=m));
  col.out[1:m] = blueScale[subInd];
  
  subInd = floor(seq(from=1,to=256,length.out=n-m));
  col.out[(m+1):n] = redScale[subInd[(n-m):1]];
return(col.out)
  
}

digit2hex <- function(digit){
  if (digit>=256){
    print('cant do it for a number greater than 256')
    return(NULL)
  }else{
    if (digit==0){
      return('00')
    }
    else if (digit < 16){
      temp = sprintf('%x',digit);
      temp2 = paste('0',temp,sep='');
      return(temp2);
    }
    else if (digit < 256){
      temp = sprintf('%x',digit);
      return(temp);
    }
    
  }
  
}
