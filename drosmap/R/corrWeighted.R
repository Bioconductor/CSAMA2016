corrWeighted <- function(x,y,w = NULL,method = 'pearson'){

    if (method == 'pearson'){

        if (is.null(w)){
            w = rep(1/length(x),length(x));
        }

        w = w/sum(w);

        meanX = sum(x*w);
        meanY = sum(y*w);

        meanXY = sum(x*w*y);
        meanX2 = sum(w*(x^2));
        meanY2 = sum(w*(y^2));

        covXY = meanXY - meanX*meanY;
        varX = meanX2 - meanX^2;
        varY = meanY2 - meanY^2;

        corrXY = covXY/sqrt(varX*varY);

        return(list(corrXY = corrXY,varX = varX,varY = varY,covXY = covXY));
    }

    if (method == 'spearman'){
        x2 = rank(x);
        y2 = rank(y);
        return(corrWeighted(x2,y2,w,method = 'pearson'));
    }

}
