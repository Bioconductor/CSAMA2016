genCircle<-function(n){
theta = seq(from = 0 , to = 2*pi, length.out = n+1);
x = cos(theta);
y = sin(theta);
x = -x[1:n];
y = y[1:n];
return(list(x=x,y=y));
}

genEllipse<-function(n){
	theta = seq(from = 0 , to = 2*pi, length.out = n+1);
	x = cos(theta);
	y = .5*sin(theta);
	x = c(x[1:n],x[1]);
	y = c(y[1:n],y[1]);
	y[y>=0.48] = .48;
	y[y<=-0.48] = -.48;

	return(list(x=x,y=y));
}