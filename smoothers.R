dyn.load("~/github/ZevRTricks/smoother1.so")
zsmooth<-function(dat){
	xvals <- 0
	Yhats  <- 0
	h<-20000

	for (i in seq(1,max(dat$V2),by=100)){
		dat2<-dat[dat$V2 < i + h & dat$V2 > i - h,]
		sum.num <- 0
		sum.dem <- 0
		ldat<-length(dat2$V1)
		for(j in 1:ldat){
			maj.term <- kernel((i - dat2[j,2])/h)
			sum.num  <-  sum.num + dat2[j,4] * maj.term
			sum.dem  <-  sum.dem + maj.term
		}
		Yhat<-sum.num / sum.dem
		xvals<-c(xvals, i)
		Yhats<-c(Yhats, Yhat)
	}
	return(cbind(xvals,Yhats))
}

kernel<-function(x){
	return(dnorm(x))
#	return(2.718^(-x^2/2))
}


zsmooth2<-function(x, ypts, xpts, h){
	n <- length(x)
	
	nxpts <- length(xpts)
	dens  <- .C("kernel_smooth", as.double(x), as.double(ypts), as.double(xpts), as.integer(n), as.integer(nxpts), as.double(h), result = double(length(x)))
dens[["result"]]
}