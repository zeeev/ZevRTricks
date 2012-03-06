dyn.load("~/github/ZevRTricks/mean_window.so")
zwindow<-function(xpts, ypts, window){
	nxpts <- length(xpts)
	dens  <- .C("gslide_mean", as.double(xpts), as.double(ypts), as.double(xpts), as.integer(nxpts), as.double(window), result = double(length(xpts)))
dens[["result"]]
}