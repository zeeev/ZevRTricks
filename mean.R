# This function was inspired by Brad Demarest
# It does a sliding window across genomic coords
# and produces a mean value for the values
# Provide an X vector and Y vector the same size
# the windows grow at either end.
# The list does not need to be sorted

g_slide<-function(ypts, xpts, h){
	dyn.load("~/github/ZevRTricks/mean_window.so")
	nxpts <- length(xpts)
	dens  <- .C("gslide_mean", as.double(ypts), as.double(xpts), as.integer(nxpts), as.integer(h), result = double(nxpts))
dens[["result"]]
}