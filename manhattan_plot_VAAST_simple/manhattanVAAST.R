manhattanVAAST<-function(n.features, sig.level, vaast.simple, the.title, sig.line=TRUE, axis.text=TRUE, custom.xlab="index", sig.hjust=0, sig.vjust=0, the.color="NULL"){

#gtools for mixed sorting

require(gtools)


# READ IN THE DATA
dat<-read.table(vaast.simple, header=FALSE, skip=1, sep="\t", fill=TRUE)

# RID ZERO VALUE LINES
dat$V4<-as.numeric(as.character(dat$V4))
dat<-dat[dat$V4 > 0,]

# HEAVY FORMATTING DON'T DIG TOO DEEP OR YOUR BRAIN WILL FRY

# only care about first five columns
dat<-dat[,1:5]

# only care about lines with data found by chr grep
dat<-dat[grep(dat$V5, pattern="chr"),]

#parsing apart fifth column

pos.dat<-strsplit(as.character(dat$V5), split=":|;", perl=TRUE)

#returning values on index

dat$seqid<-as.character(sapply(pos.dat, FUN=function(xx){return(xx[[1]][1])}))
dat$pos<-as.numeric(as.character(sapply(pos.dat, FUN=function(xx){return(xx[[2]][1])})))

dat<-dat[mixedorder(dat$seqid),]



loff<-tapply(dat$pos, INDEX=dat$seqid, FUN=function(x){nmax<- max(x); c(nmax,rep(0, length(x)-1))}, simplify=TRUE)
loff2 <-cumsum(unlist(loff[mixedorder(names(loff))]))
#loff2<- cumsum(c(0, loff2[1:(length(loff2) -1)]))
dat$relpos <- dat$pos + loff2
dat$index<-1:length(dat$relpos)


#recoding x axis data

axis.name<-unique(dat$seqid)
axis.name.pos.rel<-as.vector(tapply(dat$index, INDEX=dat$seqid, FUN=function(x){middle<-(max(x) + min(x))/2}))
axis.name.pos.real<-sort(as.vector(tapply(dat$relpos, INDEX=dat$seqid, FUN=function(x){middle<-(max(x) + min(x))/2})))

#call to the plot
print(is.null(the.color))
 if(is.null(the.color)){the.col<-as.factor(dat$seqid)}

plot(y=-log10(as.numeric(as.character(dat$V3))), x=dat$relpos, col=the.color, xaxt="n", xlab=custom.xlab, pch=20, ylab="-log10(p-value)", main=the.title)

if(sig.line == TRUE){ abline(h=-log10(sig.level/n.features), lty=2, lwd=3, col="grey")
 text(x=(0.5 * max(dat$relative.pos)) + sig.hjust, y=0.5 + -log10(sig.level/n.features) + sig.vjust, "genome-wide sig. level", cex=1.0)}
 
 if(axis.text == TRUE){axis(1, at=axis.name.pos.real, labels=axis.name, las=2)}
 if(axis.text == FALSE){axis(1, at=axis.name.pos.real, labels=FALSE, las=2)}
 return(dat)
 
}

 
 
 
 
