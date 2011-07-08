dat<-read.csv(file="~/Desktop/pigeon.scaffold.dist", header=FALSE, sep="\t")
library(ergm)



plot4N50<-function(sequence_lengths, plot_values){
pdf("N50s.hist.pdf")
par(mfrow=c(2,2))
N50<-wtd.median(sequence_lengths, weight=sequence_lengths)
LN50<-log10(wtd.median(sequence_lengths, weight=sequence_lengths))
	for(n in plot_values){
	dat2<-sequence_lengths[sequence_lengths > n]
	min<-summary(log10(dat2))[1][1]
	max<-summary(log10(dat2))[6][1]

	hist(log10(dat2), xlab="log10(sequence length)",main=paste("seq length >=", n), xlim = c(min,max))
	abline(v=N50, col="red", lty=3, lwd= 3)
	}
	mtext(text="Contigs/Scaffolds/Chromosomes N50 value and Hist")
	dev.off()
	print(paste("the n50 for the Contigs/Scaffolds/Chromosomes:", N50))
}

a<-c(100,1000,10000,100000)
plot4N50(dat$V2, a)