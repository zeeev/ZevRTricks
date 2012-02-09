

	require(ggplot2)
	require(plyr)
	require(RColorBrewer)
	
	fam<-dir(pattern="*.fam")
	Qmatrix <- dir(pattern="*.Q")
	
	temp.name<-read.csv(fam, sep=" ", header=FALSE)
	
	
	parse.Q.files<-function(x){
		dat<-read.csv(file=x, sep=" ",header=FALSE)
		ldat<-length(dat)
		dat<-cbind(temp.name$V1,temp.name$V2,dat)
		colnames(dat)<-c("group_membership", "names", 1:ldat)
		dat2<-melt(dat)	
		dat2$Krun<-rep(ldat, length(dat2$variable))
		colnames(dat2)<-c("Group", "Name","Admixture.Group","Value", "Krun")
		col.order<-cluster(dat)
		dat2<-cbind(dat2,col.order)
			return(dat2)
		
}

my.data<-ldply(Qmatrix, parse.Q.files)

#ggplot(my.data, aes(x=reorder(Name, Value*as.numeric(Admixture.Group), Group), y=Value, fill=Admixture.Group))+geom_bar(stat="identity")+facet_grid(Krun ~ .)

cluster<-function(x){
	row.names(x)<-as.character(x[,2])
	x<-x[,-2]
	x[,1]<-as.numeric(x[,1])	
	heatmap.data<-heatmap(as.matrix(x))
	my.color.order<-heatmap.data$rowInd
	print(my.color.order)
	return(my.color.order)
}