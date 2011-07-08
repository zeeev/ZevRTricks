plot.admixture<-function(directory){

	require(ggplot2)
	require(plyr)
	require(RColorBrewer)

	fam<-dir(pattern="*.fam")

	temp.name<-read.csv(fam, sep=" ", header=FALSE)
	name<-temp.name$V2

	new<-NULL
	final<-NULL
	
	Qmatrix <- dir(pattern="*.Q")

	parse.Q.files<-function(x){
		dat<-read.csv(file=x, sep=" ",header=FALSE)
		ldat<-length(dat)
		colnames(dat)<-c(paste("group",1:ldat,sep=""))
		row.names(dat)<-name
		dat<-cbind(row.names(dat),dat)
		dat<-melt(dat)
		dat<-cbind(dat, rep(paste("Krun",ldat, sep=""), length(dat[,1])))
		colnames(dat)<-c("ID", "Group","Value","Ngroups")
		return(dat)
	}

	plotz<-function(x){
	
		new.names<-NULL
		
		facto<-function(x){
			x<-gsub(pattern="group", replacement="",x)
			newvector<-c(x)	
			newvector<-as.numeric(newvector)
			weight<-seq(10000000*length(newvector),1,length.out=length(newvector))
			newvector<-weight*newvector
			newvector<-paste(newvector, sep="", collapse="")
			newvector<-gsub(newvector, pattern="[\\.]", replacement="", perl=TRUE)
			newvector<-as.numeric(newvector)
			return(newvector)
		}
		
		groups <-gsub(pattern="group", replacement="",x[,2])
		x<-x[order(x$ID, x$Value),]
		print(x)
		newx<-aggregate(x[,c(-1,-3,-4)],by=list(x$ID), facto)
		
		for(y in 1:length(x$ID)){
			pat<-as.character(x[y,1])
			hit<-newx[grep(pattern=pat, newx$Group.1),]
			new.names<-c(new.names, hit[1,2])
		}
		
		x<-cbind(x,new.names)
		x$Groupz<-sub(x=x$Group, pattern="group", replacement="")
		x$Groupz<-as.numeric(x$Group)
		
		kval<-max(as.numeric(groups))
		print(kval)
		par(ask=TRUE)
		plots<-ggplot(x, aes(x=reorder(ID, Value*Groupz), y=Value, fill=Group))+geom_bar(stat="identity") +scale_fill_manual(values = rainbow(kval))+ opts(axis.text.x=theme_text(angle=-90, hjust=0))+labs(x="Indv", y="Membership")+opts(title=paste("K=", kval, sep=""))

return(plots)

	}

	cleanup<-function(x){
		rm(x)
	}

	test<-lapply(Qmatrix, parse.Q.files)
	out<-lapply(test, plotz)
	return(out)
}

#namespace<-ls()
#namespace<-namespace[-6]
#lapply(namepace, cleanup)

