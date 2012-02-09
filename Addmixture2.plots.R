plot.admixture<-function(directory){
	
	require(ggplot2)
	require(plyr)
	require(RColorBrewer)

	Qmatrix <- dir(directory,pattern="*.Q")
	fam<-dir(directory, pattern="*.fam")
	temp.name<-read.csv(paste(directory, fam, sep=""), sep=" ", header=FALSE)
	
	parse.Q.files<-function(x){
		dat<-read.csv(file=paste(directory,x, sep=""), sep=" ",header=FALSE)
		ldat<-length(dat)
		dat<-cbind(temp.name$V1,temp.name$V2,dat)
		colnames(dat)<-c("group_membership", "names", 1:ldat)
		dat2<-melt(dat)	
		dat2$Krun<-rep(ldat, length(dat2$variable))
		colnames(dat2)<-c("Group", "Name","Admixture.Group","Value", "Krun")
		dat2$Name<-paste(dat2$Name, dat2$Group, sep="  ")
		return(dat2)
	}	

	order.plot.vector<-function(datframe){
		datframe<-datframe[order(datframe$Value, decreasing=TRUE),]	
		order.vector<-0
		for(i in 1:length(datframe[,1])){
				order.vector<-order.vector + as.numeric(as.character(datframe[i,3]))*as.numeric(as.character(datframe[i,4]))+i  
		}

		order.vector<-rep(order.vector, length(datframe$Value))
		
		return(cbind(datframe,order.vector))
	}
	
	plotgg<-function(datframe){
		the.plot<-NULL
		my.max.k<-max(as.numeric(as.character(datframe$Admixture.Group)))
		print(head(datframe))
		print(my.max.k)
		my.col<-colors()[c(26,547,498,69,33,51,536)]
		
		if(my.max.k > 2){
		the.plot<-ggplot(datframe, aes(x=reorder(Name, order.vector), y=Value, fill=Admixture.Group))+geom_bar(stat="identity")+scale_fill_manual(values = my.col[1:my.max.k], name="Admixture group") + opts(axis.text.x=theme_text(angle=-90, hjust=0, size=22),axis.text.y=theme_text(size=18), plot.background=theme_blank(),title=paste("K=", my.max.k, sep=""),plot.title=theme_text(size=32),legend.background=theme_rect(col = NA),strip.background = theme_blank() ,legend.position = "none"
, plot.margin = unit(c(0,0,-1,-1), "lines"))+labs(x="", y="")


		}
		else{
			the.plot<-ggplot(datframe, aes(x=reorder(Name, order.vector), y=Value, fill=Admixture.Group))+geom_bar(stat="identity")+scale_fill_manual(values = my.col[1:my.max.k], name="Admixture group") + opts(axis.text.x=theme_text(angle=-90, hjust=0, size=22),axis.text.y=theme_text(size=18), plot.background=theme_blank(),title=paste("K=", my.max.k, sep=""),plot.title=theme_text(size=32),panel.margin = unit(0, "lines"),legend.background = theme_rect(col = NA)
,legend.position = "none",strip.background = theme_blank(), plot.margin = unit(c(0,0,-1,-1), "lines"))+labs(x="bird & group", y="membership (proportion)")+labs(x="", y="")
				}
		return(the.plot)
	}




dat<-ldply(Qmatrix, parse.Q.files)
dat<-ddply(dat, .(Krun, Name), order.plot.vector)
my.plots<-daply(dat, .(Krun), plotgg)
return(my.plots)
}



plot.all.together<-function(file.name, plotCols, list.of.plots){
	
	numPlots = length(list.of.plots)
	plotRows = ceiling(numPlots/plotCols)
	# Fiddle with the to adjust your plot dimentions 
	pdf(file=paste(file.name, "pdf", sep="."),bg="transparent", width=18*plotCols, height=8*plotRows)
	
	grid.newpage() 
	pushViewport(viewport(layout = grid.layout(plotRows, plotCols))) 
	vplayout <- function(x, y) 
    viewport(layout.pos.row = x, layout.pos.col = y) 

	# Make each plot, in the correct location
	for (i in 1:numPlots) {
    		curRow = ceiling(i/plotCols)
    		curCol = (i-1) %% plotCols + 1
    		print(my.plots[[i]], vp = vplayout(curRow, curCol ))
	}

	dev.off() 
	
}
	


	
	