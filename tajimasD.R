
a_1 <- function(n_seq){
	a1<-0
	for (i in 1:(n_seq-1)){
		a1<-a1 + 1/i
	}
	return(a1)
}

a_2 <- function(n_seq){
	a2<-0
	for (i in 1:9){
		a2<-a2 + 1/i^2
	}
	return(a2)
}

b_1 <- function(n_seq){
	b1<-(n_seq + 1) / (3*(n_seq -1))
	return(b1)
}

b_2 <- function(n_seq){
	b2<-2*(n_seq^2 + n_seq +3)/(9*n_seq*(n_seq -1))
	return(b2)
}

c_1 <-function(a1, b1){
	c1<-b1  - (1/a1)
	return(c1)
}

c_2 <-function(a1, a2, b2, n_seq){
	c2<- b2 - (n_seq + 2)/(a1*n_seq) + (a2/a1^2)
	return(c2)
}

e_1<-function(a1, c1){
	e1<-c1/a1
	return(e1)
}

e_2<-function(a1, a2, c2){
	e2<-c2/(a1^2+a2)
	return(e2)
}



TajimasD<-function(n_seqs){
	a1<-a_1(n_seqs)
	a2<-a_2(n_seqs)
	b1<-b_1(n_seqs)
	b2<-b_2(n_seqs)
	c1<-c_1(a1, b1)
	c2<-c_2(a1, a2, b2, n_seqs)
	e1<-e_1(a1, c1)
	e2<-e_2(a1, a2, c2)
	print(c(a1,a2,b1,b2,c1,c2,e1,e2))
	
}