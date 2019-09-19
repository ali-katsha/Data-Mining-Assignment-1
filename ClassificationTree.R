read_file <-function(){
  
  d<-read.table("credit.txt",header=TRUE,sep=",")
  m <- as.matrix(d)
  return(m);
}








data = read_file()