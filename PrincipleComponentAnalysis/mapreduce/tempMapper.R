#! /usr/bin/env Rscript


#invec <- scan("stdin")
#mat <- outer(invec,invec, "*")
#l<-1
#for(i in 1:5){
#  cat(invec[i], "\t1\n", sep="")

#  for(k in 1:5){
#    cat(paste(as.character(l), mat[i,k], sep="") sep="\n")
#    l <- l+1
#  }
#}

#! /usr/bin/env Rscript

#stdin


invec <- readLines(file("barcolumn2/s2p0.txt"))
x <- length(invec)

for(i in 1:x){
  cat(as.character(invec[i]), "\t1\n", sep="")
}


#stdin
#invec <- scan("barcolumn2/s1p0.txt")
#covmat <- outer(invec,invec, "*")
#x <- length(invec)
#l <- 1

#for(i in 1:x){
#  for(k in 1:x){
#   cat('word', l, "\t", covmat[i,k], "\n", sep="")
#   l <- l+1
#  }
#}





