#Principle Component Analysis
#Create a covariance matrix of all scan concerted into a column with the mean subtracted. Then calculate eigenvectors and values. 

files<-list.files("barcolumn")
covmat <- matrix(0, ncol=19347, nrow = 19347)
i <- 0
for(name in files){
   cat(i)
   invec <- scan(paste("barcolumn/", name, sep=""))
   covmat <- covmat + outer(invec,invec, "*")
   i <- i + 1
}
eigens <- eigen(covmat, symmetric = TRUE)

