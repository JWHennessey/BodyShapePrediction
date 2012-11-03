#Principle Component Analysis
#Create a covariance matrix for each vector then perform eigenvector decomposition for each matrix

xCol <- matrix(ncol=1, nrow=6449)
yCol <- matrix(ncol=1, nrow=6449)
zCol <- matrix(ncol=1, nrow=6449)


xCovMat <- matrix(0, ncol=6449, nrow = 6449)
yCovMat <- matrix(0, ncol=6449, nrow = 6449)
zCovMat <- matrix(0, ncol=6449, nrow = 6449)


files<-list.files("BarPosition0")
print('Start Columns')
for(name in files){
  text <- readLines(paste("BarPosition0/", name, sep=""),  n=6449, encoding="UTF-8")
  for(i in 1:6449){
      xyzLine <- sub("v ", "", text[i])
      xyzList <- unlist(strsplit(xyzLine, " "))

      xCol[i, 1] =  as.numeric(xyzList[1])
      yCol[i, 1] =  as.numeric(xyzList[2])
      zCol[i, 1] =  as.numeric(xyzList[3])

  }
}


print('Start Cov Mats')
for(i in 1:6449){
  print('Outer Loop')
  print(i)
  for(k in 1:6449){
    xCovMat[i, k]  <- xCovMat[i, k]  + (xCol[i, 1] * xCol[k, 1])
    yCovMat[i, k]  <- yCovMat[i, k]  + (yCol[i, 1] * yCol[k, 1])
    zCovMat[i, k]  <- zCovMat[i, k]  + (zCol[i, 1] * zCol[k, 1])
  }
}

print('Start xEigen')
xEigen <- eigen(xCovMat, symmetric = TRUE)
print('Start yEigen')
yEigen <- eigen(yCovMat, symmetric = TRUE)
print('Start zEigen')
zEigen <- eigen(zCovMat, symmetric = TRUE)
