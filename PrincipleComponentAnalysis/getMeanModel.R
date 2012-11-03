files<-list.files("R/VectorsOnly")

mat <-matrix(1:19347, ncol=3)

for(i in 1:6449){
  mat[i, 1] <- 0
  mat[i, 2] <- 0
  mat[i, 3] <- 0
}

i=1
for(name in files){
  text <- readLines(paste("R/VectorsOnly/", name, sep=""),  n=6449, encoding="UTF-8")
  
  for(k in 1:6449){    
    xyzLine <- sub("v ", "", text[k])
    xyzList <- unlist(strsplit(xyzLine, " "))

    mat[k, 1] =  mat[k, 1] + as.numeric(xyzList[1])
    mat[k, 2] =  mat[k, 2] + as.numeric(xyzList[2])
    mat[k, 3] =  mat[k, 3] + as.numeric(xyzList[3])
  }

  i = i+1
}

len = length(files)

for(k in 1:6449){   
    mat[k, 1] =  mat[k, 1] / len
    mat[k, 2] =  mat[k, 2] / len
    mat[k, 3] =  mat[k, 3] / len
}

stringmat <-matrix(1:25796, ncol=4)

for(k in 1:6449){
    stringmat[k, 1] =  'v'  
    stringmat[k, 2] =  as.character(mat[k, 1])
    stringmat[k, 3] =  as.character(mat[k, 2])
    stringmat[k, 4] =  as.character(mat[k, 3])
}


write.table(stringmat, "R/means/tempVer.txt", quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)

avgModel2 <- read.table('R/means/tempVer.txt')
faces <- read.table('R/faces.txt')

total2 <- rbind(avgModel2, faces)

write.table(total2, "R/means/finalObjGroupD.obj", quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)




