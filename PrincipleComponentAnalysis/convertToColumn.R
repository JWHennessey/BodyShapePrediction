#Convert .obj files into single column files without faces. 

files<-list.files('Position0')

for(name in files){		
				
  columns <- matrix(ncol=1, nrow=19347)
  #text <- readLines("means/originalMeanModel.obj",  n=6449, encoding="UTF-8")
  text <- readLines(paste("Position0/", name, sep=""),  n=6449, encoding="UTF-8")

  i <-1  
    for(k in 1:6449){    
     xyzLine <- sub("v ", "", text[k])
      xyzList <- unlist(strsplit(xyzLine, " "))
      columns[i, 1] =  as.numeric(xyzList[1])
      i <- i + 1
      columns[i, 1] =  as.numeric(xyzList[2])
      i <- i + 1
      columns[i, 1] =  as.numeric(xyzList[3])
      i <- i + 1
  }
  
  name <- sub('.obj', '.txt', name)
  
  write.table(columns, paste("position0column/", name, sep=""), quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)
}


