files<-list.files("R/Position0")

xmean <- readLines('R/means/xMean.txt', n=6449, encoding='UTF-8')
ymean <- readLines('R/means/yMean.txt', n=6449, encoding='UTF-8')
zmean <- readLines('R/means/zMean.txt', n=6449, encoding='UTF-8')

for(name in files){
	text <- readLines(paste("R/Position0/", name, sep=""),  n=6449, encoding="UTF-8")
	mat <-matrix(1:19347, ncol=3)	

	for(k in 1:6449){    
    		xyzLine <- sub("v ", "", text[k])
    		xyzList <- unlist(strsplit(xyzLine, " "))

    		mat[k, 1] =  as.numeric(xyzList[1]) - as.numeric(xmean[k])
    		mat[k, 2] =  as.numeric(xyzList[2]) - as.numeric(ymean[k])
    		mat[k, 3] =  as.numeric(xyzList[3]) - as.numeric(zmean[k])
  	}

	name <- str_replace(name, '.obj', '.txt')
	
	write.table(mat, paste(str_trim("R/BarPosition0/"), str_trim(name)), quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)
	
}

