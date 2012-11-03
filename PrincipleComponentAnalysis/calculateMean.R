text <- readLines("R/means/originalMeanModel.obj",  n=6449, encoding="UTF-8")

xmat <- matrix(1:6449, ncol=1)
ymat <- matrix(1:6449, ncol=1)
zmat <- matrix(1:6449, ncol=1)

for(k in 1:6449){
    xyzLine <- sub("v ", "", text[k])
    xyzList <- unlist(strsplit(xyzLine, " "))

    xmat[k, 1] = xyzList[1]
    ymat[k, 1] = xyzList[2]
    zmat[k, 1] = xyzList[3]
}

write.table(xmat, "R/means/xMean.txt", quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)

write.table(ymat, "R/means/yMean.txt", quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)

write.table(zmat, "R/means/zMean.txt", quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)

