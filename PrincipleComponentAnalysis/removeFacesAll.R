files<-list.files("Position0")
for(name in files){
  text <- readLines(paste("Position0/", name, sep=""),  n=6449, encoding="UTF-8")
  fileConn<-file(paste("VectorsOnly/", name, sep=""))
  writeLines(text, fileConn)
  close(fileConn)
}

