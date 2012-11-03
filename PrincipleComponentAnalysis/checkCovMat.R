#Script to check that the covarience matrix is correct

files <- list.files('barcolumn')
x <- 0
i <- 0
for(name in files){
  invec <- scan(paste("barcolumn/", name, sep=""), n=1)
  cat(i, '\t')
  cat(name, '\t')
  cat(invec, '\n')
  i <- 1 + i
  x <- x + (invec * invec)
}
cat('\n\n X Total:', x)
cat('\n Cotmat[1,1]', covmat[1,1])
