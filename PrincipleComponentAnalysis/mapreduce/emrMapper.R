#! /usr/bin/env Rscript

trimWhiteSpace <- function(line) gsub("(^ +)|( +$)", "", line)
splitIntoWords <- function(line) unlist(strsplit(line, "[[:space:]]+"))

con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    line <- trimWhiteSpace(line)
    words <- splitIntoWords(line)
    ## **** can be done as cat(paste(words, "\t1\n", sep=""), sep="")
    for (w in words)
        cat(w, "\t1\n", sep="")
}
close(con)

 # text <- readLines("stdin",  n=19347, encoding="UTF-8")
   #covmat <- matrix(ncol=19347, nrow=19347)
   #for(i in 1:19347){
   #   cat(i)
   #   for(k in 1:19347{
   #       covmat[i, k]  <- as.numeric(trimWhiteSpace(text[i])) * as.numerictrim(WhiteSpace(text[k]))
   #   }
   #}
  #cat(covmat)
