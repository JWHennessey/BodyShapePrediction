#! /usr/bin/env Rscript

trimWhiteSpace <- function(line) gsub("(^ +)|( +$)", "", line)
splitIntoWords <- function(line) unlist(strsplit(line, "[[:space:]]+"))
final <- matrix(0, ncol=19347, nrow=19347)
## **** could wo with a single readLines or in blocks
con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        
    line <- trimWhiteSpace(line)
    words <- splitIntoWords(line)
    final <- final + matrix(as.numeric(words), ncol=19347, nrow=19347)
}
close(con)
cat(final)

