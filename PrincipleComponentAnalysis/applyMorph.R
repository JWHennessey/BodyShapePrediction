#Collection of functions to morph model from the mean using the principle components and create a new .obj file

applyMorph <- function(vectors, func, amount, name){

  vNum <- length(vectors)

  newModel <- matrix(scan("means/originalMeanColumn.txt",  n=19347), ncol=1, nrow=19347)

  for(i in 1:19347){
    total <- 0
    if(vNum > 0){
      for(k in 1:vNum){
        newModel[i, 1] <- addEigen(newModel[i, 1],  func[k], (amount[k] * eigens$vectors[i, vectors[k]]))
      }
    }
  }

  stringmat <-matrix(ncol=4, nrow=6449)
  l <- 1
  for(k in 1:6449){
      stringmat[k, 1] =  'v'  
      stringmat[k, 2] =  as.character(newModel[l, 1])
      l <- l + 1
      stringmat[k, 3] =  as.character(newModel[l, 1])
      l <- l + 1
      stringmat[k, 4] =  as.character(newModel[l, 1])
      l <- l + 1
  }

  write.table(stringmat, "NewModels/temp.txt", quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)
  newModelTable <- read.table('NewModels/temp.txt')
  faces <- read.table('faces.txt')

  total <- rbind(newModelTable, faces)
  write.table(total, paste("NewModels/", paste(name, ".obj", sep=""), sep=""), quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)
}

addEigen <- function(modelValue, func, amount){
    ans <- 0
    if(func == "+")
    {
      ans <- modelValue + amount
    }
    else if(func == "-")
    {
      ans <- modelValue - amount
    }
    return(ans)
}
