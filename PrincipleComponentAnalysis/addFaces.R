#Script to add faces to the vertices

model <- read.table('**Location of vertices **')
faces <- read.table('**Location of faces **')

newmodel <- rbind(model, faces)

write.table(newmodel, "newmodel.obj", quote = FALSE, sep = " ", row.names= FALSE, col.names = FALSE)
