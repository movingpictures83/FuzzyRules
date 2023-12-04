library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
library(caret)

dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")


input <- function(inputfile) {
	pfix = prefix()
  parameters <<- read.table(inputfile, as.is=T);
  rownames(parameters) <<- parameters[,1];
   # Need to get the three files
   csvfile <<- paste(pfix, parameters["csvfile", 2], sep="/")

   training <<- read.csv(csvfile)
   fitControl <<- readRDS(paste(pfix, parameters["fitControl", 2], sep="/"))
   myType <<- "Regular"
  if ("type" %in% rownames(parameters)) {
  myType  <<- as.integer(parameters["type", 2])
  }


}

run <- function() {}

output <- function(outputfile) {
set.seed(825)
if (myType == "Chi") {
result <- train(Class ~ ., data = training, 
                 method = "FRBCS.CHI", 
                 trControl = fitControl)
}
else if (myType == "GBML") {
result <- train(Class ~ ., data = training, 
                 method = "FH.GBML", 
                 trControl = fitControl)
}
else if (myType == "SLAVE") {
result <- train(Class ~ ., data = training, 
                 method = "SLAVE", 
                 trControl = fitControl)
}
else {
result <- train(Class ~ ., data = training, 
                 method = "FRBCS.W", 
                 trControl = fitControl)
}
print(result)
saveRDS(result, outputfile)
}
