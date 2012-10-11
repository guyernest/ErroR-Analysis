# change to the new directory
setwd("/Users/Guy/Development/nBA/NLP/")

#data <- read.table("/Users/Guy/Development/nBA/NLP/OperationalCategory-May2.tsv", header=TRUE, sep="\t")
data <- read.table(file.choose(), header=FALSE, sep="\t", comment.char = "", quote = "")

confMatrix <- function(data, human, guess) {

  #compute frequency of actual categories
  actual <- as.data.frame(table(data[[human]]))
  names(actual) <- c("Actual","ActualFreq")

  #build confusion matrix
  confusion <- as.data.frame(table(data[[human]], data[[guess]]))
  names(confusion) <- c("Actual","Predicted","Freq")

  #calculate percentage of test cases based on actual frequency
  confusion <- merge(confusion, actual, by=c("Actual"))
  confusion$Percent <- confusion$Freq/confusion$ActualFreq*100
  
  # Sorting by frequency
  confusion <- confusion[with(confusion,order(-ActualFreq)), ]
  
  return (confusion)
}  

# Function to find what are the diagonal (correct) tiles in the matrix
# It supports both numeric and character values
isDiagonal <- function(human, guess) {
  
  if (is.numeric(human)) 
    e <- as.double(human) == as.double(guess) 
  else
    e <- tolower(as.character(human))==tolower(as.character(guess))
  
  return (e)
}

asPercent <- function(number) {
  if (is.numeric(number))
    return (paste(sprintf("%.1f", number),"%", sep =""))
  else
    return ("")
}


confusion <- confMatrix(data, "V3", "V4")

# Calculating the precision for each prediction
predictMeans <- ddply(data,~NLP1,summarise,precision=length(Human1[isDiagonal(Human1,NLP1)])/length(NLP1[NLP1 != "Not Stated"]))
# Adding the precision above to the the confusion matrix
confusion <- merge(confusion, predictMeans, by.x="Predicted", by.y="NLP1")


#install.packages("ggplot2")
library(ggplot2)

#render plot
tile <- ggplot(aes(x=Actual,y=Predicted),data=confusion)
# Setting the labels for the axis
tile <- tile + labs(x="Actual",y="Predicted")
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- tile + geom_tile(aes(fill=Freq),data=subset(confusion, !isDiagonal(Actual,Predicted)), color="black",size=0.1) +
 scale_fill_gradient(name = "count", low="grey",high="red")
# Now we draw diagonal tiles. 
tile <- tile + geom_tile(data=subset(confusion, isDiagonal(Actual,Predicted)), color="green", size=0.5, fill="green")

# Adding the count of the cases in each non zero tile
tile <- tile + geom_text(aes(label=Freq),data=subset(confusion, Freq > 0), colour="blue", size=4, vjust = 1.2) 

# next we render text values. If you only want to indicate values greater than zero then use data=subset(confusion, Percent > 0)
tile <- tile + geom_text(aes(label=paste(sprintf("%.1f", Percent),"%", sep ="")),data=subset(confusion, Percent > 0), colour="black", size=4, vjust = -0.2) 
#  + scale_fill_gradient(name = "% of Actual", low="grey",high="red")

# Adding the count of the total cases in each category
tile <- tile + geom_text(aes(y = -0.5, label=ActualFreq), alpha=0.1, size=5, angle=90, hjust=0) 

# Precision of each row
tile <- tile + geom_text(aes(x=-0.5, label=asPercent(100*precision)), alpha=0.5, size=4, hjust=-0.5)


tile + opts(axis.text.x=theme_text(angle=90))

