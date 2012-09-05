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

confusion <- confMatrix(data, "V3", "V4")

install.packages("ggplot2")
library(ggplot2)

#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() + geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) + labs(x="Actual",y="Predicted")

# next we render text values. If you only want to indicate values greater than zero then use data=subset(confusion, Percent > 0)
tile <- tile + geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=subset(confusion, Percent > 0), colour="black", size=4, vjust = -0.2) + 
  scale_fill_gradient(name = "% of Actual", low="grey",high="red")

tile <- tile + geom_text(aes(x=Actual,y=Predicted, label=Freq),data=subset(confusion, Freq > 0), colour="white", size=3, vjust = 1.2) 

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
#tile <- tile + geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0)

tile + opts(axis.text.x=theme_text(angle=90))

