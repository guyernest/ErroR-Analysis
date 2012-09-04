# change to the new directory
setwd("/path/to/files/")

# Load a predefine file
#cats <- read.table("/path/to/files/fileName.tsv", header=FALSE, sep="\t")
#cats <- read.table("/path/to/files/fileName.tsv", header=FALSE, sep="\t", comment.char = "", , quote = "")
# Or choose the file to load
cats <- read.table(file.choose(), header=FALSE, sep="\t")
cats <- read.table(file.choose(), header=FALSE, sep="\t", comment.char = "", quote = "")

# Adding Match counter (0|1) based on matching rule (it can be also comparing V3 and V4)
cats$Match[cats$V2=='true'] <- 1.0
cats$Match[cats$V2=='false'] <- 0

# Adding Significant counter (0|1) based on matching rule (it can be if V4 is null)
cats$Sig[cats$V1=='1.0'] <- 1.0
cats$Sig[cats$V1=='null'] <- 0

library(plyr)
humanMeans <- ddply(cats,~V3,summarise,recall=mean(Match),precision=sum(Match)/sum(Sig),nrow=length(V3))
# Sorting by frequency
humanMeans <- humanMeans[with(humanMeans,order(-nrow)), ]
# Calculating the marginal error for each category
margin <- ((1-humanMeans$precision)*humanMeans$nrow)/sum(humanMeans$nrow)
humanMeans <- data.frame(humanMeans,margin)
 
 
#Plot the category breakdown
library(ggplot2)
# For formating the scales
library(scales) 

ggplot(humanMeans, aes(y=recall, x=reorder(V3,nrow), size=20)) + 
  # The recall symbols
  geom_point(colour="#FF9999", shape = '+', size = 15) + 
  # The percision symbols
  geom_point(aes(y=precision, x=reorder(V3,nrow)), shape = '+', size=15) + 
  # Chart Title
  opts(title="Precision and Recall per Categories") + 
  # X-Axis Titles
  scale_x_discrete(name="Categories (by Frequency)") + 
  # Y-Axis
  scale_y_continuous(name="Recall & Precision", labels=percent) +
  # Legend
  #opts(legend.position="none") + 
  # Flip 
  coord_flip()

pdfFile <-c("/path/to/files/outputFile.pdf")
pdf(pdfFile)

ggsave(pdfFile)