# change to the new directory
setwd("/path/to/files/")

# Load a predefine file
#cats <- read.table("/path/to/files/fileName.tsv", header=FALSE, sep="\t", comment.char = "", , quote = "")
# Or choose the file to load
cats <- read.table(file.choose(), header=FALSE, sep="\t", comment.char = "", quote = "")

# Adding Match counter (0|1) based on matching rule (it can be also comparing V3 and V4)
cats$Match[cats$V2=='true'] <- 1.0
cats$Match[cats$V2=='false'] <- 0

# Adding Significant counter (0|1) based on matching rule (it can be if V4 is null)
cats$Sig[cats$V1=='1.0'] <- 1.0
cats$Sig[cats$V1=='null'] <- 0

recall <- mean(cats$Match)
precision <- sum(cats$Match)/sum(cats$Sig)

# Calculating the recall of lower level categories (not the root = '15')
lowLevelRecall <- mean(cats[cats$V3 != '15',5])

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

# Plotting the frequencies
ggplot(humanMeans, aes(y=nrow, x=reorder(V3,nrow))) + 
  geom_bar() +
  opts(title="Categories Frequencies") + 
  scale_x_discrete(name="Categories (by Frequency)") + 
  scale_y_log10(name="# cases", breaks = c(10,100,1000,10000,100000)) +
  coord_flip()
  
# Plotting the recall and percision of each category
ggplot(humanMeans, aes(y=recall, x=reorder(V3,nrow))) + 
  # The recall symbols
  geom_point(colour="#FF9999", shape = '+', aes(size = nrow)) + 
  stat_smooth(aes(group = 1),colour="#FF9999", method ="loess", se = FALSE) +
  geom_hline(yintercept=recall, colour="#FF9999") +
  # The percision symbols
  geom_point(aes(y=precision, x=reorder(V3,nrow),size = nrow), shape = '+') + 
  geom_text(aes(label=sprintf("%.1f", precision*100), size=11, hjust = -1)) +
  stat_smooth(aes(y=precision, group = 1),colour="#000000", method ="loess", se = FALSE) +
  scale_size(trans = log_trans(), range = c(1,15)) + 
  geom_hline(yintercept=precision) +
  # Text of the size of each label
  geom_text(aes(y=-0.02, label=nrow)) +
  # Chart Title
  opts(title="Precision and Recall per Categories") + 
  # X-Axis Titles
  scale_x_discrete(name="Categories (by Frequency)") + 
  # Y-Axis
  scale_y_continuous(name="Recall & Precision", labels=percent, breaks = c(recall, precision, seq(from = 0, to= 1.0, by = 0.2))) +
  # Legend
  opts(legend.position="none") + 
  # Flip 
  coord_flip()

pdfFile <-c("outputFile.pdf")
pdf(pdfFile)

ggsave(pdfFile)