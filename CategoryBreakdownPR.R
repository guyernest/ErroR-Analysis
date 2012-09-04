# change to the new directory
setwd("/Users/Guy/Development/nBA/NLP/")

cats <- read.table("/Users/Guy/Development/nBA/NLP/NonOperationalCategory.tsv", header=TRUE, sep="\t")
 
library(plyr)
humanMeans <- ddply(cats,~Human1,summarise,recall=mean(Match1),precision=sum(Match1)/sum(Sig1),nrow=length(Human1))
# Sorting by frequency
humanMeans <- humanMeans[with(humanMeans,order(-nrow)), ]
# Calculating the marginal error for each category
margin <- ((1-humanMeans$precision)*humanMeans$nrow)/sum(humanMeans$nrow)
humanMeans <- data.frame(humanMeans,margin)
 
 
#Plot the category breakdown
library(ggplot2)

ggplot(humanMeans, aes(y=recall, x=reorder(V3,-nrow), size=20)) + 
  # The recall symbols
  geom_point(colour="#FF9999", shape = '+', size = 15) + 
  # The percision symbols
  geom_point(aes(y=precision, x=reorder(V3,-nrow)), shape = '+', size=15) + 
  # Chart Title
  opts(title="Precision and Recall per Categories") + 
  # Axis Titles
  scale_x_discrete(name="Categories (by Frequency)") + scale_y_continuous(name="Recall & Precision") +
  # Legend
  opts(legend.position="none") + 
  # X-Axis text rotation 
  opts(axis.text.x=theme_text(angle=90))

pdfFile <-c("/Users/Guy/Development/nBA/NLP/PrecisionAndRecall.pdf")
pdf(pdfFile)

ggsave(pdfFile)