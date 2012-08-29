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
ggplot(humanMeans, aes(y=recall, x=reorder(Human1,-nrow), size=20)) + geom_point(colour="#FF9999") + 
geom_point(aes(y=precision, x=reorder(Human1,-nrow), size=20)) + opts(title="Precision and Recall per Categories") + 
scale_x_discrete(name="Categories (by Frequency)") +scale_y_continuous(name="Recall & Precision") +
opts(legend.position="none")+ opts(axis.text.x=theme_text(angle=90))

pdfFile <-c("/Users/Guy/Development/nBA/NLP/PrecisionAndRecall.pdf")
pdf(pdfFile)

ggsave(pdfFile)