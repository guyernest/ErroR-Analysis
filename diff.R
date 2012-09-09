# Loading the 2 files for the diff
#OC1 <- read.delim("/path/to/file1.tsv", header=F)
#OC2 <- read.delim("/path/to/file2.tsv", header=F)

# Alternatively you can open file chooser:
OC1 <- read.delim(file.choose(), header=F)
OC2 <- read.delim(file.choose(), header=F)

Match1 <- OC1$V2[OC1$V4 != OC2$V4]
Match2 <- OC2$V2[OC1$V4 != OC2$V4]

Cat2 <- OC2$V4[OC1$V4 != OC2$V4]
Cat1 <- OC1$V4[OC1$V4 != OC2$V4]
CatT <- OC1$V3[OC1$V4 != OC2$V4]

#Subj <- OC1$V11[OC1$V4 != OC2$V4]
#Prop <- OC1$V12[OC1$V4 != OC2$V4]

#diff <- data.frame(CatT, Cat1, Cat2, Match1, Match2, Subj, Prop)
diff <- data.frame(CatT, Cat1, Cat2, Match1, Match2)


paste(nrow(diff), "/", nrow(OC1), '=', sprintf("%.3f",(nrow(diff)/nrow(OC1))*100), '% were changed', sep="")

library(plyr)
diffCats <- ddply(diff, ~CatT, summarise, 
                  originalNull=(length(Cat1[Cat1=='null'])),
                  originalTrue=(length(Match1[Match1=='true'])), 
                  originalFalse=(length(Match1[Match1=='false' & Cat1 != 'null'])), 
                  modifiedNull=(length(Cat2[Cat2=='null'])),
                  
                  modifiedTrue=(length(Match2[Match2=='true'])), 
                  modifiedFalse=(length(Match2[Match2=='false' & Cat2 != 'null'])),
                  nrow=length(Cat1))
diffCats <- diffCats[with(diffCats,order(-nrow)), ]

attach(diffCats)

diffCats$diffNull=originalNull-modifiedNull
diffCats$diffTrue=modifiedTrue-originalTrue
diffCats$diffFalse=modifiedFalse-originalFalse

diffCats
diff[diff$CatT=='12',]

aConf = confMatrix(diff, "CatT", "Cat1")
bConf = confMatrix(diff, "CatT", "Cat2")

# Plot the changes on the confusion Matrix

tile <- ggplot() + 
  geom_tile(aes(x=Actual, y=Predicted,fill=Freq),data=aConf, color="black",size=0.1) + 
  labs(x="Actual",y="Predicted")

tile <- tile + geom_text(aes(x=Actual,y=Predicted, label=Freq),data=subset(aConf, Freq > 0), colour="black", size=4, vjust = -0.2) + 
   scale_fill_gradient(name = "Count", low="grey",high="red")
tile <- tile + geom_text(aes(x=Actual,y=Predicted, label=Freq), data=subset(bConf, Freq > 0), colour="white", size=4, vjust = 1)

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile <- tile + geom_tile(aes(x=Actual,y=Predicted),data=subset(aConf, tolower(as.character(Actual))==as.character(Predicted)), color="green", size=0.5, fill="green", alpha=0)

tile + opts(axis.text.x=theme_text(angle=90))

