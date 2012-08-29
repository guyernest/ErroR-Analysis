# Loading the 2 files for the diff
OC1 <- read.delim("/Users/yeytani/data/insights/2012-08-26_12:17/CompareNonOperationalCategory.all.results.tsv", header=F)
OC2 <- read.delim("/Users/yeytani/data/insights/2012-08-26_12:16/CompareNonOperationalCategory.all.results.tsv", header=F)

# Alternatively you can open file chooser:
#OC1 <- read.delim(file.choose(), header=F)
#OC2 <- read.delim(file.choose(), header=F)

Match1 <- OC1$V2[OC1$V4 != OC2$V4]
Match2 <- OC2$V2[OC1$V4 != OC2$V4]

Cat2 <- OC2$V4[OC1$V4 != OC2$V4]
Cat1 <- OC1$V4[OC1$V4 != OC2$V4]
CatT <- OC1$V3[OC1$V4 != OC2$V4]

Subj <- OC1$V11[OC1$V4 != OC2$V4]
Prop <- OC1$V12[OC1$V4 != OC2$V4]

diff <- data.frame(CatT, Cat1, Cat2, Match1, Match2, Subj, Prop)

paste(nrow(diff), "/", nrow(OC1), '=', format(nrow(diff)/nrow(OC1)))

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
diff[diff$CatT=='Offering',]
