
OC1 <- read.delim(file.choose(), header=F)


# Extracting the probability of the first category
OC1$Prob1[OC1$V6=='null'] <- 0
OC1$Prob1[OC1$V6!='null'] <- as.double(sub("(.*?)([0-9.E-]+)(.*)","\\2",OC1$V6[OC1$V6!='null']))

# Normalizing the match data
OC1$Match[OC1$V2=='true'] <- 1
OC1$Match[OC1$V2=='false'] <- 0

par(mfrow=c(3,6))

for (cat in unique(OC1$V4))
{
  cati <- subset(OC1, V4==cat)
  boxplot(Prob1~V2,data=cati,varwidth=TRUE, main=paste(cat,round(100*length(cati$V4)/length(OC1$V4), 2), "%", sep=" "))
}

# plot a ROC curve for a single prediction run
# and color the curve according to cutoff.
library(ROCR)
library(verification)

par(mfrow=c(3,6))

for (cat in unique(OC1$V4))
{
  cati <- subset(OC1, V4==cat)
  
  #cati <- subset(OC1, V4=='quality')
  pred <- prediction(cati$Prob1, cati$Match)
# ROC Curve  
#  perf <- performance(pred,"tpr", "fpr")  
# Percision / Recall chart
  perf <- performance(pred,"prec", "rec")
  plot(perf,colorize = TRUE, main=paste(cat,round(100*length(cati$V4)/length(OC1$V4), 2), "%", sep=" "))
  (auc <- as.numeric(performance(pred, measure = "auc", x.measure = "cutoff")@y.values))
  roc.plot(cati$Match,cati$Prob1, xlab = "False positive rate", ylab = "True positive rate", main = auc, CI = T, n.boot = 100, plot = "both", binormal = TRUE)
  
}

# Testing the second guess

# Extracting the second label and its probability
OC1$Cat2[OC1$V6!='null'] <- sub("(.+?,)([a-z ]*)(=.*)","\\2",OC1$V6[OC1$V6!='null'])
OC1$Prob2[OC1$V6=='null'] <- 0
OC1$Prob2[OC1$V6!='null'] <- as.double(sub(".*?(,[a-z ]*=([0-9.E-]*)).*","\\2",OC1$V6[OC1$V6!='null']))

OC1$isCat2 <- 0 
OC1$isCat2[tolower(OC1$V3) == OC1$Cat2] <- 1 

# Extracting the third label and its probability
OC1$Cat3[OC1$V6!='null'] <- sub(".*?(,.*?,([a-z ]*)=[0-9\\.]*).*","\\2",OC1$V6[OC1$V6!='null'])
OC1$Prob3[OC1$V6=='null'] <- 0
OC1$Prob3[OC1$V6!='null'] <- as.double(sub(".*?(,.*?,[a-z ]*=([0-9.E-]*)).*","\\2",OC1$V6[OC1$V6!='null']))

OC1$isCat3 <- 0 
OC1$isCat3[tolower(OC1$V3) == OC1$Cat3] <- 1 


paste(sprintf("%.3f",(mean(OC1$Match))*100), '% were 1st Guess / ', 
      sprintf("%.3f",(mean(OC1$isCat2))*100), '% were 2nd Guess / ',
      sprintf("%.3f",(mean(OC1$isCat3))*100), '% were 3rd Guess', sep="")

OC1$Ratio <- OC1$Prob2/OC1$Prob1
OC1$Diff <- OC1$Prob1 - OC1$Prob2


mean(OC1$Ratio[!is.na(OC1$Ratio) & OC1$Match == 1])
mean(OC1$Ratio[!is.na(OC1$Ratio) & OC1$isCat2 == 1])
hist(OC1$Ratio[!is.na(OC1$Ratio) & OC1$isCat2 == 1])
hist(OC1$Ratio[!is.na(OC1$Ratio) & OC1$Match == 1 & OC1$Ratio > 0.2 & OC1$Ratio < 1], breaks =8)

# How to extract the score of a specific category
catV <- as.double(sub(paste(".*",cat,"=([0-9.E-]+).*", sep=""),"\\1",OC1$V6))

# Writing output file of the new normalized table
write.table(toUpload, "OC1.tsv", sep="\t")