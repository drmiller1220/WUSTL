setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\WUSTL")

debatewords <- read.csv("debatewords.csv", header=TRUE)

debatewords$prop_positive_words <- debatewords$Number.of.positive.words/debatewords$Number.of.non.stop.words
debatewords$prop_negative_words <- debatewords$Number.of.negative.words/debatewords$Number.of.non.stop.words

debatewords$prop_positive_words_lan <- debatewords$Number.of.Lancaster.stemmed.positive.words/debatewords$Number.of.non.stop.words
debatewords$prop_negative_words_lan <- debatewords$Number.of.Lancaster.stemmed.negative.words/debatewords$Number.of.non.stop.words

debatewords$prop_positive_words_port <- debatewords$Number.of.Porter.stemmed.positive.words/debatewords$Number.of.non.stop.words
debatewords$prop_negative_words_port <- debatewords$Number.of.Porter.stemmed.negative.words/debatewords$Number.of.non.stop.words

debatewords$prop_positive_words_snow <- debatewords$Number.of.Snowball.stemmed.positive.words/debatewords$Number.of.non.stop.words
debatewords$prop_negative_words_snow <- debatewords$Number.of.Snowball.stemmed.negative.words/debatewords$Number.of.non.stop.words

layout(matrix(c(1,2,3,4,5,5), nrow=3, byrow=TRUE), 
       heights = c(0.35,0.35,0.3))

plot(y=debatewords$prop_positive_words[which(debatewords$Speaker=="OBAMA")],
     x=debatewords$Statement.Number[which(debatewords$Speaker=="OBAMA")],
     type="p", pch=19, col="dodgerblue", main = "Positive and Negative Word Proportions",
     xlab="Statement Number", ylab="Proportion", ylim=c(0,1), xlim=c(0,167))
points(y=debatewords$prop_negative_words[which(debatewords$Speaker=="OBAMA")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="OBAMA")],
       type="p", pch=3, col="dodgerblue")
points(y=debatewords$prop_positive_words[which(debatewords$Speaker=="LEHRER")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="LEHRER")],
       type="p", pch=19, col="forestgreen")
points(y=debatewords$prop_negative_words[which(debatewords$Speaker=="LEHRER")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="LEHRER")],
       type="p", pch=3, col="forestgreen")
points(y=debatewords$prop_positive_words[which(debatewords$Speaker=="ROMNEY")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="ROMNEY")],
       type="p", pch=19, col="firebrick1")
points(y=debatewords$prop_negative_words[which(debatewords$Speaker=="ROMNEY")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="ROMNEY")],
       type="p", pch=3, col="firebrick1")

plot(y=debatewords$prop_positive_words_lan[which(debatewords$Speaker=="OBAMA")],
     x=debatewords$Statement.Number[which(debatewords$Speaker=="OBAMA")],
     type="p", pch=19, col="dodgerblue", main = "Positive and Negative Word Proportions\n (Lancaster Stemming)",
     xlab="Statement Number", ylab="Proportion", ylim=c(0,1), xlim=c(0,167))
points(y=debatewords$prop_negative_words_lan[which(debatewords$Speaker=="OBAMA")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="OBAMA")],
       type="p", pch=3, col="dodgerblue")
points(y=debatewords$prop_positive_words_lan[which(debatewords$Speaker=="LEHRER")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="LEHRER")],
       type="p", pch=19, col="forestgreen")
points(y=debatewords$prop_negative_words_lan[which(debatewords$Speaker=="LEHRER")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="LEHRER")],
       type="p", pch=3, col="forestgreen")
points(y=debatewords$prop_positive_words_lan[which(debatewords$Speaker=="ROMNEY")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="ROMNEY")],
       type="p", pch=19, col="firebrick1")
points(y=debatewords$prop_negative_words_lan[which(debatewords$Speaker=="ROMNEY")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="ROMNEY")],
       type="p", pch=3, col="firebrick1")

plot(y=debatewords$prop_positive_words_port[which(debatewords$Speaker=="OBAMA")],
     x=debatewords$Statement.Number[which(debatewords$Speaker=="OBAMA")],
     type="p", pch=19, col="dodgerblue", main = "Positive and Negative Word Proportions\n (Porter Stemming)",
     xlab="Statement Number", ylab="Proportion", ylim=c(0,1), xlim=c(0,167))
points(y=debatewords$prop_negative_words_port[which(debatewords$Speaker=="OBAMA")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="OBAMA")],
       type="p", pch=3, col="dodgerblue")
points(y=debatewords$prop_positive_words_port[which(debatewords$Speaker=="LEHRER")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="LEHRER")],
       type="p", pch=19, col="forestgreen")
points(y=debatewords$prop_negative_words_port[which(debatewords$Speaker=="LEHRER")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="LEHRER")],
       type="p", pch=3, col="forestgreen")
points(y=debatewords$prop_positive_words_port[which(debatewords$Speaker=="ROMNEY")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="ROMNEY")],
       type="p", pch=19, col="firebrick1")
points(y=debatewords$prop_negative_words_port[which(debatewords$Speaker=="ROMNEY")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="ROMNEY")],
       type="p", pch=3, col="firebrick1")

plot(y=debatewords$prop_positive_words_snow[which(debatewords$Speaker=="OBAMA")],
     x=debatewords$Statement.Number[which(debatewords$Speaker=="OBAMA")],
     type="p", pch=19, col="dodgerblue", main = "Positive and Negative Word Proportions\n (Snowball Stemming)",
     xlab="Statement Number", ylab="Proportion", ylim=c(0,1), xlim=c(0,167))
points(y=debatewords$prop_negative_words_snow[which(debatewords$Speaker=="OBAMA")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="OBAMA")],
       type="p", pch=3, col="dodgerblue")
points(y=debatewords$prop_positive_words_snow[which(debatewords$Speaker=="LEHRER")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="LEHRER")],
       type="p", pch=19, col="forestgreen")
points(y=debatewords$prop_negative_words_snow[which(debatewords$Speaker=="LEHRER")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="LEHRER")],
       type="p", pch=3, col="forestgreen")
points(y=debatewords$prop_positive_words_snow[which(debatewords$Speaker=="ROMNEY")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="ROMNEY")],
       type="p", pch=19, col="firebrick1")
points(y=debatewords$prop_negative_words_snow[which(debatewords$Speaker=="ROMNEY")],
       x=debatewords$Statement.Number[which(debatewords$Speaker=="ROMNEY")],
       type="p", pch=3, col="firebrick1")

par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("Obama","Romney",
                            "Lehrer", "Positive", "Negative"), lty=c(1,1,1,0,0),
                            pch=c(NA,NA,NA,19,3),
       col = c("dodgerblue","firebrick1","forestgreen","black","black"))

### Questions
### The plots present the positive and negative word proportions for the different kinds of
### stemming or full word analysis.  Each speaker is color-coded as indicated by the legend,
### and dots represent positive word proportions while crosses represent negative word proportions
### Across the plots, we notice that there are more extreme measured proportions when using
### Lancaster stemming than using the other types of stemming (or no stemming at all).  