setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\WUSTL\\HW1")

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
###
### Looking at fluctuations in negative and positive tone, we see that negativity is more prevalent 
### at the beginning, middle, and end of the debate.  This may correspond with the candidates
### jockeying for position--t the beginning of the debate they both try to gain the upper
### hand; after settling into an equilibrium, they become negative again in order to gain the
### advantage; and finally, after this midpoint scuffle, they become negative at the end in
### order to strike a knockout blow.  We also see that Obama has a few statements with very high
### (around 50%) proportions of negativity, which may suggest that he was doing poorly and 
### needed to regain momentum (this is supported by the widespread notion that Obama was not
### prepared for the debate and underperformed).  Interestingly, Romney appeared to have kept
### his composure when Obama displayed flashes of anger, refraining from responding with 
### negative language of his own.
