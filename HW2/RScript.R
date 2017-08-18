setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\WUSTL\\HW2")

unigram_dm <- read.csv("unigram_dm.csv", header=TRUE)
trigram_dm <- read.csv("trigram_dm.csv", header=TRUE)

##############################################################
# linear discrimination
# unigrams

sessions_u <- unigram_dm[which(unigram_dm$Statement.Author=="Sessions"),]
sessions_u <- subset(sessions_u, select = -Statement.Author)
# converting frequency counts to rates
for(i in 1:nrow(sessions_u)){
  sum_words <- sum(sessions_u[i,])
  sessions_u[i,] <- sessions_u[i,]/sum_words
}

shelby_u <- unigram_dm[which(unigram_dm$Statement.Author=="Shelby"),]
shelby_u <- subset(shelby_u, select = -Statement.Author)
# converting frequency counts to rates
for(i in 1:nrow(shelby_u)){
  sum_words <- sum(shelby_u[i,])
  shelby_u[i,] <- shelby_u[i,]/sum_words
}

sessions_u_mu <- colMeans(sessions_u)
sessions_u_var <- apply(sessions_u, 2, FUN = function(x) var(x))
shelby_u_mu <- colMeans(shelby_u)
shelby_u_var <- apply(shelby_u, 2, FUN = function(x) var(x))

lda_u_weight <- (sessions_u_mu - shelby_u_mu) / (sessions_u_var + shelby_u_var)

lda_u_weight_sorted <- rev(sort(lda_u_weight))

lda_u_sessions_topwords <- lda_u_weight_sorted[1:20]
lda_u_shelby_topwords <- lda_u_weight_sorted[(length(lda_u_weight_sorted)-19):length(lda_u_weight_sorted)]

# trigrams

sessions_t <- trigram_dm[which(trigram_dm$Statement.Author=="Sessions"),]
sessions_t <- subset(sessions_t, select = -Statement.Author)
# converting frequency counts to rates
for(i in 1:nrow(sessions_t)){
  sum_words <- sum(sessions_t[i,])
  sessions_t[i,] <- sessions_t[i,]/sum_words
}

shelby_t <- trigram_dm[which(trigram_dm$Statement.Author=="Shelby"),]
shelby_t <- subset(shelby_t, select = -Statement.Author)
# converting frequency counts to rates
for(i in 1:nrow(shelby_t)){
  sum_words <- sum(shelby_t[i,])
  shelby_t[i,] <- shelby_t[i,]/sum_words
}

sessions_t_mu <- colMeans(sessions_t, na.rm = TRUE)
sessions_t_var <- apply(sessions_t, 2, FUN = function(x) var(x, na.rm = TRUE))
shelby_t_mu <- colMeans(shelby_t, na.rm = TRUE)
shelby_t_var <- apply(shelby_t, 2, FUN = function(x) var(x, na.rm = TRUE))

lda_t_weight <- (sessions_t_mu - shelby_t_mu) / (sessions_t_var + shelby_t_var)

lda_t_weight_sorted <- rev(sort(lda_t_weight))

lda_t_sessions_topwords <- lda_t_weight_sorted[1:20]
lda_t_shelby_topwords <- lda_t_weight_sorted[(length(lda_t_weight_sorted)-19):length(lda_t_weight_sorted)]

##############################################################
# standardized mean difference
# unigrams

diff_means_u <- sessions_u_mu - shelby_u_mu
stddev_diff_u <- sqrt((sessions_u_var/dim(sessions_u)[1]) + (shelby_u_var/dim(shelby_u)[1]))
stand_diff_means_u <- diff_means_u/stddev_diff_u
stand_diff_means_u_sorted <- rev(sort(stand_diff_means_u))

stand_diff_means_u_sessions_topwords <- stand_diff_means_u_sorted[1:20]
stand_diff_means_u_shelby_topwords <- stand_diff_means_u_sorted[(length(stand_diff_means_u_sorted)-19):length(stand_diff_means_u_sorted)]

# trigrams

diff_means_t <- sessions_t_mu - shelby_t_mu
stddev_diff_t <- sqrt((sessions_t_var/dim(sessions_t)[1]) + (shelby_t_var/dim(shelby_t)[1]))
stand_diff_means_t <- diff_means_t/stddev_diff_t
stand_diff_means_t_sorted <- rev(sort(stand_diff_means_t))

stand_diff_means_t_sessions_topwords <- stand_diff_means_t_sorted[1:20]
stand_diff_means_t_shelby_topwords <- stand_diff_means_t_sorted[(length(stand_diff_means_t_sorted)-19):length(stand_diff_means_t_sorted)]

##############################################################
# standardized log odds
# unigrams

sessions_u_total <- sum(colSums(sessions_u))
sessions_u_pi <- colSums(sessions_u + 1) / (sessions_u_total + (dim(sessions_u)[2] - 1))

shelby_u_total <- sum(colSums(shelby_u))
shelby_u_pi <- colSums(shelby_u + 1) / (shelby_u_total + (dim(shelby_u)[2] - 1))

logodds_u <- log(sessions_u_pi / (1-sessions_u_pi)) - log(shelby_u_pi / (1 - shelby_u_pi))
varlogodds_u <- (1/(colSums(sessions_u)+1)) + (1/(colSums(shelby_u)+1))

standardized_logodds_u <- logodds_u / sqrt(varlogodds_u)

standardized_logodds_u_sorted <- rev(sort(standardized_logodds_u))

standardized_logodds_u_sessions_topwords <- standardized_logodds_u_sorted[1:20]
standardized_logodds_u_shelby_topwords <- standardized_logodds_u_sorted[(length(standardized_logodds_u_sorted)-19):length(standardized_logodds_u_sorted)]


# trigrams

sessions_t_total <- sum(colSums(sessions_t, na.rm = TRUE))
sessions_t_pi <- colSums(sessions_t + 1, na.rm = TRUE) / (sessions_t_total + (dim(sessions_t)[2] - 1))

shelby_t_total <- sum(colSums(shelby_t, na.rm = TRUE))
shelby_t_pi <- colSums(shelby_t + 1, na.rm = TRUE) / (shelby_t_total + (dim(shelby_t)[2] - 1))

logodds_t <- log(sessions_t_pi / (1-sessions_t_pi)) - log(shelby_t_pi / (1 - shelby_t_pi))
varlogodds_t <- (1/(colSums(sessions_t, na.rm = TRUE)+1)) + (1/(colSums(shelby_t, na.rm = TRUE)+1))

standardized_logodds_t <- logodds_t / sqrt(varlogodds_t)

standardized_logodds_t_sorted <- rev(sort(standardized_logodds_t))

standardized_logodds_t_sessions_topwords <- standardized_logodds_t_sorted[1:20]
standardized_logodds_t_shelby_topwords <- standardized_logodds_t_sorted[(length(standardized_logodds_t_sorted)-19):length(standardized_logodds_t_sorted)]

##################
# word separating plots

# linear discriminant analysis

plot(c(lda_u_sessions_topwords,lda_u_shelby_topwords), 1:40, pch="", 
     xlab="Weight (Sessions <--> Shelby)", ylab="", yaxt="n",
     main="Top 40 Discriminating unigrams\n using linear discriminant analysis")
text(c(lda_u_sessions_topwords,lda_u_shelby_topwords), 1:40, 
     label=names(c(lda_u_sessions_topwords,lda_u_shelby_topwords)), cex=0.6)

plot(c(lda_t_sessions_topwords,lda_t_shelby_topwords), 1:40, pch="", xlim=c(-100,20),
     xlab="Weight (Sessions <--> Shelby)", ylab="", yaxt="n",
     main="Top 40 Discriminating trigrams\n using linear discriminant analysis")
text(c(lda_t_sessions_topwords,lda_t_shelby_topwords), 1:40, 
     label=names(c(lda_t_sessions_topwords,lda_t_shelby_topwords)), cex=0.6)

# standard mean difference

plot(c(stand_diff_means_u_sessions_topwords, stand_diff_means_u_shelby_topwords), 1:40, pch="", 
     xlab="Weight (Sessions <--> Shelby)", ylab="", yaxt="n",
     main="Top 40 Discriminating unigrams\n using standard mean difference")
text(c(stand_diff_means_u_sessions_topwords, stand_diff_means_u_shelby_topwords), 1:40, 
     label=names(c(stand_diff_means_u_sessions_topwords, stand_diff_means_u_shelby_topwords)), cex=0.6)

plot(c(stand_diff_means_t_sessions_topwords, stand_diff_means_t_shelby_topwords), 
     1:40, pch="", xlim=c(-40,20),
     xlab="Weight (Sessions <--> Shelby)", ylab="", yaxt="n",
     main="Top 40 Discriminating trigrams\n using standard mean difference")
text(c(stand_diff_means_t_sessions_topwords, stand_diff_means_t_shelby_topwords), 1:40, 
     label=names(c(stand_diff_means_t_sessions_topwords, stand_diff_means_t_shelby_topwords)), cex=0.6)

# standardized log odds

plot(c(standardized_logodds_u_sessions_topwords, standardized_logodds_u_shelby_topwords), 1:40, pch="", 
     xlab="Weight (Sessions <--> Shelby)", ylab="", yaxt="n",
     main="Top 40 Discriminating unigrams\n using standardized log odds ratio")
text(c(standardized_logodds_u_sessions_topwords, standardized_logodds_u_shelby_topwords), 1:40, 
     label=names(c(standardized_logodds_u_sessions_topwords, standardized_logodds_u_shelby_topwords)), cex=0.6)

plot(c(standardized_logodds_t_sessions_topwords, standardized_logodds_t_shelby_topwords), 
     1:40, pch="", xlim=c(-4.0, 0),
     xlab="Weight (Sessions <--> Shelby)", ylab="", yaxt="n",
     main="Top 40 Discriminating trigrams\n using standardized log odds ratio")
text(c(standardized_logodds_t_sessions_topwords, standardized_logodds_t_shelby_topwords), 1:40, 
     label=names(c(standardized_logodds_t_sessions_topwords, standardized_logodds_t_shelby_topwords)), cex=0.6)

#############

### Across the three discrimination methods, there does not seem to a great deal of consistency.
### For unigrams, we see a few that are discriminating for Sessions (Shelby) for both the log-
### odds and SMD, including "offic" and "x2005" (but none for Shelby).  Little seems to be
### similar across the LDA and SMD methods.  Turning to LDA vs. log-odds, we see some common
### discriminating words, but they are discriminating for the opposite senators (i.e. in LDA
### academia is most discriminating for Sessions, but in log-odds is most discriminating for
### Shelby).  I checked for obvious errors in my code that would have caused this result, but
### did not locate any easy-to-spot ones, but this dissimilarity is likely a result of an
### error somewhere.

### Looking to the trigrams, we see that they tend to be a little more consistently
### discriminating.  Looking at LDA and SMD, we see that in both the trigrams "washington..senate"
### and "offer..senate" are discriminating for Shelby (but nothing is in common for Sessions).
### Looking at LDA and SMD, we see even more commonality for Shelby and Sessions, as the same
### Shelby common trigrams are present as well as some for Sessions, such as "news.rekeas.announc"
### thus, while some error likely led to not wholly similar results across methods, the trigrams
### results are more consistent than those for the unigrams.

###################################################
### document similarity

set.seed(1)
sessions_sample <- sessions_t[sample(1:236, 200),]
shelby_sample <- shelby_t[sample(1:1102, 200),]

all_sample <- rbind(sessions_sample, shelby_sample)
all_sample <- all_sample[,-c(which(colSums(all_sample, na.rm = TRUE)==0))]
all_sample <- all_sample[complete.cases(all_sample),]

eu_distances <- matrix(nrow = dim(all_sample)[1], ncol = dim(all_sample)[1])

for(i in 1:dim(all_sample)[1]){
  for(j in 1:dim(all_sample)[1]){
    if(is.na(eu_distances[i,j])==TRUE){
      dist <- sqrt(sum((all_sample[i,]-all_sample[j,])^2))
      eu_distances[i,j] <- dist
      eu_distances[j,i] <- dist
    }
    print(paste(i, j))
  }
}

write.csv(eu_distances, "eu_distances.csv")

which(eu_distances == min(eu_distances, na.rm = TRUE), arr.ind = TRUE)
which(eu_distances == max(eu_distances, na.rm = TRUE), arr.ind = TRUE)

###

eu_distances_tfidf <- matrix(nrow = dim(all_sample)[1], ncol = dim(all_sample)[1])

idf <- apply(all_sample, 2, FUN= function(x) log(dim(eu_distances_tfidf)[1]/length(which(x>0))))

all_sample_idf <- as.matrix(t(apply(all_sample, 1, function(x) x*idf)))

eu_distances_tfidf <- as.matrix(dist(all_sample_idf, method="euclidian"))

write.csv(eu_distances_tfidf, "eu_distances_tfidf.csv")

which(eu_distances_tfidf == min(eu_distances_tfidf, na.rm = TRUE), arr.ind = TRUE)
which(eu_distances_tfidf == max(eu_distances_tfidf, na.rm = TRUE), arr.ind = TRUE)

###

cosine_sim <- matrix(nrow = dim(all_sample)[1], ncol = dim(all_sample)[1])

for(i in 1:dim(all_sample)[1]){
  for(j in 1:dim(all_sample)[1]){
    if(is.na(cosine_sim[i,j])==TRUE){
      cosine <- sum(all_sample[i,]*all_sample[j,])/sqrt(sum(all_sample[i,]^2)*sum(all_sample[j,]^2))
      cosine_sim[i,j] <- cosine
      cosine_sim[j,i] <- cosine
    }
    print(paste(i, j))
  }
}

write.csv(cosine_sim, "cosine_sim.csv")

which(cosine_sim == min(cosine_sim, na.rm = TRUE), arr.ind = TRUE)
which(cosine_sim == max(cosine_sim, na.rm = TRUE), arr.ind = TRUE)

###

cosine_sim_tfidf <- matrix(nrow = dim(all_sample)[1], ncol = dim(all_sample)[1])

for(i in 1:dim(all_sample_idf)[1]){
  for(j in 1:dim(all_sample_idf)[1]){
    if(is.na(cosine_sim_tfidf[i,j])==TRUE){
      cosine <- sum(all_sample_idf[i,]*all_sample_idf[j,])/sqrt(sum(all_sample_idf[i,]^2)*sum(all_sample_idf[j,]^2))
      cosine_sim_tfidf[i,j] <- cosine
      cosine_sim_tfidf[j,i] <- cosine
    }
    print(paste(i, j))
  }
}

write.csv(cosine_sim_tfidf, "cosine_sim_tfidf.csv")

which(cosine_sim_tfidf == min(cosine_sim_tfidf, na.rm = TRUE), arr.ind = TRUE)
which(cosine_sim_tfidf == max(cosine_sim_tfidf, na.rm = TRUE), arr.ind = TRUE)

###

normalized_wcs <- all_sample
for (i in 1:nrow(normalized_wcs)){
  normalized_wcs[i,]<- normalized_wcs[i,]/sum(normalized_wcs[i,])
  print(i)
}

sigma = 100
gaussian_matrix <- exp(-(as.matrix(dist(normalized_wcs)))/100)

write.csv(gaussian_matrix, "gaussian_matrix.csv")

which(gaussian_matrix == min(gaussian_matrix, na.rm = TRUE), arr.ind = TRUE)
which(gaussian_matrix == max(gaussian_matrix, na.rm = TRUE), arr.ind = TRUE)

###

idf_normalized <- apply(normalized_wcs, 2, FUN = function(x) log(dim(normalized_wcs)[1]/length(which(x>0))))
normalized_tfidf <- as.matrix(t(apply(normalized_wcs, 1, function(x) x*idf_normalized)))
gaussian_matrix_tfidf <- exp(-(as.matrix(dist(normalized_tfidf)))/sigma)

write.csv(gaussian_matrix_tfidf, "gaussian_matrix_tfidf.csv")

which(gaussian_matrix_tfidf == min(gaussian_matrix_tfidf, na.rm = TRUE), arr.ind = TRUE)
which(gaussian_matrix_tfidf == max(gaussian_matrix_tfidf, na.rm = TRUE), arr.ind = TRUE)

#####################################################

### to find the documents in order to do part 3, I needed to use some poor R hacks since I
### did not save the press release names to my document-term matricies.  Generally, I needed
### to find the index of the sampled document, determine which of the two senators' samples
### contained that index, and manually determine which press releases corresponded with those
### indicies.  Indicies of document similarity matricies correspond with those in "all_sample"
### so we can figure out which document each is by finding the rowname of the index of all_sample

### To start with, most of the similarity/dissimilarity lists are very long for each technique,
### which seems to be a consequence of how few trigrams are shared between documents; in most
### cases, documents share 0 or 1 trigrams with other documents, such that many documents are
### "most similar" and many are "most dissimilar."  This is likely due to some error I made
### while parsing the documents in Python.

### Taking the likely flawed data as given, we start with the Euclidian distance matrix.  One
### of the most similar pairs correspond to two of Jeff Sessions' press releases, one on
### October 25, 2007 announcing federal intervention in a souther drought, and one on June
### 28, 2007 announcing the defeat of an immigration bill.  While both press releases have
### positive language (announcing pleasure with the federal intervention, and trumpeting the
### immigration bill's defeat as a victory for the American people), the press releases may
### be evaluated as most similar due to the shared header and/or footer which likely
### generated a shared trigram.  One of the most dissimilar pairs also comes from Jeff Sessions
### and is a press release on February 1, 2007, applauding the passage of a minimum wage
### will, and one on March 22, 2007, announcing the introduction of a bill for a reading program.
### These press releases may likely be unrelated because one speaks to the passage of a bill
### while the other speaks to the introduction of a bill

### The other methods seems to also identify very large numbers of similar and dissimilar
### documents, which again is likely a consequence of errors in parsing resulting in few
### shared trigrams.  However, the euclidian and gaussian methods using tfidf weighting 
### consistently identify one pair of press releases--both from Sessions' office--,
### but for wholly different reasons.  

### One, from June 14, 2007, pertains to the passage of a VA bill out of committee, and the other, from
### July 19, 2005, pertains to the opening of a federally-funded road at a wildlife refuge.
### While the Euclidian tfidf method identifies these documents as most similar, the 
### gaussian tfidf method identifies these doucments as least similar.  Reading the documents,
### there does not seem to be an apparent way to explain this discrepancy.