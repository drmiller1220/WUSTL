setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\WUSTL\\HW5")

### Problem 1

library(stm)

nyt_dtm <- read.csv("C:\\Users\\drmiller1220\\Documents\\GitHub\\WUSTL\\HW3\\unigram_dm.csv", header = TRUE)
nyt_dtm_md <- subset(nyt_dtm, select = c(Story.Title, Story.Desk))
nyt_dtm <- subset(nyt_dtm, select = -c(Story.Title, Story.Desk))

nyt_dtm_words <- data.frame(colnames(nyt_dtm), 1:1000)

stm_list <- list()
for(i in 1:dim(nyt_dtm)[1]){
  words <- nyt_dtm_words[,2]
  counts <- nyt_dtm[i,]
  each_word <- as.matrix(rbind(words, counts))
  each_word <- unname(each_word)
  each_word <- each_word[, which(each_word[2,]>0)]
  stm_list[[i]] <- each_word
}

stm_model <- stm(stm_list, vocab = colnames(nyt_dtm), K=8, prevalence = ~nyt_dtm_md$Story.Desk)
labelTopics(stm_model)
# topic 1 looks to be a "legal" topic, with FREX words like "police," "court," and "lawyer"
# topic 2 looks to be a "football" topic with FREX words like "yard" and "jet"
# topic 3 looks to be a "political" category with FREX words like "republican" and "democrat"
# topic 4 looks to be an international business category with FREX words like "china" and "market"
# topic 5 looks to be a foreign policy topic with FREX words like "iraq", "muslim," and "american"
# topic 6 looks to be an entertainment topic with FREX words like "music" and "film"
# topic 7 looks to be a medical and health category with FREX words like "health" and "cancer"
# topic 8 looks to be a baseball topic with FREX words like "contract" and "yankee"
colSums(stm_model$theta)
# the distribution among topics are approximately 0.31, 0.30, 0.43, 0.33, 0.41, 0.64, 0.28, and 0.18

lda_model <- stm(stm_list, vocab = colnames(nyt_dtm), K=8)
labelTopics(lda_model)
# topic 1 looks to be a "legal" topic, with FREX words like "justic," "court," and "lawyer"
# topic 2 looks to be an entertainment topic with FREX words like "music" and "art"
# topic 3 is ambiguous; maybe TV given FREX word "television" and "hour"?
# topic 4 is also ambiguous; maybe either entertainment with "film" or health with "patient"?
# topic 5 looks to be a baseball topic with FREX words like "player" and "yankee"
# topic 6 looks to be a "political" category with FREX words like "republican" and "democrat"
# topic 7 looks to be a foreign policy topic with FREX words like "iraq", "muslim," and "american"
# topic 8 looks to be a business category with FREX words like "employee" and "price"
colSums(lda_model$theta)
# the distribution among topics are approximately 0.24, 0.34, 0.51, 0.32, 0.35, 0.48, 0.28, and 0.37

# there are some similaries and differences across models.  both models have some common topics,
# like legal and political, buth they also have differences (such as STM having a football topic).
# while the topics in STM could be identified with the FREX words, some of the categories in LDA
# were ambiguous.  While some proportions of categories across documents were similar, such as political
# (0.41 in STM and 0.48 in LDA), some were very different, such as baseball (0.18 vs. 0.35).  The
# difference between STM and LDA for baseball appears to be a result of LDA including all sports
# under the baseball category, whereas STM had separate categories for baseball and football.  The 
# lack of clarity for some of the categories, as well as the differences in the baseball categories,
# would suggest that the model performed better when we included the story desk as a prevalence
# covariate

### Problem 2

# section 1

# looking at the slides from the 5th lecture, we know that the covariance matrix sigma (capitalized)
# time the eigenvector for component i is equal to eigenvalue i times the eigenvector for
# component i.  the eigenvector describes the loadings for each observation on component i, and
# the eigenvalue describes the weight assigned to component i (e.g. the proportion of variance
# explained by component i).  Thus, if the variance of the eigenvalues is large, then we know
# that we have some large eigenvalues and some small eigenvalues.  This would suggest that some
# eigenvalues are sufficiently large to explain the data with a small number of dimensions.  Conversely,
# if the variance of the eigenvalues is small, then they are all approximately the same value, and
# trying to summarize the data with only a few dimensions would not summarize the data well. Thus,
# as Will Wise says, as the variance of the eigenvalues increases, this means that some
# eigenvalues are very large and thus can summarize the data well in a few dimensions

###

library(glmnet)

mach_dtm <- read.csv("unigram_dm.csv", header = TRUE)
mach_dtm_md <- subset(mach_dtm, select = c(Name))
mach_dtm <- subset(mach_dtm, select = -c(Name))

# normalizing counts in dtm
mach_norm<- mach_dtm
for(z in 1:nrow(mach_norm)){
  mach_norm[z,]<- mach_norm[z,]/sum(mach_norm[z,])
}

## section 2

mach_pca <- prcomp(mach_norm, scale=TRUE)
screeplot(mach_pca, type="l", npcs=10, ylim=c(0,12))
# looking at the plot of variance explained by each additional component, we don't see a large 
# degree of movement as we move left to right; the drops in variance decrease with each
# component added, but these drops are very slight.  This suggests that each component is
# describing a subjectively large amount of variance, such that the documents are not well-
# summarized by a low-dimensional scaling

plot(x=mach_pca$x[,1], y=mach_pca$x[,2], pch="", ylab="Second Dimension", xlab="First Dimension",
     main="Plot of First and Second Principal\n Components of The Prince")
text(x=mach_pca$x[,1], y=mach_pca$x[,2], 
     label=rownames(mach_dtm), cex=0.6)

rev(sort(mach_pca$rotation[,1]))[1:20] # 20 words with strongest positive relationship with 1st dim
rev(sort(mach_pca$rotation[,2]))[1:20] # 20 words with strongest positive relationship with 2st dim

# from the plot of the documents against the 1st and 2nd principal components, we see that the first
# dimension appears to be more discriminating for the documents than the first, as the distribution
# of documents along the first dimension is less sparse than along the second dimension.  Looking
# at the words associated with each dimension, we see that the words in the first dimension seems
# to relate to the qualities of a leader and interpersonal relations, while the words in the second
# dimension relate more directly to the art of statecraft itself.  Thus, this suggests that the
# ways to summarize the documents that campture the most variance relate to the degree to which
# they discuss the qualities of the leader and the art of statecraft in more generic terms
### Problem 3

mach_eu <- as.matrix(dist(mach_norm, method="euclidian")) #finding euclidian distance of all
                                                          # documents

mach_mds <- cmdscale(mach_eu, k=2)

mach_mds_pca <- prcomp(mach_mds, scale=FALSE)
# we do not include a screeplot here because we are limiting ourselves to 2 dimensions

plot(x=mach_mds_pca$x[,1], y=mach_mds_pca$x[,2], pch="", ylab="Second Dimension", 
     xlab="First Dimension",
     main="Plot of First and Second Principal\n Components of The Prince")
text(x=mach_mds_pca$x[,1], y=mach_mds_pca$x[,2], 
     label=rownames(mach_dtm), cex=0.6)

cor(mach_pca$x[,1], mach_mds_pca$x[,1]) # correlation between embeddings is -0.74, so there is a
# strong negative correlation; this means that the dimensions are strongly related, but the way
# that the PCA algorithm calculated the locations of the documents, the substantive meanings of
# the first dimension are reversed

###

mach_man <- as.matrix(dist(mach_norm, method="manhattan")) #finding euclidian distance of all
# documents

mach_mds_man <- cmdscale(mach_man, k=2)

mach_mds_man_pca <- prcomp(mach_mds_man, scale=FALSE)
# we do not include a screeplot here because we are limiting ourselves to 2 dimensions

plot(x=mach_mds_man_pca$x[,1], y=mach_mds_man_pca$x[,2], pch="", ylab="Second Dimension", 
     xlab="First Dimension",
     main="Plot of First and Second Principal\n Components of The Prince")
text(x=mach_mds_man_pca$x[,1], y=mach_mds_man_pca$x[,2], 
     label=rownames(mach_dtm), cex=0.6)

cor(mach_pca$x[,1], mach_mds_man_pca$x[,1]) # correlation between embeddings is -0.88, so there is again
# a strong negative correlation; this means that the dimensions are strongly related, but the way
# that the PCA algorithm calculated the locations of the documents, the substantive meanings of
# the first dimension are reversed

# these results suggest that PCA and MDS are similar, as they both yielded 1st dimensions
# that summarized the data in very similar ways (though with reversed signs)
