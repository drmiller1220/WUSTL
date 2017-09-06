setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\WUSTL\\HW3")

library(e1071)
library(glmnet)

nyt_dtm <- read.csv("unigram_dm.csv", header=TRUE)
nyt_dtm_md <- subset(nyt_dtm, select = c(Story.Title, Story.Desk))
nyt_dtm <- subset(nyt_dtm, select = -c(Story.Title, Story.Desk))

# normalizing counts in dtm
nyt_norm<- nyt_dtm
for(z in 1:nrow(nyt_norm)){
  nyt_norm[z,]<- nyt_norm[z,]/sum(nyt_norm[z,])
}

###########
### objective function on slide 113 of lecture 2
### sum over all documents and all clusters; multiply by cluster indicator; if document i
### in cluster k, take summed squared euclidian distance of each term j between the cluster
### center theta k and the document

obj_func <- function(dtm, kcluster_obj){
  obj_func_value <- NULL
  for(i in 1:length(kcluster_obj$size)){
    for(j in 1:length(kcluster_obj$cluster)){
      if(kcluster_obj$cluster[j]==i){
        value <- sum((dtm[j,]-kcluster_obj$centers[i,])^2)
        obj_func_value <- append(obj_func_value, value)
      }
    }
  }
  total_value <- sum(obj_func_value)
  return(total_value)
}

obj_func_values <- NULL
for(i in 1:(dim(nyt_dtm)[1]-1)){
  k_cluster<- kmeans(nyt_norm, centers = i)
  obj_func_value <- obj_func(nyt_norm, k_cluster)
  obj_func_values <- append(obj_func_values, obj_func_value)
  print(i)
}

plot(y=obj_func_values, x=1:length(obj_func_values), ylab="Objective Function Value", xlab="Number of Clusters",
     main="Objective Function Value for Varying Numbers of Clusters")

## see saved JPEG for plot

### apply k-means with 6 clusters, set seed to replicate

set.seed(89)
k_cluster<- kmeans(nyt_norm, centers = 6)
table(k_cluster$cluster)

### label cluster using computer methods

diff_scores <- NULL
for(i in 1:length(k_cluster$size)){
  cluster_selected <- k_cluster$centers[i,]
  clusters_not <- k_cluster$centers[-i,]
  mean_clusters_not <- apply(k_cluster$centers[-i,], 2, FUN=function(x) mean(x, na.rm = TRUE))
  cluster_diff <- cluster_selected - mean_clusters_not
  diff_scores <- rbind(diff_scores, cluster_diff)
}

rev(sort(diff_scores[1,]))[1:10] # the first cluster seems to have to do with sports, but
                                 # it is not clear exactly what about sports it has to do with
rev(sort(diff_scores[2,]))[1:10] # the second cluster seems to have to do with sports injuries
rev(sort(diff_scores[3,]))[1:10] # the third cluster seems to have to do with the trial surrounding
                                 # the murder of laci peterson
rev(sort(diff_scores[4,]))[1:10] # the fourth cluster seems to have to do with the New York marathon
rev(sort(diff_scores[5,]))[1:10] # the fifth cluster seems to have to do with the election, and
                                 # possibly the pre-election articles
rev(sort(diff_scores[6,]))[1:10] # the sixth cluster seems to have to do with the election,
                                 # and possibly the post-election articles

### label cluster using hand methods
stories_to_read <- NULL
for(i in c(1, 5, 6)){
  stories <- k_cluster$cluster[which(k_cluster$cluster==i)]
  sampled_stories <- sample(as.numeric(names(stories)), 5, replace=FALSE)
  stories_to_read <- rbind(stories_to_read, sampled_stories)
}

# the 2nd, 3rd, and 4th clusters have less than 5 stories, so it isn't necessary to sample them.
# looking at the stories from cluster 1, they appear to be soft news stories; of the 5 stories
# I happened to sample, 2 were entertainment, 1 was religious, 1 was sports, and 1 was medical.
# The 1 story from cluster 2 was a summary of sports transactions.  The two stories from cluster
# 3 were about the Laci Peterson trial.  Of the 4 stories from cluster 4, 3 were about the
# New York marathon and 1 was about sports transactions.  Stories from cluster 5 were pre-election
# coverage, and stories from cluster 6 were post-election coverage.

################################################################
### Part 2

nyt_emo <- read.csv("nyt_emo.csv", header=TRUE, stringsAsFactors = FALSE)
# checking to see if positivity varies pre/post election across all desks; stories after the
# election are slightly more positive in terms of raw numbers, but we fail to reject the
# null hypothesis that stories after the election are more positive (p=0.29)
t.test(as.numeric(nyt_emo$Positivity[which(nyt_emo$After.Election==0)]),
       as.numeric(nyt_emo$Positivity[which(nyt_emo$After.Election==1)]))

mean(as.numeric(nyt_emo$Positivity[which(nyt_emo$Desk=="Business/Financial Desk")]))
mean(as.numeric(nyt_emo$Positivity[which(nyt_emo$Desk=="The Arts/Cultural Desk")]))
mean(as.numeric(nyt_emo$Positivity[which(nyt_emo$Desk=="National Desk")]))
mean(as.numeric(nyt_emo$Positivity[which(nyt_emo$Desk=="Foreign Desk")]))
mean(as.numeric(nyt_emo$Positivity[which(nyt_emo$Desk=="Sports Desk")]))
mean(as.numeric(nyt_emo$Positivity[which(nyt_emo$Desk=="Editorial Desk")]))
mean(as.numeric(nyt_emo$Positivity[which(nyt_emo$Desk=="Health & Fitness")]))
mean(as.numeric(nyt_emo$Positivity[which(nyt_emo$Desk=="Science Desk")]))
mean(as.numeric(nyt_emo$Positivity[which(nyt_emo$Desk=="Metropolitan Desk")]))

# looking descriptively at the positivity values for each of the 9 desks, health and fitness
# and arts and culture are the most positive, and the foreign desk is the least positive

pairwise.t.test(nyt_emo$Positivity, nyt_emo$Desk)

# performing pairwise t-tests, we see that the only distinguishable differences are that the
# business desk, the national desk, and the arts and culture desk are all significantly more
# positive than the foreign desk

###################################################################################
### Part 3

# subsetting on business/financial and national articles

selected_stories_nums <- as.numeric(row.names(nyt_dtm_md[which(nyt_dtm_md$Story.Desk=="Business/Financial Desk" |
                                     nyt_dtm_md$Story.Desk=="National Desk"),]))
selected_stories <- nyt_dtm[selected_stories_nums,]

###

# I spent a long time trying to figure out how to even start going about coding up the naive
# Bayes, and never got far... so I am weak and gave up... particularly because everyone else who
# finished this homework has been unable to figure it out, so that gave me close to zero hope
# of achieving it myself.... :(

# naive_bayes <- function(dtm, categories, alpha, lambda, train_frac){
#   to_train <- sample(1:dim(dtm)[1], train_frac, replace = FALSE)
#   training_data <- dtm[, to_train]
#   training_categories <- categories[, to_train]
#   
# }

###
# using naive Bayes function

desks <- nyt_dtm_md[selected_stories_nums,]$Story.Desk
desks <- as.factor(desks)
desks <- droplevels(desks)

nb_predictions <- NULL
for(i in 1:length(desks)){
  out<- naiveBayes(desks[-i] ~., selected_stories[-i,])
  predicts<- predict(out, as.data.frame(selected_stories[i,]))
  nb_predictions <- append(as.character(predicts), nb_predictions)
}

nb_acc <- sum(nb_predictions == desks)/length(desks) #poor performance of 51%

###

story_sample <- sample(as.numeric(row.names(selected_stories)), dim(selected_stories)[1]/2, replace = FALSE)

y.train<- nyt_dtm_md[story_sample, 2]
x.train<- as.matrix(nyt_dtm[story_sample,])

y.valid<- nyt_dtm_md[story_sample, 2]
x.valid<- as.matrix(nyt_dtm[story_sample,])

y.train <- droplevels(y.train)
y.valid <- droplevels(y.valid)

y.valid_num <- ifelse(y.valid == "National Desk", 1, 0)

### Code from lecture

##cv.glmnet uses a ``cross validation" method to determine a key parameter in LASSO, I strongly recommend using it
##alpha = 1 is the LASSO
##alpha = 0 is Ridge

### Compare to LASSO

train.model <- cv.glmnet(x = x.train, y = y.train, alpha = 1, nfolds=10, family="binomial", type.measure="mse")

predict.new<- predict(train.model, newx = x.valid, s = train.model$lambda.min)

predict.probs<- 1/(1 + exp(-predict.new))
final.pred<- ifelse(predict.probs>0.5, 1, 0)
lasso_acc <- sum(final.pred == y.valid_num)/length(y.valid_num) # 98% accuracy

### Compare to Ridge

train.model <- cv.glmnet(x = x.train, y = y.train, alpha = 0, nfolds=10, family="binomial", type.measure="mse")

predict.new<- predict(train.model, newx = x.valid, s = train.model$lambda.min)

predixct.probs<- 1/(1 + exp(-predict.new))
final.pred<- ifelse(predict.probs>0.5, 1, 0)
ridge_acc <- sum(final.pred == y.valid_num)/length(y.valid_num) # also 98% accuracy

### both LASSO and ridge outperform naive Bayes