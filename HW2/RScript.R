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

###################################################
### document similarity

set.seed(1)
sessions_sample <- sessions_t[sample(1:236, 100),]
shelby_sample <- shelby_t[sample(1:1102, 100),]

all_sample <- rbind(sessions_sample, shelby_sample)
all_sample <- all_sample[,-c(which(colSums(all_sample, na.rm = TRUE)==0))]
