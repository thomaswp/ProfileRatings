library('readr')
library('plyr')
library('ggplot2')
library('reshape2')

se <- function(x) sqrt(var(x)/length(x))

gender <- read_csv("gender.dat", col_names = FALSE, col_types = cols(X2 = col_character()))
colnames(gender) <- c("id", "gender")

ratings <- read_csv("~/GitHub/Rankings/ratings.dat", col_names = FALSE)
colnames(ratings) <- c("source", "target", "rating")

maleIDs <- gender$id[gender$gender == "M"]
femaleIDs <- gender$id[gender$gender == "F"]
allIDs <- c(maleIDs, femaleIDs)

ratings <- ratings[ratings$source %in% allIDs & ratings$target %in% allIDs,]

mfRatings <- ratings[ratings$source %in% maleIDs & ratings$target %in% femaleIDs,]
fmRatings <- ratings[ratings$source %in% femaleIDs & ratings$target %in% maleIDs,]

cut <- 15

maleRaters <- ddply(mfRatings, c("source"), summarize, n=length(rating), mRating=mean(rating))
maleRaters <- maleRaters[maleRaters$n > cut,]
maleRated <- ddply(fmRatings, c("target"), summarize, n=length(rating), mRated=mean(rating))
maleRated <- maleRated[maleRated$n > cut,]

males <- merge(maleRaters, maleRated, by.x="source", by.y="target", all=F)
colnames(males) <- c("id", "nRating", "mRating", "nRated", "mRated")
cor(males$mRating, males$mRated)

femaleRaters <- ddply(fmRatings, c("source"), summarize, n=length(rating), mRating=mean(rating))
femaleRaters <- femaleRaters[femaleRaters$n > cut,]
femaleRated <- ddply(mfRatings, c("target"), summarize, n=length(rating), mRated=mean(rating))
femaleRated <- femaleRated[femaleRated$n > cut,]

females <- merge(femaleRaters, femaleRated, by.x="source", by.y="target", all=F)
colnames(females) <- c("id", "nRating", "mRating", "nRated", "mRated")
cor(females$mRating, females$mRated)

mfRatings <- merge(mfRatings, females[,c("id", "mRated")], by.x="target", by.y="id", all=F)
fmRatings <- merge(fmRatings, males[,c("id", "mRated")], by.x="target", by.y="id", all=F)

mfRatings$bias <- mfRatings$rating - mfRatings$mRated
fmRatings$bias <- fmRatings$rating - fmRatings$mRated

# > quantile(males$mRated)
# 0%       25%       50%       75%      100% 
# 1.000000  5.304348  6.729443  8.215909 10.000000 
# > quantile(females$mRated)
# 0%       25%       50%       75%      100% 
# 1.000000  3.780488  5.272727  6.845126 10.000000 

maleBias <- ddply(mfRatings, c("source"), summarize, mBias=mean(bias), 
                  mBias0=mean(bias[mRated < 3.8]), mBias1=mean(bias[mRated >= 3.8 & mRated < 5.3]),
                  mBias2=mean(bias[mRated >= 5.3 & mRated < 6.8]), mBias3=mean(bias[mRated >= 6.8]))
femaleBias <- ddply(fmRatings, c("source"), summarize, mBias=mean(bias), 
                    mBias0=mean(bias[mRated < 5.3]), mBias1=mean(bias[mRated >= 5.3 & mRated < 6.7]),
                    mBias2=mean(bias[mRated >= 6.7 & mRated < 8.2]), mBias3=mean(bias[mRated >= 8.2]))

males <- merge(males, maleBias, by.x="id", by.y="source", all=F)
females <- merge(females, femaleBias, by.x="id", by.y="source", all=F)

males$gender <- "M"
females$gender <- "F"

all <- rbind(males, females)
all$mRatedInt <- round(all$mRated)

bins <- ddply(all, c("gender", "mRatedInt"), summarize, n=length(mBias), seBias=se(mBias), mBias=mean(mBias),
              mBias0=mean(mBias0,na.rm=T), mBias1=mean(mBias1,na.rm=T), mBias2=mean(mBias2,na.rm=T), mBias3=mean(mBias3,na.rm=T))

ggplot(bins, aes(x=mRatedInt, y=mBias, color=gender)) + geom_line() + geom_errorbar(aes(ymin=mBias-seBias, ymax=mBias+seBias))

melted <- melt(bins[,c(-3:-5)], id=c("gender", "mRatedInt"))
ggplot(melted[melted$gender=="M",], aes(x=mRatedInt, y=value, color=variable)) + geom_line()
