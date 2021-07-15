# get libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(readr)
library(lubridate)
library(stringr)
library(dplyr)

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes



# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

write_csv(edx, "edx.csv")
write_csv(validation, "validation.csv")

#prepares dataset edx for exploratory data analysis, needs to be applied to validation later on 

# convert timestamp to review_date, extract publication year (pub_year) from title, separate by genre
# 2-step extraction of pub_year to avoid mix-up when a date year is part of the actual movie title
pattern1 <- "\\(\\d{4}\\)"
pattern2 <-"\\d{4}"

edx_work <- edx %>% 
  mutate(review_date = as_datetime(timestamp),pub_year=as.numeric(str_match(str_match(title,pattern1),pattern2)))

write_csv(edx_work, "edx_work.csv")

# In this script, the edx_work dataset is 
# further divided into set edx_work_test (20%) and edx_work_train (80%) using the procedure
# provided for defining edx and validation data sets

# edx_test set will be 20% of edx data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx_work$rating, times = 1, p = 0.2, list = FALSE)
edx_work_train <- edx_work[-test_index,]
temp <- edx_work[test_index,]

# Make sure userId and movieId in edx_test set are also in edx_train
edx_work_test <- temp %>% 
  semi_join(edx_work_train, by = "movieId") %>%
  semi_join(edx_work_train, by = "userId")

# Add rows removed from edx_test set back into edx_train set
removed <- anti_join(temp, edx_work_test)
edx_work_train <- rbind(edx_work_train, removed)

rm(test_index, temp, removed)
write_csv(edx_work_train, "edx_work_train.csv")
write_csv(edx_work_test, "edx_work_test.csv")

## load datasets
edx_work_train <- read_csv("data/edx_work_train.csv")
edx_work_test <- read_csv("data/edx_work_test.csv")
## dataset description
mu <- mean(edx_work_train$rating)
sd <- sd(edx_work_train$rating)
mu
sd
n_distinct(edx_work_train$userId) #number of different users
n_distinct(edx_work_train$movieId) # number of different movies

#pub_year effects are analysed by calculating the mean rating for every publication year in the data.
pub_year_effects <- edx_work_train %>% 
  group_by(pub_year) %>%
  summarize(n_ratings=n(),avg_rating=mean(rating), b_py=mean(rating-mu))

fig_1 <- ggplot(data=pub_year_effects,aes(pub_year,n_ratings))+geom_point() + 
  xlab("publication year") + 
  ylab("total number of ratings") +
  theme_bw()

fig_2 <- ggplot(data=pub_year_effects,aes(pub_year,avg_rating))+geom_point() + 
  xlab("publication year") +
  ylab("average rating") +
  geom_hline(yintercept = mu,colour ="red") +
  geom_hline(yintercept = mu+sd,colour ="red",linetype="33") +
  geom_hline(yintercept = mu-sd,colour ="red",linetype="33") + 
  annotate("text", x = 1920, y = mu + 0.1, label = "AVG", color="red") + 
  annotate("text", x = 1920, y = mu + sd - 0.1, label = "+1 SD", color="red") +
  annotate("text", x = 1920, y = mu - sd + 0.1, label = "-1 SD", color="red") +
  theme_bw()
#print figures
fig_1
fig_2

#calculate mean rating per genre
genre_effects <- edx_work_train %>% 
  group_by(genres) %>% 
  summarize(n_ratings=n(),genre_avg=mean(rating))

fig_3 <- ggplot(data=genre_effects,aes(genres,n_ratings))+geom_point() + 
  scale_y_log10() +
  xlab("genre/sub-genre") + 
  ylab("log10 total number of ratings") +
  theme_bw() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

fig_4 <- ggplot(data=genre_effects,aes(reorder(genres, -genre_avg, sum),y=genre_avg))+geom_bar(stat="identity")  +
  xlab("genre/sub-genre") +
  ylab("average rating") +
  geom_hline(yintercept = mu,colour ="red") +
  geom_hline(yintercept = mu+sd,colour ="red",linetype="33") +
  geom_hline(yintercept = mu-sd,colour ="red",linetype="33") +
  annotate("text", x = "Children", y = mu +0.2, label = "AVG", color="red", size = 4 ) + 
  annotate("text", x = "Children", y = mu + sd - 0.2, label = "+1 SD", color="red", size = 4 ) +
  annotate("text", x = "Children", y = mu - sd + 0.2, label = "-1 SD", color="red", size = 4 ) +
  theme_bw() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

#print figures
fig_3
fig_4

#calculate mean rating per 100s ratings a movie has recieved in total

nr_effects <- edx_work_train %>% 
  group_by(movieId) %>% 
  summarize(n_rating_round=round(n_distinct(userId)/100)) %>%
  left_join(x=edx_work_train,y=.,by="movieId") %>%
  group_by(n_rating_round) %>%
  summarize(n_ratings=n(),avg_rating=mean(rating))

fig_5 <- ggplot(data=nr_effects,aes(n_rating_round,n_ratings))+geom_point() + 
  xlab("number of ratings in 100s") + 
  ylab("total number of ratings") +
  theme_bw() 

fig_6 <- ggplot(data=nr_effects,aes(n_rating_round,avg_rating))+geom_point() + 
  xlab("number of ratings in 100s") +
  ylab("average rating") +
  geom_hline(yintercept = mu,colour ="red") +
  geom_hline(yintercept = mu+sd,colour ="red",linetype="33") +
  geom_hline(yintercept = mu-sd,colour ="red",linetype="33") +
  annotate("text", x = 0.1, y = mu +0.05, label = "AVG", color="red") + 
  annotate("text", x = 0.1, y = mu + sd - 0.05, label = "+1 SD", color="red") +
  annotate("text", x = 0.1, y = mu - sd + 0.05, label = "-1 SD", color="red")

#print figures

fig_5
fig_6

#review frequency of user, stratification to multiples of ten
rpu_effects <- edx_work_train %>% group_by(userId) %>% 
  mutate(reviews_per_user =round(n_distinct(movieId)/10)) %>% 
  group_by(reviews_per_user) %>% 
  summarize(n_ratings=n(), rpu_avg = mean(rating),n_users=n_distinct(userId))

slice_max(rpu_effects,n_ratings) # give the user group with the most ratings
slice_max(rpu_effects,n_users) # give the biggest user group

fig_7 <- ggplot(data=rpu_effects,aes(reviews_per_user,n_ratings))+geom_point() + 
  xlab("user group (reviews per user in 10s)") + 
  ylab("number of rated movies") +
  theme_bw() 

fig_8 <- ggplot(data=rpu_effects,aes(reviews_per_user,rpu_avg))+geom_point() + 
  xlab("user group (reviews per user in 10s)") +
  ylab("average rating") +
  geom_hline(yintercept = mu,colour ="red") +
  geom_hline(yintercept = mu+sd,colour ="red",linetype="33") +
  geom_hline(yintercept = mu-sd,colour ="red",linetype="33") +
  annotate("text", x = 40, y = mu +0.1, label = "AVG", color="red") + 
  annotate("text", x = 40, y = mu + sd - 0.1, label = "+1 SD", color="red") +
  annotate("text", x = 40, y = mu - sd + 0.1, label = "-1 SD", color="red") +
  theme_bw()

# print figures
fig_7
fig_8


# round the review_date to weeks and assign an order for a specific user Id and review week
review_order_train <- edx_work_train %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  select(userId,review_week) %>%
  distinct() %>%
  group_by(userId) %>%
  summarize(review_week,review_order=order(review_week))

# join review_week and review_order into edx_work_train to be able to plot time courses
time_course <- edx_work_train %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  left_join(x=.,y=review_order_train, by=c("userId","review_week"))
# calculate average rating per review week
review_week_effects <- time_course %>% group_by(review_week) %>%
  summarise(n_ratings=n(),avg_rating=mean(rating))
# calculate average rating per review order
review_order_effects <- time_course %>% group_by(review_order) %>%
  summarise(n_ratings=n(),avg_rating=mean(rating))

fig_9 <- ggplot(data=review_week_effects,aes(review_week,avg_rating))+geom_point() + 
  xlab("review week") +
  ylab("average rating") +
  geom_hline(yintercept = mu,colour ="red") +
  geom_hline(yintercept = mu+sd,colour ="red",linetype="33") +
  geom_hline(yintercept = mu-sd,colour ="red",linetype="33") +
  annotate("text", x=as.POSIXct("1995-03-31 00:00:00"), y = mu +0.1, label = "AVG", color="red") + 
  annotate("text", x=as.POSIXct("1995-03-31 00:00:00"), y = mu + sd - 0.1, label = "+1 SD", color="red") +
  annotate("text", x=as.POSIXct("1995-03-31 00:00:00"), y = mu - sd + 0.1, label = "-1 SD", color="red") +
  theme_bw()

fig_10 <- ggplot(data=review_order_effects,aes(review_order,avg_rating))+geom_point() + 
  xlab("order of review week") +
  ylab("average rating") +
  geom_hline(yintercept = mu,colour ="red") +
  geom_hline(yintercept = mu+sd,colour ="red",linetype="33") +
  geom_hline(yintercept = mu-sd,colour ="red",linetype="33") +
  annotate("text", x=5, y = mu +0.1, label = "AVG", color="red") + 
  annotate("text", x=5, y = mu + sd - 0.1, label = "+1 SD", color="red") +
  annotate("text", x=5, y = mu - sd + 0.1, label = "-1 SD", color="red") +
  theme_bw()

# print figures

fig_9
fig_10

# timespan t in years from publication year to user review
t_effects <- edx_work_train %>% mutate(t= year(round_date(review_date, "year")) - pub_year) %>% 
  group_by(t) %>% 
  summarize(n_ratings=n(),t_avg = mean(rating), b_t=mean(rating-mu))

fig_11 <- ggplot(data=t_effects,aes(t,n_ratings))+geom_point() + 
  xlab("t years after publication") + 
  ylab("total number of ratings") +
  theme_bw() 

fig_12 <- ggplot(data=t_effects,aes(t,t_avg))+geom_point() + 
  xlab("years after publication") +
  ylab("average rating") +
  geom_hline(yintercept = mu,colour ="red") +
  geom_hline(yintercept = mu+sd,colour ="red",linetype="33") +
  geom_hline(yintercept = mu-sd,colour ="red",linetype="33") +
  annotate("text", x = 0.1, y = mu +0.1, label = "AVG", color="red") + 
  annotate("text", x = 0.1, y = mu + sd - 0.1, label = "+1 SD", color="red") +
  annotate("text", x = 0.1, y = mu - sd + 0.1, label = "-1 SD", color="red") +
  theme_bw() 

#print figures

fig_11
fig_12

#Modelling of movie effects

# genre bias
genre_bias <- edx_work_train %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu)) %>%
  ungroup()

# pub year bias
pub_year_bias <- edx_work_train %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  group_by(pub_year) %>%
  summarize(b_py=mean(rating-mu-b_g)) %>%
  ungroup()

# movie bias
movie_bias <- edx_work_train %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  group_by(movieId) %>%
  summarize(b_i=mean(rating-mu-b_g-b_py),n_rating=n_distinct(userId)) %>%
  ungroup()

fig_13 <- ggplot(data=genre_bias,aes(b_g))+geom_histogram(binwidth = 0.1) + 
  xlab("genre bias") +
  ylab("frequency") +
  theme_bw()
fig_14 <- ggplot(data=pub_year_bias,aes(b_py))+geom_histogram(binwidth = 0.1)  + 
  xlab("publication year bias") +
  ylab("frequency") +
  theme_bw() 
fig_15 <- ggplot(data=movie_bias,aes(b_i))+geom_histogram(binwidth = 0.1)  + 
  xlab("true movie bias") +
  ylab("frequency") +
  theme_bw() 

# print figures

fig_13
fig_14
fig_15

#Modelling of user effects

# user bias
user_bias <- edx_work_train %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u=mean(rating-mu-b_g-b_py-b_i)) %>%
  ungroup()

# review week bias
review_week_bias <- edx_work_train %>% 
  mutate(t= year(round_date(review_date, "year")) - pub_year,review_week=round_date(review_date,"week")) %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  left_join(x=.,y=user_bias,by="userId") %>%
  group_by(review_week) %>%
  summarize(b_w=mean(rating-mu-b_g-b_py-b_i-b_u)) %>%
  ungroup()

# t bias
t_bias <- edx_work_train %>% 
  mutate(t= year(round_date(review_date, "year")) - pub_year,review_week=round_date(review_date,"week")) %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  left_join(x=.,y=user_bias,by="userId") %>%
  left_join(x=.,y=review_week_bias,by="review_week") %>%
  group_by(t) %>%
  summarize(b_t=mean(rating-mu-b_g-b_py-b_i-b_u-b_w)) %>%
  ungroup()

fig_16 <- ggplot(data=t_bias,aes(b_t))+geom_histogram(binwidth = 0.1)  + 
  xlab("t bias") +
  ylab("frequency") +
  theme_bw()

fig_17 <- ggplot(data=user_bias,aes(b_u))+geom_histogram(binwidth = 0.1)  + 
  xlab("user bias") +
  ylab("frequency") +
  theme_bw()

fig_18 <- ggplot(data=review_week_bias,aes(b_w))+geom_histogram(binwidth = 0.1)  + 
  xlab("review week bias") +
  ylab("frequency") +
  theme_bw()

# print figures

fig_16
fig_17
fig_18


## define the settings for cross-validation.
ctrl_pars <- trainControl(method="cv", number=5)

#define the training dataset:

# define the user-specific review order
review_order_train <- edx_work_train %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  select(userId,review_week) %>%
  distinct() %>%
  group_by(userId) %>%
  summarize(review_week,review_order=order(review_week)) %>%
  ungroup()

# join to data set
edx_work_train_order <- edx_work_train %>% 
  mutate(t= year(round_date(review_date, "year")) - pub_year,review_week=round_date(review_date,"week")) %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  left_join(x=.,y=review_order_train, by=c("userId","review_week"))

# complete the dataset
edx_work_train_eln <- edx_work_train_order %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  left_join(x=.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=user_bias,by="userId") %>%
  left_join(x=.,y=review_week_bias,by="review_week") %>%
  left_join(x=.,y=t_bias,by="t") %>%
  select(rating,movieId,userId,b_g,b_py,b_i,b_u,b_w,b_t,n_rating,review_order,review_week)


#define the test data set

# define the user-specific review order
review_order_test <- edx_work_test %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  select(userId,review_week) %>%
  distinct() %>%
  group_by(userId) %>%
  summarize(review_week,review_order=order(review_week)) %>%
  ungroup()

# join to data set
edx_work_test_order <- edx_work_test %>% 
  mutate(t= year(round_date(review_date, "year")) - pub_year,review_week=round_date(review_date,"week")) %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  left_join(x=.,y=review_order_test, by=c("userId","review_week"))

# complete the dataset
edx_work_test_eln <- edx_work_test_order %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  left_join(x=.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=user_bias,by="userId") %>%
  left_join(x=.,y=review_week_bias,by="review_week") %>%
  left_join(x=.,y=t_bias,by="t") %>%
  select(rating,movieId,userId,b_g,b_py,b_i,b_u,b_w,b_t,n_rating,review_order,review_week)

#building the models

set.seed(1, sample.kind="Rounding")
eln_model_1 <- train(rating ~ b_py + b_g + b_i + b_t + b_u + b_w,
                     data = edx_work_train_eln,
                     method = "glmnet",
                     trControl = ctrl_pars,
                     metric = "RMSE",
                     family="gaussian",
                     tuneGrid = expand.grid(alpha =seq(0,1,0.33),lambda = seq(0.0001,1,length =100)))

set.seed(1, sample.kind="Rounding")
eln_model_2 <- train(rating ~ b_g + b_py + b_i + b_u + b_t + b_w + n_rating + review_order,
                     data = edx_work_train_eln,
                     method = "glmnet",
                     trControl = ctrl_pars,
                     metric = "RMSE",
                     family="gaussian",
                     tuneGrid = expand.grid(alpha =seq(0,1,0.33),lambda = seq(0.0001,1,length =100)))

set.seed(1, sample.kind="Rounding")
eln_model_3 <- train(rating ~ b_g + b_py + b_i + b_u + b_t + n_rating + review_order,
                     data = edx_work_train_eln,
                     method = "glmnet",
                     trControl = ctrl_pars,
                     metric = "RMSE",
                     family="gaussian",
                     tuneGrid = expand.grid(alpha =seq(0,1,0.33),lambda = seq(0.0001,1,length =100)))

set.seed(1, sample.kind="Rounding")
eln_model_4 <- train(rating ~ b_g + b_py + b_i + b_u + b_t + n_rating + review_order + review_week,
                     data = edx_work_train_eln,
                     method = "glmnet",
                     trControl = ctrl_pars,
                     metric = "RMSE",
                     family="gaussian",
                     tuneGrid = expand.grid(alpha =seq(0,1,0.33),lambda = seq(0.0001,1,length =100)))


#making the predictions

eln_predict_1 <- predict(eln_model_1,newdata = edx_work_test_eln, na.action=na.pass)

eln_predict_2 <- predict(eln_model_2,newdata = edx_work_test_eln, na.action=na.pass)

eln_predict_3 <- predict(eln_model_3,newdata = edx_work_test_eln, na.action=na.pass)

eln_predict_4 <- predict(eln_model_4,newdata = edx_work_test_eln, na.action=na.pass)


# replace 'NA' in prediction with mu

eln_predict_1 <- tibble(eln_predict_1) %>% mutate(eln_predict_1 = ifelse(is.na(eln_predict_1) == TRUE, mu, eln_predict_1))

eln_predict_2 <- tibble(eln_predict_2) %>% mutate(eln_predict_2 = ifelse(is.na(eln_predict_2) == TRUE, mu, eln_predict_2))

eln_predict_3 <- tibble(eln_predict_3) %>% mutate(eln_predict_3 = ifelse(is.na(eln_predict_3) == TRUE, mu, eln_predict_3))

eln_predict_4 <- tibble(eln_predict_4) %>% mutate(eln_predict_4 = ifelse(is.na(eln_predict_4) == TRUE, mu, eln_predict_4))

# calculate RMSE

rmse_1 <- RMSE(edx_work_test_eln$rating,eln_predict_1$eln_predict_1)

rmse_2 <- RMSE(edx_work_test_eln$rating,eln_predict_2$eln_predict_2)

rmse_3 <- RMSE(edx_work_test_eln$rating,eln_predict_3$eln_predict_3)

rmse_4 <- RMSE(edx_work_test_eln$rating,eln_predict_4$eln_predict_4)


# print rmses
rmse_1
rmse_2
rmse_3
rmse_4

# comparison of model performance (cross-validation)
model_list <- list(eln_model_1 = eln_model_1,eln_model_2 = eln_model_2,  eln_model_3 = eln_model_3, eln_model_4 = eln_model_4)
res <- resamples(model_list)
summary(res)

# calculate bias for edx dataset

# genre bias
genre_bias <- edx_work %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu)) %>%
  ungroup()

# pub year bias
pub_year_bias <- edx_work %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  group_by(pub_year) %>%
  summarize(b_py=mean(rating-mu-b_g)) %>%
  ungroup()

# movie bias
movie_bias <- edx_work %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  group_by(movieId) %>%
  summarize(b_i=mean(rating-mu-b_g-b_py),n_rating=n_distinct(userId)) %>%
  ungroup()

# user bias
user_bias <- edx_work %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u=mean(rating-mu-b_g-b_py-b_i)) %>%
  ungroup()

# review week bias
review_week_bias <- edx_work %>% 
  mutate(t= year(round_date(review_date, "year")) - pub_year,review_week=round_date(review_date,"week")) %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  left_join(x=.,y=user_bias,by="userId") %>%
  group_by(review_week) %>%
  summarize(b_w=mean(rating-mu-b_g-b_py-b_i-b_u)) %>%
  ungroup()

# t bias
t_bias <- edx_work %>% 
  mutate(t= year(round_date(review_date, "year")) - pub_year,review_week=round_date(review_date,"week")) %>%
  left_join(x.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  left_join(x=.,y=user_bias,by="userId") %>%
  left_join(x=.,y=review_week_bias,by="review_week") %>%
  group_by(t) %>%
  summarize(b_t=mean(rating-mu-b_g-b_py-b_i-b_u-b_w)) %>%
  ungroup()

#define the training dataset:

# define the user-specific review order
review_order <- edx_work %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  select(userId,review_week) %>%
  distinct() %>%
  group_by(userId) %>%
  summarize(review_week,review_order=order(review_week))

# join to data set
edx_work_order <- edx_work %>% 
  mutate(t= year(round_date(review_date, "year")) - pub_year,review_week=round_date(review_date,"week")) %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  left_join(x=.,y=review_order, by=c("userId","review_week"))

# complete the dataset
edx_work_eln <- edx_work_order %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  left_join(x=.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=user_bias,by="userId") %>%
  left_join(x=.,y=review_week_bias,by="review_week") %>%
  left_join(x=.,y=t_bias,by="t") %>%
  select(rating,movieId,userId,b_g,b_py,b_i,b_u,b_w,b_t,n_rating,review_order,review_week)

validation <- read_csv("validation.csv")

validation_work <- validation %>% 
  mutate(review_date = as_datetime(timestamp),pub_year=as.numeric(str_match(str_match(title,pattern1),pattern2)))

review_order_validation<- validation_work %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  select(userId,review_week) %>%
  distinct() %>%
  group_by(userId) %>%
  summarize(review_week,review_order=order(review_week))

# join to data set
validation_work_order <- validation_work %>% 
  mutate(t= year(round_date(review_date, "year")) - pub_year,review_week=round_date(review_date,"week")) %>%
  mutate(review_week=round_date(review_date,"week")) %>%
  left_join(x=.,y=review_order_validation, by=c("userId","review_week"))

validation_work_eln <- validation_work_order %>%
  left_join(x=.,y=movie_bias,by="movieId") %>%
  left_join(x=.,y=genre_bias,by="genres") %>%
  left_join(x=.,y=pub_year_bias,by="pub_year") %>%
  left_join(x=.,y=user_bias,by="userId") %>%
  left_join(x=.,y=review_week_bias,by="review_week") %>%
  left_join(x=.,y=t_bias,by="t") %>%
  select(rating,movieId,userId,b_g,b_py,b_i,b_u,b_w,b_t,n_rating,review_order,review_week)

# train final model
set.seed(1, sample.kind="Rounding")
eln_model_final <- train(rating ~ b_g + b_py + b_i + b_u + b_t + b_w + n_rating + review_order,
                         data = edx_work_eln,
                         method = "glmnet",
                         trControl = ctrl_pars,
                         metric = "RMSE",
                         family="gaussian",
                         tuneGrid = expand.grid(alpha =seq(0,1,0.166),lambda = seq(0.0001,1,length =100)))

#make prediciton ofn validation dataset
eln_predict_final <- predict(eln_model_final,newdata = validation_work_eln, na.action=na.pass)

# fill NAs with mu
eln_predict_final <- tibble(eln_predict_final) %>% mutate(eln_predict_final = ifelse(is.na(eln_predict_final) == TRUE, mu, eln_predict_final))

#calculate final rmse
rmse_final <- RMSE(validation_work_eln$rating,eln_predict_final$eln_predict_final)

# print rmse
rmse_final

#summarize final model and compare to eln_model_2
res_final <- resamples(list(eln_model_final=eln_model_final,eln_model_2=eln_model_2))
summary(res_final)
eln_model_final$bestTune

#give variable importance
varImp(eln_model_final)
