##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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
dim(edx)
colnames(edx)
length(which(edx$rating == 3)) 

length(unique(edx$movieId))
length(unique(edx$userId))


length(edx$rating[which(grepl('Drama' , edx$genres , fixed = T))])
length(edx$rating[which(grepl('Comedy' , edx$genres , fixed = T))])
length(edx$rating[which(grepl('Thriller' , edx$genres , fixed = T))])
length(edx$rating[which(grepl('Romance' , edx$genres , fixed = T))])

edx %>% 
    group_by(title) %>%
  summarise(n = n()) %>% arrange(desc(n))

edx %>% 
  group_by(title) %>%
  summarise(n = n()) %>% arrange(desc(n))

edx %>% 
  group_by(rating) %>%
  summarise(n = n()) %>% arrange(desc(n))

edx %>% 
  ggplot(aes(rating)) + geom_histogram(color = 'Black' , 
                                       fill = 'yellow' , bins = 30) +
  theme_hc()

#LINEAR MODEL

#overall mean of ratings for train set
mu <- mean(train_set$rating)

#Train / Test split of edx dataset
test_index <- createDataPartition(y = edx$rating , times = 1 , p = 0.2 ,
                                  list = F)
test_set <- edx %>% slice(test_index)
train_set <- edx %>% slice(-test_index)

# Make sure userId and movieId in test set are also in train set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# RMSE function for evaluation throughout the project 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# RMSE using just the overall average of train set
naive_rmse <- RMSE(test_set$rating , mu)
naive_rmse

#Movie specific effect on ratings (b_i)
edx %>% group_by(movieId) %>% 
  summarise(n = n())%>% ggplot(aes(n) ) + geom_histogram(color = 'black' , fill = 'orange') +
  scale_x_log10() + theme_hc() + scale_color_hc() + xlab('No. of Ratings') + ylab('No. of Movies')

movie_effect <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

y_hat_b_i <- mu + test_set %>%
  left_join(movie_effect , by = 'movieId') %>%
  .$b_i

model1_rmse <- RMSE(test_set$rating , y_hat_b_i)

#User Specific effect on ratings(b_u)
edx %>% group_by(userId) %>% 
  summarise(n = n()) %>% ggplot(aes(n)) + 
  geom_histogram(color = 'black' , fill = "skyblue") + scale_x_log10() +
  theme_hc() + xlab('No. of Ratings') + ylab('No of Users')

user_effect <- train_set %>%
  left_join(movie_effect , by= 'movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

#combined effect of b_u & b_i
y_bi_bu <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

model2_rmse <- RMSE(test_set$rating , y_bi_bu)  

if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)

#Time specific effect on ratings (b_t)
edx <- mutate(edx, date = as_datetime(timestamp))

edx %>% 
  mutate(year = round_date(date  , unit = 'year')) %>%
  group_by(movieId , year) %>%
  summarise(n = n()) %>%
  ggplot() + geom_boxplot(aes( year , n , group = year)) +
  scale_y_sqrt() + theme_hc()

# We see small effect of time on user ratings
edx %>% mutate(date = round_date(date  , unit = 'week' , week_start = getOption('lubridate.week.start',1) )) %>%
  group_by(date) %>% summarise(n = n()) %>%
  ggplot(aes(date , n)) + geom_point(color = 'darkgreen') + 
  geom_smooth() + theme_hc() + xlab('Date(unit - week)') + ylab('No. of Ratings')

time_effect <- train_set %>% 
  left_join(user_effect , by = 'userId') %>%
  left_join(movie_effect , by = 'movieId') %>%
  mutate(date = round_date(as_datetime(timestamp)  , unit = 'week' )) %>%
  group_by(date) %>%
  summarise(b_t = mean(rating - mu - b_i - b_u))

#Combined effect mu + b_u + b_i + b_t
test_set <- test_set %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) 


y_hat_buit <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>% 
  left_join(time_effect , by= 'date') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>% .$pred

# Not much improvement in RMSE considering time effect
model3_rmse <- RMSE(test_set$rating , y_hat_buit)

#Reguralized movie + user effects on ratings


movie_titles <- edx %>%
  select(movieId , title) %>%
  distinct()

#movie with 1/2 reviews are highly rated due to lack of regularization
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_effect) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_effect) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:100) %>% 
  ggplot(aes(title , b_i)) + geom_point(aes(size = n , color = n)) +
  theme(axis.text.x=element_blank()) + guides(color = F) +theme_hc() +
  xlab('Movies(Top 100)') 

lambdas <- seq(0,10 , 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  pred <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(test_set$rating , pred))
})

qplot(lambdas , rmses ) + theme_hc()
 
lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- data_frame(methods=c("Just the average" ,"movie effect","movie + user effects",
                                     "movie + user + time effects"),
                           RMSE = c(naive_rmse , model1_rmse, model2_rmse,model3_rmse))
rmse_results <- bind_rows(rmse_results,
                          data_frame(methods="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

#matrix factorization

if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)

#genres also has strong effect on ratings 
genres_df <- edx %>% 
  group_by(genres) %>%
  summarise(n = n()) %>% filter(n > 1000)

edx %>% filter(genres %in% genres_df$genres) %>%
  group_by(genres) %>%
  summarise(mean = mean(rating) , sd = sd(rating)) %>% slice(1:20) %>%
  mutate(genres = str_wrap(genres , width = 20)) %>%
  ggplot(aes(genres , mean)) + geom_point() + 
  geom_errorbar(aes(ymin = mean - sd , ymax = mean+sd)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  theme_hc()

#Principal Component Analysis

if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
library(ggrepel)

train_small <- train_set %>%
  group_by(movieId) %>%
  filter(n() >= 500 ) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 500 ) %>% ungroup()

y <- train_small %>%
  select(movieId , userId , rating) %>% 
  spread(movieId , rating) %>% as.matrix()

rownames(y) <- y[,1]
y <- y[,-1]

movie_titles <- train_small %>%
  select(movieId , title) %>% distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

y <- sweep(y , 2 , colMeans(y ,na.rm = T))
y <- sweep(y , 1 , rowMeans(y ,na.rm = T))

y[is.na(y)] <- 0
pca <- prcomp(y)

plot(pca$sdev) 

var_exp <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_exp , cex = 1)
lines(x = c(0,1300), y = c(.90, .90))


pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>% 
  ggplot(aes(PC1 , PC2 )) + geom_point() +
  geom_text_repel(aes(PC1 , PC2 , label = name , color = 'green') , 
                  data = filter(pcs ,
                                PC1 < -0.1 | PC1 > 0.1 | 
                                  PC2 < -0.075 | PC2 > 0.05)) + 
  theme_hc() + guides(color = F)

#MATRIX FACTORIZATION MODEL

train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_data <- with(test_set, data_memory(user_index = userId, 
                                         item_index = movieId, 
                                         rating     = rating))

r <- Reco()

options <- r$tune(train_data, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                         costp_l1 = 0, costq_l1 = 0,
                                         nthread = 1, niter = 10))

r$train(train_data, opts = c(options$min, nthread = 1, niter = 20))

y_hat_recomm <- r$predict(test_data, out_memory())

model4_rmse <- RMSE(test_set$rating , y_hat_recomm)

rmse_results <- bind_rows(rmse_results,
                          data_frame(methods="Matrix factorization Model",  
                                     RMSE = model4_rmse))
rmse_results %>% knitr::kable()

#FINAL VALIDATION

# REGULARIZED MOVIE + USER MODEL (EDX V/S VALIDATION)
validation <- validation %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week"))

mu_edx <- mean(edx$rating)
b_i_edx <- edx %>%
  group_by(movieId) %>%
  summarize(b_i_edx = sum(rating - mu_edx)/(n()+lambda))
b_u_edx <- edx %>% 
  left_join(b_i_edx, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_edx = sum(rating - b_i_edx - mu_edx)/(n()+lambda))
b_t_edx <- edx %>% 
  left_join(b_u_edx , by = 'userId') %>%
  left_join(b_i_edx , by = 'movieId') %>%
  mutate(date = round_date(as_datetime(timestamp)  , unit = 'week' )) %>%
  group_by(date) %>%
  summarise(b_t_edx = mean(rating - mu_edx - b_i_edx - b_u_edx))
pred_edx <- 
  validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>% 
  left_join(b_t_edx , by = 'date')%>%
  mutate(pred_edx = mu_edx + b_i_edx + b_u_edx + b_t_edx) %>%
  .$pred_edx

rmse1_final <- RMSE(validation$rating , pred_edx)

# MATRIX FACTORIZATION MODEL (EDX V/S VALIDATION)

edx_data <-  with(edx, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
validation_data <- with(validation, data_memory(user_index = userId, 
                                        item_index = movieId, 
                                        rating     = rating))

r$train(edx_data, opts = c(options$min, nthread = 1, niter = 20))

recomm_edx <- r$predict(validation_data , out_memory())

rmse2_final <- RMSE(validation$rating , recomm_edx)

rmse_final <- data_frame(methods=c("Regularized movie + user + time effect (edx v/s validation)" ,"Matrix factorization (edx v/s validation)"),
                                         RMSE = c(rmse1_final , rmse2_final))
rmse_final %>% knitr::kable()
