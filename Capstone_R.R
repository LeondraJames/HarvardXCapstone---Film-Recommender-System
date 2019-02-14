#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,] 

# Make sure userId and movieId in validation set are also in edx set
#validation set 

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

########################################################################################################
########################################################################################################
########################################################################################################


#Save edx and validation files
save(edx, file="edx.RData")
save(validation, file = "validation.RData")

########################################
####### Load Additional Libraries ######
########################################

library(lubridate)
library(ggplot2)
library(caret)
library(dplyr)
library(ggthemes)
library(tidyr)
library(knitr)
library(rmarkdown)

########################################
### Data Cleaning: Timestamp to Year ###
########################################

#Timestamp to Years
edx$timestamp <- edx$timestamp %>% 
  as.numeric() %>% 
  as.POSIXct(origin = "1970-01-01") %>% 
  year()
 
colnames(edx)[colnames(edx) == "timestamp"] <- "Year"

#################################
### Exploratory Data Analysis ###
#################################

#Spread of User Ratings
ratings_count <- edx %>% 
  mutate(rating = as.factor(rating)) %>% 
  group_by(rating) %>% 
  summarise(number_of_ratings = n()) %>% 
  mutate(prop = (number_of_ratings / sum(number_of_ratings)) * 100) %>% 
  select(rating, number_of_ratings, prop) 

ggplot(ratings_count, aes(rating, number_of_ratings, fill = 'red')) + 
  guides(fill=FALSE) +
  geom_bar(stat = "identity") +
  xlab("Rating") +
  ylab("Number of Ratings") +
  ggtitle("Frequency of Ratings Per Rating Value") +
  scale_fill_hue(c=45, l=80) +
  theme_calc()

#Top 10 most rated genres
edx %>% 
  separate_rows(genres, sep = "\\|") %>% 
  group_by(genres) %>%
  summarise(number_of_ratings = n()) %>% 
  arrange(desc(number_of_ratings)) %>% 
  head(n = 10)

#Average rating for top 5 genres
edx %>% 
  separate_rows(genres, sep = "\\|") %>% 
  select(genres, rating) %>% 
  group_by(genres) %>%
  filter(genres %in% c("Drama","Comedy","Action","Thriller","Adventure")) %>% 
  ggplot(aes(x = factor(genres), y = rating, fill = genres)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  coord_flip() +
  xlab("Film Genre") +
  ylab("Film Rating") 

#Average rating for all genres
edx %>% 
  separate_rows(genres, sep = "\\|") %>% 
  select(genres, rating) %>% 
  group_by(genres) %>%
  ggplot(aes(x = factor(genres), y = rating, fill = genres)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  coord_flip() +
  xlab("Film Genre") +
  ylab("Film Rating") 


#Average rating by Year
edx %>% 
  separate_rows(genres, sep = "\\|") %>% 
  select(Year, rating) %>% 
  group_by(Year) %>%
  ggplot(aes(x = factor(Year), y = rating, fill = Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  coord_flip() +
  xlab("Release Year") +
  ylab("Film Rating") 

#Genre popularity over time
genres_v_time <- edx %>% 
  select(movieId, Year, genres) %>% 
  mutate(genres = as.factor(genres)) %>% 
  group_by(Year, genres) %>% 
  summarise(number = n())

genres_v_time %>% 
  filter(genres %in% c("Drama","Comedy","Thriller","Sci-Fi")) %>% 
  ggplot(aes(x = Year, y = number)) +
  geom_line(aes(color = genres)) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2000, 2010, by = 5)) +
  theme_hc() +
  ggtitle("Top Genre Popularity Over Time")

#Top 10 rated films by average
df_avg_rating <- edx %>% 
  mutate(rating = as.numeric(rating)) %>% 
  select(movieId, title, rating, Year) %>%
  group_by(movieId, title, Year) %>%
  summarise(count = n(), mean = mean(rating), min = min(rating), max = max(rating)) %>%
  ungroup() %>%
  arrange(desc(mean)) %>% 
  head(n = 10)
print(df_avg_rating)


#Top 10 rated films by weighted average

weighted_rating <- function(R, v, m, C) {
  return (v/(v+m))*R + (m/(v+m))*C
}

df_avg_rating <- df_avg_rating %>%
  mutate(wr = weighted_rating(mean, count, 500, mean(mean))) %>%
  arrange(desc(wr))

head(df_avg_rating, n = 10)

#Best rated film per decade
best_per_decade <- df_avg_rating %>%
  mutate(decade = Year  %/% 10 * 10) %>%
  arrange(Year, desc(wr)) %>%
  group_by(decade) %>%
  summarise(title = first(title), wr = first(wr), mean = first(mean), count = first(count))

head(best_per_decade)

#########################################
########### Data Partitioning ###########
#########################################
#Convert response variable back to numeric value for RMSE calculation purposes
set.seed(1)
train_index <- createDataPartition(y = edx$rating, times = 1, p = 0.7, list = FALSE)
train <- edx[train_index,] #for training model
test <- edx[-train_index,] #for testing model

#Will use "validation" object for final algorithm performance review


################################
########### MODELING ###########
################################

#For calculating RMSE for each model
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}


##################### Method #1 #########################
###########Using the average rating to compute ##########
#########################################################
#Predict the same rating for each film for all films and all users = the average rating.
# Any variance is therefore random variation
mu_1 <- mean(train$rating)
naive_rmse <- RMSE(test$rating, mu_1)


#Store Model 1 results to rmse_results
rmse_results <- data_frame(method = "Using the average rating", RMSE = naive_rmse)

##################### Method #2 #########################
########### + Remove Bias of Film Ratings ###############
#########################################################

mu_2 <- mean(train$rating)

movie_avg <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_2))

predicted_ratings <- mu_2 + test %>% 
  left_join(movie_avg, by = 'movieId') %>% 
  .$b_i

model_2_rmse <- RMSE(test$rating, predicted_ratings)

#Store Model 2 results to rmse_results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie Effect Model",
                                     RMSE = model_2_rmse))

##################### Method #3 #########################
########### + Remove Bias of Users ######################
#########################################################

user_avg <- test %>% 
  left_join(movie_avg, by = 'movieId') %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu_2 - b_i))

predicted_ratings <- test %>% 
  left_join(movie_avg, by = 'movieId') %>% 
  left_join(user_avg, by = 'userId') %>% 
  mutate(pred = mu_2 + b_i + b_u) %>%
  .$pred
  
model_3_rmse <- RMSE(test$rating, predicted_ratings)

#Store Model 3 results to rmse_results
rmse_results <- bind_rows(rmse_results, data_frame(method = "Movie + User Effects Model",
                                                   RMSE = model_3_rmse))

#############################################
########## EVALUATION & VALIDATION ##########
#############################################

#Review RMSE of all 3 models
print(rmse_results)

#Testing Optimal Model #3 on validation set
valid_predicted_ratings <- validation %>% 
  left_join(movie_avg, by = 'movieId') %>% 
  left_join(user_avg, by = 'userId') %>% 
  mutate(pred = mu_2 + b_i + b_u) %>%
  .$pred

model_3_valid <- RMSE(validation$rating, valid_predicted_ratings)


#Final Results
  
rmse_results <- bind_rows(rmse_results, data_frame(method = "Validation Result",
                                                   RMSE = model_3_valid))
