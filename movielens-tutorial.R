## ----Naming report sections for cross-reference, include=FALSE------------------------------------------------------------------------------
# This is not used for the model or for any computation, it is only use to build the report in a dynamic way and
# centralise section titles in one place
sec_one <- "Method and workflow"
sec_two <- "Data processing"
sec_three <- "Modelling"
sec_four <- "Results"
sec_five <- "Conclusion"


## ----Data origin parameters, include=FALSE--------------------------------------------------------------------------------------------------
# If "local_data" is set to FALSE, the coded will download and process data as if doesn't exist locally.
# If set to "TRUE", the code will load local data to lower execution time.
# This variable is used in the rmarkdown code chunks to be dynamic execution possible.
local_data <- TRUE


## ----Load packages, results='hide', message = FALSE, warning = FALSE------------------------------------------------------------------------

library(knitr)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(tidyverse)
library(recosystem)
library(ggthemes)
library(ggrepel)
library(scales)



## ----Load local datasets if any, echo=FALSE, results='hide', eval=local_data, cache = TRUE--------------------------------------------------
## # Load these datasets if they are locally avaiable and local_data == TRUE. Update the path to match your own.
## # Otherwise run the r code below which is currently not
## # evaluated (evaluate = FALSE).
## # Loading locally available dataset is faster in case of re-run.
## load("data/train_set.RData")
## load("data/train_test.RData")    # Contains both the train and test sets
## load("data/test_set.RData")
## load("data/validation.RData")
## 
## # Tuning parameters for the matrix factorisation algorithm.
## # Generating this the first time is particularly long, so better doing it only once.
## opts <- readRDS("data/reco-opts.rds")


## ----Download dataset, results='hide', eval=!local_data-------------------------------------------------------------------------------------
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


## ----Prepare dataset, results='hide', eval=!local_data--------------------------------------------------------------------------------------
# Import data from ratings.dat and add column names userId, movieId, 
# rating and timestamp
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# Import data from movies.dat, split the data string into 3 values 
# allocated to 3 columns (movieId, title and genres)
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(levels(movieId))[movieId],
         title = as.character(title), 
         genres = as.character(genres))

# Join movies and ratings together in movielens
movielens <- left_join(ratings, movies, by = "movieId")

# Split the title column in two columns, "title" and "year"
movielens <- movielens %>% mutate(ttyr = title) %>%
  extract(title, c("title", "year"), regex = "(.*)\\s\\((\\d+)\\)", convert = TRUE)

# Save "movielens" data frame in "data" directory for later use if needed
# Creates "data" repository in the current directory if it doesn't exist 
ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)
save("movielens", file = "data/movielens.RData")


## ----Split datasets, results='hide', eval=!local_data---------------------------------------------------------------------------------------
# Validation set will be 10% of "movielens" data
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, list = FALSE)
train_test <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure "userId" and "movieId" in "validation" set are also in "train_test" set
validation <- temp %>% 
  semi_join(train_test, by = "movieId") %>%
  semi_join(train_test, by = "userId")

# Add rows removed from "validation" set back into "train_test" set
removed <- anti_join(temp, validation)
train_test <- rbind(train_test, removed)

# Split "train_test" data set between "test_set" and "train_set"
set.seed(100, sample.kind="Rounding")

test_index <- createDataPartition(y = train_test$rating, 
                                  times = 1, p = 0.2, list = FALSE)
train_set <- train_test[-test_index,]
temp <- train_test[test_index,]

# Making sure that there are no "userId" or "movieId" in "test_set" which are not in
# "training_set"
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>% 
  semi_join(train_set, by = "userId")

# If any rows has been discarded from "test_set", add it back to the "train_set"
removed <- anti_join(temp, test_set)
train_set <- rbind(removed, train_set)

# Making sure that no rows has been lost (should return TRUE)
nrow(train_test) == nrow(train_set) + nrow(test_set)

# Do a bit of cleaning:
rm(dl, ratings, movies, test_index, temp, removed, movielens)

# The following datasets now exist: "validation", "test_set", "train_set."
# Save these data frames for future use in "data" directory to avoid repeating the work
# Creates "data" repository in the current directory if doesn't exist 
ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)
save("train_test", file = "data/train_test.RData")
save("test_set", file = "data/test_set.RData")
save("train_set", file = "data/train_set.RData")
save("validation", file = "data/validation.RData")
# To use locally stored data on the next run of this rmarkdown, set local_data to TRUE


## ----Start data exploration and visualisation, results=FALSE, cache = TRUE------------------------------------------------------------------
# View the structure of the dataset
str(train_test)


## ----Number of unique movies and users, message = FALSE, warning = FALSE, cache = TRUE------------------------------------------------------
# Number of unique movies and users:
kable(train_test %>% 
        summarize(n_ratings = n(), unique_users = n_distinct(userId), 
                  unique_movies = n_distinct(movieId)), 
      caption = "Number of unique movies and users")


## ----Rating distribution, fig.align='center', cache = TRUE----------------------------------------------------------------------------------
# Rating distribution
theme_set(theme_fivethirtyeight(base_size = 10))
theme_update(axis.title = element_text())
train_test %>% ggplot(aes(rating)) +
  geom_histogram(color = "white", bins = 10, fill="lightblue") + 
  ggtitle("Rating distribution") +
  xlab("Rating") +
  ylab("Count") +
  scale_y_continuous(labels = comma)


## ----Mean and median, cache = TRUE----------------------------------------------------------------------------------------------------------
mean(train_test$rating)
median(train_test$rating)


## ----Ratings per user and per movie, warning=FALSE, message=FALSE, cache = TRUE-------------------------------------------------------------
# Number of ratings received by the 10 most rated movies
movie_ratings <- train_test %>% 
  select(movieId, title, rating) %>% 
  group_by(movieId, title) %>% 
  summarize(n_ratings = n(), average = mean(rating))

kable(head(movie_ratings %>% arrange(desc(n_ratings)), 10), 
  caption = "Ratings of the 10 most rated movies")


## ----Nb ratings for the 10 least rated movies, message=FALSE, cache = TRUE------------------------------------------------------------------
# Number of ratings received by the 10 least rated movies
kable(head(movie_ratings %>% arrange(n_ratings), 10), 
      caption = "Ratings of the 10 least rated movies")


## ----Histogram of number of ratings per movie, fig.align='center', message=FALSE, cache = TRUE----------------------------------------------
# Histogram of the number of ratings per movie
# (limiting plotting at 500 ratings per movie max for visibility).
ratings_per_movie <- movie_ratings %>%
  filter(n_ratings < 500)

ggplot(ratings_per_movie, aes(x=n_ratings)) +
  geom_histogram(bins = 100, color = "White", fill="lightblue") + 
  ggtitle("Distribution of ratings per movie") +
  xlab("Ratings per movie") +
  ylab("Count") +
  scale_y_continuous(labels = comma)


## ----Histogram of number of ratings per user,  fig.align='center', message=FALSE, cache = TRUE----------------------------------------------
# Histogram of the number of ratings per user
# (limiting plotting at 500 ratings per user max for visibility).
user_ratings <- train_test %>% 
  select(userId, rating) %>% 
  group_by(userId) %>% 
  summarize(n_ratings = n(), average = mean(rating))

ratings_per_users <- user_ratings %>% 
  filter(n_ratings < 500)

ggplot(ratings_per_movie, aes(x=n_ratings)) +
  geom_histogram(bins = 100, color = "White", fill="lightblue") + 
  ggtitle("Distribution of ratings per user") +
  xlab("Ratings per user") +
  ylab("Count") +
  scale_y_continuous(labels = comma)

# Maximum of number of ratings per user
max(user_ratings$n_ratings)


## ----Density of movie averages,  fig.align='center', cache = TRUE---------------------------------------------------------------------------
# Density of movie averages
ggplot(movie_ratings, mapping = aes(x=average)) +
  geom_density(fill="lightblue", alpha=0.5) +
  ggtitle("Distribution of movie averages") +
  xlab("Movie average") +
  ylab("Count") +
  scale_y_continuous(labels = comma)


## ----Number of ratings vs. movie average,  fig.align='center', message=FALSE, cache = TRUE--------------------------------------------------
# Graph of the number of ratings per movie vs. movie average
movie_ratings %>% ggplot(aes(x=average, y=n_ratings)) +
  geom_point(alpha=0.2) +
  geom_smooth(alpha=1) +
  ggtitle("Number of ratings vs. movie average") +
  geom_label_repel(aes(label=ifelse(n_ratings>27000,title,'')),
                   size=4,
                   hjust=0,
                   vjust=0,
                   box.padding   = 0.25,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  xlab("Movie average") +
  ylab("Nb of ratings") +
  scale_y_continuous(labels = comma)


## ----Distribution of user averages,  fig.align='center', cache = TRUE-----------------------------------------------------------------------
# Density plot of the user averages
ggplot(user_ratings, mapping = aes(x=average, y = after_stat(count))) +
  geom_density(fill="lightblue", alpha=0.5) +
  ggtitle("Distribution of user averages") +
  xlab("User average") +
  ylab("Count") +
  scale_y_continuous(labels = comma)


## ----Means of user averages and movie averages, cache = TRUE--------------------------------------------------------------------------------
# Comparison of mean of user averages and movie averages
message("Mean of user averages: ", round(mean(user_ratings$average), 2))
message("Mean of movie averages: ", round(mean(movie_ratings$average), 2))

## ----Cleaning, results='hide', include=FALSE------------------------------------------------------------------------------------------------
# Do some cleaning before moving on
rm(ratings_per_users, user_ratings, ratings_per_movie, movie_ratings)


## ----Genre of the 10 most rated movies, message=FALSE, cache = TRUE-------------------------------------------------------------------------
kable(head(train_test %>% group_by(title, genres) %>% 
       summarize(n_rating = n()) %>% 
       arrange(desc(n_rating)), 10),
      caption = "Genres of the 10 most rated movies")


## ----Distinct genre groupings, cache = TRUE-------------------------------------------------------------------------------------------------
n_distinct(train_test$genres)


## ----10 most common genres combinations, cache = TRUE---------------------------------------------------------------------------------------

genre_comb <- data.frame(sort(table(train_test$genres), decreasing = TRUE))
colnames(genre_comb) <- c("Combination", "Count")
kable(t(t(head(genre_comb, 10))), 
      caption = "The 10 most common genre combinations")


## ----Extraction and counting of unique genres, cache = TRUE---------------------------------------------------------------------------------
list_genres <- unique(unlist(strsplit(train_test$genres, split = "\\|")))
print(list_genres)
print(length(list_genres))


## ----Histogram of the number of movie releases per year, fig.align='center', cache = TRUE---------------------------------------------------
# Distribution of movie releases per year of release.
year_movie <- train_test %>% distinct(movieId, .keep_all = TRUE)

message("Last movie released in: ", max(year_movie$year))

ggplot(year_movie, aes(x=year)) +
  geom_histogram(bins = 85, color = "White", fill="lightblue") + 
  ggtitle("Distribution of movie releases per year of release") +
  xlab("Year of release") +
  ylab("Number of movies") +
  scale_y_continuous(labels = comma)


## ----Number of ratings vs. year of release, message = FALSE, fig.align='center', cache = TRUE-----------------------------------------------
# Number of ratings vs. year of release.
rating_year <- train_test %>% 
  group_by(year) %>% 
  summarize(n_ratings = n())

ggplot(train_test, aes(x=year)) +
  geom_histogram(bins = 85, color = "White", fill="lightblue") + 
  ggtitle("Distribution of ratings vs. year of release") +
  xlab("Year of release") +
  ylab("Nb of ratings") +
  scale_y_continuous(labels = comma)


## ----10 release years with most ratings, message=FALSE, cache = TRUE------------------------------------------------------------------------
kable(head(train_test %>% group_by(year) %>% 
       summarize(n_rating = n()) %>% 
       arrange(desc(n_rating)), 10), 
      caption = "10 release years with the most ratings")


## ----Nb of ratings per movie vs. year of release, message=FALSE, fig.align='center', cache = TRUE-------------------------------------------
# Nb or ratings per movie vs. release year of the movies.
rating_movie_year <- train_test %>%
       group_by(movieId, year) %>%
       summarise(n_ratings = n()) %>% 
       group_by(year) %>% 
       summarise(movie_average = mean(n_ratings))

ggplot(rating_movie_year, aes(x=year, y=movie_average)) +
  geom_col(color = "White", fill="lightblue") + 
  ggtitle("Average number of ratings per movie vs. year of release") +
  xlab("Year of release") +
  ylab("Average nb. of ratings/movie") +
  scale_y_continuous(labels = comma)


## ---- results='hide', include=FALSE---------------------------------------------------------------------------------------------------------
# Do some cleaning before moving on
rm(rating_year, year_movie, list_genres, rating_movie_year, genre_comb)


## ----Oldest and youngest ratings, cache = TRUE----------------------------------------------------------------------------------------------
# Finding the youngest and oldest ratings.
birthyear_rating <- train_test  %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(ts_year = floor_date(date, unit = "year"))

message("Year of the oldest rating: ", year(min(birthyear_rating$ts_year)))
message("Year of the youngest rating: ", year(max(birthyear_rating$ts_year)))


## ----Number of ratings per year, message = FALSE, fig.align='center', cache = TRUE----------------------------------------------------------
# Number of ratings done per year
ratings_year <- birthyear_rating %>%
  group_by(ts_year) %>%
  summarize(n_rating = n())

ratings_year %>% ggplot(aes(ts_year, n_rating)) +
  geom_point() +
  ggtitle("Number of ratings per year") +
  xlab("Year") +
  ylab("Number of ratings") +
  scale_y_continuous(labels = comma)



## ---- results='hide', include= FALSE--------------------------------------------------------------------------------------------------------
# Do some cleaning before moving on
rm(birthyear_rating, ratings_year)


## ----RMSE function, cache = TRUE------------------------------------------------------------------------------------------------------------
# Declare RMSE function:
RMSE <- function(y_predic, y_real){
  sqrt(mean((y_predic-y_real)^2))
}


## ----Results data frame and updating fuction, cache = TRUE----------------------------------------------------------------------------------
# Create data frame to store the results of the various models
model_results <- data.frame(
  model_nb = integer(), 
  type = character(), 
  lambda =  numeric(), 
  rmse = numeric(), 
  stringsAsFactors = FALSE)

# Since model type will be factor we can create a function to make it
# easier to add a new row and a new factor to model_results:
add_row_results <- function(row, model_nb, new_type, opt_lambda, new_rmse){
  model_results[row,  "model_nb"] <- model_nb
  model_results[row, "type"] <- new_type
  model_results[row, "lambda"] <- opt_lambda  
  model_results[row, "rmse"] <- new_rmse
  return (model_results)
}


## ----Improvement function, cache = TRUE-----------------------------------------------------------------------------------------------------
# Returns the variation in % from model_1 rmse to model_2 rmse.
improvement <- function(model_1, model_2){
  model_results$rmse[model_2]
  model_results$rmse[model_1]
  return (-(model_results$rmse[model_2]-model_results$rmse[model_1])
          /model_results$rmse[model_1]*100)
}


## ----Base model - Naive mean, message=FALSE, warning=FALSE, cache = TRUE--------------------------------------------------------------------
# First initial models, naive_rmse
model_nb <- 1
mu <- mean(train_set$rating)
# Calculate the RMSE
rmse <- RMSE(test_set$rating, mu)
# Store result in the summary dataframe model_results
model_results <- add_row_results(model_nb, model_nb, "naive mean", NA, rmse)
# Display results
kable(model_results, caption = "Summary of results")


## ----Movie effect (regularised), message=FALSE, warning=FALSE, cache = TRUE-----------------------------------------------------------------
model_nb <- 2
# Define a sequence of penalisation factor (lambda) for the regularization.
lambda <- seq(1, 4, 0.5)

# Declare the function that calculates the RMSE corresponding to each
# regularised average per movie based on the argument lbd.
rmse_lambda <- function(lbd){
  movie_reg_avg <- train_set %>% 
    group_by(movieId) %>% 
    summarize(bm = sum(rating - mu)/(lbd + n()))
  
  # Run the model on the test_set, compare the predicted ratings with the real one
  # and return the RMSE.
  movie_preds <- test_set %>% 
    left_join(movie_reg_avg, by="movieId") %>% 
    pull(bm)
  return(RMSE(test_set$rating, mu + movie_preds))
}

# Apply the previous function to each item in the lambda sequence.
rmse <- sapply(lambda, rmse_lambda)

# Plot the different RMSE for the different values of 'lbd'.
plot(lambda, rmse)



## ----Extract best lambda, cache = TRUE------------------------------------------------------------------------------------------------------
best_lbd <- lambda[which.min(rmse)]
# Store result in the summary dataframe model_results.
model_results <- add_row_results(
  model_nb, model_nb, "movie effect with regularisation", best_lbd, min(rmse))
# Display results
kable(model_results, caption = "Summary of results")


## -------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the improvement between this model and the previous one.
improvement(1,2)


## ----User effect (Regularised), message=FALSE, warning=FALSE, cache = TRUE------------------------------------------------------------------
model_nb <- 3
# Define a sequence of penalisation factor (lambda) for the regularization.
lambda <- seq(3, 7, 0.5)

# Calculate the movie effect using the best lambda identified previously.
movie_reg_avg <- train_set %>% 
  group_by(movieId) %>% 
  summarize(bm = sum(rating - mu)/(3 + n()))

# Declare the function that calculates the RMSE corresponding to each
# regularised average per movie based on the argument lbd.
rmse_lambda <- function(lbd){
  user_reg_avg <- train_set %>%
    left_join(movie_reg_avg, by='movieId') %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bm - mu)/(lbd + n()))
  
  # Run the model on the test_set, compare the predicted ratings with
  # the real one and return the RMSE.
  movie_user_preds <- test_set %>%
    left_join(movie_reg_avg, by="movieId") %>%
    left_join(user_reg_avg, by="userId") %>%
    mutate(pred = mu + bu + bm) %>%
    pull(pred)
  return(RMSE(test_set$rating, movie_user_preds))
}

# Apply the previous function to each item in the lambda sequence.
rmse <- sapply(lambda, rmse_lambda)

# Plot the different RMSE for the different values of 'lbd'.
plot(lambda, rmse)


## ---- cache = TRUE--------------------------------------------------------------------------------------------------------------------------
best_lbd <- lambda[which.min(rmse)]
# Store results in the summary dataframe model_results.
model_results <- add_row_results(
  model_nb, model_nb, "movie and user effects with reg.", best_lbd, min(rmse))
# Display results
kable(model_results, caption = "Summary of results")


## -------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the improvement between this model and the previous one.
improvement(2,3)


## ----Genre effect (regularised), warning=FALSE, message=FALSE, cache = TRUE-----------------------------------------------------------------
model_nb <- 4
# Define a sequence of penalisation factor (lambda) for the regularization.
lambda <- seq(15, 25, 1)

# Calculates the user effect using the best lambda identified previously.
user_reg_avg <- train_set %>%
  left_join(movie_reg_avg, by="movieId") %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - bm - mu)/(4.5 + n()))

# Declares the function that calculates the RMSE corresponding to each 
# regularised average per movie based on the argument lbd.
rmse_lambda <- function(lbd){
  genre_reg_avg <- train_set %>%
    left_join(movie_reg_avg, by="movieId") %>%
    left_join(user_reg_avg, by="userId") %>%
    group_by(genres) %>%
    summarize(bg = sum(rating - bu - bm - mu)/(lbd + n()))
  
  # Run the model on the test_set, compare the predicted ratings with 
  # the real one and return the RMSE.
  mug_preds <- test_set %>%
    left_join(movie_reg_avg, by="movieId") %>%
    left_join(user_reg_avg, by="userId") %>%
    left_join(genre_reg_avg, by="genres") %>%
    mutate(pred = mu + bg + bu + bm) %>%
    pull(pred)
  return(RMSE(test_set$rating, mug_preds))
}

# Apply the previous function to each item in the lambda sequence.
rmse <- sapply(lambda, rmse_lambda)

# Plot the different RMSE for the different values of 'lbd'.
plot(lambda, rmse)


## -------------------------------------------------------------------------------------------------------------------------------------------
best_lbd <- lambda[which.min(rmse)]
# Store result in the summary dataframe model_results.
model_results <- add_row_results(
  model_nb, model_nb, "movie, user and genre effects with reg.", best_lbd, min(rmse))
# Display results
kable(model_results, caption = "Summary of results")


## -------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the improvement between this model and the previous one.
improvement(3,4)


## ----Release date effect (regularised), message=FALSE, warning=FALSE, cache = TRUE----------------------------------------------------------
model_nb <- 5
# Define a sequence of penalisation factor (lambda) for the regularization.
lambda <- seq(0, 10, 2)

# Calculate the genre effect using the best lambda identified previously.
genre_reg_avg <- train_set %>%
    left_join(movie_reg_avg, by="movieId") %>%
    left_join(user_reg_avg, by="userId") %>%
    group_by(genres) %>%
  summarize(bg = sum(rating - bm - bu - mu)/(20 + n()))

# Declare the function that calculates the RMSE corresponding to each 
# regularised average per movie based on the argument lbd.
rmse_lambda <- function(lbd){
  year_reg_avg <- train_set %>%
    left_join(movie_reg_avg, by="movieId") %>%
    left_join(user_reg_avg, by="userId") %>%
    left_join(genre_reg_avg, by="genres") %>%
    group_by(year) %>%
    summarize(by = sum(rating - bm - bu - bg - mu)/(lbd + n()))
  
  # Run the model on the test_set, compare the predicted ratings with 
  # the real one and return the RMSE.
  mugy_preds <- test_set %>%
    left_join(movie_reg_avg, by="movieId") %>%
    left_join(user_reg_avg, by="userId") %>%
    left_join(genre_reg_avg, by="genres") %>%
    left_join(year_reg_avg, by="year") %>%
    mutate(pred = mu + by + bg + bu + bm) %>%
    pull(pred)
  return(RMSE(test_set$rating, mugy_preds))
}

# Apply the previous function to each item in the lambda sequence.
rmse <- sapply(lambda, rmse_lambda)

# Plot the different RMSE for the different values of 'lbd'.
plot(lambda, rmse)


## -------------------------------------------------------------------------------------------------------------------------------------------
best_lbd <- lambda[which.min(rmse)]
# Store result in the summary dataframe model_results
model_results <- add_row_results(
  model_nb, model_nb, "m, u, g and year effects with reg.", best_lbd, min(rmse))
# Display results
kable(model_results, caption = "Summary of results")


## -------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the improvement between this model and the previous one.
improvement(4,5)


## ----Date of rating effect (regularised), message=FALSE, warning=FALSE, cache = TRUE--------------------------------------------------------
model_nb <- 6
# Define a sequence of penalisation factor (lambda) for the regularisation.
lambda <- c(200, 300, 400, 500, 600, 800)

unit_date = "week"

# Update the train_set and test_set datasets with a new column, date, 
# which contains the conversion of timestamp to date object.
train_set <- train_set %>% mutate(date = as_datetime(timestamp))
test_set <- test_set %>% mutate(date = as_datetime(timestamp))

year_reg_avg <- train_set %>%
    left_join(movie_reg_avg, by="movieId") %>%
    left_join(user_reg_avg, by="userId") %>%
    left_join(genre_reg_avg, by="genres") %>%
    group_by(year) %>%
    summarize(by = sum(rating - bm - bu - bg - mu)/(n()))  

# Declare the function that calculates the RMSE corresponding to each 
# regularised average per movie based on the argument lbd.
rmse_lambda <- function(lbd){
  date_reg_avg <- train_set %>%
    left_join(movie_reg_avg, by='movieId') %>%
    left_join(user_reg_avg, by="userId") %>%
    left_join(genre_reg_avg, by="genres") %>%
    left_join(year_reg_avg, by="year") %>%
    group_by(unit_date = round_date(date, unit = unit_date)) %>%
    summarize(bts = sum(rating - bm - bu - bg - by - mu)/(lbd + n()))
  
  # Run the model on the test_set, compare the predicted ratings 
  # with the real one and return the RMSE.
  mugyd_preds <- test_set %>%
    left_join(movie_reg_avg, by="movieId") %>%
    left_join(user_reg_avg, by="userId") %>%
    left_join(genre_reg_avg, by="genres") %>%
    left_join(year_reg_avg, by="year") %>%
    mutate(unit_date = round_date(date, unit = unit_date)) %>%
    left_join(date_reg_avg, by="unit_date") %>%
    mutate(pred = mu +  bts + by + bg + bu + bm) %>%
    pull(pred)
  return(RMSE(test_set$rating, mugyd_preds))
}

# Calculate the date effect using the best lambda identified 
# previously for future use.
date_reg_avg <- train_set %>%
  left_join(movie_reg_avg, by='movieId') %>%
  left_join(user_reg_avg, by="userId") %>%
  left_join(genre_reg_avg, by="genres") %>%
  left_join(year_reg_avg, by="year") %>%
  group_by(week = round_date(date, unit = "week")) %>%
  summarize(bts = sum(rating - bm - bu - bg - by - mu)/(500 + n()))

# Apply the previous function to each item in the lambda sequence.
rmse <- sapply(lambda, rmse_lambda)

# Plot the different RMSE for the different values of 'lbd'.
plot(lambda, rmse)


## -------------------------------------------------------------------------------------------------------------------------------------------
best_lbd <- lambda[which.min(rmse)]
# Store result in the summary dataframe model_results
model_results <- add_row_results(
  model_nb, model_nb, "m, u, g, y and date effects with reg.", best_lbd, min(rmse))
# Display results
kable(model_results, caption = "Summary of results")


## -------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the improvement between this model and the previous one.
improvement(5,6)


## ----Improvement base model vs. movie+user reg bias-----------------------------------------------------------------------------------------
round(improvement(1,3),2)


## ----Improvement movie + user bias vs. all the biases---------------------------------------------------------------------------------------
round(improvement(3,6),2)


## ----Matrix factorisation - create train and test matries, cache = TRUE---------------------------------------------------------------------
# Load the corresponding package
library(recosystem)

reco_train <- data_memory(user_index = train_set$userId,
                          item_index = train_set$movieId,
                          rating = train_set$rating,
                          index1 = TRUE)

reco_test <- data_memory(user_index = test_set$userId,
                          item_index = test_set$movieId,
                          index1 = TRUE)



## ----Create RecoSys object, cache = TRUE----------------------------------------------------------------------------------------------------
reco_obj <- Reco()


## ----Train with 10 iterations, cache = TRUE-------------------------------------------------------------------------------------------------
# Train the algorithm with the default options except for niter 
# and nthread, without tuning
reco_obj$train(reco_train, opts = c(nthread = 4, niter = 10))


## ----Calculate predictions, cache = TRUE----------------------------------------------------------------------------------------------------
# Calculate predicted values with 10 iterations
reco_preds <- reco_obj$predict(reco_test, out_memory())
rmse <- RMSE(reco_preds, test_set$rating)


## ----Update model_results df----------------------------------------------------------------------------------------------------------------
model_nb <- 7
model_results <- add_row_results(
  model_nb, model_nb, "Matrix factorisation, 10 iterations, no tuning", NA, rmse)
# Display results
kable(model_results, caption = "Summary of results")


## ----Train with 20 iterations, cache = TRUE-------------------------------------------------------------------------------------------------
model_nb <- 8
# Train the algorithm with the default options except for nthread, without tuning
reco_obj$train(reco_train, opts = c(nthread = 4, niter = 20))


## ----Calculate prediction with new model, cache = TRUE--------------------------------------------------------------------------------------
# Calculate predicted values
reco_preds <- reco_obj$predict(reco_test, out_memory())
rmse <- RMSE(reco_preds, test_set$rating)
# Update model_results
model_results <- add_row_results(
  model_nb, model_nb, "Matrix factorisation, 20 iterations, no tuning", NA, rmse)
# Display results
kable(model_results, caption = "Summary of results")


## -------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the improvement between this model and the previous one.
improvement(7, 8)


## ----Download saved reco model, eval=local_data, echo=FALSE, include=FALSE, cache = TRUE----------------------------------------------------
## # If local_data this block is executed and the tuning parameters
## # reco-opts.rds gets loaded and used.
## opts <- readRDS("data/reco-opts.rds")


## ----Run tunning function, eval=!local_data, message=FALSE, warning=FALSE, cache = TRUE-----------------------------------------------------
# Run the tuning function to generate optimised parameters. This takes time.
opts <- reco_obj$tune(reco_train, opts = list(nthread  = 4, niter = 10))

# Save the opts object for future use. Creates "data" repository 
# in the current directory if doesn't exist. 
ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)
saveRDS(opts, file = "data/reco-opts.rds")


## ----Train model with optimised parameters, message=FALSE, warning=FALSE, cache = TRUE------------------------------------------------------
model_nb <- 9
# Train the algorithm with the default options except for nthread, with tuning
reco_obj$train(reco_train, opts = c(opts$min, nthread = 4, niter = 20))


## ----Calculate new predictions with tuned model, cache = TRUE-------------------------------------------------------------------------------
# Calculate predicted values
reco_preds <- reco_obj$predict(reco_test, out_memory())
rmse <- RMSE(reco_preds, test_set$rating)
# Update model_results
model_results <- add_row_results(
  model_nb, model_nb, "Matrix factorisation, 20 iterations, tuning", NA, rmse)
# Display results
kable(model_results, caption = "Summary of results")


## -------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the improvement between this model and the previous one.
improvement(8, 9)


## ----RMSE Linear Model applied to validation set, message=FALSE, warning=FALSE, cache = TRUE------------------------------------------------
validation <- validation %>% mutate(date = as_datetime(timestamp))

# Apply all the biases to the validation set.
mugyd_preds <- validation %>%
  left_join(movie_reg_avg, by="movieId") %>%
  left_join(user_reg_avg, by="userId") %>%
  left_join(genre_reg_avg, by="genres") %>%
  left_join(year_reg_avg, by="year") %>%
  mutate(week = round_date(date, unit = "week")) %>%
  left_join(date_reg_avg, by="week") %>%
  mutate(pred = mu +  bts + by + bg + bu + bm) %>%
  pull(pred)

# Calculate the RMSE.
rmse_LM <- RMSE(validation$rating, mugyd_preds)
rmse_LM


## -------------------------------------------------------------------------------------------------------------------------------------------
# Update model_results
model_nb <- 10
model_results <- add_row_results(
  model_nb, model_nb, "Full linear model, validation set", NA, rmse_LM)
kable(model_results, caption = "Summary of results")


## ----RMSE Matrix Factorisation model applied to validation set,  message=FALSE, warning=FALSE, cache = TRUE---------------------------------

# Prepare the validation data in the suitable format.
reco_validation <- data_memory(user_index = validation$userId,
                              item_index = validation$movieId,
                              index1 = TRUE)

# Run predictions using the MF model.
reco_preds <- reco_obj$predict(reco_validation, out_memory())

# Calculate RMSE
rmse_MF <- RMSE(reco_preds, validation$rating)
rmse_MF


## -------------------------------------------------------------------------------------------------------------------------------------------
# Update model_results
model_nb <- 11
model_results <- add_row_results(
  model_nb, model_nb, "Matrix factorisation, validation set", NA, rmse_MF)
kable(model_results, caption = "Summary of results")


## -------------------------------------------------------------------------------------------------------------------------------------------
round(improvement(10, 11),3)

