## importing libraries

library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

## importing the data

setwd("E:/R/Projects/Movie Recommendation System")
movie_data <- read.csv("./IMDB-dataset/movies.csv")
rating_data <- read.csv("./IMDB-dataset/ratings.csv")

head(movie_data)
summary(movie_data)

head(rating_data)
summary(rating_data)

## data pre-processing
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], "[|]", type.convert = TRUE), 
                              stringsAsFactors = FALSE)
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 <- matrix(0, 10330, 18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for(index in 1:nrow(movie_genre2)){
    for(col in 1:ncol(movie_genre2)){
        gen_col <- which(genre_mat1[1,] == movie_genre2[index, col])
        genre_mat1[index+1,gen_col] <- 1
    }
}
head(genre_mat1)

# remove first row
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors = FALSE)
head(genre_mat2)

# convert from characters to integers
for(col in 1:ncol(genre_mat2)){
    genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

str(genre_mat2)


# create a 'search matrix' that will allow us to perform an easy search
# of the films by specifying the genre present in our list

searchMatrix <- cbind(movie_data[,1:2], genre_mat2)
head(searchMatrix)



rating_matrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm = F)
View(rating_matrix)

# removing user id's
rating_matrix <- as.matrix(rating_matrix[,-1])
View(rating_matrix)

# converting rating matrix into a recommenderlab sparse matrix
rating_matrix <- as(rating_matrix, "realRatingMatrix")
rating_matrix


# overview of some of the important parameters that provide us various options 
# for building recommendation systems for movies

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model, "[[", "description")


# Implementing a single model in our R project - Item Based Collaborative Filtering

recommendation_model$IBCF_realRatingMatrix$parameters



## exploring similar data

similarity_matrix <- similarity(rating_matrix[1:4,], method = "cosine", which = "users")
as.matrix(similarity_matrix)

image(as.matrix(similarity_matrix), main = "User's Similarities")



# similarity that is shared between the films

movie_similarity <- similarity(rating_matrix[, 1:4], method = "cosine", which = "items")
as.matrix(movie_similarity)

image(as.matrix(movie_similarity), main = "Movies similarity")


# most unique ratings

rating_values <- as.vector(rating_matrix@data)
rating_values
unique(rating_values)

# count of movie ratings

table(rating_values)




## Most viewed movies - visualization

#count views for each movie
movie_views <- colCounts(rating_matrix)
movie_views

# create data frame of views
table_views <- data.frame(movie = names(movie_views), views = movie_views)
table_views

# sort by no. of views
table_views <- table_views[order(table_views$views, decreasing = TRUE), ]
table_views

table_views$title <- NA
table_views

for(index in 1:10325){
    table_views[index, 3] = subset(movie_data, 
                                   movie_data$movieId == table_views[index,1])$title
}


ggplot(table_views[1:6, ], aes(x = title, y = views)) +
    geom_bar(stat="identity", fill = 'steelblue') +
    geom_text(aes(label=views), vjust=-0.3, size=3.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Total Views of the Top Films")



## Heatmap of Movie Ratings

image(rating_matrix[1:20, 1:25], axes = FALSE, 
      main = "Heatmap of the first 25 rows and 25 columns")




## Performing Data Preparation

# we have set the threshold for the minimum number of users who have rated a film as 50
# and minumum of 50 views per film

movie_ratings <- rating_matrix[rowCounts(rating_matrix) > 50,
                              colCounts(rating_matrix) > 50]
movie_ratings     # 420 users and 447 films




minimum_movies <- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")



# Visualization of the distribution of the average ratings per user
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill = I("steelblue"), col = I("red")) +
    ggtitle("Distribution of the average rating per user")





## Data Normalization

normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")



## Data Binarization

binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)

good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")




# splitting the dataset into 80% training set and 20% test set

sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]


## Building the Recommendation System using R

recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters


recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)




model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")




sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)

sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red")) + 
    ggtitle("Distribution of the column count")




top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations




user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
    movies_user2[index] <- as.character(subset(movie_data,
                                               movie_data$movieId == movies_user1[index])$title)
}
movies_user2



recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:4]










