#Loading the packages
library(dplyr) #grammar of data manipulation
library(recommenderlab) #Developing and testing Recommender Algorithms
library(Matrix) #Sparse and Dense Matrix Classes and Methods for and operatios on these matrices
library(methods) #Formally defined methods and classes for R objects
library(ggplot2) #Create Elegant Data Visualisations Using the Grammar of Graphics
library(knitr) #A General-Purpose Package for Dynamic Report Generation in R
library(data.table) #extension of data.frame
library(grid) #A rewrite of the graphics layout capabilities, plus some support for interaction
library(gridExtra) #Provides a number of user-level functions to work with "grid" graphics, notably to arrange multiple grid-based plots on a page, and draw tables
library(DT) #Data objects in R can be rendered as HTML tables using the JavaScript library 'DataTables'
library(stringr) #a cohesive set of functions designed to make working with strings as easy as possible.
library(tidyr) #It's designed specifically for data tidying (not general reshaping or aggregating) and works well with 'dplyr' data pipelines.

#loading the files
books<-read.csv("Desktop/Book_Recommender/books.csv")
ratings<-read.csv("Desktop/Book_Recommender/ratings.csv")
tags<-read.csv("Desktop/Book_Recommender/tags.csv")
book_tags<-read.csv("Desktop/Book_Recommender/book_tags.csv")

#Viewing the files
View(books)
View(ratings)
View(tags)
View(book_tags)

#--------------------------------------------------
#Data Cleaning

#Removing Duplicate ratings

ratings %>% group_by(user_id, book_id) %>% mutate(Number_of_ratings = n()) -> ratings
table(ratings$Number_of_ratings)
ratings %>% filter(Number_of_ratings>1)-> duplicate_ratings

#remove duplicate ratings
ratings %>% filter(Number_of_ratings==1)->ratings

#Removing users that rated less than 3 books
ratings %>% group_by(user_id) %>% mutate(ratings_given = n()) -> ratings
ratings %>% filter(ratings_given > 2) -> ratings

#------------------------------------------------
#Data Exploration
#Tasks:
  # 1. Extract a sample set of 2% records from ratings.csv
  # 2. Make a plot of the "distribution of ratings"
  # 3. Make a plot of "number of ratings per each book"
  # 4. Make a plot of the percentage distribution of different "genres"
  # 5. Find the top 10 books with highest ratings
  # 6. Find the top 10 most popular books

#1. Extracting a sample set of 2% records
set.seed(1) #starting number for random number generation
ratings_fraction <- 0.02 #percentage of dataset we want in sample = 2%
users <- unique(ratings$user_id) #creating a vector of all unique user ids
sample_users <- sample(users, round(ratings_fraction*length(users))) #takes 2% of all users
nrow(ratings)
sample_ratings <- ratings %>% filter(user_id %in% sample_users) #ratings given by the selected 2% of users
nrow(sample_ratings)

#2. Making a plot of the distribution of ratings
sample_ratings %>%
  ggplot(aes(x=rating, fill=factor(rating)))  +
    geom_bar(color="grey20") +
      scale_fill_brewer(palette = "YlGnBu") +
          guides(fill= FALSE)

#3. Make a plot of "number of ratings per each book"

sample_ratings %>%
  group_by(book_id) %>% summarise(number_of_ratings_per_book = n()) %>%
  ggplot(aes(x=number_of_ratings_per_book)) + geom_bar(fill="orange", color="grey20", width = 1) +
    coord_cartesian(c(0,40))

#4 Make a plot of the percentage distribution of the different genres
#Finding the count of different genres

genres <- str_to_lower(c("Art","Biography","Business","Chick lit", "Children's","Christian","Classics","Comics","Cookbooks","Crime","Fantasy","Graphic Novels","Historical Fiction","History","Horror","Humor and Comedy","Manga","Memoir","Music","Mystery","Paranormal","Philosophy","Poetry","Psychology","Religion","Romance","Science","Science Fiction","Self Help","Suspense","Spirituality","Sports","Thriller","Travel","Young Adult"))
available_genres <- genres[str_to_lower(genres) %in% tags$tag_name]
available_tags <- tags$tag_id[match(available_genres, tags$tag_name)]

book_tags %>% filter(tag_id %in% available_tags) %>%
  group_by(tag_id) %>% summarise(n=n()) %>%
    ungroup() %>%
      mutate(sumN=sum(n), percentage=n/sumN) %>% arrange(-percentage) %>%
        left_join(tags, by="tag_id")-> book_info

book_info %>% ggplot(aes(reorder(tag_name, percentage),percentage, fill=percentage)) +
  geom_bar(stat="identity") + labs(x="Genres", y="Percentage") + coord_flip() +
    scale_fill_distiller(palette="YlOrRd")

# 5. Find the top 10 books with highest ratings
books %>% arrange(-average_rating) %>%
  top_n(10, wt=average_rating) %>%
    select(book_id, title, authors, average_rating) -> top10

# 6. Find the top 10 most popular books
books %>% arrange(-ratings_count) %>%
  top_n(10, wt=ratings_count) %>%
    select(book_id, title, authors, ratings_count) -> popular10

#-------------------------------------------------
#Restructuring data to build collaborative filtering

dimension_names <- list(user_id=sort(unique(sample_ratings$user_id)), book_id=sort(unique(sample_ratings$book_id)))
ratingmat <- spread(select(sample_ratings, user_id, book_id, rating), book_id, rating) %>% select(-user_id) #to spread our data, i.e., assign all user_ids to rows and book_ids to columns

class(ratingmat) #still exists as a data frame. Spread creates an object that is a data frame
ratingmat <- as.matrix(ratingmat) #converts the data frame to a matrix
class(ratingmat) #to check if it is a matrix
ratingmat[1:5,1:5] #to see the first 5 rows and columns
#but we still have the user ids and we dont really need them and so we get rid of them
ratingmat[,-1] -> ratingmat
ratingmat[1:5,1:5]
dimnames(ratingmat) <- dimension_names
dim(ratingmat)

#--------------------------
#Converting the matrix into a real rating matrix. This is necessary for collaborative filtering/ recommendation system
ratingmat0 <- ratingmat
ratingmat0[is.na(ratingmat0)] <-0 #replaces all NA values with 0
sparse_rating <- as(ratingmat0, "sparseMatrix") #creates a sparse matrix, i.e., replaces all the 0s with . --> saves space
real_rating <- new("realRatingMatrix", sparse_rating) #finally creating the matrix we can build the recommendation engine with
real_rating

#-------------------------
#splitting the sample into a train and test set
sample(x=c(T,F), size = nrow(real_rating), replace= T, prob = c(0.8,0.2)) -> split_ratio
real_rating[split_ratio,] -> recc_train
real_rating[!split_ratio,] -> recc_test

#creating the model
Recommender(data=recc_train, method="UBCF") -> recc_model_ubcf
n_recommended_ubcf <- 6
predict(recc_model_ubcf, newdata=recc_test, n=n_recommended_ubcf) -> predicted_ubcf

predicted_ubcf@items[[1]] -> recc_books_user1
predicted_ubcf@itemLabels[recc_books_user1]

books %>% filter(id==569) %>% select(title, authors)
