
#View data frame updates#<---packages here--->
library(rpart)
library(rpart.plot)
library(e1071)
library(tm)
library(stringi)
library(textreadr)
library(e1071)
library(xgboost)

#<---The Functions are here--->
IsCapsMore <- function(text) {
  
  uppercase_matches <- gregexpr("[A-Z]", text)
  
  
  if (length(uppercase_matches[[1]]) == 0) {
    
    uppercase_count <- 0
  } else {
    uppercase_count <- sum(attr(uppercase_matches[[1]], "match.length") > 0)
  }
  
  
  total_chars <- nchar(text)
  
  # Calculate percentage of uppercase characters
  if (total_chars > 0) {
    caps_char_percentage <- (uppercase_count / total_chars) * 100
  } else {
    caps_char_percentage <- 0  # No characters to calculate percentage
  }
  
  # Return based on percentage of uppercase characters
  if (caps_char_percentage >=15.5) {
    return(caps_char_percentage) 
  } else {
    return(caps_char_percentage)  
  }
}

count_syllables <- function(word) {
  stri_count_words(stri_replace_all_regex(word, "[^aeiouy]", ""))
}

clean_text <- function(text){
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))  # Convert text to lowercase
  corpus <- tm_map(corpus, removePunctuation)              # Remove punctuation
  corpus <- tm_map(corpus, removeNumbers)                  # Remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)
}


#<-Shams Functions start->
detect_spacing_type <- function(text) {
  # Split the text into lines
  lines <- unlist(strsplit(text, "\n"))
  
  # Count non-empty lines
  non_empty_count <- sum(nchar(lines) > 0)
  
  # Count occurrences of "\n\n"
  double_newline_count <- length(gregexpr("\n\n", text)[[1]])
  
  # Check if "\n\n" count is exactly one less than the number of non-empty lines
  if (double_newline_count == non_empty_count - 1) {
    return(double_newline_count)  # Condition met
  } else {
    return(double_newline_count)  # Condition not met
  }
}
#<---End of the Function section--->

#<---Main here--->
#Load File  
news <- read.csv("news.csv")

#init more cols
news$X.1 <- 0
news$X.2 <- 0
news$X.3 <- 0

#selecting certain columns
selected_data <- news[, 1:7]
colnames(selected_data)[1] <- "Number_ID"
colnames(selected_data)[5] <- "IsCaps"
colnames(selected_data)[6] <- "line_spacing"
colnames(selected_data)[7] <- "LessThan5000"



selected_data[, 1] <- as.numeric(selected_data[[1]])

#applying  functions 
selected_data $IsCaps <- apply(selected_data ["title"],1,IsCapsMore)
#<-shams code start->
selected_data$line_spacing <- sapply(selected_data[[3]], detect_spacing_type)
selected_data$LessThan5000 <- ifelse(selected_data[, 1] < 5000, 1, 0)
#<-shams code end->

selected_data <- na.omit(selected_data)
head(selected_data)
selected_data <- selected_data[selected_data$label %in% c("REAL", "FAKE"), ]
selected_data$label <- as.factor(selected_data$label)

#Split data
set.seed(123) 

index <- sample(1:nrow(selected_data), size = 0.75 * nrow(selected_data)) 
train_data <- selected_data[index, ]
test_data <- selected_data[-index, ]


# Prepare data
train_matrix <- as.matrix(train_data[, c("IsCaps", "line_spacing")])
test_matrix <- as.matrix(test_data[, c("IsCaps", "line_spacing")])
train_label <- as.numeric(train_data$label) - 1  # Convert to binary 0,1
test_label <- as.numeric(test_data$label) - 1

# Train XGBoost model
xgb_model <- xgboost(data = train_matrix, label = train_label, 
                     nrounds = 100, objective = "binary:logistic")

# Predict using the model
xgb_prediction <- predict(xgb_model, test_matrix)
xgb_pred_class <- ifelse(xgb_prediction > 0.5, "REAL", "FAKE")

# Evaluate Accuracy
conf_matrix <- table(Predicted = xgb_pred_class, Actual = test_data$label)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("XGBoost Accuracy:", round(accuracy * 100, 2), "%"))



#View data frame updates





