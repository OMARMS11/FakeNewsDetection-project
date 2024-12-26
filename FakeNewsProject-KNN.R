# Load necessary packages
library(class)  
library(e1071)  
library(tm)
library(stringi)


#<---Functions are here---> 
IsCapsMore <- function(text) {
  uppercase_matches <- gregexpr("[A-Z]", text)
  if (length(uppercase_matches[[1]]) == 0) {
    uppercase_count <- 0
  } else {
    uppercase_count <- sum(attr(uppercase_matches[[1]], "match.length") > 0)
  }
  total_chars <- nchar(text)
  if (total_chars > 0) {
    caps_char_percentage <- (uppercase_count / total_chars) * 100
  } else {
    caps_char_percentage <- 0
  }
  return(caps_char_percentage)
}

detect_spacing_type <- function(text) {
  lines <- unlist(strsplit(text, "\n"))
  non_empty_count <- sum(nchar(lines) > 0)
  double_newline_count <- length(gregexpr("\n\n", text)[[1]])
  return(double_newline_count)
}

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#<---Functions end here--->

# Load data
news <- read.csv("news.csv")

# Initialize and preprocess
news$X.1 <- 0
news$X.2 <- 0
news$X.3 <- 0

selected_data <- news[, 1:7]
colnames(selected_data)[1] <- "Number_ID"
colnames(selected_data)[5] <- "IsCaps"
colnames(selected_data)[6] <- "line_spacing"
colnames(selected_data)[7] <- "LessThan5000"

selected_data[, 1] <- as.numeric(selected_data[[1]])

selected_data$IsCaps <- apply(selected_data["title"], 1, IsCapsMore)
selected_data$line_spacing <- sapply(selected_data[[3]], detect_spacing_type)
selected_data$LessThan5000 <- ifelse(selected_data[, 1] < 5000, 1, 0)

selected_data <- na.omit(selected_data)
selected_data <- selected_data[selected_data$label %in% c("REAL", "FAKE"), ]
selected_data$label <- as.factor(selected_data$label)

# Normalize numeric features for KNN

selected_data$line_spacing <- normalize(selected_data$line_spacing)
selected_data$IsCaps <- normalize(selected_data$IsCaps)

# Split data
set.seed(123)
index <- sample(1:nrow(selected_data), size = 0.60 * nrow(selected_data))
train_data <- selected_data[index, ]
test_data <- selected_data[-index, ]

# Prepare features and labels for KNN
train_features <- train_data[, c("line_spacing", "IsCaps")]
test_features <- test_data[, c("line_spacing", "IsCaps")]
train_labels <- train_data$label
test_labels <- test_data$label

# Train and predict with KNN
k <- 5  
knn_prediction <- knn(train = train_features, 
                      test = test_features, 
                      cl = train_labels, 
                      k = k)

# Confusion matrix for KNN
knn_conf_matrix <- table(Predicted = knn_prediction, Actual = test_labels)
print(knn_conf_matrix)

# Accuracy for KNN
knn_accuracy <- sum(diag(knn_conf_matrix)) / sum(knn_conf_matrix)
print(paste("KNN Accuracy:", round(knn_accuracy * 100, 2), "%"))
