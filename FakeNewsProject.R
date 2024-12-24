#<---packages here--->


#<---The Functions are here--->
IsCapsMore <- function(text) {
  # Count upper case characters
  uppercase_matches <- gregexpr("[A-Z]", text)
  
  # Check if there are any matches
  if (length(uppercase_matches[[1]]) == 0) {
    # If no uppercase letters found, the count is 0
    uppercase_count <- 0
  } else {
    uppercase_count <- sum(attr(uppercase_matches[[1]], "match.length") > 0)
  }
  
  # Count total characters
  total_chars <- nchar(text)
  
  # Calculate percentage of uppercase characters
  if (total_chars > 0) {
    caps_char_percentage <- (uppercase_count / total_chars) * 100
  } else {
    caps_char_percentage <- 0  # No characters to calculate percentage
  }
  
  # Return based on percentage of uppercase characters
  if (caps_char_percentage >=15.5) {
    return(1)  # Assuming you want to return 1 if percentage >= 5
  } else {
    return(0)  # Return 0 if percentage < 5
  }
}

capsFuncAcc <- function(r1,r2){
  Pos_counter <- 0
  
  if (r1 == "REAL" && r2 == "0") {
    Pos_counter <- Pos_counter + 1
  } else if (r1 == "FAKE" && r2 == "1") {
    Pos_counter <- Pos_counter + 1
  }
  return(Pos_counter)
}

#<---Main here--->
#Load File  
news <- read.csv("news.csv")

#init more cols
news$X.2 <- 0
news$X.3 <- 0
news$X.4 <- 0
news$X.5 <- 0

#selecting certain columns
selected_news_cols <- news[,c("title","label","X.1","X.2","X.3","X.4","X.5")]
colnames(selected_news_cols) <- c("title","label","IsCaps","att2","att3","att4","att5")

#applying caps function
selected_news_cols$IsCaps <- apply(selected_news_cols["title"],1,IsCapsMore)


#View data frame updates
View(selected_news_cols)

