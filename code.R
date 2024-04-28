# Load AFINN lexicon
afinn <- read.delim("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt", header = FALSE, sep = "\t", col.names = c("word", "score"))

# Define negation terms
negation_terms <- c("not", "no", "never")

# Function to analyze sentiment with negation handling
analyze_sentiment <- function(sentence, afinn_lexicon, negation_terms) {
  # Tokenize the sentence into words
  words <- strsplit(tolower(sentence), "\\s+")[[1]]
  
  # Initialize sentiment score
  sentiment_score <- 0
  
  # Initialize negation flag
  negation_flag <- FALSE
  
  # Calculate sentiment score for each word
  for (i in 1:length(words)) {
    word <- words[i]
    
    # Check for negation terms
    if (word %in% negation_terms) {
      negation_flag <- TRUE
    }
    
    # Adjust sentiment score based on negation
    if (word %in% afinn_lexicon$word) {
      if (negation_flag) {
        sentiment_score <- sentiment_score - afinn_lexicon$score[afinn_lexicon$word == word]
        negation_flag <- FALSE  # Reset negation flag
      } else {
        sentiment_score <- sentiment_score + afinn_lexicon$score[afinn_lexicon$word == word]
      }
    } 
    # Words not in the lexicon are treated as neutral
  }
  
  # Return the sentiment score
  return(sentiment_score)
}

# Load the data from Data.csv
data <- read.csv("~/IS392 Final Project/sentiment_analyzer/Data.csv")

# Extract the text data from the specified column and first 500 rows
text_data <- as.character(data[1:500, 9])

# Initialize a list to store the text data
text_list <- list()

# Store the text data in a list
for (i in 1:500) {
  text_list[[paste0("text", i)]] <- text_data[i]
}

# Analyze sentiment for each text
sentiment_scores <- numeric(length(text_list))
for (i in 1:500) {
  sentiment_scores[i] <- analyze_sentiment(text_list[[paste0("text", i)]], afinn, negation_terms)
}

# Print the sentiment scores
for (i in 1:500) {
  cat(paste("Sentiment score for text", i, ":", sentiment_scores[i]), "\n")
}

positive <- sum(sentiment_scores > 0)
negative <- sum(sentiment_scores < 0)
neutral <- sum(sentiment_scores == 0)
total <- length(sentiment_scores)
cat("Proportion of positive sentiments:", positive / total, "\n")
cat("Proportion of negative sentiments:", negative / total, "\n")
cat("Proportion of neutral sentiments:", neutral / total, "\n")

min_sentiment_score <- min(sentiment_scores)
max_sentiment_score <- max(sentiment_scores)
cat("Minimum sentiment score:", min_sentiment_score, "\n")
cat("Maximum sentiment score:", max_sentiment_score, "\n")

# Calculate the mean of the sentiment scores
mean_sentiment_score <- mean(sentiment_scores)

# Print the mean sentiment score
cat("Mean sentiment score:", mean_sentiment_score, "\n")

sd_sentiment_score <- sd(sentiment_scores)
cat("Standard deviation of sentiment scores:", sd_sentiment_score, "\n")

hist(sentiment_scores, breaks = 50, main = "Histogram of Sentiment Scores", xlab = "Sentiment Score", ylab = "Frequency")

#Test to verify scores of each individual variable
test1 <- "This drug may not be for everyone but its wonderful for me! It makes me a totally different person, a better person."
sentiment_score <- analyze_sentiment(test1, afinn, negation_terms)
print(sentiment_score)

