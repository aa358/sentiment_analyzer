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
      } else {
        sentiment_score <- sentiment_score + afinn_lexicon$score[afinn_lexicon$word == word]
      }
    }
  }
  
  # Return the sentiment score
  return(sentiment_score)
}

# Load the data from Data.csv
data <- read.csv("~/IS392 Final Project/sentiment_analyzer/Data.csv")

# Initialize a list to store the text data
text_list <- list()

# Extract the text data from the specified column and rows
for (i in 1:10) {
  text_list[[paste0("text", i)]] <- as.character(data[i, 9])
}

# Analyze sentiment for each text
sentiment_scores <- numeric(length(text_list))
for (i in 1:10) {
  sentiment_scores[i] <- analyze_sentiment(text_list[[paste0("text", i)]], afinn, negation_terms)
}

# Print the sentiment scores
for (i in 1:10) {
  cat(paste("Sentiment score for text", i, ":", sentiment_scores[i]), "\n")
}

#Test to verify scores of each individual variable
test1 <- "This drug may not be for everyone but its wonderful for me! It makes me a totally different person, a better person."
sentiment_score <- analyze_sentiment(test1, afinn, negation_terms)
print(sentiment_score)

