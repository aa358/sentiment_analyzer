# Load AFINN lexicon
afinn <- read.delim("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt", header = FALSE, sep = "\t", col.names = c("word", "score"))

# Define negation terms
negation_terms <- c("not", "no", "never")

# Function to analyze sentiment with negation handling
analyze_sentiment <- function(text, aspect, afinn_lexicon, negation_terms) {
  # Tokenize the text into sentences
  sentences <- unlist(strsplit(tolower(text), "[.!?]"))
  
  # Initialize sentiment score
  sentiment_scores <- numeric(length(sentences))
  
  # Calculate sentiment score for each sentence
  for (i in 1:length(sentences)) {
    words <- unlist(strsplit(sentences[i], "\\s+"))
    sentiment_score <- 0
    negation_flag <- FALSE
    
    # Calculate sentiment score for each word
    for (word in words) {
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
    }
    
    # Store sentiment score for the sentence
    sentiment_scores[i] <- sentiment_score
  }
  
  # Return the sum of sentiment scores for all sentences
  return(sum(sentiment_scores))
}

# Load the data from input.csv
data <- read.csv("input.csv")

# Function for aspect-based sentiment analysis
aspect_sentiment_analysis <- function(data, afinn_lexicon, negation_terms) {
  results <- data.frame()
  
  for (i in 1:nrow(data)) {
    text <- as.character(data[i, "commentsReview"])
    aspect <- as.character(data[i, "urlDrugName"])
    
    # Analyze sentiment for the text and aspect
    sentiment_score <- analyze_sentiment(text, aspect, afinn_lexicon, negation_terms)
    
    # Append results
    result <- data.frame(
      Text = text,
      Aspect = aspect,
      Sentiment_Score = sentiment_score
    )
    results <- rbind(results, result)
  }
  
  return(results)
}

# Perform aspect-based sentiment analysis
aspect_results <- aspect_sentiment_analysis(data, afinn, negation_terms)

# Load the 'here' package
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# Set the working directory to where the script is located
here::set_here()

# Save results to a separate file
write.csv(aspect_results, "aspect_based_sentiment_analysis_results.csv", row.names = FALSE)

# Print summary statistics
summary(aspect_results$Sentiment_Score)
