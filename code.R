#Load AFINN lexicon
afinn <- read.delim("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt", header = FALSE, sep = "\t", col.names = c("word", "score"))

#Define negation terms
negation_terms <- c("not", "no", "never")

#Function to analyze sentiment with negation handling
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
    
  #  Adjust sentiment score based on negation
    if (word %in% afinn_lexicon$word) {
      if (negation_flag) {
        sentiment_score <- sentiment_score - afinn_lexicon$score[afinn_lexicon$word == word]
      } else {
        sentiment_score <- sentiment_score + afinn_lexicon$score[afinn_lexicon$word == word]
      }
    }
  }
  
#  Return the sentiment score
  return(sentiment_score)
}

# Example usage
text1 <- "I love this movie! It's fantastic."
text2 <- "This is a very bad movie. I loved it!"
text3 <- "I'm feeling really happy today!"
text4 <- "The weather is terrible. I hate rainy days."
text5 <- "I'm thrilled to hear the news! It's fantastic!"
sentiment_score <- analyze_sentiment(text1, afinn, negation_terms)
print(sentiment_score)
sentiment_score <- analyze_sentiment(text2, afinn, negation_terms)
print(sentiment_score)
sentiment_score <- analyze_sentiment(text3, afinn, negation_terms)
print(sentiment_score)
sentiment_score <- analyze_sentiment(text4, afinn, negation_terms)
print(sentiment_score)
sentiment_score <- analyze_sentiment(text5, afinn, negation_terms)
print(sentiment_score)