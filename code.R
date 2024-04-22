# *** code ***

afinn <- read.delim("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt", header = FALSE, sep = "\t", col.names = c("word", "score"))

analyze_sentiment <- function(sentence, afinn_lexicon) {
  # Tokenize the sentence into words
  words <- strsplit(tolower(sentence), "\\s+")[[1]]
  
  # Initialize sentiment score
  sentiment_score <- 0
  
  # Calculate sentiment score for each word
  for (word in words) {
    if (word %in% afinn_lexicon$word) {
      sentiment_score <- sentiment_score + afinn_lexicon$score[afinn_lexicon$word == word]
    }
  }
  
  # Return the sentiment score
  return(sentiment_score)
}

# Example usage
text1 <- "I love this movie! It's fantastic."
text2 <- "This is a very bad movie. I loved it!"
text3 <- "I'm feeling really happy today!"
text4 <- "The weather is terrible. I hate rainy days."
text5 <- "I'm thrilled to hear the news! It's fantastic!"
sentiment_score <- analyze_sentiment(text1, afinn)
print(sentiment_score)
sentiment_score <- analyze_sentiment(text2, afinn)
print(sentiment_score)
sentiment_score <- analyze_sentiment(text3, afinn)
print(sentiment_score)
sentiment_score <- analyze_sentiment(text4, afinn)
print(sentiment_score)
sentiment_score <- analyze_sentiment(text5, afinn)
print(sentiment_score)
