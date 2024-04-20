# *** code ***

# Install and load required packages
install.packages("textclean")
install.packages("dplyr")
install.packages("purrr")
install.packages("quanteda")

library(textclean)
library(dplyr)
library(purrr)
library(quanteda)

# Load AFINN lexicon
afinn <- read.delim("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt", header = FALSE, sep = "\t", col.names = c("word", "score"))

# Function to calculate sentiment score of a text
calculate_sentiment <- function(text, lexicon) {
  tokens <- tokens(text, remove_punct = TRUE)
  tokens <- tokens_tolower(tokens)
  scores <- sum(tokens_lookup(tokens, lexicon$word, lexicon$score, nomatch = 0))
  return(scores)
}

# Example text
text1 <- "This is a very bad movie. I loved it!"
text2 <- "I'm feeling really happy today!"
text3 <- "The weather is terrible. I hate rainy days."
text4 <- "I'm thrilled to hear the news! It's fantastic!"

# Calculate sentiment score
sentiment_score <- calculate_sentiment(text4, afinn)

# Print sentiment score
print(sentiment_score)
