# Construct file paths
script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
positive_file <- file.path(script_directory, "positive_words.txt")
negative_file <- file.path(script_directory, "negative_words.txt")
data_file <- file.path(script_directory, "Data.csv")

# Function to read word file
read_word_file <- function(file_name) {
  words <- readLines(file_name, warn = FALSE)
  words <- iconv(words, "UTF-8", "ASCII", sub = "byte")
  words <- tolower(words)
  return(words)
}

# Read positive and negative word files
positive_words <- read_word_file(positive_file)
negative_words <- read_word_file(negative_file)

# Function to analyze sentiment
analyze_sentiment <- function(text) {
  words <- tolower(strsplit(text, "\\s+")[[1]])
  sentiment_score <- 0
  
  for (i in seq_along(words)) {
    word <- words[i]
    if (word %in% positive_words) {
      sentiment_score <- sentiment_score + 1
    } else if (word %in% negative_words) {
      sentiment_score <- sentiment_score - 1
    }
    
    if (word %in% c("not", "no", "never")) {
      if (i + 1 <= length(words)) {
        next_word <- words[i + 1]
        if (next_word %in% positive_words) {
          sentiment_score <- sentiment_score - 1
        } else if (next_word %in% negative_words) {
          sentiment_score <- sentiment_score + 1
        }
      }
    }
  }
  
  if (sentiment_score > 0) {
    return("Positive")
  } else if (sentiment_score < 0) {
    return("Negative")
  } else {
    return("Neutral")
  }
}

# Read sentences from data file and analyze sentiment
data_sentences <- readLines(data_file)
for (sentence in data_sentences) {
  sentiment <- analyze_sentiment(sentence)
  print(paste("Sentence:", sentence))
  print(paste("Sentiment:", sentiment))
}
