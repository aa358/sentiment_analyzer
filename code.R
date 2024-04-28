# Make sure your Data.csv file is saved under same directory 
# Install required package
install.packages("sentimentr")

# Load required libraries
library(data.table)
library(sentimentr)

# Define function for sentiment analysis
analyze_sentiment <- function(text) {
  if (is.character(text)) {
    sentiment_score <- sentiment(text)$sentiment
    return(sentiment_score)
  } else {
    return(0)  # Return 0 sentiment score for NA or non-string values
  }
}

# Define function for aspect-based sentiment analysis
aspect_sentiment_analysis <- function(data) {
  results <- list()
  for (i in 1:nrow(data)) {
    text <- as.character(data[i, "commentsReview"])
    aspect <- as.character(data[i, "urlDrugName"])
    sentiment_score <- analyze_sentiment(text)
    result <- list(
      'Text' = text,
      'Aspect' = aspect,
      'Sentiment_Score' = sentiment_score
    )
    results[[i]] <- result
  }
  return(rbindlist(results))
}

# Define main function
main <- function() {
  # Load data from CSV
  tryCatch({
    data <- fread("Data.csv")
  }, error = function(e) {
    cat("Data.csv not found in the current directory.\n")
    return()
  })
  
  # Perform aspect-based sentiment analysis
  analyzed_data <- aspect_sentiment_analysis(data)
  
  # Display results
  cat("Aspect-Based Sentiment Analysis Results:\n")
  print(analyzed_data)
}

# Execute main function
main()
