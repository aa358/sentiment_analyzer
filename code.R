# Make sure your Data.csv file is saved under the same directory 

# Install required package
install.packages("sentimentr")
library(sentimentr)
library(data.table)

# Define function for sentiment analysis
analyze_sentiment <- function(text) {
  sentences <- get_sentences(text)
  sentiment_scores <- sapply(sentences, function(sentence) {
    if (is.character(sentence)) {
      sentiment_score <- sentiment(sentence)$sentiment
      return(sentiment_score)
    } else {
      return(0)  # Return 0 sentiment score for NA or non-string values
    }
  })
  return(mean(sentiment_scores))  # Return average sentiment score for all sentences
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
  return(as.data.frame(do.call(rbind, results)))
}

# Define main function
main <- function() {
  # Load data from CSV
  tryCatch({
    data <- fread("Data.csv")
  }, error = function(e) {
    cat("Please Wait.... \n")
    return()
  })
  
  # Perform aspect-based sentiment analysis
  analyzed_data <- aspect_sentiment_analysis(data)
  
  # Save results to a separate file
  output_file <- "aspect_sentiment_analysis_results.csv"
  fwrite(analyzed_data, file = output_file)
  cat("Aspect-Based Sentiment Analysis Results saved to ", output_file, "\n")
  
  # Print number of rows written to the output file
  cat("Number of rows written to the output file: ", nrow(analyzed_data), "\n")
}

# Execute main function
main()
