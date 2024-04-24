class SentimentAnalyzer:
    def __init__(self, positive_file, negative_file):
        # Initialize positive and negative word lists
        self.positive_words = self.read_word_file(positive_file)
        self.negative_words = self.read_word_file(negative_file)

    def read_word_file(self, file_name):
        words = []
        with open(file_name, 'r', encoding='utf-8', errors='ignore') as file:
            for line in file:
                words.append(line.strip().lower())
        return words

    def analyze_sentiment(self, text):
        # Split text into words
        words = text.lower().split()

        # Initialize sentiment score
        sentiment_score = 0
        
        # Iterate through each word
        for i, word in enumerate(words):
            if word in self.positive_words:
                # Increment score for positive words
                sentiment_score += 1
            elif word in self.negative_words:
                # Decrement score for negative words
                sentiment_score -= 1

            # Check for negation words
            if word in ['not', 'no', 'never']:
                # Invert the sentiment of the following word
                if i + 1 < len(words):
                    next_word = words[i + 1]
                    if next_word in self.positive_words:
                        sentiment_score -= 1
                    elif next_word in self.negative_words:
                        sentiment_score += 1

        # Determine sentiment
        if sentiment_score > 0:
            return "Positive"
        elif sentiment_score < 0:
            return "Negative"
        else:
            return "Neutral"

if __name__ == "__main__":
    positive_file = "positive_words.txt"
    negative_file = "negative_words.txt"
    sentiment_analyzer = SentimentAnalyzer(positive_file, negative_file)
    while True:
        user_input = input("Enter a sentence (type 'exit' to quit): ")
        if user_input.lower() == 'exit':
            break
        else:
            sentiment = sentiment_analyzer.analyze_sentiment(user_input)
            print("Sentiment:", sentiment)
