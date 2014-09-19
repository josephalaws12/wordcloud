# Simple text analysis based on the paper Data Science with R Text Mining
# Grahm Williams
# June 2014
# 
# Visit http://onepager.togaware.com/ more OnePageR's.

library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(wordcloud) # Library for plotting wordclouds
# library(Rgraphviz) # Correlation plots.

# Path to file
cname <- file.path(".","cvs")
cname

# Input of text files for various types of documents
# PDF
# docs <- Corpus(DirSource(cname),readerControl = list(reader=readPDF))

# simple text
# docs <- Corpus(DirSource(cname))

#cvs file from Purcell
jokes <- read.csv(file='./cvs/ArabSpringHumor.csv', head=TRUE, sep=',')
docs = Corpus(VectorSource(jokes[2]))


# Simple transofrmations for special characters, words, etc

for (j in seq(docs))
{
  docs[[j]] <- gsub("\\...", " ", docs[[j]])
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
}

inspect(docs)


docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("department", "email"))
docs <- tm_map(docs, stripWhitespace)

# Needed to fix error:Error: inherits(doc, "TextDocument") is not TRUE
docs <- tm_map(docs,PlainTextDocument)

# word stem
# docs <- tm_map(docs, stemDocument)


freq <- colSums(as.matrix(dtm))
ord <- order(freq)

freq

# Start the wordcloud mapping.

set.seed(123) # Keeps the layout the same

# Dcoument word term matix

dtm <- DocumentTermMatrix(docs)

tdm <- TermDocumentMatrix(docs)

m = as.matrix(tdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# output to screen and png file
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
png("purcell.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

