# Simple text analysis based on the paper Data Science with R Text Mining
# Grahm Williams
# June 2014
# 
# Visit http://onepager.togaware.com/ more OnePageR's.

library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(wordcloud) # Library for plotting wordcloudsd
library(gdata) # library for reading excel
# library(Rgraphviz) # Correlation plots.

# Path to file
# cname <- file.path(".","txt")
# cname

# Input of text files for various types of documents
# PDF
# docs <- Corpus(DirSource(cname),readerControl = list(reader=readPDF))


# simple text
# docs <- Corpus(DirSource(cname))

# cvs file from Purcell
#
# jokes <- read.csv(file='./purcell/ML-AS-Jokes.csv', head=TRUE, sep=',')
# docs = Corpus(VectorSource(jokes[2]))


# This section is for using ShareOK data cleaned up using the Clean_Shareok ipython notebook
#
# shareok <- read.csv(file='./shareok/11244-1.csv', head=TRUE, sep=',')
# shareok <- read.csv(file='./shareok/college_dept.csv', head=TRUE, sep=',')
# shareok <- read.csv(file='./shareok/advisors.csv', head=TRUE, sep=',')
# docs = Corpus(VectorSource(shareok[46]))
# docs = Corpus(VectorSource(shareok[52]))
# docs = Corpus(VectorSource(shareok[2])) # department column
# docs = Corpus(VectorSource(shareok[1])) # college & advisor column

# OU STEM Outcomes
stem_outcome <- read.xls('./stem/STEM_SLOs_2014.xlsx')
# stem_outcome <- read
docs <- Corpus(VectorSource(stem_outcome[1]))

# Twitter
#
# library(twitteR)


# Simple transofrmations for special characters, words, etc

for (j in seq(docs))
{
  docs[[j]] <- gsub("\\.\\.\\.", " ", docs[[j]])
  docs[[j]] <- gsub("\\::", " ", docs[[j]])
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\\'", " ", docs[[j]])
}




docs <- tm_map(docs, tolower)
# docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("english"))
# docs <- tm_map(docs, removeWords, c("may", "also", "can"))
docs <- tm_map(docs, removeWords, c("college", "school", "department", "jeannine",  "rainbolt", "weitzenhoffer", 'mewbourne', 'phillips','conoco', 
                                    'homer','dodge', 'gaylord', 'michael', 'price', 'family'))
docs <- tm_map(docs, stripWhitespace)

# Needed to fix error:Error: inherits(doc, "TextDocument") is not TRUE
docs <- tm_map(docs,PlainTextDocument)

# inspect(docs)

# word stem
# docs <- tm_map(docs, stemDocument)



# Start the wordcloud mapping.

set.seed(123) # Keeps the layout the same

# Dcoument word term matix

dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

freq <- colSums(as.matrix(dtm))
ord <- order(freq)
freq


m = as.matrix(tdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# output to screen and png file
#
# wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
# png("purcell.png", width=12, height=8, units="in", res=300)
# wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))



# wordcloud(dm$word, dm$freq, max.words=20, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
png("wordcloud.png", width=12, height=8, units="in", res=300)

# png("shareok_departments.png", width=12, height=8, units="in", res=300)
# png("shareok_colleges.png", width=12, height=8, units="in", res=300)
# png("shareok_advisors.png", width=12, height=8, units="in", res=300)


wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))


dev.off()

library(xtable)

word_freqs.table <- xtable(dm)
digits(word_freqs.table) <- 0
print(word_freqs.table, floating=FALSE)

