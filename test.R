library(tm)
library(wordcloud)

shareok <- read.csv(file='./shareok/11244-1.csv', head=TRUE, sep=',')
# docs = Corpus(VectorSource(shareok[46]))
docs = Corpus(VectorSource(shareok[52]))

dept <- shareok[52
                ]
cbind(dept, read.table(text = as.character(dept$ou.group), sep = ":"))

