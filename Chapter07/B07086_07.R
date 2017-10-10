# Extracting unstructured text data from a plain web page

sourceURL <- "https://en.wikipedia.org/wiki/Programming_with_Big_Data_in_R"
link2web <- url(sourceURL)
htmlText <- readLines(link2web)
close(link2web)

# Extracting text data from an HTML page
library(rvest)
sourceURL <- "https://en.wikipedia.org/wiki/Programming_with_Big_Data_in_R"
htmlTextData <- read_html(sourceURL)

html_text(html_nodes(htmlTextData,xpath="//title"))
html_text(html_nodes(htmlTextData,xpath="//p"))


# Extracting text data from an HTML page using the XML library
library(XML)
sourceURL <- "https://en.wikipedia.org/wiki/Programming_with_Big_Data_in_R"
link2web <- url(sourceURL)
htmlText <- readLines(link2web)
close(link2web)
html_doc <- htmlTreeParse(htmlText,useInternalNodes=T)
pageTitle <- xpathSApply(html_doc,"//title",xmlValue)

htmldocTree <- htmlTreeParse(sourceURL, useInternalNodes = T)

# Extracting text data from PubMed

library(pubmed.mineR)
library(RISmed)
keyword <- "Deep Learning"
search_query <- EUtilsSummary(keyword, retmax=10)
summary(search_query)
fetch <- EUtilsGet(search_query)


pmid <- PMID(fetch)
years <- YearPubmed(fetch)
Jtitle <- Title(fetch)
articleTitle <- ArticleTitle(fetch)
abstracts <- AbstractText(fetch)
ptype <- unlist(lapply(PublicationType(fetch),function(x) as.character(x[[1]][1])))
NlmUniqueID <- NlmUniqueID(fetch)
Country <- Country(fetch)

# Importing unstructured text data from a plain text file
absText <- readLines("deepLearning.txt")


#Importing plain text data from a PDF file

library(tm)

pdfFileNames <- list.files(pattern = "pdf$")
importPDF <- readPDF(control=list(text="-layout"))
library(pdftools)
abstractPDFfiles <- Corpus(URISource(pdfFileNames), 
                           readerControl = list(reader = importPDF))
txt <- pdf_text("abstract_1.pdf")

txt <- sapply(pdfFileNames, pdf_text)
info <- pdf_info(pdfFileNames[1])
names(info)

# Pre-processing text data for topic modeling and sentiment analysis


library(pubmed.mineR)
library(RISmed)
keyword <- "Deep Learning"
search_query <- EUtilsSummary(keyword, retmax=50)
summary(search_query)
extractedResult <- EUtilsGet(search_query)


pmid <- PMID(extractedResult)
years <- YearPubmed(extractedResult)
Jtitle <- Title(extractedResult)
articleTitle <- ArticleTitle(extractedResult)
abstracts <- AbstractText(extractedResult)


library(tm)
AbstractCorpus <- Corpus(VectorSource(abstracts))
AbstractCorpus <- tm_map(AbstractCorpus, content_transformer(tolower))
AbstractCorpus <- tm_map(AbstractCorpus, removePunctuation)
AbstractCorpus <- tm_map(AbstractCorpus, removeNumbers)
Stopwords <- c(stopwords('english'))
AbstractCorpus <- tm_map(AbstractCorpus, removeWords, Stopwords)
AbstractCorpus <- tm_map(AbstractCorpus, stemDocument)
trmDocMat <- TermDocumentMatrix(AbstractCorpus, control = list(minWordLength = 1))

# Creating a word cloud to explore unstructured text data
library(pubmed.mineR)
library(RISmed)
keyword <- "Deep Learning"
search_query <- EUtilsSummary(keyword, retmax=50)
summary(search_query)
extractedResult <- EUtilsGet(search_query)


pmid <- PMID(extractedResult)
years <- YearPubmed(extractedResult)
Jtitle <- Title(extractedResult)
articleTitle <- ArticleTitle(extractedResult)
abstracts <- AbstractText(extractedResult)


library(tm)
AbstractCorpus <- Corpus(VectorSource(abstracts))
AbstractCorpus <- tm_map(AbstractCorpus, content_transformer(tolower))
AbstractCorpus <- tm_map(AbstractCorpus, removePunctuation)
AbstractCorpus <- tm_map(AbstractCorpus, removeNumbers)
Stopwords <- c(stopwords('english'))
AbstractCorpus <- tm_map(AbstractCorpus, removeWords, Stopwords)
AbstractCorpus <- tm_map(AbstractCorpus, stemDocument)
trmDocMat <- TermDocumentMatrix(AbstractCorpus, control = list(minWordLength = 1))


tdmMatrix <- as.matrix(trmDocMat)
freq <- sort(rowSums(tdmMatrix),decreasing=TRUE)
tdmDat <- data.frame(word = names(freq),freq=freq)
rownames(tdmDat) <- NULL

library(wordcloud)
wordcloud(tdmDat$word,tdmDat$freq,rot.per=.15,min.freq=10)

# Using regular expression in text processing

sourceURL <- "https://en.wikipedia.org/wiki/Programming_with_Big_Data_in_R"
link2web <- url(sourceURL)
htmlText <- readLines(link2web)
close(link2web)

# Remove html tags
htmlText = gsub("<.*?>", "", htmlText)
# remove puntuation
htmlText = gsub("[[:punct:]]", " ", htmlText)
# remove numbers
htmlText <- gsub('\\d+', '', htmlText)

#htmlText = gsub("[[:blank:]]", " ", htmlText)
htmlText =gsub("[[:space:]]", " ", htmlText)
