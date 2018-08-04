rm(list=ls())

#Set working directory
setwd("E:/1ST SEM/eng/edwisor_assignments/text_minning")

#load libraries
library(stringr)
library(tm)
library(wordcloud)
#library(slam)


# here is a pdf for mining
url <- "https://drive.google.com/file/d/1gZCnlhwVMBIE0SugUUxDIgQrfVz-cDQR/view"
dest <- tempfile(fileext = ".pdf")
download.file(url, dest, mode = "wb")

# set path to pdftotxt.exe and convert pdf to text
exe <- "C:/Program Files/xpdf-tools-win-4.00/xpdf-tools-win-4.00/bin32/pdftotext.exe"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

# get txt-file name and open it  
filetxt <- sub(".pdf", ".txt", dest)
system(filetxt)


txt <- readLines(filetxt) # don't mind warning..

df=txt

#case folding
txt <- tolower(txt)

#remove stopwords
txt <- removeWords(txt, c("\\f", stopwords()))


corpus <- Corpus(VectorSource(txt))

#remove puctuation marks
corpus <- tm_map(corpus, removePunctuation)

#convert to text document matrix 
tdm <- TermDocumentMatrix(corpus)


#Convert term document matrix into dataframe
TDM_data = as.data.frame(t(as.matrix(tdm))) 

library(slam)
##calculate the terms frequency
words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum)

#Convert into matrix
words_freq = as.matrix(words_freq)

#Convert to proper dataframe
words_freq = data.frame(words_freq)


#Convert row.names into index
words_freq$words = row.names(words_freq)
row.names(words_freq) = NULL
words_freq = words_freq[,c(2,1)]
names(words_freq) = c("Words", "Frequency")



words_freq <- words_freq[order(words_freq$Words),]
rownames(words_freq) <- 1:nrow(words_freq)
words_freq=words_freq[-(1:15), , drop = FALSE]
rownames(words_freq) <- 1:nrow(words_freq)

#storing the result in an excel sheet
library(xlsx) #load the package
write.xlsx(x =words_freq , file = "test.excelfile.xlsx",
           sheetName = "TestSheet", row.names = FALSE)


#Most frequent terms which appears in atleast 700 times
findFreqTerms(tdm, 10)


##wordcloud
postCorpus_WC = corpus

library(RColorBrewer)
library(wordcloud)
pal2 = brewer.pal(8,"Dark2")
png("wordcloud_v22.png", width = 12, height = 8, units = 'in', res = 300)
wordcloud(postCorpus_WC, scale = c(5,.2), min.freq = 2, max.words = 150, random.order = FALSE, rot.per = .15, colors = pal2)
dev.off()
