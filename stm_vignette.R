######################################
## Text as Data Workshop
## Author: Lily Fesler
## Adapted STM vignette: https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf
## Modified: 5/29/19
######################################

rm(list = ls())

### Install Packages (only need to do this once)
install.packages("stm")
install.packages("wordcloud")

library(stm)
library(wordcloud)

data <- read.csv("http://scholar.princeton.edu/sites/default/files/bstewart/files/poliblogs2008.csv")
head(data)

# preprocess: convert to lower case, remove punctuation, remove stopwords
# remove numbers, and stem
processed <- textProcessor(data$documents, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# remove infrequent terms
# this will also remove any rows in metadata due to text rows being discarded
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab,
                        processed$meta, lower.thresh = 15)


# estimate structural topic model, allowing prevalence of topics to vary by
# rating (conservative/liberal)
# note: this takes a long time to run!
poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                          K = 20, prevalence =~ rating + s(day),
                          max.em.its = 75, data = out$meta,
                          init.type = "Spectral")

# you can also load the poliblogPrevFit results here to save time
# instead of running stm command above
load(url("http://goo.gl/VPdxlS"))

# thetas represent the proportion of each document dedicated to each topic
# for document 1, these are the proportions (which sum to 1)
poliblogPrevFit$theta[1,]
sum(poliblogPrevFit$theta[1,])

# read most probable words to create topic labels
labelTopics(poliblogPrevFit, c(3, 7, 20))

# view topic proportions
par(mfrow = c(1, 1), mar=c(3,3,3,3))
plot(poliblogPrevFit, type = "summary", xlim = c(0, .3))


# read documents that have highest topic prevalence
# note: this pulls from the first 200 characters of the document (shortdoc) since
# the full documents are too long
thoughts3 <- findThoughts(poliblogPrevFit, texts = shortdoc, 
                  n = 2, topics = 3)$docs[[1]]
thoughts20 <- findThoughts(poliblogPrevFit, texts = shortdoc,
                  n = 2, topics = 20)$docs[[1]]
par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))
plotQuote(thoughts3, width = 30, main = "Topic 3")
plotQuote(thoughts20, width = 30, main = "Topic 20")

# visualize words in a given topic using a word cloud
par(mfrow = c(1, 1), mar=c(1,1,1,1))
cloud(poliblogPrevFit, topic = 7)

# estimating metadata/topic relationships
# uncertainty = Global allows uncertainty from topics to be taken into account
prep <- estimateEffect(1:20 ~ rating + s(day), poliblogPrevFit,
                       meta = out$meta, uncertainty = "Global")
summary(prep, topics=1)
summary(prep, topics = 20)

# visualize metadata relationships

# binary: conservative/liberal
par(mfrow = c(1, 1), mar=c(3,3,3,3))
plot(prep, covariate = "rating", topics = c(3, 7, 20), 
     model = poliblogPrevFit, method = "difference", 
     cov.value1 = "Liberal", cov.value2 = "Conservative", 
     xlab = "More Conservative ... More Liberal", 
     main = "Effect of Liberal vs. Conservative", 
     xlim = c(-.1, .1), labeltype = "custom", 
     custom.labels = c('Obama', 'Sarah Palin','Bush Presidency'))

# continuous: day
par(mfrow = c(1, 1), mar=c(5,5,5,5))
plot(prep, "day", method = "continuous", topics = 7, 
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"), 
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
                labels = monthnames)

# which words are used more by conservatives, and which by liberals?
par(mfrow = c(1, 1), mar=c(1,1,1,1))
plot(poliblogContent, type = "perspectives", topics = 11)

# which words are associated with topic 12 vs topic 20?
plot(poliblogPrevFit, type = "perspectives", topics = c(12, 20))
