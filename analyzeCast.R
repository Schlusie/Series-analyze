
setwd("D:/Dropbox/zeug/Serien/")
source("./functions.R")
library(plyr)
library(dplyr)
library(ggplot2)
require(igraph)
require(reshape2)
library(wordcloud)
library(tm)
library(XML)


data <- read.table("data.txt", sep=";", header=TRUE, stringsAsFactor=FALSE)

# fullcast Aller serien herunterladen und in ein data.frame packen
castListFull <- dlply(data, .(series), 
                      .fun=function(x) {cat(x[[1]]," ~ ");getCast2(x[2])})
castdfFull <- ldply(castListFull, function(x) x, .progress="text")
castdfFull$episodes <- as.numeric(castdfFull$episodes)
 # NA wegen unknown

# alle schauspieler mit mehr als 4 auftritten in einer serie herausfiltern 
cast <-  ddply(castdfFull, .(series), function(x){
  x %.% filter(episodes > 4) %.% select(ID) %.% droplevels()
})
castdf <- cbind(castdfFull[castdfFull$ID %in% cast$ID,], app=1)

save(castdf, castdfFull, file="castdf.RData")
load("castdf.RData")

# häufigkeit eines stars in der tabelle
castdfSum <- castdf %.% 
  group_by(ID, name) %.% 
  summarise(app = sum(app),
            which = do.call("paste",  list(series, collapse=", "))) %.% 
  arrange(desc(app), name, ID)
castdfSum <- as.data.frame(castdfSum)
filter(castdfSum, app)

filter(castdf, ID=="nm0003493") %.% arrange(desc(episodes))
filter(castdf, ID=="nm0002326") %.% arrange(desc(episodes))
filter(castdf, ID=="nm0432228") %.% arrange(desc(episodes))
filter(castdf, ID=="nm0492687") %.% arrange(desc(episodes))
filter(castdf, ID=="nm0144657") %.% arrange(desc(episodes))
filter(castdf, ID=="nm0437032") %.% arrange(desc(episodes))
filter(castdf, ID=="nm0000405") %.% arrange(desc(episodes))
filter(castdf, ID=="nm0233304") %.% arrange(desc(episodes))
filter(castdf, ID=="nm0785594") %.% arrange(desc(episodes))
filter(castdf, ID=="nm1112955") %.% arrange(desc(episodes))
filter(castdf, ID=="nm0155389") %.% arrange(desc(episodes))


top20Lev <- (castdfSum[1:20, ] %.% arrange(app))$app[1]
top20df <- castdfSum[(castdfSum$app) %in% c(top20Lev:max(castdfSum$app)),] %.% droplevels()
top20df$name <- factor(top20df$name, levels=top20df$name)

pdf("MostCast.pdf", width=8.5, height=11.5)
ggplot(top20df, aes(x=name, y=app)) + geom_bar(stat="identity") + coord_flip() + theme_bw()
dev.off()

#===============================================================================
#===============================================================================
data <- read.table("data.txt", sep=";", header=TRUE, stringsAsFactor=FALSE)

# download 
keywords <- dlply(data, .(series), 
                  .fun = function(x){cat(x[[1]]," ~ ")
                                     getKeywords(x[2])})

keywordsdf <- melt(keywords)
keywordsdf <- data.frame(series = keywordsdf$L1, keyword = keywordsdf$value)
keywordsdf <- droplevels(keywordsdf[which(keywordsdf$keyword != ""), ])
keywordsdf <- ddply(keywordsdf, .(series), function(x){
  ser <- unique(x$series) %.% as.character()
  akt <- filter(data, series==ser)$aktuell
  cbind(x, aktuell=akt)
})

str(keywordsdf)

#######
# wordcloud
#######
ptm <- proc.time()
pdf("keywords.pdf", width=11.5, height=8.5)
keywordsdf$keywordMerged <- gsub(" ([^ ])", "\\U\\1", keywordsdf$keyword, perl=TRUE)
colorF <- colorRampPalette(c("grey40", "red"))
cols <- colorF(max(table(keywordsdf$keywordMerged)))
wordcloud(keywordsdf$keywordMerged, random.order=FALSE, colors=cols, rot.per=0, min.freq=1)

##
keywordscount <- table(keywordsdf$keyword)

countSorted <- sort(keywordscount, TRUE)
lastSorted <- sort(which(countSorted==countSorted[20]), TRUE)[1]
top20plus <- names(countSorted)[1:lastSorted]
df <- data.frame(words=factor(top20plus, levels=top20plus),
                 counts=keywordscount[top20plus])

ggplot(df, aes(x=words, y=counts, fill=counts)) + geom_bar(stat="identity", colour="grey30") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour="black"))
dev.off()
proc.time() - ptm
# User      System verstrichen 
# 68.90       10.23      150.23 

#######
# wordcloud aktuell
#######
ptm <- proc.time()
df <- filter(keywordsdf, aktuell=="ja") %.% droplevels()
pdf("keywordsAktuell.pdf", width=11.5, height=8.5)
df$keywordMerged <- gsub(" ([^ ])", "\\U\\1", df$keyword, perl=TRUE)
colorF <- colorRampPalette(c("grey40", "red"))
cols <- colorF(max(table(df$keywordMerged)))
wordcloud(df$keywordMerged, random.order=FALSE, colors=cols, rot.per=0, min.freq=1)

##
keywordscount <- table(df$keyword)

countSorted <- sort(keywordscount, TRUE)
lastSorted <- sort(which(countSorted==countSorted[20]), TRUE)[1]
top20plus <- names(countSorted)[1:lastSorted]
df <- data.frame(words=factor(top20plus, levels=top20plus),
                 counts=keywordscount[top20plus])

ggplot(df, aes(x=words, y=counts, fill=counts)) + geom_bar(stat="identity", colour="grey30") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour="black"))
dev.off()
proc.time() - ptm
# User      System verstrichen 
# 68.90       10.23      150.23 

#===============================================================================
# Genres
#===============================================================================
data <- read.table("data.txt", sep=";", header=TRUE, stringsAsFactor=FALSE)

# download 
genres <- dlply(data, .(series), .progress="text",
                .fun = function(x){cat(x[[1]]," ~ ")
                                   getGenres(x[2])})

genresdf <- melt(genres)
genresdf <- data.frame(series = genresdf$L1, genre = genresdf$value)
genresdf <- droplevels(genresdf[which(genresdf$genre != ""), ])

#######
# wordcloud
#######
pdf("genres.pdf", width=11.5, height=8.5)
colorF <- colorRampPalette(c("grey40", "red"))
cols <- colorF(max(table(genresdf$genre)))
wordcloud(genresdf$genre, random.order=FALSE, colors=cols, rot.per=0, min.freq=1)

##
genrescount <- table(genresdf$genre)

countSorted <- sort(genrescount, TRUE)
top <- names(countSorted)
df <- data.frame(words=factor(top, levels=top),
                 counts=genrescount[top])

ggplot(df, aes(x=words, y=counts, fill=counts)) + geom_bar(stat="identity", colour="grey30") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour="black"))
dev.off()

