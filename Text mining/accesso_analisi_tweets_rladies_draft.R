# installare i pacchetti mancanti
install.packages("rtweet")
install.packages("wordcloud")

# richiamare i pacchetti
library(rtweet)
library(devtools)
library(dplyr)
library(magrittr)
library(stringr)
library(syuzhet)
library(gridExtra)
library(plyr)
library(wordcloud)
library(qdap)
library(sentR)
library(wordcloud2)
library(e1071)
library(tm)
library(caret)
library(e1071)
library(tidyverse)

# creazione account Twitter normale

# creazione account Twitter dev

# https://developer.twitter.com

# identifichiamoci con la nostra api

## api

api_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
api_secret_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"

## autenticazione
token <- create_token(
        app = "R_ladies_test1",
        consumer_key = api_key,
        consumer_secret = api_secret_key,
        access_token = access_token,
        access_secret = access_token_secret)

# codici woeid

# https://en.wikipedia.org/wiki/WOEID

# https://www.findmecity.com

# New York
get_trends(woeid = 2459115)

# London
get_trends(woeid = 44418)

# Italia
get_trends(woeid = 23424853)

# con le coordinate geografiche

get_trends(lat = 45, lng = 9)

# per nome

get_trends("rome")

# salviamo un oggetto

trends_italy <- get_trends("italy")

View(trends_italy)

# cerchiamo tramite parola chiave

rl1 <- search_tweets("#rladies", n = 18000, include_rts = FALSE)

View(rl1)

# come possiamo rifinire l'estrazione dati

rl1 <- search_tweets("#rladies", n = 18000, include_rts = FALSE, language = 'en', 
                     since = "2021-05-10")

rl1 %>% ts_plot("1 day") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
                x = NULL, y = NULL,
                title = "Frequenza dei tweet sul tema",
                subtitle = "Twitter status (tweet) counts aggregated using 1 day intervals",
                caption = "\nSource: Data collected from Twitter's REST API via rtweet")


rl2 <- search_tweets("#rladies", n = 18000, include_rts = FALSE, retryonratelimit = TRUE,
                     type = "recent", "lang:en", geocode = lookup_coords("usa"))

# interroghiamo un account

# gli ultimi aggiornamenti
get_timeline("@AstroSamantha")

# i suoi amici
get_friends("@AstroSamantha")

# i suoi followers
get_followers("@AstroSamantha")

# i suoi status preferiti
get_favorites("@AstroSamantha", n = 100)

# gli user che parlano di un certo argomento
AS_fans <- search_users("#AstroSamantha", n = 1000)

# @KamalaHarris

# cerchiamo i dati 

kh_test1 <- search_users("@KamalaHarris", n = 1000)

# luoghi da cui twittano

length(unique(kh_test1$location))

# grafico

kh_test1 %>%
        dplyr::count(location, sort = TRUE) %>%
        mutate(location = reorder(location, n)) %>%
        top_n(10) %>%
        ggplot(aes(x = location, y = n)) +
        geom_col() +
        coord_flip() +
        labs(x = "Count",
             y = "Location",
             title = "Where Twitter users are from - unique locations ")

sort(table(kh_test1$location), decreasing = TRUE)

# pulizia della variabile

kh_test1$location <- str_trim(kh_test1$location)

kh_test1$location[kh_test1$location == ""] <- "n/a"

# ripetere riga 124

#####################################################

az <- search_tweets("#astrazeneca", n = 15000, include_rts = FALSE)
pfizer <- search_tweets("#pfizer", n = 15000, include_rts = FALSE, retryonratelimit = TRUE)
moderna <- search_tweets("#moderna", n = 15000,include_rts = FALSE, retryonratelimit = TRUE)

# scarichiamo i dati

save_as_csv(az, "az.csv")
save_as_csv(pfizer, "pfizer.csv")
save_as_csv(moderna, "moderna.csv")
 
az2 <- read.csv("az.csv")
pfizer2 <- read.csv("pfizer.csv")
moderna2 <- read.csv("moderna.csv")

nrow(az)
nrow(pfizer)
nrow(moderna)

# mettiamo a confronto i tweet

az_plot <- az %>% ts_plot("1 day") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
                x = NULL, y = NULL,
                title = "Frequenza dei tweet su AstraZeneca",
                subtitle = "Twitter status (tweet) counts aggregated using 1 day intervals",
                caption = "\nSource: Data collected from Twitter's REST API via rtweet")

pf_plot <- pfizer %>% ts_plot("1 day") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
                x = NULL, y = NULL,
                title = "Frequenza dei tweet su Pfizer",
                subtitle = "Twitter status (tweet) counts aggregated using 1 day intervals",
                caption = "\nSource: Data collected from Twitter's REST API via rtweet")

mod_plot <- moderna %>% ts_plot("1 day") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
                x = NULL, y = NULL,
                title = "Frequenza dei tweet su Moderna",
                subtitle = "Twitter status (tweet) counts aggregated using 1 day intervals",
                caption = "\nSource: Data collected from Twitter's REST API via rtweet")

grid.arrange(az_plot, pf_plot, mod_plot, ncol=2)

az <- az[az$lang == "en",]
pfizer <- pfizer[pfizer$lang== "en",]
moderna <- moderna[moderna$lang== "en",]

nrow(az)
nrow(pfizer)
nrow(moderna)

taz <- az$text
tpfizer <- pfizer$text
tmoderna <- moderna$text

# pacchetti per la sentiment analysis

sent_taz <- get_sentiment(taz, method = "nrc")
sent_tpfizer <- get_sentiment(tpfizer, method = "nrc")
sent_tmoderna <- get_sentiment(tmoderna, method = "nrc")

az1 <- table(sent_taz)
az1 <- as.data.frame(az1)
names(az1)[1] <- "sentiment_score"
names(az1)[2] <- "astrazeneca"

pfizer1 <- table(sent_tpfizer)
pfizer1 <- as.data.frame(pfizer1)
names(pfizer1)[1] <- "sentiment_score"
names(pfizer1)[2] <- "pfizer"

moderna1 <- table(sent_tmoderna)
moderna1 <- as.data.frame(moderna1)
names(moderna1)[1] <- "sentiment_score"
names(moderna1)[2] <- "moderna"

df <- full_join(az1, pfizer1,"sentiment_score")

df <- full_join(df, moderna1, "sentiment_score")

df$sentiment_score <- as.numeric(as.character(df$sentiment_score))

str(df)

par("mfcol"=c(3, 1))
plot(df$sentiment_score, df$astrazeneca, type = "l", col = "red")
plot(df$sentiment_score, df$pfizer, type = "l", col = "blue")
plot(df$sentiment_score, df$moderna, type = "l", col = "green")

# pacchetto shuyzet 

# get_nrc_sentiment()

get_nrc_sentiment(taz[1:100])
sent1 <- colSums(get_nrc_sentiment(taz[1:100]))

sent1 <- sort(sent1, decreasing = TRUE)

barplot(sent1, col = rainbow(length(sent1)))

# get_sentiment(text, method = "syuzhet")

# altri dizionari per calcolare il sentiment

# importiamo le liste di parole positive e negative
pos <- scan("/Users/valentinaporcu/R_test/_dataset/opinion_lex/positive-words.txt", what = "character", comment.char = ";")
neg <- scan("/Users/valentinaporcu/R_test/_dataset/opinion_lex/negative-words.txt", what = "character", comment.char = ";")

score.sentiment <- function(sentences, pos, neg, .progress="none")
{
        require(plyr)
        require(stringr)
        
        scores = laply(sentences, function(sentence, pos, neg) {
                
                # normalizziamo il testo tramite la funzione gsub() e le espressioni regolari
                sentence <- gsub("[[:punct:]]", "", sentence)
                sentence <- gsub("[[:cntrl:]]", "", sentence)
                sentence <- gsub("\\d+", "", sentence)
                sentence <- iconv(sentence, "UTF-8", "ASCII")
                # convertiamo in minuscolo
                sentence <- tolower(sentence)
                
                # creiamo i vettori di parole
                word.list <- str_split(sentence, "\\s+")
                # delistiamo le parole
                words <- unlist(word.list)
                
                # confrontiamo le parole con le liste di parole positive e negative
                pos.matches <- match(words, pos)
                neg.matches <- match(words, neg)
                
                
                pos.matches <- !is.na(pos.matches)
                neg.matches <- !is.na(neg.matches)
                
                # creiamo un punteggio dove sommiamo le parole positive e le negative
                score <- sum(pos.matches) - sum(neg.matches)
                
                return(score)
        }, pos, neg, .progress=.progress )
        
        scores.df <- data.frame(score=scores, text=sentences)
        return(scores.df)
}

score.sentiment(taz[1:100], pos, neg)

polkey <- sentiment_frame(positive.words, negative.words)

View(counts(polarity(taz[1:100], polarity.frame=polkey)))

# qdap contiene liste di parole positive e negative in inglese
# https://github.com/valentinap lessici in francese e italiano

# convertiamo in corpus, in modo da poterlo trattare con tm 
text1 <- VCorpus(VectorSource(taz))

# metodi supervisionati

# ripuliamo il testo
text1 <- tm_map(text1, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
text1 <- tm_map(text1, content_transformer(tolower))
text1 <- tm_map(text1, removeNumbers)
text1 <- tm_map(text1, removePunctuation)
text1 <- tm_map(text1, function(x)removeWords(x,stopwords('en')))
text1 <- tm_map(text1, PlainTextDocument)
text1 <- tm_map(text1, stemDocument)

# creiamo la matrice termini documenti
tdm <- TermDocumentMatrix(text1, control = list(minWordLength = 1))

# calcoliamo la frequenza dei termini
term.freq <- rowSums(as.matrix(tdm))

# eliminiamo i termini con frequenza inferiore a 20
term.freq <- subset(term.freq, term.freq >= 20)

# visualizziamo i venti termini più frequenti
findFreqTerms(tdm, lowfreq = 30)

# convertiamo in matrice
mat1 <- as.matrix(tdm)
word_freqs <- sort(rowSums(mat1), decreasing = TRUE)

barplot(word_freqs[1:20], col = heat.colors(20))

# rimuoviamo i termini meno utilizzati
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)

# convertiamo la matrice Termini-Documenti in matrice semplice
mat2 <- as.matrix(tdm2)

# calcoliamo la distanza
distM <- dist(scale(mat2))
fit <- hclust(distM, method = "ward.D2")

# creiamo il dendrogramma
plot(fit)

# creiamo una seconda wordcloud ripulita dai termini meno usati
word_freqs2 <- sort(rowSums(mat2), decreasing = TRUE)
dm2 <- data.frame(word = names(word_freqs2), freq = word_freqs2)
wordcloud(dm2$word, dm2$freq, random.order = FALSE , colors = brewer.pal(8,"Dark2"))

wordcloud2(dm2, size = 0.7,shape = 'triangle',
           rotateRatio = 0.5, minSize = 1)

# metodi supervisionati per il calcolo della sentiment analysis
# possiamo creare dei modelli che prendono in considerazione dei testi
# già polarizzati e applicare dei modelli probabilistici 
# questo di consente maggiore flessibilità dal punto di vista delle lingue

pos <- VCorpus(DirSource(directory = "/Users/valentinaporcu/R_test/_dataset/review_polarity/txt_sentoken/pos", encoding = "UTF-8"))
neg <- VCorpus(DirSource(directory = "/Users/valentinaporcu/R_test/_dataset/review_polarity/txt_sentoken/neg", encoding = "UTF-8"))

rec <- c(pos, neg)

rec <- tm_map(rec, content_transformer(tolower))
rec <- tm_map(rec, removePunctuation)
rec <- tm_map(rec, removeWords, stopwords("english"))
rec <- tm_map(rec, removeNumbers)
rec <- tm_map(rec, stripWhitespace)
rec <- tm_map(rec, content_transformer(stemDocument))


tfidf <- DocumentTermMatrix(rec, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
sr <- as.matrix(removeSparseTerms(tfidf, sparse = 0.7))

sent <- c(rep("pos",1000), rep("neg", 1000))
freq <- colSums(as.matrix(TermDocumentMatrix(rec)))
df <- as.data.frame(cbind(sent, freq, sr))

set.seed(12345)
random <- runif(nrow(df))
df2 <- df[order(random),]


train <- df2[1:1500, -2]
test <- df2[1501:2000, -2]

####### naive bayes

model <- naiveBayes(train, as.factor(train[, 1]))

predictions <- predict(model, test)

table(predictions, test[,1])

confusionMatrix(data= predictions, reference=as.factor(test$sent), positive="pos")

###################################

# avevamo già effettuato la pulizia del testo
text1 <- VCorpus(VectorSource(taz))

# ripuliamo il testo
text1 <- tm_map(text1, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
text1 <- tm_map(text1, content_transformer(tolower))
text1 <- tm_map(text1, removeNumbers)
text1 <- tm_map(text1, removePunctuation)
text1 <- tm_map(text1, function(x)removeWords(x,stopwords('en')))
my_stop <- c("astrazeneca", "covid", "coronavirus", "\n")
text1 <- tm_map(text1, removeWords, my_stop)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
text1 <- tm_map(text1, removeURL, lazy=TRUE) 
text1 <- tm_map(text1, PlainTextDocument)
text1 <- tm_map(text1, stemDocument)

tfidf2 <- DocumentTermMatrix(text1)
mat2 <- as.matrix(tfidf2)

df3 <- as.data.frame(mat2)

predictions2 <- predict(model, df3)

table(predictions2)

# topic models

# install.packages(“devtools”)
require("devtools")

install_github("mananshah99/sentR")
library("sentR")

# il pacchetto sentR ci permette di inserire dei vettori di parole positive e negative in maniera autonoma
# ad esempio i dizionari di qdap, o altri tra quelli che abbiamo visto e creato qua sopra

posita <- scan("~/ITA_polarized/posITA.txt", what = "character", comment.char = ";")

negita <- scan("~/ITA_polarized/negITA.txt", what = "character", comment.char = ";")

classify.aggregate(sentences, positive.words, negative.words)

frase <- c("questo libro è molto bello, mi piace molto la storia e i protagonisti sono adorabili")

classify.aggregate(frase, posita, negita)

# pacchetto per le emoji
# hadleyemo

