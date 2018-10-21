# Kratom Analysis
# Amy Jin
# 10/20/18

#--------------------------------- 2016 Part D analysis  ---------------------------------
data<- read.csv("your\\path\\to\\the\\part_d_data",
                header = T)
View(data)
library(fiftystater)
library(viridis)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(ROAuth)
library(streamR)
library(twitteR)
library(grid)

head(data)
View(data)
summary(data)

#  Opioid_Prescribing Count dist
data1 <- data[complete.cases(data[,8]),]

groupdata <- aggregate(data1$Opioid.Claim.Count, by = list(data1$NPPES.Provider.State),sum)
groupdata$claim_percent = groupdata$x/sum(groupdata$x)
colnames(groupdata) <- c('State', 'Claim_count', 'Claim_percent')
groupdata <- groupdata[order(-groupdata$Claim_count),]
groupdata %>%
  ggplot()+
  geom_bar(mapping=aes(x=reorder(State, Claim_count), y=Claim_count), stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0,6956583)) +
  ylab("State") +
  xlab('Claim Count') +
  ggtitle('Medicare Part D Prescriber Claim Count in 2016') +
  theme(axis.text = element_text(size=6))

#  Opioid_Prescribing_Rate dist
data2 <- data[complete.cases(data[,8]),]
data2$Opioid_Prescribing_Rate = data2$Opioid.Claim.Count/data2$Total.Claim.Count
groupdata <- aggregate(data2$Opioid_Prescribing_Rate, by = list(data2$NPPES.Provider.State),FUN = mean,na.rm=T)
colnames(groupdata) <- c('State', 'Opioid_Prescribing_Rate')
groupdata <- groupdata[order(-groupdata$Opioid_Prescribing_Rate),]
groupdata %>%
  ggplot()+
  geom_bar(mapping=aes(x=reorder(State, Opioid_Prescribing_Rate), y=Opioid_Prescribing_Rate), stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.17865489)) +
  ylab("State") +
  xlab('Opioid Prescribing Rate') +
  ggtitle('Medicare Part D Opioid Prescribing Rate in 2016') +
  theme(axis.text = element_text(size=6))

# NPI dist
data3 <- data[is.na(data$Opioid.Claim.Count)==0 & data$Opioid.Claim.Count !='' & data$Opioid.Claim.Count >0, ]
nrow(data3) # 500,435 total NPIs prescribed opioid 
nrow(data) # 1,131,550 total NPIs 
data3$count1 <- 1
grouopdata <- aggregate(data3$count1, by = list(data3$NPPES.Provider.State),FUN = sum,na.rm=T)
colnames(groupdata) <- c('State', 'Opioid_Prescribing_NPI_Count')
groupdata <- groupdata[order(-groupdata$Opioid_Prescribing_NPI_Count),]
groupdata %>%
  ggplot()+
  geom_bar(mapping=aes(x=reorder(State, Opioid_Prescribing_NPI_Count), y=Opioid_Prescribing_NPI_Count), stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0,50424)) +
  ylab("State") +
  xlab('Opioid Prescribing NPI Count') +
  ggtitle('NPI Count Who  Prescribed Opioid in 2016') +
  theme(axis.text = element_text(size=6))

cor(data3$Total.Claim.Count, data3$Opioid.Claim.Count) # 0.4846799

# NPI Specialty Count Analysis
data3 <- data[is.na(data$Opioid.Claim.Count)==0 & data$Opioid.Claim.Count !='' & data$Opioid.Claim.Count >0, ]
nrow(data3) # 500,435 total NPIs prescribed opioid 
data3$count1 <- 1
groupdata <- aggregate(data3$count1, by = list(data3$Specialty.Description),FUN = sum,na.rm=T)
colnames(groupdata) <- c('Specialty_Description', 'Opioid_Prescribing_NPI_Count')
groupdata <- groupdata[order(-groupdata$Opioid_Prescribing_NPI_Count),]
groupdata <- groupdata[1:30,]
groupdata %>%
  ggplot()+
  geom_bar(mapping=aes(x=reorder(Specialty_Description, Opioid_Prescribing_NPI_Count), y=Opioid_Prescribing_NPI_Count), stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0,83101)) +
  ylab("NPI Count") +
  xlab('Specialty Description') +
  ggtitle('NPI Count of Each Specialty Who Prescribed Opioid in 2016') +
  theme(axis.text = element_text(size=6))

# Specialty Description Analysis
data3 <- data[is.na(data$Opioid.Claim.Count)==0 & data$Opioid.Claim.Count !='' & data$Opioid.Claim.Count >0, ]
nrow(data3) # 500,435 total NPIs prescribed opioid 
groupdata <- aggregate(data3$Opioid.Claim.Count, by = list(data3$Specialty.Description),FUN = sum,na.rm=T)
colnames(groupdata) <- c('NPI_Specialty_Description', 'Opioid_Claim_Count')
groupdata <- groupdata[order(-groupdata$Opioid_Claim_Count),]
groupdata <- groupdata[1:30,]
groupdata %>%
  ggplot()+
  geom_bar(mapping=aes(x=reorder(NPI_Specialty_Description, Opioid_Claim_Count), y=Opioid_Claim_Count), stat = 'identity') +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0,19844664)) +
  ylab("Opioid Claim Count") +
  xlab('NPI Specialty Description') +
  ggtitle('Opioid Claim Distribution over NPI Specialty in 2016') +
  theme(axis.text = element_text(size=6))

data5 <- data[data$NPPES.Provider.State == 'AS',]
View(data5)


#--------------------------------- Tweets analysis  ---------------------------------
# Oct 12 - Oct 20
credential <- OAuthFactory$new(consumerKey='',
                               consumerSecret='',
                               requestURL='',
                               accessURL='',
                               authURL='h')

#authentication process
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
credential$handshake(cainfo="cacert.pem")

# Collecting tweets in real time with streamR for karatom
filterStream("kratom_tweets_1018.json", track = c("kratom",'Thang','Kakuam','Thom','Ketum','Biak','Mitragyna speciosa korth'),locations = c(-125, 25, -66, 50), timeout=10000, oauth = credential) # ,"opioids"
kratom_tweets_1018.df <- parseTweets("kratom_tweets_1018.json", verbose = FALSE)
c( length(grep("kratom", kratom_tweets_1018.df$text, ignore.case = TRUE)),
   length(grep("opioids", kratom_tweets_1018.df$text, ignore.case = TRUE)) )

write.csv(kratom_tweets_1018.df , file="your\\path\\to\\the\\data")

# Collecting tweets in real time with streamR for karatom and opioids
filterStream("kratom_opo_tweets_1018.json", 
             track = c("kratom",'Thang','Kakuam','Thom','Ketum','Biak','Mitragyna speciosa korth','OxyContin',
                       'Vicodin','heroin','opioids','fentanyl','morphine',
                       'Buprenorphine','Fentanyl Patch','Hydromorphone','Levorphanos','Oxymorphone',
                       'Oxymorphone','Tapentadol','Methadone','Tramadol','Butorphanol nasal spray',
                       'Codeine','Fentanyl buccal','Fentanyl injection','Fentanyl nasal spray',
                       'Fentanyl transmucosal system','Hydromorphone','Meperidine','Morphine immediate release',
                       'Oxymorphone','Pentazocine + Acetaminophen','Tapentadol','Tramadol'),
             locations = c(-125, 25, -66, 50), timeout=10000, oauth = credential) #running now ,"opioids"
kratom_opo_tweets_1018.df <- parseTweets("kratom_opo_tweets_1018.json", verbose = FALSE)
#c( length(grep("kratom", kratom_opo_tweets_1018.df$text, ignore.case = TRUE)),
#   length(grep("opioids", kratom_opo_tweets_1018.df$text, ignore.case = TRUE)) )

write.csv(kratom_opo_tweets_1018.df , file="you\\path\\to\\the\\data")

map.data <- map_data("state")
points <- data.frame(x = as.numeric(kratom_opo_tweets_1018.df$lon), y = as.numeric(kratom_opo_tweets_1018.df$lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue")
View(kratom_opo_tweets_1018.df)


#--------------------------------- Tweets Sentiment analysis  ---------------------------------
df_tweets <- twListToDF(searchTwitter('setapp OR #setapp', n = 1000, lang = 'en')) %>%
  dmap_at('text', conv_fun)

# preprocessing and tokenization
it_tweets <- itoken(df_tweets$text,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    ids = df_tweets$id,
                    progressbar = TRUE)

# creating vocabulary and document-term matrix
dtm_tweets <- create_dtm(it_tweets, vectorizer)

# transforming data with tf-idf
dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)

# loading classification model
glmnet_classifier <- readRDS('glmnet_classifier.RDS')

# predict probabilities of positiveness
preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]

# adding rates to initial dataset
df_tweets$sentiment <- preds_tweets

cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")

set.seed(932)
samp_ind <- sample(c(1:nrow(df_tweets)), nrow(df_tweets) * 0.1) # 10% for labeling

# plotting
ggplot(df_tweets, aes(x = created, y = sentiment, color = sentiment)) +
  theme_minimal() +
  scale_color_gradientn(colors = cols, limits = c(0, 1),
                        breaks = seq(0, 1, by = 1/4),
                        labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
                        guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_point(aes(color = sentiment), alpha = 0.8) +
  geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_smooth(size = 1.2, alpha = 0.2) +
  geom_label_repel(data = df_tweets[samp_ind, ],
                   aes(label = round(sentiment, 2)),
                   fontface = 'bold',
                   size = 2.5,
                   max.iter = 100) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
  ggtitle("Tweets Sentiment Rate (Probability of Positiveness)")

