#by Christopher Penndorf
library(tidyverse)
library(XML)

#because we used a lot of data the prep and calculations take a lot of time
#so we saved our data frame and you can load it right away to save time
load("sentimentdata06.RData")
load("sentimentdata16.RData")

#find out which protocols are relevant for our analysis
files <- list.files(path="~/ALLBUS_komplett/periode16/", pattern="*.xml", full.names=TRUE, recursive=FALSE)
relevantfiles <- vector()
  for(i in 1:length(files)){
    data <- xmlToDataFrame(files[i])
    if(as.integer(substring(data[4,1],7,)) == 2006){
      if(length(grep("asyl",unlist(strsplit(tolower(data[6,1])," "),use.names = FALSE))) > 4 | 
         length(grep("einwanderung",unlist(strsplit(tolower(data[6,1])," "),use.names = FALSE))) > 1 |
         length(grep("flucht",unlist(strsplit(tolower(data[6,1])," "),use.names = FALSE))) > 2){
        relevantfiles <- c(files[i], relevantfiles)
      }
    }
  }

files <- list.files(path="/home/christopher/Downloads/Telegram Desktop/ALLBUS_komplett/periode18/", pattern="*.xml", full.names=TRUE, recursive=FALSE)
relevantfiles <- vector()
for(i in 1:length(files)){
  data <- xmlToDataFrame(files[i])
  if(as.integer(substring(data[4,1],7,)) == 2016){
    if(length(grep("asyl",unlist(strsplit(tolower(data[6,1])," "),use.names = FALSE))) > 8 | 
       length(grep("einwanderung",unlist(strsplit(tolower(data[6,1])," "),use.names = FALSE))) > 4 |
       length(grep("flucht",unlist(strsplit(tolower(data[6,1])," "),use.names = FALSE))) > 4){
      relevantfiles <- c(files[i], relevantfiles)
    }
  }
}

#creat our sentiment vectors according to the seminar 
sentiments <- rbind(read_tsv("SentiWS_v2.0_Negative.txt", col_names = F), read_tsv("SentiWS_v2.0_Positive.txt", col_names = F)) %>%
  rename(lemma = X1, score = X2)


sentiments_alternatives <- sentiments %>%
  select(3,2) %>%
  separate_rows(X3, sep=",") %>%
  rename(lemma = X3)

sentiments_cleaned <- sentiments %>%
  mutate(lemma = map_chr(strsplit(sentiments$lemma, "|", fixed = T), 1)) %>%
  select(1,2)

sentiments_comb <- rbind(sentiments_cleaned, sentiments_alternatives) %>%
  mutate(lemma = tolower(lemma)) %>%
  group_by(lemma) %>% 
  summarise(score = mean(score)) %>% 
  ungroup

sentimentVector <- sentiments_comb$score
names(sentimentVector) <- sentiments_comb$lemma

#make a new dataframe for our text and date only 
df <- as.data.frame(cbind(1:22))
colnames(df) <- c("Datum")
df$text <- 0

#add only the text and dates of every protocol
for(i in 1:length(relevantfiles)){
  data <- xmlToList(relevantfiles[i])
  df[i,1] <- data$DATUM
  df[i,2] <- data$TEXT
}
#fucntions according to the seminar. We had to add remove \n and \t so we added that too
preprocess_corpus <- function(x) {
  research_corpus <- tolower(x)  # force to lowercase
  research_corpus <- gsub("\n", " ", research_corpus)
  research_corpus <- gsub("\t", "", research_corpus)
  research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
  research_corpus <- gsub("-", "", research_corpus)  # remove hyphens
  research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
  research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
  research_corpus <- trimws(research_corpus)
  research_corpus <-str_replace_all(research_corpus, "[\r\n]" , "")
  research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers
  research_corpus <- gsub("^ *|(?<= ) | *$", "", research_corpus, perl = TRUE) # Remove multiple whitespace
  return(research_corpus)
}

#function not changed from the seminar
check_sentinment <- function(x) {
  words <- unlist(strsplit(x, " ", fixed = T))
  score <- mean(sentimentVector[words], na.rm = T)
  ifelse(is.nan(score), NA, score)
}
#now we prep our text and add the sentimentscore
df$preptext <- 0
df$sentimentscore <- 0
for(i in 1:length(relevantfiles)){
  df[i,3] <- preprocess_corpus(df[i,2])
  df[i,4] <- check_sentinment(df[i,3])
}

#to order the dates correctly we hade to format them 
df$Date <- as.Date("2000-01-01")
for(i in 1:nrow(df)){
  df[i,5] <- as.Date(df[i,1], format = "%d.%m.%Y")
}
colnames(df) <- c("Datum", "text", "preptext", "SentimentScore", "Date")
#plot the df
#load the 2016 data and format the date to get the plot out of that year
df %>%
  ggplot() +
  theme_bw() +
  geom_col(aes(Date, SentimentScore, fill = SentimentScore)) +
  scale_fill_viridis_c() +
  coord_cartesian(ylim = c(0.075,-0.025))


