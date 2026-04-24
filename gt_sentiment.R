###call in packages***

library(googlesheets4)
library(devtools)
library(stringr)
library(reshape2)
library(dplyr)
library(readr)
library(leaflet)
library(htmlwidgets)
library(webshot)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(wordcloud)

##pull in data
sheet_url <- "https://docs.google.com/spreadsheets/d/1DqinWqMIFWkyUTTd_f1RNwfmRtLQBm0BHyHHgubFFj8/edit?usp=sharing"

df <- read_sheet(sheet_url)

#setwd("C:/Users/LisaH/OneDrive - GOM/Desktop/WFH/sentiment/gray triggerfish 25")
#df<-read.csv("gt_comments.csv",header=T)



###create new database that is only the comments NOTE: that column and row changes each time
#rg<-df[1:200,1]
rg <- as.character(df[[7]])
#rg$id <- 1:nrow(rg)
#rg <- gsub("\\$" ,"", rg)
#rg <- gsub("[?]", "'", rg)
rg <- gsub("shark", "sharks", rg)
out <- c()

###modify the Bing library for more fisheries context
dropword <-c("bloom","redtide","discard", "wreck","bait","shipwreck","limit","shark","lure","shallow","significant","catch","caught","catching","worked","hang","pretty","aggressive","hogs")
sentiments <- get_sentiments("bing") %>% filter(!word %in% dropword)
sentiment.neg<-tibble(word=c("bloom","redtide","discard","harder","smaller","small","decrease","decreased","fewer","overfished","lower","less","diminished","shark","sharks","dolphin","dolphins"),sentiment=c("negative"))
sentiment.pos<-tibble(word=c("bite","large","larger","biting","increase","increased","plenty","uptick","aggressive"),sentiment=c("positive"))
additional_sentiment <- sentiment.neg%>%
  rbind(sentiment.pos)
new_sentiment <- sentiments%>%
  rbind(additional_sentiment)


###run a loop that looks that scores each comment by sentiment
for(i in 1:length(rg)){
  #for(i in 1:2){
  tokens.i <- data_frame(text = rg[i]) %>% unnest_tokens(word, text)
  x <- inner_join(tokens.i, new_sentiment)
  POS <- nrow(subset(x, sentiment=="positive"))
  NEG <- nrow(subset(x, sentiment=="negative"))
  SENT <- POS - NEG
  df <- data.frame(positive=POS, negative=NEG, sentiment=SENT)
  df$standardized <- df$sentiment/nrow(x) ###standardize for number of words
  df$words <- nrow(x)
  out <- rbind(out, df)
}


###organize sentiment category into groups (-1 to -.33 neg, -.33 to .33 neutral, .33 to 1 pos)
out2 <- na.omit(out)
out2$standardized <- as.numeric(as.character(out2$standardized))
out2$lab <- cut(out2$standardized, breaks = c(-1,-.33,.33,1), include.lowest=TRUE)
out2$type <- with(out2, ifelse(standardized < -.33, '-1',
                               ifelse(standardized > .33, '1', '0')))
colors <- c("#fc8d59", "#ffffbf", "#99d594")
out2 %>% 
  group_by(lab) %>%
  summarise(no_rows = length(lab))

out$type <- with(out, ifelse(standardized < -.33, '-1',
                             ifelse(standardized > .33, '1', '0')))

###write out the database for John to use for mapping
write.csv(out,"C:/Users/LisaH.GMFMC/OneDrive - Gulf of Mexico Fishery Mgmt Council/Desktop/WFH/sentiment/spanish\\spanish_john.csv")       



### run sentiment analysis to examine each word
rg2<-tibble(line=1:201, text=rg)
rg3<-rg2%>%unnest_tokens(word,text)

bing_word_counts <- rg3 %>%
  inner_join(new_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


###This creates the word sentiment bar chart
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  scale_fill_manual(values=c("#fc8d59","#99d594"))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +theme(axis.title=element_text(size=14),axis.text=element_text(size=12))+theme(strip.text=element_text(size=14))+
  scale_y_continuous()+
  coord_flip()

###This creates the sentiment word cloud
par(mar = c(0, 0, 0, 0))
rg3 %>%
  inner_join(new_sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#fc8d59","#99d594"), 
                   max.words = 30, title.size=1.5,match.color=TRUE)
