###########################################################################
######                      Fisherman Feedback Analysis              ######
######                             Gag Grouper 2026                  ######
###########################################################################
# This is only for the first section of analysis, previously done in excel
# Date created: 04/17/2026
# Created by: Leo Eguia
# Requires initial cleanup in excel - see Fisherman Feedback Clean for Analysis Template

###########################################################################
# Initialize --------------------------------------------------------------
###########################################################################
# Load packages and data --------------------------------------------------


rm(list=ls()) #clear environment
setwd("C:/Users/Leo/Documents/R working directory/Fisherman Feedback") #adjust accordingly
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
#Load data
#ffdat <- read.csv("Gag_Fisherman Feedback_2026_LE analysis.csv") #Change this for different species
ffdat_OG <- read_xls("Gag_Fisherman Feedback_2026_CLEAN for analysis.xls") #Change this for different species


# Initial data clean up ---------------------------------------------------

#Sectors to split out by
main_sectors <- c("Private Recreational", "For-Hire", "Commercial")

#Dummy Sector column to preserve original
#"Fleet" column will be dropped in the next step
ffdat_OG$Fleet <- ffdat_OG$`Association with the Fishery`

#This will create 4 new columns, with binary values for presence/absence of each fleet. 3 of which will be the main_sectors, and it will bin every other response into "Other". 
#Un-comment the one below if you want all responses in binary form. 
ffdat <- ffdat_OG %>%
  separate_rows(`Fleet`, sep = ";") %>%
  mutate(`Fleet` = str_trim(`Fleet`)) %>%
  filter(`Fleet` != "") %>% 
  mutate(`Fleet` = if_else(`Fleet` %in% main_sectors,`Fleet`, "Other")) %>%
  mutate(value = 1)%>%
  pivot_wider(names_from = `Fleet`,values_from = value, values_fill = 0, values_fn = max)

### This does not bin other responses into "Other" category
# ffdat <- ffdat_OG %>%
#   separate_rows(`Fleet`, sep = ";") %>%
#   mutate(`Fleet` = str_trim(`Fleet`)) %>%
#   filter(`Fleet` != "") %>% 
#   mutate(value = 1) %>%
#   pivot_wider(names_from = `Fleet`,values_from = value, values_fill = 0)


###########################################################################
# Overall Feedback --------------------------------------------------------
###########################################################################
# Total # of unique responses ---------------------------------------------

# Number of rows = number of unique responses
# Duplicates removed prior to analysis

numresp <- nrow(ffdat)

# Comments by overall sentiment (pie) -------------------------------------


#Frequency table of overall sentiment
overall_sent <- as.data.frame(table(ffdat$`Final Overall Sentiment`))
#Add Sentiment labels
overall_sent$Sentiment <- c("Negative","Neutral/Mixed","Positive")
#Add proportions
overall_sent$prop <- overall_sent$Freq/sum(overall_sent$Freq)

#Adds label locations and identifiers
overall_sent <- overall_sent %>% 
  arrange(desc(Freq)) %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1))

# Pie Chart
overall_sent_plot <- ggplot(overall_sent, aes(x = 1, y = prop, fill = Sentiment)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),position = position_stack(vjust = 0.5), color = "black", size = 6) +
  theme_void() +
  scale_fill_manual(values = c("#ED7D31","#FFFF00","#92D050")) +
  labs(title = "Overall Sentiment") +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.background  = element_rect(fill = "white", color = NA), #Comment out for no background
        panel.background = element_rect(fill = "white", color = NA)) #Comment out for no background

print(overall_sent_plot)
ggsave("Broad Sentiment Plots/Overall Sentiment.png", 
       plot = overall_sent_plot,
       width = 8, #Adjust width as needed
       height = 8, #Adjust height as needed
       units = "in", 
       dpi = 700)

# Total # of responses related to stock condition -------------------------


stockcond <- ffdat[ffdat$`Final Related to Stock Condition`=='y',]
(tot_stockcond <- nrow(stockcond))

#Total Positive stock condition
(pos_cond <- nrow(stockcond[stockcond$`Final  Stock Condition`==1,]))
#Total Negative stock condition
(neg_cond <- nrow(stockcond[stockcond$`Final  Stock Condition`==-1,]))
#Total neutral stock condition
(neut_cond <- nrow(stockcond[stockcond$`Final  Stock Condition`==0,]))
#Compare all responses
table(stockcond$`Final  Stock Condition`)


# Comments by stock condition (pie) ---------------------------------------


# Prep data for Pie Chart
stock_sent <- data.frame(Sentiment = c('Positive','Neutral/Mixed','Negative'),Freq = c(pos_cond,neut_cond,neg_cond))
#Add in proportions
stock_sent$prop <- stock_sent$Freq/sum(stock_sent$Freq)
#Adds label locations and identifiers
stock_sent <- stock_sent %>% 
  arrange(desc(Freq)) %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1), small = prop < 0.08, mid = prop >0.08 & prop < 0.14, large = prop>0.14)

# Pie Chart
stock_sent_plot <- ggplot(stock_sent, aes(x = 1, y = prop, fill = Sentiment)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),position = position_stack(vjust = 0.5), color = "black", size = 6) +
  theme_void() +
  scale_fill_manual(values = c("#ED7D31","#FFFF00","#92D050")) +
  labs(title = "Stock Condition Sentiment") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.background  = element_rect(fill = "white", color = NA), #Comment out for no background
        panel.background = element_rect(fill = "white", color = NA)) #Comment out for no background)

print(stock_sent_plot)
ggsave("Broad Sentiment Plots/Stock Condition Sentiment.png", 
       plot = stock_sent_plot,
       width = 8, #Adjust width as needed
       height = 8, #Adjust height as needed
       units = "in", 
       dpi = 700)


###########################################################################
# By Sector ---------------------------------------------------------------
###########################################################################
# # of unique responses by sector (pie) -----------------------------------
#Separate out each sector for ease
PR <- ffdat[ffdat$`Private Recreational`==1,]
FH <- ffdat[ffdat$`For-Hire`==1,]
CM <- ffdat[ffdat$`Commercial`==1,]
#Number of rows for filtered datasets to unique sectors
#Some responses come from multiple sectors, so PR+FH+CM likely greater than original dataset
(PR_resp <- nrow(PR))
(FH_resp <- nrow(FH))
(CM_resp <- nrow(CM))


### Prep data for Pie Chart 

num_sector <- data.frame(Fleet = c("Private Recreational","For-Hire","Commercial"), Freq = c(PR_resp,FH_resp,CM_resp))
#num_sector$Prop <- num_sector$Freq/numresp 
num_sector$prop <- num_sector$Freq/sum(num_sector$Freq)

num_sector <- num_sector %>% 
  arrange(desc(Freq)) %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1))


### Pie Chart 
sectorpieplot <- ggplot(num_sector, aes(x = 1, y = prop, fill = Fleet)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),position = position_stack(vjust = 0.5), color = "black", size = 6) +
  theme_void() +
  scale_fill_manual(values = c("#4472C4","#A5A5A5","#CDCDFF")) +
  labs(title = "Responses by Sector") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.background  = element_rect(fill = "white", color = NA), #Comment out for no background
        panel.background = element_rect(fill = "white", color = NA)) #Comment out for no background)

print(sectorpieplot)
ggsave("Broad Sentiment Plots/Responses by Sector Pie.png", 
       plot = sectorpieplot,
       width = 8, #Adjust width as needed
       height = 8, #Adjust height as needed
       units = "in", 
       dpi = 700)


# Unique responses by sector (bar) ----------------------------------------

sectorbarplot <- ggplot(num_sector, aes(x = Fleet, y = Freq, fill = Fleet)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Responses by Sector", x = "", y = "", fill = "") +
  scale_fill_manual(values = c("#4472C4","#A5A5A5","#CDCDFF")) +
  theme_minimal()+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),panel.grid.minor.y = element_line())

print(sectorbarplot)
ggsave("Broad Sentiment Plots/Responses by Sector Bar.png", 
       plot = sectorbarplot,
       width = 12, #Adjust width as needed
       height = 8, #Adjust height as needed
       units = "in", 
       dpi = 700)

# Overall sentiment by sector (bar) ---------------------------------------
### Prepare data for analysis
#Private Rec
PRsent <- as.data.frame(table(PR$`Final Overall Sentiment`))
PRsent$Fleet <- "Private Recreational"
PRsent$prop <- PRsent$Freq/sum(PRsent$Freq)

#For-hire
FHsent <- as.data.frame(table(FH$`Final Overall Sentiment`))
FHsent$Fleet <- "For-Hire"
FHsent$prop <- FHsent$Freq/sum(FHsent$Freq)

#Commercial
CMsent <- as.data.frame(table(CM$`Final Overall Sentiment`))
CMsent$Fleet <- "Commercial"
CMsent$prop <- CMsent$Freq/sum(CMsent$Freq)

#Combine and prep for plot
sectorsent <- rbind(PRsent,FHsent,CMsent)
sectorsent <- sectorsent %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1),
         Sentiment = recode(Var1, "-1" = "Negative", "0" = "Neutral/Mixed", "1" = "Positive"))
#Sets the order of sector
sectorsent$Fleet <- factor(sectorsent$Fleet, levels = unique(sectorsent$Fleet))
#Sets the order of sentiment
sectorsent$Sentiment <- factor(sectorsent$Sentiment, levels = c("Positive", "Neutral/Mixed","Negative"))

###Plot
sect_sent_plot <- ggplot(sectorsent, aes(x = Fleet, y = Freq, fill = Sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Overall Sentiment by Sector", x = "", y = "", fill = "") +
  scale_fill_manual(values = c("#92D050","#FFFF00","#ED7D31")) +
  theme_minimal()+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),panel.grid.minor.y = element_line())
print(sect_sent_plot)
ggsave("Broad Sentiment Plots/Overall Sentiment by Sector.png", 
       plot = sect_sent_plot, 
       width = 12, #Adjust width as needed
       height = 8, #Adjust height as needed
       units = "in", 
       dpi = 700)
# Stock condition by sector (bar) -----------------------------------------

### Prepare data for analysis
#Private Rec
PRstock_sent <- as.data.frame(table(PR$`Final  Stock Condition`))
PRstock_sent$Fleet <- "Private Recreational"
PRstock_sent$prop <- PRstock_sent$Freq/sum(PRstock_sent$Freq)

#For-hire
FHstock_sent <- as.data.frame(table(FH$`Final  Stock Condition`))
FHstock_sent$Fleet <- "For-Hire"
FHstock_sent$prop <- FHstock_sent$Freq/sum(FHstock_sent$Freq)

#Commercial
CMstock_sent <- as.data.frame(table(CM$`Final  Stock Condition`))
CMstock_sent$Fleet <- "Commercial"
CMstock_sent$prop <- CMstock_sent$Freq/sum(CMstock_sent$Freq)

#Combine and prep for plot
sectorstock_sent <- rbind(PRstock_sent,FHstock_sent,CMstock_sent)
sectorstock_sent <- sectorstock_sent %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1),
         Sentiment = recode(Var1, "-1" = "Negative", "0" = "Neutral/Mixed", "1" = "Positive"))
#Sets the order of sector
sectorstock_sent$Fleet <- factor(sectorstock_sent$Fleet, levels = unique(sectorstock_sent$Fleet))
#Sets the order of sentiment
sectorstock_sent$Sentiment <- factor(sectorstock_sent$Sentiment, levels = c("Positive", "Neutral/Mixed","Negative"))

###Plot
sect_stock_sent_plot <- ggplot(sectorstock_sent, aes(x = Fleet, y = Freq, fill = Sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Stock Condition Sentiment by Sector", x = "", y = "", fill = "Subgroup") +
  scale_fill_manual(values = c("#92D050","#FFFF00","#ED7D31")) +
  theme_minimal()+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),panel.grid.minor.y = element_line())
print(sect_stock_sent_plot)
#Save the plot to the filepath
ggsave("Broad Sentiment Plots/Stock Condition Sentiment by Sector.png", 
       plot = sect_stock_sent_plot, 
       width = 12, #Adjust width as needed
       height = 8, #Adjust height as needed
       units = "in", 
       dpi = 700)
