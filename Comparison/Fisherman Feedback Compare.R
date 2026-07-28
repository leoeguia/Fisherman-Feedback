###########################################################################
######                      Fisherman Feedback Analysis              ######
######                         Gag Grouper Comparison                ######
######                             Broad Sentiment                   ######
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
library(scales)
#Load data
#ffdat <- read.csv("Gag_Fisherman Feedback_2026_LE analysis.csv") #Change this for different species
ffdat_2021_OG <- read_excel("Something's Fishy_December 2020_gag_Eguia sentiment analysis.xlsx", sheet = "Sheet 1 - Gag_Something's Fishy")
ffdat_OG <- read_xls("Gag_Fisherman Feedback_2026_CLEAN for analysis.xls") #Change this for different species

# 2021 Initial Data Cleanup -----------------------------------------------
ffdat_2021 <- ffdat_2021_OG %>%
  select(-`Comment Number`,-`EM Sentiment`,-`CRS Sentiment`) %>%
  dplyr::rename(`Please describe your gag observations` = `Please describe your observations`,
         `Final Overall Sentiment` = `Final Sentiment`,
         `Final Related to Stock Condition` = `Related to Abundance (y/n)`) %>%
  rowwise() %>%
  mutate(
    Fleet = paste(
      c(if (str_detect(`Association with the Fishery`,
                       regex("commercial", ignore_case = TRUE)))
        "Commercial",
        if (str_detect(`Association with the Fishery`,
                       regex("for hire|for-hire|charter|charters|guide|party boat",ignore_case = TRUE)))
          "For-Hire",

        if (str_detect(`Association with the Fishery`,regex("private|private recreational|recreational",ignore_case = TRUE)))
          "Private Recreational"),
      collapse = ";"),
    Fleet = if_else(Fleet == "", `Association with the Fishery`, Fleet)
  ) %>%
  ungroup() %>% 
  mutate(Year = 2021) #%>% 
 # relocate(`Final  Stock Condition`, .after = `Final Related to Stock Condition`)

# 2026 Initial data clean up ---------------------------------------------------

#Dummy Sector column to preserve original
#"Fleet" column will be dropped in the next step
ffdat_OG$Fleet <- ffdat_OG$`Association with the Fishery`
ffdat_OG$Year <- 2026

# Bind years and tally sectors --------------------------------------------

#Sectors to split out by
main_sectors <- c("Private Recreational", "For-Hire", "Commercial")

ffdat <- rbind(ffdat_2021, ffdat_OG)
#This will create 4 new columns, with binary values for presence/absence of each fleet. 3 of which will be the main_sectors, and it will bin every other response into "Other". 
#Un-comment the one below if you want all responses in binary form. 
ffdat <- ffdat %>%
  separate_rows(`Fleet`, sep = ";") %>%
  mutate(`Region` = str_trim(`Fleet`)) %>%
  filter(`Fleet` != "") %>% 
  mutate(`Region` = if_else(`Fleet` %in% main_sectors,`Fleet`, "Other")) %>%
  mutate(value = 1)%>%
  pivot_wider(names_from = `Fleet`,values_from = value, values_fill = 0, values_fn = max)

### This does not bin other responses into "Other" category
# ffdat <- ffdat_OG %>%
#   separate_rows(`Fleet`, sep = ";") %>%
#   mutate(`Fleet` = str_trim(`Fleet`)) %>%
#   filter(`Fleet` != "") %>% 
#   mutate(value = 1) %>%
#   pivot_wider(names_from = `Fleet`,values_from = value, values_fill = 0)


# Check "Other" Category --------------------------------------------------
#Manually fix some of the fleets
others <- ffdat[ffdat$Other==1,]
ffdat$`For-Hire` <- ifelse(ffdat$`FIRST Name`%in% c("Andrea","Capt Forest"),1,ffdat$`For-Hire`)
#write.csv(ffdat, file = "Combined Gag FF 2021 and 2026.csv")
###########################################################################
# Combined Overall Feedback -----------------------------------------------
###########################################################################
# Combined Comments by overall sentiment (pie) -------------------------------------


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
#Sets the order of sentiment
overall_sent$Sentiment <- factor(overall_sent$Sentiment, levels = c("Positive", "Neutral/Mixed","Negative"))
# Pie Chart
overall_sent_plot <- ggplot(overall_sent, aes(x = 1, y = prop, fill = Sentiment)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Freq),position = position_stack(vjust = 0.5), color = "black", size = 6) +
  theme_void() +
  scale_fill_manual(values = c("#92D050","#FFFF00","#ED7D31")) +
  labs(title = "Overall Comment Sentiment") +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.text = element_text(size = 16, color = '#595959'),
        plot.title = element_text(hjust = 0.5, size = 14, color = '#595959'))

print(overall_sent_plot)

# ggsave("Comparison/Broad Sentiment Plots/Combined Overall Sentiment.png",
#        plot = overall_sent_plot,
#        width = 5.41, #Adjust width as needed
#        height = 3.25, #Adjust height as needed
#        units = "in",
#        dpi = 700)

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

pos_stockcond <- stockcond[stockcond$`Final  Stock Condition`==1,]
neut_stockcond <- stockcond[stockcond$`Final  Stock Condition`==0,]
neg_stockcond <- stockcond[stockcond$`Final  Stock Condition`==-1,]
# Comments by stock condition (pie) ---------------------------------------


# Prep data for Pie Chart
stock_sent <- data.frame(Sentiment = c('Negative','Neutral/Mixed','Positive'),Freq = c(neg_cond,neut_cond,pos_cond))
#Add in proportions
stock_sent$prop <- stock_sent$Freq/sum(stock_sent$Freq)
#Adds label locations and identifiers
stock_sent <- stock_sent %>% 
  #arrange(-Sentiment) %>% 
  mutate(label_pos = 1-(cumsum(prop)-prop/2),label = scales::percent(prop,accuracy = 1))
#Sets the order of sentiment
#stock_sent$Sentiment <- factor(stock_sent$Sentiment, levels = c("Positive", "Neutral/Mixed","Negative"))
# Pie Chart
stock_sent_plot <- ggplot(stock_sent, aes(x = 1, y = prop, fill = Sentiment)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Freq, y=label_pos, x=1.3), color = "black", fontface = 'bold', size = 4) +
  theme_void() +
  #scale_fill_manual(values = c("#ED7D31","#FFFF00","#92D050")) +
  scale_fill_manual(values = c("Negative" = "#ED7D31","Neutral/Mixed"  = "#FFFF00","Positive" = "#92D050"),breaks = c("Positive", "Neutral/Mixed", "Negative"))+
  labs(title = "Stock Condition Sentiment") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, color = '#595959'),
        plot.title = element_text(hjust = 0.5, size = 14, color = '#595959'),
        plot.background  = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA)) 

print(stock_sent_plot)
# ggsave("Comparison/Broad Sentiment Plots/Combined Stock Condition Sentiment.png",
#        plot = stock_sent_plot,
#        height = 3.42, #Adjust width as needed
#        width = 5.03, #Adjust height as needed
#        units = "in",
#        dpi = 700)

# of unique responses by sector (pie) -----------------------------------
#Separate out each sector for ease
PR <- ffdat[ffdat$`Private Recreational`==1,]
FH <- ffdat[ffdat$`For-Hire`==1,]
CM <- ffdat[ffdat$`Commercial`==1,]
#Number of rows for filtered datasets to unique sectors
#Some responses come from multiple sectors, so PR+FH+CM likely greater than original dataset
(PR_resp <- nrow(PR))
(FH_resp <- nrow(FH))
(CM_resp <- nrow(CM))

#Total sector responses (greater than actual responses)
(PR_resp+FH_resp+CM_resp)

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
  geom_text(aes(label = Freq, y=label_pos, x=1.3), color = "black", size = 4,fontface = "bold") +
  theme_void() +
  scale_fill_manual(values = c("#4472C4","#CCCCFF","#A6A6A6")) +
  #labs(title = "Responses by Sector") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = '#595959'),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.background  = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA)) 

print(sectorpieplot)
# ggsave("Comparison/Broad Sentiment Plots/Combined Responses by Sector Pie.png", 
#        plot = sectorpieplot,
#        width = 4.28, #Adjust width as needed
#        height = 3.05, #Adjust height as needed
#        units = "in", 
#        dpi = 700)

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
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Overall Sentiment by Sector", x = "", y = "", fill = "") +
  scale_fill_manual(values = c("#92D050","#FFFF00","#ED7D31")) +
  #scale_y_continuous(breaks = seq(0,150,25), limits = c(0,150))+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, color = "#595959"),
        plot.title = element_text(hjust = 0.5, size = 14, color = '#595959'),
        axis.text = element_text(size = 9, color = "#595959"),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),panel.grid.minor.y = element_blank())
print(sect_sent_plot)
# ggsave("Comparison/Broad Sentiment Plots/Combined Overall Sentiment by Sector.png", 
#        plot = sect_sent_plot, 
#        width = 6.5, #Adjust width as needed
#        height = 3.76, #Adjust height as needed
#        units = "in", 
#        dpi = 700)

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

(tot_sector_stock_sent <- sum(sectorstock_sent$Freq))
###Plot
sect_stock_sent_plot <- ggplot(sectorstock_sent, aes(x = Fleet, y = Freq, fill = Sentiment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Stock Condition Sentiment by Sector", x = "", y = "", fill = "Subgroup") +
  scale_fill_manual(values = c("#92D050","#FFFF00","#ED7D31")) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.margin = margin(t = -10),
        plot.margin = margin(5, 5, 0, 5),
        legend.title = element_blank(),
        legend.text = element_text(size = 9, color = '#595959'),
        plot.title = element_text(hjust = 0.5, size = 14, color = '#595959'),
        axis.text = element_text(size = 9, color = '#595959'),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),panel.grid.minor.y = element_blank())
print(sect_stock_sent_plot)
#Save the plot to the filepath
# ggsave("Comparison/Broad Sentiment Plots/Combined Stock Condition Sentiment by Sector.png", 
#        plot = sect_stock_sent_plot, 
#        width = 6.11, #Adjust width as needed
#        height = 2.81, #Adjust height as needed
#        units = "in", 
#        dpi = 700)
###########################################################################
# 2021 vs 2026 ------------------------------------------------------------
###########################################################################
# Comparison of Overall Sentiment -----------------------------------------

#Frequency table of overall sentiment
compare_sent <- as.data.frame(table(ffdat$Year,ffdat$`Final Overall Sentiment`))
#Add Sentiment labels
compare_sent$Sentiment <- ifelse(compare_sent$Var2==-1, "Negative",
                                 ifelse(compare_sent$Var2==0, "Neutral/Mixed","Positive"))
compare_sent <- compare_sent %>% 
  arrange(Var1)
FF21 <- compare_sent[compare_sent$Var1==2021,]
FF21$prop <- FF21$Freq/sum(FF21$Freq)
FF26 <- compare_sent[compare_sent$Var1==2026,]
FF26$prop <- FF26$Freq/sum(FF26$Freq)
#Add proportions
compare_sent$prop <- ifelse(compare_sent$Var1==2021,
                            compare_sent$Freq/sum(compare_sent$Freq[compare_sent$Var1==2021]),
                            compare_sent$Freq/sum(compare_sent$Freq[compare_sent$Var1==2026]))
  

#Adds label locations and identifiers
compare_sent <- compare_sent %>% 
  arrange(desc(Freq)) %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1))
#Set year as factor
compare_sent$Year <- as.factor(compare_sent$Var1)
#Sets the order of sentiment
compare_sent$Sentiment <- factor(compare_sent$Sentiment, levels = c("Positive", "Neutral/Mixed","Negative"))

###Bar chart - DON'T USE THIS
comp_sent_plot <- ggplot(compare_sent, aes(x = Year, y = Freq, fill = Sentiment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Overall Sentiment by Year", x = "", y = "", fill = "") +
  scale_fill_manual(values = c("#92D050","#FFFF00","#ED7D31")) +
  #scale_y_continuous(breaks = seq(0,150,25), limits = c(0,150))+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, color = "#595959"),
        plot.title = element_text(hjust = 0.5, size = 14, color = '#595959'),
        axis.text = element_text(size = 9, color = "#595959"),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),panel.grid.minor.y = element_blank())
print(comp_sent_plot)
# ggsave("Comparison/Broad Sentiment Plots/Sentiment Comps Bar.png", 
#        plot = comp_sent_plot, 
#        width = 9,height = 6, 
#        units = "in", dpi = 700)


##STacked bar chart - USE THIS
sent_stacked_plot <- ggplot(compare_sent, aes(x = Year, y = prop, fill = Sentiment)) +
  geom_col(color = "white", linewidth = 0.6, position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#92D050","#FFFF00","#ED7D31"))+
  theme_minimal(base_size = 18) +
  labs(x = NULL, y = "Proportion", fill = "Sentiment") +
  theme(axis.text.x = element_text(hjust = 1), panel.grid.major.x = element_blank())+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16))

print(sent_stacked_plot)
# ggsave("Comparison/Broad Sentiment Plots/Sentiment Comps Stacked.png",
#        plot = sent_stacked_plot,
#        width = 9,height = 6,
#        units = "in", dpi = 700)



# Compare Total # of responses related to stock condition -----------------

(comp_tot_stockcond <- table(stockcond$Year))

### 2021
#Total Positive stock condition
(pos_cond21 <- nrow(stockcond[stockcond$`Final  Stock Condition`==1 & stockcond$Year==2021,]))
#Total Negative stock condition
(neg_cond21 <- nrow(stockcond[stockcond$`Final  Stock Condition`==-1 & stockcond$Year==2021,]))
#Total neutral stock condition
(neut_cond21 <- nrow(stockcond[stockcond$`Final  Stock Condition`==0 & stockcond$Year==2021,]))

### 2026
#Total Positive stock condition
(pos_cond26 <- nrow(stockcond[stockcond$`Final  Stock Condition`==1 & stockcond$Year==2026,]))
#Total Negative stock condition
(neg_cond26 <- nrow(stockcond[stockcond$`Final  Stock Condition`==-1 & stockcond$Year==2026,]))
#Total neutral stock condition
(neut_cond26 <- nrow(stockcond[stockcond$`Final  Stock Condition`==0 & stockcond$Year==2026,]))
#Compare all responses
table(stockcond$Year,stockcond$`Final  Stock Condition`)

#pos_stockcond <- stockcond[stockcond$`Final  Stock Condition`==1,]
#neut_stockcond <- stockcond[stockcond$`Final  Stock Condition`==0,]
#neg_stockcond <- stockcond[stockcond$`Final  Stock Condition`==-1,]
# Compare Comments by stock condition -------------------------------------


# Prep data for Plot
comp_stock_sent <- data.frame(Sentiment = rep(c('Negative','Neutral/Mixed','Positive'),each=2),Freq = c(neg_cond21,neg_cond26,neut_cond21,neut_cond26,pos_cond21,pos_cond26), Year = rep(c(2021,2026)))
#Add proportions
comp_stock_sent$prop <- ifelse(comp_stock_sent$Year==2021, 
                           comp_stock_sent$Freq/sum(comp_stock_sent$Freq[comp_stock_sent$Year==2021]),
                           comp_stock_sent$Freq/sum(comp_stock_sent$Freq[comp_stock_sent$Year==2026]))
#Adds label locations and identifiers
comp_stock_sent <- comp_stock_sent %>% 
  arrange(desc(Freq)) %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1))

#Set year as factor
comp_stock_sent$Year <- as.factor(comp_stock_sent$Year)
#Sets the order of sentiment
comp_stock_sent$Sentiment <- factor(comp_stock_sent$Sentiment, levels = c("Positive", "Neutral/Mixed","Negative"))

###Bar chart - DON'T USE THIS
comp_stock_sent_plot <- ggplot(comp_stock_sent, aes(x = Year, y = Freq, fill = Sentiment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Overall Sentiment by Year", x = "", y = "", fill = "") +
  scale_fill_manual(values = c("#92D050","#FFFF00","#ED7D31")) +
  #scale_y_continuous(breaks = seq(0,150,25), limits = c(0,150))+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, color = "#595959"),
        plot.title = element_text(hjust = 0.5, size = 14, color = '#595959'),
        axis.text = element_text(size = 9, color = "#595959"),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),panel.grid.minor.y = element_blank())
print(comp_stock_sent_plot)
# ggsave("Comparison/Broad Sentiment Plots/Stock Sentiment Comps Bar.png", 
#        plot = comp_stock_sent_plot, 
#        width = 9,height = 6, 
#        units = "in", dpi = 700)


##STacked bar chart - USE THIS
stocksent_stacked_plot <- ggplot(comp_stock_sent, aes(x = Year, y = prop, fill = Sentiment)) +
  geom_col(color = "white", linewidth = 0.6, position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#92D050","#FFFF00","#ED7D31"))+
  theme_minimal(base_size = 18) +
  labs(x = NULL, y = "Proportion", fill = "Sentiment") +
  theme(axis.text.x = element_text(hjust = 1), panel.grid.major.x = element_blank())+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16))

print(stocksent_stacked_plot)
# ggsave("Comparison/Broad Sentiment Plots/Stock Sentiment Comps Stacked.png",
#        plot = stocksent_stacked_plot,
#        width = 9,height = 6,
#        units = "in", dpi = 700)


# Compare Unique Responses by Fleet ---------------------------------------
#Private Rec
PR_comp <- as.data.frame(table(PR$Year))
FH_comp <- as.data.frame(table(FH$Year))
CM_comp <- as.data.frame(table(CM$Year))


sector_comp <- rbind(PR_comp,FH_comp,CM_comp)
sector_comp$Sector <- rep(c("Private Recreational","For-Hire","Commercial"),each=2)
#Add proportions
sector_comp$prop <- ifelse(sector_comp$Var1==2021, 
                           sector_comp$Freq/sum(sector_comp$Freq[sector_comp$Var1==2021]),
                           sector_comp$Freq/sum(sector_comp$Freq[sector_comp$Var1==2026]))


#Adds label locations and identifiers
sector_comp <- sector_comp %>% 
  arrange(desc(Freq)) %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1))
#Set year as factor
sector_comp$Year <- as.factor(sector_comp$Var1)


###Bar Chart - Don't USE THIS
sector_comp_plot <- ggplot(sector_comp, aes(x = Year, y = Freq, fill = Sector)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Sector Responses by Year", x = "", y = "", fill = "") +
  scale_fill_manual(values = c("#4472C4","#CCCCFF","#A6A6A6")) +
  #scale_y_continuous(breaks = seq(0,150,25), limits = c(0,150))+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14, color = "#595959"),
        plot.title = element_text(hjust = 0.5, size = 14, color = '#595959'),
        axis.text = element_text(size = 9, color = "#595959"),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(),panel.grid.minor.y = element_blank())
print(sector_comp_plot)
# ggsave("Comparison/Broad Sentiment Plots/Sector Comps Bar.png", 
#        plot = sector_comp_plot, width = 9,height = 6, 
#        units = "in", dpi = 700)

##Stacked bar chart - USE THIS
sect_stacked_plot <- ggplot(sector_comp, aes(x = Year, y = prop, fill = Sector)) +
  geom_col(color = "white", linewidth = 0.6, position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#4472C4","#CCCCFF","#A6A6A6")) +
  theme_minimal(base_size = 18) +
  labs(x = NULL, y = "Proportion", fill = "Sector") +
  theme(axis.text.x = element_text(hjust = 1), panel.grid.major.x = element_blank())+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16))

print(sect_stacked_plot)
# ggsave("Comparison/Broad Sentiment Plots/Sector Comps Stacked.png", 
#        plot = sect_stacked_plot,
#        width = 9,height = 6,
#        units = "in", dpi = 700)

# Sentiment by Fleet Comparison -------------------------------------------
### Prepare data for analysis
#Private Rec
PRsent_comp <- as.data.frame(table(PR$Year,PR$`Final Overall Sentiment`))
PRsent_comp$Fleet <- "Private Recreational"
PRsent_comp$prop <- ifelse(PRsent_comp$Var1==2021, 
                           PRsent_comp$Freq/sum(PRsent_comp$Freq[PRsent_comp$Var1==2021]),
                           PRsent_comp$Freq/sum(PRsent_comp$Freq[PRsent_comp$Var1==2026]))

#For-hire
FHsent_comp <- as.data.frame(table(FH$Year,FH$`Final Overall Sentiment`))
FHsent_comp$Fleet <- "For-Hire"
FHsent_comp$prop <- ifelse(FHsent_comp$Var1==2021,
                           FHsent_comp$Freq/sum(FHsent_comp$Freq[FHsent_comp$Var1==2021]),
                           FHsent_comp$Freq/sum(FHsent_comp$Freq[FHsent_comp$Var1==2026]))

#Commercial
CMsent_comp <- as.data.frame(table(CM$Year,CM$`Final Overall Sentiment`))
CMsent_comp$Fleet <- "Commercial"
CMsent_comp$prop <- ifelse(CMsent_comp$Var1==2021,
                           CMsent_comp$Freq/sum(CMsent_comp$Freq[CMsent_comp$Var1==2021]),
                           CMsent_comp$Freq/sum(CMsent_comp$Freq[CMsent_comp$Var1==2026]))

#Combine and prep for plot
sect_sent_comp <- rbind(PRsent_comp,FHsent_comp,CMsent_comp)
sect_sent_comp <- sect_sent_comp %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1),
         Sentiment = recode(Var2, "-1" = "Negative", "0" = "Neutral/Mixed", "1" = "Positive"))
#Sets the order of sector
sect_sent_comp$Fleet <- factor(sect_sent_comp$Fleet, levels = unique(sect_sent_comp$Fleet))
#Sets the order of sentiment
sect_sent_comp$Sentiment <- factor(sect_sent_comp$Sentiment, levels = c("Positive", "Neutral/Mixed","Negative"))

sect_sent_comp$Year <- factor(sect_sent_comp$Var1)

#Multiple bars - not good
ggplot(sect_sent_comp,aes(x = Sentiment, y = prop, fill = factor(Year))) +
  geom_col(position = "dodge") +
  facet_wrap(~Fleet) +
  labs(x = "Sentiment",
       y = "Proportion",
       fill = "Year") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

#Diverging Stacked Bars

# LEFT side (Negative + half Neutral)
left <- sect_sent_comp  %>%
  mutate(plot_prop = case_when(Sentiment == "Negative" ~ -prop,Sentiment == "Neutral/Mixed" ~ -prop / 2,TRUE ~ NA_real_)) %>%
  filter(!is.na(plot_prop))
# RIGHT side (Positive + half Neutral)
right <- sect_sent_comp  %>%
  mutate(plot_prop = case_when(Sentiment == "Positive" ~ prop,Sentiment == "Neutral/Mixed" ~ prop / 2,TRUE ~ NA_real_)) %>%
  filter(!is.na(plot_prop))

#Diverging Stacked Bar Chart
sect_sent_comp_plot <- ggplot() +
  # Negative side
  geom_col(data = left,aes(x = factor(Year), y = plot_prop, fill = Sentiment),width = 0.7,position = position_stack(reverse = TRUE)) +
  # Positive side 
  geom_col(data = right,aes(x = factor(Year), y = plot_prop, fill = Sentiment),width = 0.7) +
  facet_wrap(~Fleet) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Negative" = "#ED7D31","Neutral/Mixed" = "#FFFF00","Positive" = "#92D050")) +
  labs(x = "Year",y = "Proportion",fill = "Overall Sentiment") +
  theme_minimal()

print(sect_sent_comp_plot)

# ggsave("Comparison/Broad Sentiment Plots/Compare Overall Sentiment by Sector.png",
#        plot = sect_sent_comp_plot,
#        width = 6.5, #Adjust width as needed
#        height = 3.76, #Adjust height as needed
#        units = "in",
#        dpi = 700)

### Stacked chart all on same axis
sect_sent_stacked_plot <- ggplot(sect_sent_comp,aes(x = Year, y = prop, fill = Sentiment)) +
  geom_col(position = "fill") +
  facet_wrap(~Fleet) +
  labs(x = "Sentiment",
       y = "Proportion",
       fill = "Year") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Negative" = "#ED7D31","Neutral/Mixed" = "#FFFF00","Positive" = "#92D050")) +
  labs(x = "Year",y = "Proportion",fill = "Overall Sentiment") +
  theme_minimal()


print(sect_sent_stacked_plot)

ggsave("Comparison/Broad Sentiment Plots/Compare Overall Sentiment by Sector 2.png",
       plot = sect_sent_stacked_plot,
       width = 6.5, #Adjust width as needed
       height = 3.76, #Adjust height as needed
       units = "in",
       dpi = 700)

# Stock Sentiment by Fleet Comparison -------------------------------------
### Prepare data for analysis
#Private Rec
PRstocksent_comp <- as.data.frame(table(PR$Year,PR$`Final  Stock Condition`))
PRstocksent_comp$Fleet <- "Private Recreational"
PRstocksent_comp$prop <- ifelse(PRstocksent_comp$Var1==2021, 
                           PRstocksent_comp$Freq/sum(PRstocksent_comp$Freq[PRstocksent_comp$Var1==2021]),
                           PRstocksent_comp$Freq/sum(PRstocksent_comp$Freq[PRstocksent_comp$Var1==2026]))

#For-hire
FHstocksent_comp <- as.data.frame(table(FH$Year,FH$`Final  Stock Condition`))
FHstocksent_comp$Fleet <- "For-Hire"
FHstocksent_comp$prop <- ifelse(FHstocksent_comp$Var1==2021,
                           FHstocksent_comp$Freq/sum(FHstocksent_comp$Freq[FHstocksent_comp$Var1==2021]),
                           FHstocksent_comp$Freq/sum(FHstocksent_comp$Freq[FHstocksent_comp$Var1==2026]))

#Commercial
CMstocksent_comp <- as.data.frame(table(CM$Year,CM$`Final  Stock Condition`))
CMstocksent_comp$Fleet <- "Commercial"
CMstocksent_comp$prop <- ifelse(CMstocksent_comp$Var1==2021,
                           CMstocksent_comp$Freq/sum(CMstocksent_comp$Freq[CMstocksent_comp$Var1==2021]),
                           CMstocksent_comp$Freq/sum(CMstocksent_comp$Freq[CMstocksent_comp$Var1==2026]))

#Combine and prep for plot
sect_stocksent_comp <- rbind(PRstocksent_comp,FHstocksent_comp,CMstocksent_comp)
sect_stocksent_comp <- sect_stocksent_comp %>% 
  mutate(label_pos = cumsum(prop)-prop/2,label = scales::percent(prop,accuracy = 1),
         Sentiment = recode(Var2, "-1" = "Negative", "0" = "Neutral/Mixed", "1" = "Positive"))
#Sets the order of sector
sect_stocksent_comp$Fleet <- factor(sect_stocksent_comp$Fleet, levels = unique(sect_stocksent_comp$Fleet))
#Sets the order of sentiment
sect_stocksent_comp$Sentiment <- factor(sect_stocksent_comp$Sentiment, levels = c("Positive", "Neutral/Mixed","Negative"))

sect_stocksent_comp$Year <- factor(sect_stocksent_comp$Var1)

#Multiple bars - not good
ggplot(sect_stocksent_comp,aes(x = Sentiment, y = prop, fill = factor(Year))) +
  geom_col(position = "dodge") +
  facet_wrap(~Fleet) +
  labs(x = "Sentiment",
       y = "Proportion",
       fill = "Year") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

#Diverging Stacked Bars

# LEFT side (Negative + half Neutral)
left <- sect_stocksent_comp  %>%
  mutate(plot_prop = case_when(Sentiment == "Negative" ~ -prop,Sentiment == "Neutral/Mixed" ~ -prop / 2,TRUE ~ NA_real_)) %>%
  filter(!is.na(plot_prop))
# RIGHT side (Positive + half Neutral)
right <- sect_stocksent_comp  %>%
  mutate(plot_prop = case_when(Sentiment == "Positive" ~ prop,Sentiment == "Neutral/Mixed" ~ prop / 2,TRUE ~ NA_real_)) %>%
  filter(!is.na(plot_prop))

#Diverging Stacked Bar Chart
sect_stocksent_comp_plot <- ggplot() +
  # Negative side
  geom_col(data = left,aes(x = factor(Year), y = plot_prop, fill = Sentiment),width = 0.7,position = position_stack(reverse = TRUE)) +
  # Positive side 
  geom_col(data = right,aes(x = factor(Year), y = plot_prop, fill = Sentiment),width = 0.7) +
  facet_wrap(~Fleet) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Negative" = "#ED7D31","Neutral/Mixed" = "#FFFF00","Positive" = "#92D050")) +
  labs(x = "Year",y = "Proportion",fill = "Stock Condition Sentiment") +
  theme_minimal()

print(sect_stocksent_comp_plot)

# ggsave("Comparison/Broad Sentiment Plots/Compare Stock Sentiment by Sector.png", 
#        plot = sect_stocksent_comp_plot, 
#        width = 6.5, #Adjust width as needed
#        height = 3.76, #Adjust height as needed
#        units = "in", 
#        dpi = 700)

### Stacked chart all on same axis
sect_stocksent_stacked_plot <- ggplot(sect_stocksent_comp,aes(x = Year, y = prop, fill = Sentiment)) +
  geom_col(position = "fill") +
  facet_wrap(~Fleet) +
  labs(x = "Sentiment",
       y = "Proportion",
       fill = "Year") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Negative" = "#ED7D31","Neutral/Mixed" = "#FFFF00","Positive" = "#92D050")) +
  labs(x = "Year",y = "Proportion",fill = "Stock Condition Sentiment") +
  theme_minimal()


print(sect_stocksent_stacked_plot)

ggsave("Comparison/Broad Sentiment Plots/Compare Stock Sentiment by Sector 2.png",
       plot = sect_stocksent_stacked_plot,
       width = 6.5, #Adjust width as needed
       height = 3.76, #Adjust height as needed
       units = "in",
       dpi = 700)
###########################################################################
# See who did both surveys ------------------------------------------------
###########################################################################
# Filter for people who did both surveys ----------------------------------
ffdat$`Full name` <- paste0(ffdat$`FIRST Name`," ",ffdat$`LAST Name`)
ffdat$`Full name` <- paste0(tolower(ffdat$`FIRST Name`)," ",tolower(ffdat$`LAST Name`))
bothsurveys <- ffdat %>% 
  group_by(`Full name`) %>% 
  filter(n()>1) %>% 
  ungroup() %>% 
  arrange(`Full name`)
name_check <- bothsurveys %>%
  dplyr::summarise(n = n(), .by = c(`Full name`, Year)) %>%
  filter(n > 1)
bothsurveys <- bothsurveys[!(bothsurveys$`Full name`%in% name_check$`Full name`),]

bothsurveys <- bothsurveys %>%
  group_by(`Full name`) %>%
  dplyr::mutate(name_id = cur_group_id()) %>%
  ungroup()

# Sentiment Change --------------------------------------------------------

sent_change <- bothsurveys %>% 
  select(Year,name_id,`Final Overall Sentiment`) %>% 
  pivot_wider(names_from = Year,
    values_from = `Final Overall Sentiment`,
    names_prefix = "sent_") %>% 
  mutate(sent_2021 = as.numeric(sent_2021),sent_2026 = as.numeric(sent_2026),
         sentiment_change = sent_2026 - sent_2021)

sent_change <- sent_change %>%
  mutate(sentiment_transition = case_when(sent_2021 ==  1 & sent_2026 ==  1 ~ "Positive - Positive",
                                          sent_2021 ==  1 & sent_2026 ==  0 ~ "Positive - Neutral/Mixed",
                                          sent_2021 ==  1 & sent_2026 == -1 ~ "Positive - Negative",
                                          sent_2021 ==  0 & sent_2026 ==  1 ~ "Neutral/Mixed - Positive",
                                          sent_2021 ==  0 & sent_2026 ==  0 ~ "Neutral/Mixed - Neutral/Mixed",
                                          sent_2021 ==  0 & sent_2026 == -1 ~ "Neutral/Mixed - Negative",
                                          sent_2021 == -1 & sent_2026 ==  1 ~ "Negative - Positive",
                                          sent_2021 == -1 & sent_2026 ==  0 ~ "Negative - Neutral/Mixed",
                                          sent_2021 == -1 & sent_2026 == -1 ~ "Negative - Negative",
                                          TRUE ~ NA_character_)) %>% 
  mutate(sentiment_score = case_when(sent_2021 ==  1 & sent_2026 ==  1 ~ "2",
                                     sent_2021 ==  1 & sent_2026 ==  0 ~'0.5',
                                     sent_2021 ==  1 & sent_2026 == -1 ~ '-1.5',
                                     sent_2021 ==  0 & sent_2026 ==  1 ~ '1',
                                     sent_2021 ==  0 & sent_2026 ==  0 ~'0',
                                     sent_2021 ==  0 & sent_2026 == -1 ~ '-1',
                                     sent_2021 == -1 & sent_2026 ==  1 ~ '1.5',
                                     sent_2021 == -1 & sent_2026 ==  0 ~ '-0.5',
                                     sent_2021 == -1 & sent_2026 == -1 ~ '-2',
                                     TRUE ~ NA_character_))

sent_change$sentiment_score <- as.numeric(sent_change$sentiment_score)
transition_counts <- sent_change %>%
  dplyr::count(sentiment_transition) %>%
  arrange(desc(n))
sent_change <- sent_change %>%
  mutate(direction = case_when(sent_2026 > sent_2021 ~ "Improved",sent_2026 < sent_2021 ~ "Worsened",TRUE ~ "No change"))
table(sent_change$direction)

direction_counts <- sent_change %>%
  dplyr::count(direction) %>%
  arrange(desc(n))
# Figures -----------------------------------------------------------------


ggplot(transition_counts,aes(x = reorder(sentiment_transition, n),y = n)) +
  geom_col(fill = "#4E79A7") +
  coord_flip() +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank())

sentchange_plot <- ggplot(direction_counts,aes(x = reorder(direction, n),y = n)) +
  geom_col(fill= c("#FFFF00","#ED7D31","#92D050")) +
  coord_flip() +
  labs(x = "", y = "") +
  #scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank())
print(sentchange_plot)

ggsave("Comparison/Broad Sentiment Plots/Sentiment Change Both Surveys.png",
       plot = sentchange_plot,
       width = 6.5, #Adjust width as needed
       height = 3.76, #Adjust height as needed
       units = "in",
       dpi = 700)

table(sent_change$sent_2021, sent_change$sent_2026)


# Stock Sentiment Change --------------------------------------------------



stocksent_change <- bothsurveys %>% 
  select(Year,name_id,`Final  Stock Condition`) %>% 
  pivot_wider(names_from = Year,
              values_from = `Final  Stock Condition`,
              names_prefix = "sent_") %>% 
  mutate(sent_2021 = as.numeric(sent_2021),sent_2026 = as.numeric(sent_2026),
         sentiment_change = sent_2026 - sent_2021) 

stocksent_change<-stocksent_change[!is.na(stocksent_change$sentiment_change),]
stocksent_change <- stocksent_change %>%
  mutate(sentiment_transition = case_when(sent_2021 ==  1 & sent_2026 ==  1 ~ "Positive - Positive",
                                          sent_2021 ==  1 & sent_2026 ==  0 ~ "Positive - Neutral/Mixed",
                                          sent_2021 ==  1 & sent_2026 == -1 ~ "Positive - Negative",
                                          sent_2021 ==  0 & sent_2026 ==  1 ~ "Neutral/Mixed - Positive",
                                          sent_2021 ==  0 & sent_2026 ==  0 ~ "Neutral/Mixed - Neutral/Mixed",
                                          sent_2021 ==  0 & sent_2026 == -1 ~ "Neutral/Mixed - Negative",
                                          sent_2021 == -1 & sent_2026 ==  1 ~ "Negative - Positive",
                                          sent_2021 == -1 & sent_2026 ==  0 ~ "Negative - Neutral/Mixed",
                                          sent_2021 == -1 & sent_2026 == -1 ~ "Negative - Negative",
                                          TRUE ~ NA_character_)) %>% 
  mutate(sentiment_score = case_when(sent_2021 ==  1 & sent_2026 ==  1 ~ "2",
                                     sent_2021 ==  1 & sent_2026 ==  0 ~'0.5',
                                     sent_2021 ==  1 & sent_2026 == -1 ~ '-1.5',
                                     sent_2021 ==  0 & sent_2026 ==  1 ~ '1',
                                     sent_2021 ==  0 & sent_2026 ==  0 ~'0',
                                     sent_2021 ==  0 & sent_2026 == -1 ~ '-1',
                                     sent_2021 == -1 & sent_2026 ==  1 ~ '1.5',
                                     sent_2021 == -1 & sent_2026 ==  0 ~ '-0.5',
                                     sent_2021 == -1 & sent_2026 == -1 ~ '-2',
                                     TRUE ~ NA_character_))

stocksent_change$sentiment_score <- as.numeric(stocksent_change$sentiment_score)
stock_transition_counts <- stocksent_change %>%
  dplyr::count(sentiment_transition) %>%
  arrange(desc(n))
stocksent_change <- stocksent_change %>%
  mutate(direction = case_when(sent_2026 > sent_2021 ~ "Improved",sent_2026 < sent_2021 ~ "Worsened",TRUE ~ "No change"))
table(stocksent_change$direction)

stock_direction_counts <- stocksent_change %>%
  dplyr::count(direction) %>%
  arrange(desc(n))
# Figures -----------------------------------------------------------------


ggplot(stock_transition_counts,aes(x = reorder(sentiment_transition, n),y = n)) +
  geom_col(fill = "#4E79A7") +
  coord_flip() +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank())

stock_sentchange_plot <- ggplot(stock_direction_counts,aes(x = reorder(direction, n),y = n)) +
  geom_col(fill= c("#FFFF00","#92D050","#ED7D31")) +
  coord_flip() +
  labs(x = "", y = "") +
  #scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank())
print(stock_sentchange_plot)

ggsave("Comparison/Broad Sentiment Plots/Stock Sentiment Change Both Surveys.png",
       plot = stock_sentchange_plot,
       width = 6.5, #Adjust width as needed
       height = 3.76, #Adjust height as needed
       units = "in",
       dpi = 700)

table(sent_change$sent_2021, sent_change$sent_2026)


# Geographic spread -------------------------------------------------------

bothsurvgeo <- bothsurveys
#Regions to split out by
regions <- seq(1,21)

#Dummy Region column to preserve original
#"Fleet" column will be dropped in the next step
bothsurvgeo$Region <- bothsurvgeo$`General Location of Observation`

#This will create 4 new columns, with binary values for presence/absence of each fleet. 3 of which will be the main_sectors, and it will bin every other response into "Other". 
#Un-comment the one below if you want all responses in binary form. 
bothsurvgeo2 <- bothsurvgeo %>%
  separate_rows(`Region`, sep = ";") %>%
  mutate(`Region` = str_trim(`Region`)) %>%
  filter(`Region` != "") %>% 
  mutate(value = 1)%>%
  pivot_wider(names_from = `Region`,values_from = value, values_fill = 0, values_fn = max)

regioncounts <- bothsurvgeo2 %>% 
  pivot_longer(cols = `1`:`21`,  names_to = "Region",
               values_to = "value"
  ) %>%
  filter(value == 1) %>%
  count(Region, Year) %>%
  pivot_wider(
    names_from = Year,
    values_from = n,
    names_prefix = "n_",
    values_fill = 0
  )

region_counts <- bothsurvgeo %>%
  separate_rows(`Region`, sep = ";") %>%
  mutate(`Region` = str_trim(`Region`)) %>%
  filter(`Region` != "") %>%
  dplyr::count(`Region`, `Year`) %>%
  pivot_wider(
    names_from = `Year`,
    values_from = n,
    names_prefix = "n_",
    values_fill = 0
  )
region_counts$Region <- as.numeric(region_counts$Region)
###########################################################################
######                      Fisherman Feedback Analysis              ######
######                         Gag Grouper Comparison                ######
######                             Word Frequencies                  ######
###########################################################################

# Load Packages -----------------------------------------------------------

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
library(readxl)
library(ggrepel)

###########################################################################
# Combined ----------------------------------------------------------------
###########################################################################

df <- ffdat
#df<- read_xls("Gag_Fisherman Feedback_2026_CLEAN for analysis.xls") #Change this for different species

###create new database that is only the comments NOTE: that column and row changes each time
#rg<-df[1:200,1]
rg <- as.character(df[[7]])
#rg$id <- 1:nrow(rg)
#rg <- gsub("\\$" ,"", rg)
#rg <- gsub("[?]", "'", rg)
rg <- gsub("shark", "sharks", rg, ignore.case = TRUE)
rg<- gsub("sharkss", "sharks", rg, ignore.case = TRUE)
#Combine small/smaller, large/larger, big/bigger, short/shorter
rg <- gsub("smaller", "small", rg, ignore.case = TRUE)
rg <- gsub("larger", "large", rg, ignore.case = TRUE)
rg <- gsub("bigger", "big", rg, ignore.case = TRUE)
rg <- gsub("shorter", "short", rg, ignore.case = TRUE)
rg <- gsub("no shortage", "no_shortage", rg, ignore.case = TRUE)
rg <- gsub("abundance", "abundant", rg, ignore.case = TRUE)
rg <- gsub("consistently", "consistent", rg, ignore.case = TRUE)
rg <- gsub("increased", "increase", rg, ignore.case = TRUE)
rg <- gsub("plenty", "plentiful", rg, ignore.case = TRUE)
out <- c()

###modify the Bing library for more fisheries context

dropword <-c("bloom","redtide","discard", "wreck","bait","shipwreck","limit","shark","lure","shallow","significant","catch","caught","catching","worked","hang","pretty","aggressive","hogs",'biting','like')
sentiments <- get_sentiments("bing") %>% filter(!word %in% dropword)

sentiment.neg<-tibble(word=c("bloom","redtide","discard","harder","smaller","small","decrease","decreased","fewer","overfished","lower","less","diminished","shark","sharks","dolphin","dolphins","weather","release",'season'),sentiment=c("negative"))
sentiment.pos<-tibble(word=c("bite","large","larger","biting","increase","increased","plenty","uptick","aggressive","no shortage"),sentiment=c("positive"))
additional_sentiment <- sentiment.neg%>%
  rbind(sentiment.pos)
new_sentiment <- sentiments%>%
  rbind(additional_sentiment)

#Different combos of "envious" categorized as both positive and negative
# (dupcheck <- count(new_sentiment,word) %>% 
#   filter(n>1))
# dupcomp <- new_sentiment %>% 
#   filter(word %in% dupcheck$word)


###run a loop that looks that scores each comment by sentiment
for(i in 1:length(rg)){
  #for(i in 1:2){
  tokens.i <- tibble(text = rg[i]) %>% unnest_tokens(word, text)
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




### run sentiment analysis to examine each word
rg2<-tibble(line=1:length(rg), text=rg)
rg3<-rg2%>%unnest_tokens(word,text)

overall_word_counts <- rg3 %>%
  dplyr::count(word, sort = TRUE) %>%
  ungroup()

bing_word_counts <- rg3 %>%
  inner_join(new_sentiment) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

#bing_word_counts <- gsub("no_shortage", "no shortage", bing_word_counts, ignore.case = TRUE)
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

top_words <- bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 
barchart <- ggplot(top_words, aes(word, n, fill = sentiment)) +
  scale_fill_manual(values=c("#fc8d59","#99d594"))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +theme(axis.title=element_text(size=14),axis.text=element_text(size=12))+theme(strip.text=element_text(size=14))+
  scale_y_continuous()+
  coord_flip()
print(barchart)

# ggsave("Comparison/Word Plots/Combined Most Frequent Bar Updated.png", 
#        plot = barchart,
#        width = 5.67, #Adjust width as needed
#        height = 3.37, #Adjust height as needed
#        units = "in", 
#        dpi = 700)
###This creates the sentiment word cloud
#png("Comparison/Word Plots/Combined Most Frequent Cloud_50.png",
    # width = 3.56, #Adjust width as needed
    # height = 3.37, #Adjust height as needed
    # units = "in", 
    # res = 700)
par(mar = c(0, 0, 0, 0))
rg3 %>%
  inner_join(new_sentiment) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#fc8d59","#99d594"), 
                   max.words = 50, title.size=1.5,match.color=TRUE)
# dev.off()

###########################################################################
# Top 2021 vs top 2026 ----------------------------------------------------
###########################################################################
#Look at frequency of top 2026 words in 2021 and vice versa
df <- ffdat
#df<- read_xls("Gag_Fisherman Feedback_2026_CLEAN for analysis.xls") #Change this for different species
# 2021 only ---------------------------------------------------------------


###create new database that is only the comments NOTE: that column and row changes each time
#rg<-df[1:200,1]
gag21 <- ffdat[ffdat$Year==2021,]
gag21 <- as.character(gag21[[7]])
#gag21$id <- 1:nrow(gag21)
#gag21 <- gsub("\\$" ,"", gag21)
#gag21 <- gsub("[?]", "'", gag21)
gag21 <- gsub("shark", "sharks", gag21, ignore.case = TRUE)
gag21<- gsub("sharkss", "sharks", gag21, ignore.case = TRUE)
#Combine small/smaller, large/larger, big/bigger, short/shorter
gag21 <- gsub("smaller", "small", gag21, ignore.case = TRUE)
gag21 <- gsub("larger", "large", gag21, ignore.case = TRUE)
gag21 <- gsub("bigger", "big", gag21, ignore.case = TRUE)
gag21 <- gsub("shorter", "short", gag21, ignore.case = TRUE)
gag21 <- gsub("no shortage", "no_shortage", gag21, ignore.case = TRUE)
gag21 <- gsub("abundance", "abundant", gag21, ignore.case = TRUE)
gag21 <- gsub("consistently", "consistent", gag21, ignore.case = TRUE)
gag21 <- gsub("increased", "increase", gag21, ignore.case = TRUE)
gag21 <- gsub("plenty", "plentiful", gag21, ignore.case = TRUE)
out <- c()

###modify the Bing library for more fisheries context

dropword <-c("bloom","redtide","discard", "wreck","bait","shipwreck","limit","shark","lure","shallow","significant","catch","caught","catching","worked","hang","pretty","aggressive","hogs",'biting','like')
sentiments <- get_sentiments("bing") %>% filter(!word %in% dropword)

sentiment.neg<-tibble(word=c("bloom","redtide","discard","harder","smaller","small","decrease","decreased","fewer","overfished","lower","less","diminished","shark","sharks","dolphin","dolphins","weather","release",'season'),sentiment=c("negative"))
sentiment.pos<-tibble(word=c("bite","large","larger","biting","increase","increased","plenty","uptick","aggressive","no shortage"),sentiment=c("positive"))
additional_sentiment <- sentiment.neg%>%
  rbind(sentiment.pos)
new_sentiment <- sentiments%>%
  rbind(additional_sentiment)

#Different combos of "envious" categorized as both positive and negative
# (dupcheck <- count(new_sentiment,word) %>% 
#   filter(n>1))
# dupcomp <- new_sentiment %>% 
#   filter(word %in% dupcheck$word)


###run a loop that looks that scores each comment by sentiment
for(i in 1:length(gag21)){
  #for(i in 1:2){
  tokens.i <- tibble(text = gag21[i]) %>% unnest_tokens(word, text)
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
out21 <- na.omit(out)
out21$standardized <- as.numeric(as.character(out21$standardized))
out21$lab <- cut(out21$standardized, breaks = c(-1,-.33,.33,1), include.lowest=TRUE)
out21$type <- with(out21, ifelse(standardized < -.33, '-1',
                               ifelse(standardized > .33, '1', '0')))
colors <- c("#fc8d59", "#ffffbf", "#99d594")
out21 %>% 
  group_by(lab) %>%
  summarise(no_rows = length(lab))

out$type <- with(out, ifelse(standardized < -.33, '-1',
                             ifelse(standardized > .33, '1', '0')))




### run sentiment analysis to examine each word
gag212<-tibble(line=1:length(gag21), text=gag21)
gag213<-gag212%>%unnest_tokens(word,text)

overall_word_counts21 <- gag213 %>%
  dplyr::count(word, sort = TRUE) %>%
  ungroup()

bing_word_counts21 <- gag213 %>%
  inner_join(new_sentiment) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

#bing_word_counts <- gsub("no_shortage", "no shortage", bing_word_counts, ignore.case = TRUE)
###This creates the word sentiment bar chart
bing_word_counts21 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
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

top_words21 <- bing_word_counts21 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 
barchart21 <- ggplot(top_words21, aes(word, n, fill = sentiment)) +
  scale_fill_manual(values=c("#fc8d59","#99d594"))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +theme(axis.title=element_text(size=14),axis.text=element_text(size=12))+theme(strip.text=element_text(size=14))+
  scale_y_continuous()+
  coord_flip()
print(barchart21)

# ggsave("Comparison/Word Plots/Combined Most Frequent Bar Updated.png", 
#        plot = barchart,
#        width = 5.67, #Adjust width as needed
#        height = 3.37, #Adjust height as needed
#        units = "in", 
#        dpi = 700)
###This creates the sentiment word cloud
# png("Comparison/Word Plots/Combined Most Frequent Cloud_50.png",
#     width = 3.56, #Adjust width as needed
#     height = 3.37, #Adjust height as needed
#     units = "in", 
#     res = 700)
# par(mar = c(0, 0, 0, 0))
gag213 %>%
  inner_join(new_sentiment) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#fc8d59","#99d594"), 
                   max.words = 50, title.size=1.5,match.color=TRUE)
# dev.off()

# 2026 only ---------------------------------------------------------------
df <- ffdat
###create new database that is only the comments NOTE: that column and row changes each time
#rg<-df[1:200,1]
gag26 <- df[df$Year==2026,]
gag26 <- as.character(gag26[[7]])
#gag26$id <- 1:nrow(gag26)
#gag26 <- gsub("\\$" ,"", gag26)
#gag26 <- gsub("[?]", "'", gag26)
gag26 <- gsub("shark", "sharks", gag26, ignore.case = TRUE)
gag26<- gsub("sharkss", "sharks", gag26, ignore.case = TRUE)
#Combine small/smaller, large/larger, big/bigger, short/shorter
gag26 <- gsub("smaller", "small", gag26, ignore.case = TRUE)
gag26 <- gsub("larger", "large", gag26, ignore.case = TRUE)
gag26 <- gsub("bigger", "big", gag26, ignore.case = TRUE)
gag26 <- gsub("shorter", "short", gag26, ignore.case = TRUE)
gag26 <- gsub("no shortage", "no_shortage", gag26, ignore.case = TRUE)
gag26 <- gsub("abundance", "abundant", gag26, ignore.case = TRUE)
gag26 <- gsub("consistently", "consistent", gag26, ignore.case = TRUE)
gag26 <- gsub("increased", "increase", gag26, ignore.case = TRUE)
gag26 <- gsub("plenty", "plentiful", gag26, ignore.case = TRUE)
out <- c()

###modify the Bing library for more fisheries context

dropword <-c("bloom","redtide","discard", "wreck","bait","shipwreck","limit","shark","lure","shallow","significant","catch","caught","catching","worked","hang","pretty","aggressive","hogs",'biting','like')
sentiments <- get_sentiments("bing") %>% filter(!word %in% dropword)

sentiment.neg<-tibble(word=c("bloom","redtide","discard","harder","smaller","small","decrease","decreased","fewer","overfished","lower","less","diminished","shark","sharks","dolphin","dolphins","weather","release",'season'),sentiment=c("negative"))
sentiment.pos<-tibble(word=c("bite","large","larger","biting","increase","increased","plenty","uptick","aggressive","no shortage"),sentiment=c("positive"))
additional_sentiment <- sentiment.neg%>%
  rbind(sentiment.pos)
new_sentiment <- sentiments%>%
  rbind(additional_sentiment)

#Different combos of "envious" categorized as both positive and negative
# (dupcheck <- count(new_sentiment,word) %>% 
#   filter(n>1))
# dupcomp <- new_sentiment %>% 
#   filter(word %in% dupcheck$word)


###run a loop that looks that scores each comment by sentiment
for(i in 1:length(gag26)){
  #for(i in 1:2){
  tokens.i <- tibble(text = gag26[i]) %>% unnest_tokens(word, text)
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
out26 <- na.omit(out)
out26$standardized <- as.numeric(as.character(out26$standardized))
out26$lab <- cut(out26$standardized, breaks = c(-1,-.33,.33,1), include.lowest=TRUE)
out26$type <- with(out26, ifelse(standardized < -.33, '-1',
                                 ifelse(standardized > .33, '1', '0')))
colors <- c("#fc8d59", "#ffffbf", "#99d594")
out26 %>% 
  group_by(lab) %>%
  summarise(no_rows = length(lab))

out$type <- with(out, ifelse(standardized < -.33, '-1',
                             ifelse(standardized > .33, '1', '0')))




### run sentiment analysis to examine each word
gag262<-tibble(line=1:length(gag26), text=gag26)
gag263<-gag262%>%unnest_tokens(word,text)

overall_word_counts26 <- gag263 %>%
  dplyr::count(word, sort = TRUE) %>%
  ungroup()

bing_word_counts26 <- gag263 %>%
  inner_join(new_sentiment) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

#bing_word_counts <- gsub("no_shortage", "no shortage", bing_word_counts, ignore.case = TRUE)
###This creates the word sentiment bar chart
bing_word_counts26 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
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

top_words26 <- bing_word_counts26 %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 
barchart26 <- ggplot(top_words26, aes(word, n, fill = sentiment)) +
  scale_fill_manual(values=c("#fc8d59","#99d594"))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +theme(axis.title=element_text(size=14),axis.text=element_text(size=12))+theme(strip.text=element_text(size=14))+
  scale_y_continuous()+
  coord_flip()
print(barchart26)

# ggsave("Comparison/Word Plots/Combined Most Frequent Bar Updated.png", 
#        plot = barchart,
#        width = 5.67, #Adjust width as needed
#        height = 3.37, #Adjust height as needed
#        units = "in", 
#        dpi = 700)
###This creates the sentiment word cloud
# png("Comparison/Word Plots/Combined Most Frequent Cloud_50.png",
#     width = 3.56, #Adjust width as needed
#     height = 3.37, #Adjust height as needed
#     units = "in", 
#     res = 700)
# par(mar = c(0, 0, 0, 0))
gag263 %>%
  inner_join(new_sentiment) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#fc8d59","#99d594"), 
                   max.words = 50, title.size=1.5,match.color=TRUE)
# dev.off()


# Compare top words from each survey --------------------------------------
top_words21$Year <- 2021
top_words26$Year <- 2026

all_top_words <- bind_rows(top_words21, top_words26) %>%
  distinct(word)
freq21 <- all_top_words %>%
  left_join(bing_word_counts21 %>%
      select(word, freq_21 = n),
    by = "word")

freq26 <- all_top_words %>%
  left_join(bing_word_counts26 %>%
      select(word, freq_26 = n),
    by = "word")

word_comparison <- freq21 %>%
  left_join(freq26, by = "word") %>% 
  inner_join(new_sentiment)

#Removed season
word_comparison <- word_comparison[-2,]

#Scatter Plot
ggplot(word_comparison, aes(x = freq_21, y = freq_26, color = sentiment)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = word), size = 3) +
 # geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values=c("#fc8d59","#99d594"))+
  labs(x = "Frequency in 2021",
       y = "Frequency in 2026",
       title = "Word Frequency Comparison") +
  theme_minimal()

ggplot(word_comparison, aes(y = reorder(word, freq_26), color = sentiment)) +
  geom_segment(aes(x = freq_21,xend = freq_26,yend = word),color = "gray70") +
  geom_point(aes(x = freq_21),  size = 3) +
  geom_point(aes(x = freq_26), shape = 17,  size = 3) +
  scale_color_manual(values=c("#fc8d59","#99d594"))+
  labs(x = "Frequency",y = "",title = "Change in Word Frequencies") +
  theme_minimal()

word_comparison_long <- word_comparison %>%
  pivot_longer(cols = c(freq_21, freq_26),
               names_to = "year",
               values_to = "freq") %>% 
  mutate(year = recode(year,freq_21 = "2021",freq_26 = "2026"))

ggplot(word_comparison_long,aes(x = freq,y = reorder(word, freq),color = sentiment,shape = year)) +
  geom_segment(data = word_comparison,aes(x = freq_21,xend = freq_26,y = word,yend = word),inherit.aes = FALSE,color = "gray70") +
  geom_point(size = 3) +
  scale_color_manual(values = c(negative = "#fc8d59",positive = "#99d594")) +
  scale_shape_manual(values = c("2021" = 16,"2026" = 17)) +
  labs(x = "Frequency",y = "",color = "Sentiment",shape = "Year",title = "Change in Word Frequencies") +
  theme_minimal()



###########################################################################
######                      Fisherman Feedback Analysis              ######
######                         Gag Grouper Comparison                ######
######                         Geographic Differences                ######
###########################################################################
###########################################################################
# Initialize --------------------------------------------------------------
###########################################################################

### Workflow
#start here

# 1) library Load libraries -----------------------------------------------


library(devtools)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(readr)
library(leaflet)
library(leaflegend)
library(htmlwidgets)
library(htmltools)
library(webshot)
#library(leaflet.esri) #Package no longer exists
library(leaflet.extras)
library(doBy)
library(leafem)
library(leaflet.minicharts)
library(mapview) 
library(sf)
library(sp)
library(doBy)
library(readxl)



# 2) import  --------------------------------------------------------------


df <- ffdat
### 2.1 duplicates check for duplicates here!!!

df <- df[!duplicated(df), ]
colnames(df) <- make.names(colnames(df))
### create an id variable
df$ID <- 1:nrow(df)
df$'General.Location.of.Observation' <- as.character(df$'General.Location.of.Observation')

# # Create a vector of the original type variable
# type <- c(NA, NA, NA, "neutral", NA, "negative", NA, NA, "negative", "positive", NA, "positive", NA)
# 

#df$type <- recode(df$type, positive = "1", nuetral = "0", negative="-1")

###########################################################################
# 3) obsPerArea Determine number of observations by area. -----------------
###########################################################################
# 2021 --------------------------------------------------------------------



### 3.1)  get number of observations per grid

########################################################################################
### Create number of observations per area to merge with shrimp
### Check about strings as factors = false
df21 <- df[df$Year==2021,]
## df
loc <- data.frame(x=df21$'General.Location.of.Observation', stringsAsFactors = FALSE)

loc$y <- trimws(loc$x)

### object to hold results
out <- c()

for(i in 1:length(loc$y)){
  vars <- str_split(loc$y[i], ";") ## use for semicolon separator
  vars2 <- unlist(vars[1])
  vars3 <- trimws(vars2)
  vars4 <- data.frame(vars3)
  vars4$id <- i
  colnames(vars4) <- c("Location", "id")
  out <- rbind(out, vars4)
}

out$val <- 1
out2_21 <- dcast(out, id ~ Location, value.var ="val")
out2_21[is.na(out2_21)] <- 0
#save(out2, file="out2.RData") ## for use with the  sentiment maps

out3 <- data.frame(colSums(out2_21))
out3$Location <- rownames(out3)
rownames(out3) <- NULL

##  Subset and sort the data
out3 <- subset(out3, Location!="id")
out3 <- subset(out3, Location!="Other")
out4 <- arrange(out3, Location)


nums <- data.frame(parse_number(out4$Location))
out4$nums <- parse_number(out4$Location)
out5 <- subset(out4, out4$nums !="na")
out5 <- arrange(out5, out5[,3])
out6 <- data.frame(out5[,3], out5[,1])
colnames(out6) <- c("ID", "Responses")
out7_21 <- arrange(out6, ID)
out7_21$Year <- 2021
################### warning #################################################
### summarize responses by ID, this added for gag, seems right but double check
library(doBy)
#outx_21 <- summaryBy( Responses ~ ID,  data=out7_21, FUN=sum, keep.names=TRUE)
#out7 <- outx


# 2026 --------------------------------------------------------------------

### 3.1)  get number of observations per grid

########################################################################################
### Create number of observations per area to merge with shrimp
### Check about strings as factors = false
df26 <- df[df$Year==2026,]
## df
loc <- data.frame(x=df26$'General.Location.of.Observation', stringsAsFactors = FALSE)

loc$y <- trimws(loc$x)

### object to hold results
out <- c()

for(i in 1:length(loc$y)){
  vars <- str_split(loc$y[i], ";") ## use for semicolon separator
  vars2 <- unlist(vars[1])
  vars3 <- trimws(vars2)
  vars4 <- data.frame(vars3)
  vars4$id <- i
  colnames(vars4) <- c("Location", "id")
  out <- rbind(out, vars4)
}

out$val <- 1
out2_26 <- dcast(out, id ~ Location, value.var ="val")
out2_26[is.na(out2_26)] <- 0
#save(out2, file="out2.RData") ## for use with the  sentiment maps

out3 <- data.frame(colSums(out2_26))
out3$Location <- rownames(out3)
rownames(out3) <- NULL

##  Subset and sort the data
out3 <- subset(out3, Location!="id")
out3 <- subset(out3, Location!="Other")
out4 <- arrange(out3, Location)


nums <- data.frame(parse_number(out4$Location))
out4$nums <- parse_number(out4$Location)
out5 <- subset(out4, out4$nums !="na")
out5 <- arrange(out5, out5[,3])
out6 <- data.frame(out5[,3], out5[,1])
colnames(out6) <- c("ID", "Responses")
out7_26 <- arrange(out6, ID)
out7_26$Year <- 2026
################### warning #################################################
### summarize responses by ID, this added for gag, seems right but double check
library(doBy)
#outx_21 <- summaryBy( Responses ~ ID,  data=out7_21, FUN=sum, keep.names=TRUE)
#out7 <- outx



# Combine -----------------------------------------------------------------

out7 <- out7_26 %>%
  complete(ID = 1:21,fill = list(Responses = 0, Year = 2026)) %>% 
  mutate(n_2026 = Responses) %>% 
  left_join(out7_21 %>% select(ID,n_2021 = Responses),by = "ID") %>% 
  select(-Year,-Responses)
out7$n_diff <- out7$n_2026-out7$n_2021
###########################################################################
# 3.2: georeference  ------------------------------------------------------
###########################################################################

############################################ Import the shrimp grids ########################
### This section will produce a map of the number of responses by zone:

### 3.2: grid Import shrimp grid for georeferencing

# gridShrimp <- readOGR(dsn="shp/ShrimpStatZones_1thru21_GCSWGS84.shp", 
#                       layer="ShrimpStatZones_1thru21_GCSWGS84")

#### use sf to import then conver to sp.  need to check in R 4.3+
gridShrimp_sf <- st_read("shp/ShrimpStatZones_1thru21_GCSWGS84.shp")
gridShrimp <- as(gridShrimp_sf, "Spatial")

## Extract datatable and add zone numbers
gridShrimp@data$ROW <- 0:29 

## Convert data slot to a data frame
gridShrimpdf <- gridShrimp@data
## remove some unnnecessary columns, only the first 3 are relevant
gridShrimpdf <- gridShrimpdf[,c(1:3, 9)] 


### this section in updated from previous version to accomodate situations where some grids don't have
### any responses associated with them.  In this case, the merge will result in a NA for that observation
### which should be replace by 0.  
gridShrimpdf2 <- merge(gridShrimpdf, out7, by="ID", all=TRUE)
gridShrimpdf2 <- gridShrimpdf2[1:30,] #added 8 29 2024

### replace NA with 0.  This is correct in this use case.
#gridShrimpdf2$Responses[is.na(gridShrimpdf2$Responses)] <- 0
gridShrimpdf3 <- arrange(gridShrimpdf2, ROW)
gridShrimp@data <- gridShrimpdf3



### 4) obsMap create map

### 4.1 centroidLabel

### gridShrimp.RData from CobiaSF (April 2020) https://github.com/jfroeschke/CobiaSF
##load("RData/gridShrimp.RData")
pieGrid <- gridShrimp
centroids <- data.frame(coordinates(pieGrid))
centroids$ID <- pieGrid@data$ID
centroids$ROW <- pieGrid@data$ROW
colnames(centroids) <- c("x", "y", "ID", "ROW")
centroidsPie <- centroids
centroidsPieMerge <- merge(centroidsPie, gridShrimpdf3, all=TRUE)
centroidsPieMerge2 <- summaryBy(x +y ~  ID, data=centroidsPieMerge, 
                                FUN=mean, keep.names=TRUE, id="ROW")
centroidsPieMerge3 <- arrange(centroidsPieMerge2 , ROW)
centroidsPie2 <- summaryBy(x +y ~  ID, data=centroidsPie, FUN=mean, keep.names=TRUE)
centroidsPie3 <- centroidsPie2
centroidsPie3$id <- as.numeric(as.character(centroidsPie2$ID))
centroidsPie4 <- arrange(centroidsPie3, (id))
centroidsPie3 <- centroidsPie4
centroidsPie3[5,2] <- -83.64990
centroidsPie3[5,3] <- 27.54724
centroidsPie3[6,2] <- - 83.89160
centroidsPie3[6,3] <- 28.55558
centroidsPie3[9,2] <- - 86.48438
centroidsPie3[9,3] <- 29.85970
centroidsPie3[10,2] <- -87.40723
centroidsPie3[10,3] <- 29.89781
centroidsPie3[12,2] <- -89.38477
centroidsPie3[12,3] <- 29.91685
centroidsPie3[14,2] <- -90.54932
centroidsPie3[14,3] <- 28.57487

centroidsPie4 <- merge(centroidsPie3, gridShrimpdf3, all=TRUE)
centroidsPie3 <- centroidsPie4


#### nLabels

nLabels <- centroidsPieMerge3

#nLabels$n <- rowSums(nLabels[,4:6])
nLabels$n <- nLabels$n_diff
nLabels[1,2] <- -81.25560
nLabels[1,3] <- 23.80347
nLabels[2,2] <- -82.5
nLabels[2,3] <- 23.80347
nLabels[3,2] <- -83.25
nLabels[3,3] <- 25.5
nLabels[4,2] <- -83.50
nLabels[5,2] <- -84.4
nLabels[5,3] <- 27.75
nLabels[6,2] <- -84.4
nLabels[6,3] <- 28.5
nLabels[7,2] <- -84.4
nLabels[7,3] <- 29.25
nLabels[8,2] <- -85.3
nLabels[8,3] <- 28.7
nLabels[9,2] <- -86.4
nLabels[9,3] <- 29.25
nLabels[10,2] <- -87.85
nLabels[10,3] <- 29.25
nLabels[11,2] <- -88.85
nLabels[11,3] <- 29.25
nLabels[12,3] <- 30
nLabels[13,2] <- -89.8
nLabels[13,3] <- 28.3
nLabels[14,2] <- -90.8
nLabels[14,3] <- 27.9
nLabels[15,2] <- -91.8
nLabels[15,3] <- 27.9
nLabels[16,2] <- -92.8
nLabels[16,3] <- 27.9
nLabels[17,2] <- -93.8
nLabels[17,3] <- 27.9
nLabels[18,2] <- -94.75
nLabels[18,3] <- 27.9
nLabels[20,2] <- -96.3
nLabels[20,3] <- 27.35
nLabels[21,2] <- -96.3

### Map
# library(htmlwidgets)
# library(webshot)
#webshot::install_phantomjs()

# Plot Response Changes ---------------------------------------------------


# Create a color palette for the map:
mypalette <- colorNumeric( palette="viridis", domain=gridShrimp@data$n_diff, na.color="transparent", reverse=TRUE)

m <- leaflet(gridShrimp, padding=25) %>% ## added padding for whitespace
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  # addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = TRUE, group="ShadedRelief") %>% 
  #addTiles()  %>%
  setView( lat=27.5, lng=-89.5 , zoom=6) %>%
  addPolygons( fillColor = ~mypalette(n_diff),  fillOpacity = 0.5,
               stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  addLegend("bottomright", pal = mypalette, values = ~n_diff,
            title = "Difference in number of responses",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 0.5, position=("bottomleft")
  ) %>% 
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))  %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export

m

###This adjusts the size of the legend
m <- leaflet(gridShrimp, padding=25) %>% ## added padding for whitespace
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  # addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = TRUE, group="ShadedRelief") %>% 
  #addTiles()  %>%
  setView( lat=27.5, lng=-89.5 , zoom=6) %>%
  addPolygons( fillColor = ~mypalette(n_diff),  fillOpacity = 0.5,
               stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  addLegendNumeric(pal = mypalette, values = ~n_diff,
                   width = 40, height = 150,
                   title = "Difference in number of responses",
                   labelStyle = "font-size: 18px; font-weight: bold;",
                   #labFormat = labelFormat(prefix = "$"),
                   bins = 7,
                   fillOpacity = 0.5, 
                   position=("bottomleft")
  ) %>% 
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))  %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export



m

## save html to png
saveWidget(m, "Comparison/Map Plots/ChangeResponsePlot.html", selfcontained = FALSE)
# webshot("ResponsePlot.html", file = "ResponsePlot.png",
#         cliprect = "viewport") Doesn't work correctly with padding, use below
webshot("Comparison/Map Plots/ChangeResponsePlot.html", file = "Comparison/Map Plots/ChangeResponsePlot.png")
###########################################################################
# 5: Sentiment  -----------------------------------------------------------
###########################################################################

# 2021 --------------------------------------------------------------------

#### This section will produce two maps: 1) a map of the sentiment analysis using the Bing lexicon library
####  and 2) a manual analysis 

### Thesed data were combined with the scamp (df) dataframe as prepoplulated earlier in this script.
### Section 5.1: Load required libraries

### Sectio 5.2: read in data

tmp21 <- df21


### Sectio 5.3: map the manual sentiement analysis
## Extract two columns of interest, rename, and remove blanks and code with 'n'
### need to format with an ID column 1:nrows in future, should create a script e.g., download data to get data, add appropriate
### columns, i.e., ID, sentiment, manual Sentiment, Abundance and provide to staff as necessary.  

tmp2Out2_21 <- cbind(tmp21,out2_21)
NAMES <- colnames(tmp2Out2_21)
NAMES2 <- make.names(NAMES)
names(tmp2Out2_21) <- NAMES2
tmp2Out2_21$Other <- NULL
colnames(tmp2Out2_21)[9] <- "Abundance"
colnames(tmp2Out2_21)[8] <- "Sentiment"

### Calculate sentiment for each grid
Xout_list <- list()

for (i in 1:21) {
  var_name <- paste0("X", i)
  if (var_name %in% names(tmp2Out2_21)) {
    tmpWide <- data.frame(Area = -9999, Negative = -9999,Neutral = -9999,Positive = -9999)
    X <- subset(tmp2Out2_21, tmp2Out2_21[[var_name]] == 1)
    X <- select(X, Abundance, Sentiment, ID, all_of(var_name))
    Xwide <- dcast(X, var_name ~ Sentiment, fun.aggregate = length, value.var = var_name)
    names(Xwide)[names(Xwide) == "-1"] <- "Negative"
    names(Xwide)[names(Xwide) == "0"]  <- "Neutral"
    names(Xwide)[names(Xwide) == "1"]  <- "Positive"
    colnames(Xwide)[1] <- "Area"
    Xwide <- merge(tmpWide, Xwide, all = TRUE)
    Xwide <- subset(Xwide, Area != -9999)
    Xwide$Area <- i
    Xout_list[[i]] <- Xwide
  } else {
    print(paste(var_name, "does not exist."))
  }
}
#### automate the rbind
Xout_21 <- do.call(rbind, Xout_list)


### Replace na's with 0.  In this case Na's are actual zeros
Xout_21[is.na(Xout_21)] <- 0

colnames(Xout_21) <- c("ID", "Negative", "Neutral", "Positive")
Xout_21$Responses <- Xout_21$Negative+Xout_21$Neutral+Xout_21$Positive
# 2026 --------------------------------------------------------------------
#### This section will produce two maps: 1) a map of the sentiment analysis using the Bing lexicon library
####  and 2) a manual analysis 

### Thesed data were combined with the scamp (df) dataframe as prepoplulated earlier in this script.
### Section 5.1: Load required libraries

### Sectio 5.2: read in data

tmp26 <- df26


### Sectio 5.3: map the manual sentiement analysis
## Extract two columns of interest, rename, and remove blanks and code with 'n'
### need to format with an ID column 1:nrows in future, should create a script e.g., download data to get data, add appropriate
### columns, i.e., ID, sentiment, manual Sentiment, Abundance and provide to staff as necessary.  

tmp2Out2_26 <- cbind(tmp26,out2_26)
NAMES <- colnames(tmp2Out2_26)
NAMES2 <- make.names(NAMES)
names(tmp2Out2_26) <- NAMES2
tmp2Out2_26$Other <- NULL
colnames(tmp2Out2_26)[9] <- "Abundance"
colnames(tmp2Out2_26)[8] <- "Sentiment"

### Calculate sentiment for each grid
Xout_list <- list()

for (i in 1:21) {
  var_name <- paste0("X", i)
  if (var_name %in% names(tmp2Out2_26)) {
    tmpWide <- data.frame(Area = -9999, Negative = -9999,Neutral = -9999,Positive = -9999)
    X <- subset(tmp2Out2_26, tmp2Out2_26[[var_name]] == 1)
    X <- select(X, Abundance, Sentiment, ID, all_of(var_name))
    Xwide <- dcast(X, var_name ~ Sentiment, fun.aggregate = length, value.var = var_name)
    names(Xwide)[names(Xwide) == "-1"] <- "Negative"
    names(Xwide)[names(Xwide) == "0"]  <- "Neutral"
    names(Xwide)[names(Xwide) == "1"]  <- "Positive"
    colnames(Xwide)[1] <- "Area"
    Xwide <- merge(tmpWide, Xwide, all = TRUE)
    Xwide <- subset(Xwide, Area != -9999)
    Xwide$Area <- i
    Xout_list[[i]] <- Xwide
  } else {
    print(paste(var_name, "does not exist."))
  }
}
#### automate the rbind
Xout_26 <- do.call(rbind, Xout_list)


### Replace na's with 0.  In this case Na's are actual zeros
Xout_26[is.na(Xout_26)] <- 0

colnames(Xout_26) <- c("ID", "Negative", "Neutral", "Positive")
Xout_26$Responses <- Xout_26$Negative+Xout_26$Neutral+Xout_26$Positive

# Get averages ------------------------------------------------------------
Xout_21avg <- Xout_21 %>% 
  mutate(avg_sentiment =(-Negative + Positive) /(Negative + Neutral + Positive))
### Replace na's with 0.  In this case Na's are actual zeros
Xout_21avg[is.na(Xout_21avg)] <- 0
  
Xout_26avg <- Xout_26 %>%
  complete(ID = 1:21,fill = list(Negative = 0, Neutral = 0,Positive=0)) %>% 
  mutate(avg_sentiment =(-Negative + Positive) /(Negative + Neutral + Positive))
### Replace na's with 0.  In this case Na's are actual zeros
Xout_26avg[is.na(Xout_26avg)] <- 0

Xout <- data.frame(ID = Xout_21avg$ID, avg_21 = Xout_21avg$avg_sentiment,avg_26 = Xout_26avg$avg_sentiment,
                   Sent_change = ifelse(Xout_21avg$Responses!=0&Xout_26avg$Responses!=0, 
                                        ifelse(Xout_26avg$avg_sentiment>Xout_21avg$avg_sentiment,"Improved",
                                        ifelse(Xout_26avg$avg_sentiment<Xout_21avg$avg_sentiment,"Worsened","No change")),NA),
                   n = paste0(Xout_21avg$Responses, " \u2192 ", Xout_26avg$Responses),
                   n_2021 = Xout_21avg$Responses,
                   n_2026 = Xout_26avg$Responses,
                   Sent_change_n = ifelse(Xout_21avg$Responses!=0&Xout_26avg$Responses!=0,
                                          round(Xout_26avg$avg_sentiment-Xout_21avg$avg_sentiment,3),NA))
Xout$Sent_change<-as.factor(Xout$Sent_change)
#Xout$Sent_change[17]<-NA

# Plot sentiment change ---------------------------------------------------
#### use sf to import then conver to sp.  need to check in R 4.3+
gridShrimp_sf <- st_read("shp/ShrimpStatZones_1thru21_GCSWGS84.shp")
gridShrimp <- as(gridShrimp_sf, "Spatial")

## Extract datatable and add zone numbers
gridShrimp@data$ROW <- 0:29 

## Convert data slot to a data frame
gridShrimpdf <- gridShrimp@data
## remove some unnnecessary columns, only the first 3 are relevant
gridShrimpdf <- gridShrimpdf[,c(1:3, 9)] 


### this section in updated from previous version to accomodate situations where some grids don't have
### any responses associated with them.  In this case, the merge will result in a NA for that observation
### which should be replace by 0.  
gridShrimpdf2 <- merge(gridShrimpdf, Xout, by="ID", all=TRUE)
gridShrimpdf2 <- gridShrimpdf2[1:30,] #added 8 29 2024

### replace NA with 0.  This is correct in this use case.
#gridShrimpdf2$Responses[is.na(gridShrimpdf2$Responses)] <- 0
gridShrimpdf3 <- arrange(gridShrimpdf2, ROW)
gridShrimp@data <- gridShrimpdf3



### 4) obsMap create map

### 4.1 centroidLabel

### gridShrimp.RData from CobiaSF (April 2020) https://github.com/jfroeschke/CobiaSF
##load("RData/gridShrimp.RData")
pieGrid <- gridShrimp
centroids <- data.frame(coordinates(pieGrid))
centroids$ID <- pieGrid@data$ID
centroids$ROW <- pieGrid@data$ROW
colnames(centroids) <- c("x", "y", "ID", "ROW")
centroidsPie <- centroids
centroidsPieMerge <- merge(centroidsPie, gridShrimpdf3, all=TRUE)
centroidsPieMerge2 <- summaryBy(x +y ~  ID, data=centroidsPieMerge, 
                                FUN=mean, keep.names=TRUE, id="ROW")
centroidsPieMerge3 <- arrange(centroidsPieMerge2 , ROW)
centroidsPie2 <- summaryBy(x +y ~  ID, data=centroidsPie, FUN=mean, keep.names=TRUE)
centroidsPie3 <- centroidsPie2
centroidsPie3$id <- as.numeric(as.character(centroidsPie2$ID))
centroidsPie4 <- arrange(centroidsPie3, (id))
centroidsPie3 <- centroidsPie4
# centroidsPie3[5,2] <- -83.64990
# centroidsPie3[5,3] <- 27.54724
# centroidsPie3[6,2] <- - 83.89160
# centroidsPie3[6,3] <- 28.55558
# centroidsPie3[9,2] <- - 86.48438
# centroidsPie3[9,3] <- 29.85970
# centroidsPie3[10,2] <- -87.40723
# centroidsPie3[10,3] <- 29.89781
# centroidsPie3[12,2] <- -89.38477
# centroidsPie3[12,3] <- 29.91685
# centroidsPie3[14,2] <- -90.54932
# centroidsPie3[14,3] <- 28.57487

centroidsPie3[1,2] <- -81
centroidsPie3[2,2] <- -82.5
centroidsPie3[3,2] <- -82
centroidsPie3[4,2] <- -82.5
centroidsPie3[5,2] <- -83
centroidsPie3[6,2] <- -83
centroidsPie3[7,2] <- -83.5
centroidsPie3[8,2] <- -84.9
centroidsPie3[8,3] <- 29.25
centroidsPie3[9,2] <- - 85.9
centroidsPie3[9,3] <- 30.1
centroidsPie3[10,2] <- -86.9
centroidsPie3[10,3] <- 29.75
centroidsPie3[11,2] <- -88
centroidsPie3[11,3] <- 29.25
centroidsPie3[12,2] <- -89
centroidsPie3[12,3] <- 29.87
centroidsPie3[13,2] <- -89
centroidsPie3[13,3] <- 28.655
centroidsPie3[14,2] <- -91
centroidsPie3[14,3] <- 29
centroidsPie3[15,2] <- -92
centroidsPie3[15,3] <- 28.25
centroidsPie3[16,2] <- -93
centroidsPie3[16,3] <- 29.25
centroidsPie3[17,2] <- -94
centroidsPie3[17,3] <- 28.25
centroidsPie3[18,2] <- -95
centroidsPie3[19,2] <- -96
centroidsPie3[20,3] <- 27.75
centroidsPie2 <- centroidsPie3

nLabels <- centroidsPie2

nLabels$n <- Xout$n
nLabels$n_2021 <- Xout$n_2021
nLabels$n_2026 <- Xout$n_2026
# centroidsPie4 <- merge(centroidsPie3, gridShrimpdf3, all=TRUE)
# centroidsPie3 <- centroidsPie4


#### nLabels

# nLabels <- centroidsPieMerge3

#nLabels$n <- rowSums(nLabels[,4:6])
#nLabels$n <- nLabels$n_diff
# nLabels[1,2] <- -81.25560
# nLabels[1,3] <- 23.80347
# nLabels[2,2] <- -82.5
# nLabels[2,3] <- 23.80347
# nLabels[3,2] <- -83.25
# nLabels[3,3] <- 25.5
# nLabels[4,2] <- -83.50
# nLabels[5,2] <- -84.4
# nLabels[5,3] <- 27.75
# nLabels[6,2] <- -84.4
# nLabels[6,3] <- 28.5
# nLabels[7,2] <- -84.4
# nLabels[7,3] <- 29.25
# nLabels[8,2] <- -85.3
# nLabels[8,3] <- 28.7
# nLabels[9,2] <- -86.4
# nLabels[9,3] <- 29.25
# nLabels[10,2] <- -87.85
# nLabels[10,3] <- 29.25
# nLabels[11,2] <- -88.85
# nLabels[11,3] <- 29.25
# nLabels[12,3] <- 30
# nLabels[13,2] <- -89.8
# nLabels[13,3] <- 28.3
# nLabels[14,2] <- -90.8
# nLabels[14,3] <- 27.9
# nLabels[15,2] <- -91.8
# nLabels[15,3] <- 27.9
# nLabels[16,2] <- -92.8
# nLabels[16,3] <- 27.9
# nLabels[17,2] <- -93.8
# nLabels[17,3] <- 27.9
# nLabels[18,2] <- -94.75
# nLabels[18,3] <- 27.9
# nLabels[20,2] <- -96.3
# nLabels[20,3] <- 27.35
# nLabels[21,2] <- -96.3

### Map
# library(htmlwidgets)
# library(webshot)
#webshot::install_phantomjs()
sent_change_pallete <- colorFactor(palette=c("Improved" = "#92D050","No change" = "#FFFF00","Worsened" = "#ED7D31"), domain= c("Improved", "No change", "Worsened"), na.color="transparent")

# Old map -----------------------------------------------------------------

###This adjusts the size of the legend
m <- leaflet(gridShrimp, padding=25) %>% ## added padding for whitespace
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  # addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = TRUE, group="ShadedRelief") %>% 
  #addTiles()  %>%
  setView( lat=27.5, lng=-89.5 , zoom=6) %>%
  addPolygons( fillColor = ~sent_change_pallete(Sent_change),  fillOpacity = 0.5,
               stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  addLabelOnlyMarkers(
    lng = nLabels[,2] , lat = nLabels[,3] ,
    label = ~paste0(nLabels[,6], " \u2192 ", nLabels[,7]),
    #label = ~paste( nLabels[,5]),
    #label = ~paste( nLabels[,7]),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = list("font-size" = "14px","font-weight" = "bold"))) %>% 
  addLabelOnlyMarkers(
    lng = -93.5 , lat = 24.5 ,
    label = ~paste0("Number in each grid indicates sample size from 2021"," \u2192 ", "2026"),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  addLegendFactor(pal = sent_change_pallete, values = ~Sent_change[!is.na(Sent_change)],
                   width = 40, height = 30,
                   title = "Overall Sentiment Change",
                   labelStyle = "font-size: 18px; font-weight: bold;",
                   #labFormat = labelFormat(prefix = "$"),
                   fillOpacity = 0.5, 
                   position=("bottomleft"),
                   naLabel = NULL
  ) %>% 
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))  %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export



m

## save html to png
# saveWidget(m, "Comparison/Map Plots/ChangeOvrSentPlot.html", selfcontained = FALSE)
# # webshot("ResponsePlot.html", file = "ResponsePlot.png",
# #         cliprect = "viewport") Doesn't work correctly with padding, use below
# webshot("Comparison/Map Plots/ChangeOvrSentPlot.html", file = "Comparison/Map Plots/ChangeOvrSentPlot.png")


# New map -----------------------------------------------------------------


m <- leaflet(gridShrimp, padding=25) %>% ## added padding for whitespace
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  # addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = TRUE, group="ShadedRelief") %>% 
  #addTiles()  %>%
  setView( lat=27.5, lng=-89.5 , zoom=6) %>%
  addPolygons( fillColor = ~sent_change_pallete(Sent_change),  fillOpacity = 0.5,
               stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  addLabelOnlyMarkers(
    lng = nLabels[,2],lat = nLabels[,3],
    label = lapply(seq_len(nrow(nLabels)),
      function(i) HTML(paste0(nLabels[i,6]," \u2192 ","<span style='color:blue;'>",nLabels[i,7],"</span>"))),
    labelOptions = labelOptions(noHide = TRUE,textOnly = TRUE,style = list("font-size" = "14px","font-weight" = "bold"))) %>% 
  addLabelOnlyMarkers(
    lng = -93.5 , lat = 24.5 ,
    label = ~paste0("Number in each grid indicates sample size from 2021"," \u2192 ", "2026"),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  addLegendFactor(pal = sent_change_pallete, values = ~Sent_change[!is.na(Sent_change)],
                  width = 40, height = 30,
                  title = HTML(paste0("Overall Sentiment Change<br>","<span style='color:black;'>2021</span>"," \u2192 ","<span style='color:blue;'>2026</span>")),
                  labelStyle = "font-size: 18px; font-weight: bold;",
                  #labFormat = labelFormat(prefix = "$"),
                  fillOpacity = 0.5, 
                  position=("bottomleft"),
                  naLabel = NULL
  ) %>% 
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))  %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export



m

# ## save html to png
# saveWidget(m, "Comparison/Map Plots/ChangeOvrSentPlot.html", selfcontained = FALSE)
# # webshot("ResponsePlot.html", file = "ResponsePlot.png",
# #         cliprect = "viewport") Doesn't work correctly with padding, use below
# webshot("Comparison/Map Plots/ChangeOvrSentPlot.html", file = "Comparison/Map Plots/ChangeOvrSentPlot.png")


# Test color scale map ----------------------------------------------------
#sent_change_pallete_cont <- colorNumeric( palette = c("#D55E00", "#F0E442", "#009E73"), domain=gridShrimp@data$Sent_change_n, na.color="transparent", reverse=FALSE)
sent_change_pallete_cont <- colorNumeric( palette = c("#D55E00", "#FFFF00", "#009E73"), domain=gridShrimp@data$Sent_change_n, na.color="transparent", reverse=FALSE)
#sent_change_pallete_cont <- colorNumeric( palette = c("#ED7D31", "#FFFF00", "#92D050"), domain=gridShrimp@data$Sent_change_n, na.color="transparent", reverse=FALSE)
#sent_change_pallete_cont <- colorNumeric( palette = c("#D55E00", "#FFFF00", "#009E73"), domain=c(-2,2), na.color="transparent", reverse=FALSE)


m <- leaflet(gridShrimp, padding=25) %>% ## added padding for whitespace
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  # addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = TRUE, group="ShadedRelief") %>% 
  #addTiles()  %>%
  setView( lat=27.5, lng=-89.5 , zoom=6) %>%
  addPolygons( fillColor = ~sent_change_pallete_cont(Sent_change_n),  fillOpacity = 0.5,
               stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  addLabelOnlyMarkers(
    lng = nLabels[,2],lat = nLabels[,3],
    label = lapply(seq_len(nrow(nLabels)),
                   function(i) HTML(paste0(nLabels[i,6]," \u2192 ","<span style='color:blue;'>",nLabels[i,7],"</span>"))),
    labelOptions = labelOptions(noHide = TRUE,textOnly = TRUE,style = list("font-size" = "14px","font-weight" = "bold"))) %>% 
  addLabelOnlyMarkers(
    lng = -93.5 , lat = 24.5 ,
    label = ~paste0("Number in each grid indicates sample size from 2021"," \u2192 ", "2026"),
    #label = ~paste("n = ", nLabels[,8]),,
    labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = "14px")) %>% 
  addLegendNumeric(pal = sent_change_pallete_cont, values = ~Sent_change_n[!is.na(Sent_change_n)],
                  width = 40, height = 100,
                  title = HTML(paste0("Overall Sentiment Change<br>","<span style='color:black;'>2021</span>"," \u2192 ","<span style='color:blue;'>2026</span>")),
                  labelStyle = "font-size: 18px; font-weight: bold;",
                  #labFormat = labelFormat(prefix = "$"),
                  fillOpacity = 0.5,
                  bins = 3,
                  position=("bottomleft"),
                  labels = c("Worsened","  No change", "Improved"),
                  naLabel = NULL
  ) %>%
  
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))  %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export



m
## save html to png
saveWidget(m, "Comparison/Map Plots/ChangeOvrSentPlot.html", selfcontained = FALSE)
# webshot("ResponsePlot.html", file = "ResponsePlot.png",
#         cliprect = "viewport") Doesn't work correctly with padding, use below
webshot("Comparison/Map Plots/ChangeOvrSentPlot.html", file = "Comparison/Map Plots/ChangeOvrSentPlot.png")
###########################################################################
# Manual Sentiment related to abundance -----------------------------------
###########################################################################

# 2021 --------------------------------------------------------------------
### subset to abundance only
### this should have fewer observations than previous dataset
tmp2Out2_21 <- subset(tmp2Out2_21, Abundance=="y")

## fix inconsistent name in this spreadsheet
colnames(tmp2Out2_21)[colnames(tmp2Out2_21) == "Final..Stock.Condition"] <- "Final.Stock.Condition.Sentiment"


### create a template dataframe
Xout_list <- list()

for (i in 1:21) {
  var_name <- paste0("X", i)
  if (var_name %in% names(tmp2Out2_21)& sum(tmp2Out2_21[[var_name]] > 0)) {
    tmpWide <- data.frame(Area = -9999, Negative = -9999,Neutral = -9999,Positive = -9999)
    X <- subset(tmp2Out2_21, tmp2Out2_21[[var_name]] == 1)
    X <- select(X, Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, all_of(var_name))
    X$Final.Stock.Condition.Sentiment <- factor(X$Final.Stock.Condition.Sentiment,levels = c(-1, 0, 1))
    Xwide <- dcast(X, var_name ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = var_name)
    names(Xwide)[names(Xwide) == "-1"] <- "Negative"
    names(Xwide)[names(Xwide) == "0"]  <- "Neutral"
    names(Xwide)[names(Xwide) == "1"]  <- "Positive"
    colnames(Xwide)[1] <- "Area"
    Xwide <- merge(tmpWide, Xwide, all = TRUE)
    Xwide <- subset(Xwide, Area != -9999)
    Xwide$Area <- i
    Xout_list[[i]] <- Xwide
  } else {
    print(paste(var_name, "does not exist."))
  }
}
#### automate the rbind
Xout_21 <- do.call(rbind, Xout_list)
Xout_21 <- Xout_21[,c("Area", "Negative", "Neutral", "Positive")]

### Replace na's with 0.  In this case Na's are actual zeros
Xout_21[is.na(Xout_21)] <- 0


colnames(Xout_21) <- c("ID", "Negative", "Neutral", "Positive")
Xout_21$Responses <- Xout_21$Negative+Xout_21$Neutral+Xout_21$Positive
Xout_21$Neg_Prop <- Xout_21$Negative/(Xout_21$Negative+Xout_21$Neutral+Xout_21$Positive)
Xout_21$Neut_Prop <- Xout_21$Neutral/(Xout_21$Negative+Xout_21$Neutral+Xout_21$Positive)
Xout_21$Pos_Prop <- Xout_21$Positive/(Xout_21$Negative+Xout_21$Neutral+Xout_21$Positive)

# 2026 --------------------------------------------------------------------
### subset to abundance only
### this should have fewer observations than previous dataset
tmp2Out2_26 <- subset(tmp2Out2_26, Abundance=="y")

## fix inconsistent name in this spreadsheet
colnames(tmp2Out2_26)[colnames(tmp2Out2_26) == "Final..Stock.Condition"] <- "Final.Stock.Condition.Sentiment"


### create a template dataframe
Xout_list <- list()

for (i in 1:21) {
  var_name <- paste0("X", i)
  if (var_name %in% names(tmp2Out2_26)& sum(tmp2Out2_26[[var_name]] > 0)) {
    tmpWide <- data.frame(Area = -9999, Negative = -9999,Neutral = -9999,Positive = -9999)
    X <- subset(tmp2Out2_26, tmp2Out2_26[[var_name]] == 1)
    X <- select(X, Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, all_of(var_name))
    X$Final.Stock.Condition.Sentiment <- factor(X$Final.Stock.Condition.Sentiment,levels = c(-1, 0, 1))
    Xwide <- dcast(X, var_name ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = var_name)
    names(Xwide)[names(Xwide) == "-1"] <- "Negative"
    names(Xwide)[names(Xwide) == "0"]  <- "Neutral"
    names(Xwide)[names(Xwide) == "1"]  <- "Positive"
    colnames(Xwide)[1] <- "Area"
    Xwide <- merge(tmpWide, Xwide, all = TRUE)
    Xwide <- subset(Xwide, Area != -9999)
    Xwide$Area <- i
    Xout_list[[i]] <- Xwide
  } else {
    print(paste(var_name, "does not exist."))
  }
}
#### automate the rbind
Xout_26 <- do.call(rbind, Xout_list)
Xout_26 <- Xout_26[,c("Area", "Negative", "Neutral", "Positive")]

### Replace na's with 0.  In this case Na's are actual zeros
Xout_26[is.na(Xout_26)] <- 0


colnames(Xout_26) <- c("ID", "Negative", "Neutral", "Positive")
Xout_26$Responses <- Xout_26$Negative+Xout_26$Neutral+Xout_26$Positive
Xout_26$Neg_Prop <- Xout_26$Negative/(Xout_26$Negative+Xout_26$Neutral+Xout_26$Positive)
Xout_26$Neut_Prop <- Xout_26$Neutral/(Xout_26$Negative+Xout_26$Neutral+Xout_26$Positive)
Xout_26$Pos_Prop <- Xout_26$Positive/(Xout_26$Negative+Xout_26$Neutral+Xout_26$Positive)

# Sentiment Change --------------------------------------------------------
Xout_21avg <- Xout_21 %>% 
  complete(ID = 1:21,fill = list(Negative = 0, Neutral = 0,Positive=0)) %>% 
  mutate(avg_sentiment =(-Negative + Positive) /(Negative + Neutral + Positive))
### Replace na's with 0.  In this case Na's are actual zeros
Xout_21avg[is.na(Xout_21avg)] <- 0

Xout_26avg <- Xout_26 %>%
  complete(ID = 1:21,fill = list(Negative = 0, Neutral = 0,Positive=0)) %>% 
  mutate(avg_sentiment =(-Negative + Positive) /(Negative + Neutral + Positive))
### Replace na's with 0.  In this case Na's are actual zeros
Xout_26avg[is.na(Xout_26avg)] <- 0

Xout <- data.frame(ID = Xout_21avg$ID, avg_21 = Xout_21avg$avg_sentiment,avg_26 = Xout_26avg$avg_sentiment,
                   Sent_change = ifelse(Xout_21avg$Responses!=0&Xout_26avg$Responses!=0, 
                                        ifelse(Xout_26avg$avg_sentiment>Xout_21avg$avg_sentiment,"Improved",
                                               ifelse(Xout_26avg$avg_sentiment<Xout_21avg$avg_sentiment,"Worsened","No change")),NA),
                   n = paste0(Xout_21avg$Responses, " \u2192 ", Xout_26avg$Responses),
                   n_2021 = Xout_21avg$Responses,
                   n_2026 = Xout_26avg$Responses,
                   Sent_change_n = ifelse(Xout_21avg$Responses!=0&Xout_26avg$Responses!=0,
                                          round(Xout_26avg$avg_sentiment-Xout_21avg$avg_sentiment,3),NA))
Xout$Sent_change<-as.factor(Xout$Sent_change)

# Plot --------------------------------------------------------------------


### Now merge with Centroids
#### use sf to import then conver to sp.  need to check in R 4.3+
gridShrimp_sf <- st_read("shp/ShrimpStatZones_1thru21_GCSWGS84.shp")
gridShrimp <- as(gridShrimp_sf, "Spatial")

## Extract datatable and add zone numbers
gridShrimp@data$ROW <- 0:29 

## Convert data slot to a data frame
gridShrimpdf <- gridShrimp@data
## remove some unnnecessary columns, only the first 3 are relevant
gridShrimpdf <- gridShrimpdf[,c(1:3, 9)] 


### this section in updated from previous version to accomodate situations where some grids don't have
### any responses associated with them.  In this case, the merge will result in a NA for that observation
### which should be replace by 0.  
gridShrimpdf2 <- merge(gridShrimpdf, Xout, by="ID", all=TRUE)
gridShrimpdf2 <- gridShrimpdf2[1:30,] #added 8 29 2024

### replace NA with 0.  This is correct in this use case.
#gridShrimpdf2$Responses[is.na(gridShrimpdf2$Responses)] <- 0
gridShrimpdf3 <- arrange(gridShrimpdf2, ROW)
gridShrimp@data <- gridShrimpdf3


pieGrid <- gridShrimp


centroids <- data.frame(coordinates(pieGrid))
centroids$ID <- pieGrid@data$ID
colnames(centroids) <- c("x", "y", "ID")
centroidsPie <- merge(centroids, Xout, by="ID")

#library(doBy)
centroidsPie2 <- summaryBy(x + y ~ ID, data=centroidsPie, FUN=mean, keep.names=TRUE)
centroidsPie3 <- centroidsPie2
centroidsPie3$id <- as.numeric(as.character(centroidsPie2$ID))
centroidsPie3 <- arrange(centroidsPie3, (id))

# #centroidsPie3[5,2] <- -83.64990
# centroidsPie3[5,3] <- 27.54724
# #centroidsPie3[6,2] <- - 83.89160
# centroidsPie3[6,3] <- 28.55558
# centroidsPie3[9,2] <- - 86.48438
# centroidsPie3[9,3] <- 29.85970
# centroidsPie3[10,2] <- -87.40723
# centroidsPie3[10,3] <- 29.89781
# centroidsPie3[12,3] <- 29.87

centroidsPie3[1,2] <- -81
centroidsPie3[2,2] <- -82.5
centroidsPie3[3,2] <- -82
centroidsPie3[4,2] <- -82.5
centroidsPie3[5,2] <- -83
centroidsPie3[6,2] <- -83
centroidsPie3[7,2] <- -83.5
centroidsPie3[8,2] <- -84.9
centroidsPie3[8,3] <- 29.25
centroidsPie3[9,2] <- - 85.9
centroidsPie3[9,3] <- 30.1
centroidsPie3[10,2] <- -86.9
centroidsPie3[10,3] <- 29.75
centroidsPie3[11,2] <- -88
centroidsPie3[11,3] <- 29.25
centroidsPie3[12,2] <- -89
centroidsPie3[12,3] <- 29.87
centroidsPie3[13,2] <- -89
centroidsPie3[13,3] <- 28.655
centroidsPie3[14,2] <- -91
centroidsPie3[14,3] <- 29
centroidsPie3[15,2] <- -92
centroidsPie3[15,3] <- 28.25
centroidsPie3[16,2] <- -93
centroidsPie3[16,3] <- 29.25
centroidsPie3[17,2] <- -94
centroidsPie3[17,3] <- 28.25
centroidsPie3[18,2] <- -95
centroidsPie3[19,2] <- -96
centroidsPie3[20,3] <- 27.75
centroidsPie2 <- centroidsPie3

nLabels <- centroidsPie2

nLabels$n <- Xout$n
nLabels$n_2021 <- Xout$n_2021
nLabels$n_2026 <- Xout$n_2026
#write.csv(nLabels, "nlabels.csv", row.names=FALSE)
###This adjusts the size of the legend

# Old map -----------------------------------------------------------------

m <- leaflet(gridShrimp, padding=25) %>% ## added padding for whitespace
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  # addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = TRUE, group="ShadedRelief") %>% 
  #addTiles()  %>%
  setView( lat=27.5, lng=-89.5 , zoom=6) %>%
  addPolygons( fillColor = ~sent_change_pallete(Sent_change),  fillOpacity = 0.5,
               stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  addLabelOnlyMarkers(
    lng = nLabels[,2] , lat = nLabels[,3] ,
    label = ~paste( nLabels[,5]),
    #label = ~paste( nLabels[,7]),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = list("font-size" = "14px","font-weight" = "bold"))) %>% 
  addLabelOnlyMarkers(
    lng = -97.25 , lat = 20.5 ,
    label = ~paste0("Number in each grid indicates sample size from 2021"," \u2192 ", "2026"),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  addLegendFactor(pal = sent_change_pallete, values = ~Sent_change,
                  width = 40, height = 50,
                  title = "Abundance Sentiment Change",
                  labelStyle = "font-size: 18px; font-weight: bold;",
                  #labFormat = labelFormat(prefix = "$"),
                  fillOpacity = 0.5, 
                  position=("bottomleft")
  ) %>% 
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))  %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export



m

## save html to png
#saveWidget(m, "Comparison/Map Plots/ChangeAbundSentPlot.html", selfcontained = FALSE)
# webshot("ResponsePlot.html", file = "ResponsePlot.png",
#         cliprect = "viewport") Doesn't work correctly with padding, use below
#webshot("Comparison/Map Plots/ChangeAbundSentPlot.html", file = "Comparison/Map Plots/ChangeAbundSentPlot.png")



# New map -----------------------------------------------------------------

m <- leaflet(gridShrimp, padding=25) %>% ## added padding for whitespace
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  # addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = TRUE, group="ShadedRelief") %>% 
  #addTiles()  %>%
  setView( lat=27.5, lng=-89.5 , zoom=6) %>%
  addPolygons( fillColor = ~sent_change_pallete(Sent_change),  fillOpacity = 0.5,
               stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  addLabelOnlyMarkers(
    lng = nLabels[,2],lat = nLabels[,3],
    label = lapply(seq_len(nrow(nLabels)),
                   function(i) HTML(paste0(nLabels[i,6]," \u2192 ","<span style='color:blue;'>",nLabels[i,7],"</span>"))),
    labelOptions = labelOptions(noHide = TRUE,textOnly = TRUE,style = list("font-size" = "14px","font-weight" = "bold"))) %>% 
  addLabelOnlyMarkers(
    lng = -93.5 , lat = 24.5 ,
    label = ~paste0("Number in each grid indicates sample size from 2021"," \u2192 ", "2026"),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  addLegendFactor(pal = sent_change_pallete, values = ~Sent_change[!is.na(Sent_change)],
                  width = 40, height = 30,
                  title = HTML(paste0("Abundance Sentiment Change<br>","<span style='color:black;'>2021</span>"," \u2192 ","<span style='color:blue;'>2026</span>")),
                  labelStyle = "font-size: 18px; font-weight: bold;",
                  #labFormat = labelFormat(prefix = "$"),
                  fillOpacity = 0.5, 
                  position=("bottomleft"),
                  naLabel = NULL
  ) %>% 
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))  %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export



m

## save html to png
saveWidget(m, "Comparison/Map Plots/ChangeAbundSentPlot.html", selfcontained = FALSE)
# webshot("ResponsePlot.html", file = "ResponsePlot.png",
#         cliprect = "viewport") Doesn't work correctly with padding, use below
webshot("Comparison/Map Plots/ChangeAbundSentPlot.html", file = "Comparison/Map Plots/ChangeAbundSentPlot.png")

# Test color scale map ----------------------------------------------------


#sent_change_pallete_cont <- colorNumeric( palette = c("#D55E00", "#FFFF00", "#009E73"), domain=gridShrimp@data$Sent_change_n, na.color="transparent", reverse=FALSE)
#sent_change_pallete_cont <- colorNumeric( palette = c("#ED7D31", "#FFFF00", "#92D050"), domain=gridShrimp@data$Sent_change_n, na.color="transparent", reverse=FALSE)
sent_change_pallete_cont <- colorNumeric( palette = c("#D55E00", "#FFFF00", "#009E73"), domain=c(-2,2), na.color="transparent", reverse=FALSE)


m <- leaflet(gridShrimp, padding=25) %>% ## added padding for whitespace
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  # addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = TRUE, group="ShadedRelief") %>% 
  #addTiles()  %>%
  setView( lat=27.5, lng=-89.5 , zoom=6) %>%
  addPolygons( fillColor = ~sent_change_pallete_cont(Sent_change_n),  fillOpacity = 0.5,
               stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  addLabelOnlyMarkers(
    lng = nLabels[,2],lat = nLabels[,3],
    label = lapply(seq_len(nrow(nLabels)),
                   function(i) HTML(paste0(nLabels[i,6]," \u2192 ","<span style='color:blue;'>",nLabels[i,7],"</span>"))),
    labelOptions = labelOptions(noHide = TRUE,textOnly = TRUE,style = list("font-size" = "14px","font-weight" = "bold"))) %>% 
  addLabelOnlyMarkers(
    lng = -93.5 , lat = 24.5 ,
    label = ~paste0("Number in each grid indicates sample size from 2021"," \u2192 ", "2026"),
    #label = ~paste("n = ", nLabels[,8]),,
    labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = "14px")) %>% 
  addLegendNumeric(pal = sent_change_pallete_cont, values = ~Sent_change_n[!is.na(Sent_change_n)],
                   width = 40, height = 80,
                   title = HTML(paste0("Abundance Sentiment Change<br>","<span style='color:black;'>2021</span>"," \u2192 ","<span style='color:blue;'>2026</span>")),
                   labelStyle = "font-size: 18px; font-weight: bold;",
                   #labFormat = labelFormat(prefix = "$"),
                   fillOpacity = 0.5,
                   bins = c(0,1.2),
                   position=("bottomleft"),
                   labels = c("  No change", "Improved"),
                   naLabel = NULL
  ) %>%
  
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))  %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export



m


## save html to png
saveWidget(m, "Comparison/Map Plots/ChangeAbundScaleSentPlot.html", selfcontained = FALSE)
# webshot("ResponsePlot.html", file = "ResponsePlot.png",
#         cliprect = "viewport") Doesn't work correctly with padding, use below
webshot("Comparison/Map Plots/ChangeAbundScaleSentPlot.html", file = "Comparison/Map Plots/ChangeAbundSentPlot.png")

###########################################################################################
###  Keep for documentation
session.Info <- sessionInfo()
save(session.Info, file="session.Info.RData")
save.image("redsbaoo=.RData")
