library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

# Read source file into a dataframe

covid_df <- data.frame(read_csv(file.choose()))

# Format date column
covid_df$date <- as.Date(covid_df$date,"%d-%m-%y")
View(covid_df$date)
# Extract sorted unique list of states
states <- sort(unique(covid_df$state))
 
# Extract summary pf Australia wide daily confirmed cases in a dataframe
covid_aus_df <- data.frame(covid_df %>% group_by(date) %>% 
  summarise(sum(confirmed)))
names(covid_aus_df) <- c("date","confirmed")

# Add column to hold top3 flag, initialize to 'N'
covid_aus_df["top3"] <- "N"
# Get top 3 new confimed cases values
top3 <- covid_aus_df %>% slice_max(confirmed,n=3)
# Flag rows with top3 values
covid_aus_df$top3[covid_aus_df$confirmed %in% top3$confirmed] <- "Y"

# Plot bar chart highlighting top 3
ggplot(data=covid_aus_df, aes(x=date, y= confirmed, fill = top3)) +
  geom_bar(stat= "identity") +
  scale_fill_manual( values = c("Y"="red","N"="steelblue",guide=FALSE ))+
  theme(legend.position = "none") +
  labs(title="2020 New Daily Confirmed COVID Cases in Australia", x="", y="",
       subtitle=" Data source: covid19data.com.au") + 
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d")

# Add growth_factor column as initialize to NA
covid_df["growth_factor"] <- NA

# iniitalize current date to 2020-03-17
curr_date <- as.Date("2020-03-17")

while (curr_date <= as.Date("2020-08-16")) {
  # find state wise growth factors
  for (state in states) {
    # get current day new cases
    curr <- covid_df$confirmed[covid_df$date == curr_date & 
                                 covid_df$state == state]
    
    # get previous day new cases
    prev <- covid_df$confirmed[(covid_df$date == curr_date - 1) & 
                                 covid_df$state == state]
    
    #calculate growth factor excluding cases when prev day new cases are 0
    if (prev != 0) {
      growth_factor <- curr/prev
      # add growth factor for curr day in dataframe
      covid_df$growth_factor[covid_df$date == curr_date & 
                               covid_df$state == state] <- growth_factor
    } 
  }  
  #increment curr day
  curr_date <- curr_date + 1
}

# Slice dataframe as per desire date range 
covid_df_sliced <- covid_df %>% filter(
  between(date,as.Date("2020-03-17"), as.Date("2020-08-16"))
)

# Plot growth factor as stacked bars
ggplot() + 
  geom_bar(data = covid_df_sliced,aes(x= date,y=growth_factor,fill = state),
                    stat="identity") + 
  labs(title="2020 Statewise COVID Growth Factor", x="", y="Growth Factor",
       subtitle=" Data source: covid19data.com.au") + 
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d")

