library(tidyverse)   # for working with the data
library(lubridate)   # for working with datetime data
library(skimr)       # generate a text-based overview of the data
library(visdat)      # generate plots visualizing data types and missingness
library(stringr)
library(qdapRegex)
library(ggforce)

data_dir = 'data'
target_file_o = file.path(data_dir,'olympics.csv')
target_file_g = file.path(data_dir,'gdp_per_capita.csv')

if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
if (!file.exists(target_file_o)) {
  download.file('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv',target_file_o)
}
## https://www.gapminder.org/data/
#if (!file.exists(target_file_g)) {
#  download.file('https://docs.google.com/spreadsheets/d/1h3z8u0ykcUum8P9FV8EHF9fszDYr7iPDZQ-fgE3ecls/export?format=xlsx',target_file_g)
#}

## Read in data ----
olympic_dataf = read_csv(target_file_o)
gdp_dataf = read_csv(target_file_g)

## Check the packaging
## skim(olympic_dataf)


## Post Soviet Union dissolution
small_olympic_dataf <- olympic_dataf %>%
  ## Filter year, summer and medals winner
  filter(year > 1991, season == "Summer", !is.na(medal)) %>%
  ## Removing special characters (-) and numbers from team/country names
  mutate(team = str_replace_all(team,"-",""),team = gsub("[[:digit:]]", "", team)) %>% 
  group_by(year,medal) %>%
  count(team,event) %>%
  ## Unique medals, team sport have multiple entries
  mutate(n = n/n) %>% 
  ungroup(medal) %>%
  count(team) %>% 
  rename(., country = team,medals = n) %>% 
  arrange(desc(medals), .by_group = TRUE)

o_year <- as.character(unique(small_olympic_dataf$year))
o_country <- unique(small_olympic_dataf$country)

## GDP per capita for countries in the Olympics (years matched)
small_gdp_dataf <- gdp_dataf %>% 
  filter(country %in% o_country) %>% 
  select(c(country,o_year))



df <- data.frame(matrix(ncol = 6, nrow = 0))
for (i in o_year) {
  df1 <- filter(small_olympic_dataf,year == i)
  #print(df1)
  df2 <- select(small_gdp_dataf, c(country,i))
  df2 <- rename(df2, gdp_per_capita = i)
  df2$gdp_per_capita = as.numeric(gsub("k", "e3", df2$gdp_per_capita))
  df2 <- arrange(df2, desc(gdp_per_capita)) %>% 
    mutate(gdp_rank = 1:nrow(df2))
  df1 <- merge(df1,df2,by="country",all.df1=TRUE)
  df1 <- arrange(df1, desc(medals)) %>% 
    mutate(o_rank = 1:nrow(df1))
  colnames(df) <- colnames(df1)
  df <- rbind(df, df1)
}

## Create charts
df_charts <- df %>% 
  group_by(year) %>% 
  ## Filter by rank in olympics
  filter(o_rank <= 15)
for (i in o_year){
  df1 <- filter(df_charts,year == i)
  slices <- df1$medals
  lbls <- df1$country %>% 
    #paste(.,", $",df1$gdp_per_capita,sep ="") %>% 
    paste(.,"(",df1$gdp_rank,")",sep ="")
  jpeg(paste("pie_chart_",i, ".jpg",sep =""))
  pie(slices,labels = lbls, main = paste("Top 15 Medal Winners in", i))
  dev.off()
}
jpeg(file="app.jpg")
barplot(table(df_charts$country), main = "Number of Appearances in Top 15",horiz = TRUE, las=1)
dev.off()

