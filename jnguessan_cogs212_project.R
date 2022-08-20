library(tidyverse)   # for working with the data
library(lubridate)   # for working with datetime data
library(stringr)
library(qdapRegex)
library(ggtext)
library(ggrepel)
library(rstudioapi)

## Going to code location

setwd(dirname(getActiveDocumentContext()$path))

##Creating folders and files

data_dir = 'data'
figs_dir = 'figs'
tables_dir = 'tables'
target_file_o = file.path(data_dir,'olympics.csv')
target_file_g = file.path(data_dir,'gdp_per_capita.csv')

if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
if (!dir.exists(figs_dir)) {
  dir.create(figs_dir)
}
if (!dir.exists(tables_dir)) {
  dir.create(tables_dir)
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

## create small table for doc export
write.csv(head(small_olympic_dataf),file = "tables/sod.csv")


## GDP per capita for countries in the Olympics (years matched)
o_year <- as.character(unique(small_olympic_dataf$year))
o_country <- unique(small_olympic_dataf$country)

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
  df1 <- merge(df1,df2,by="country",all.df1=TRUE)
  df1 <- arrange(df1, desc(medals)) %>% 
    mutate(o_rank = 1:nrow(df1))
  colnames(df) <- colnames(df1)
  df <- rbind(df, df1)
}

## Create charts
setwd("figs")


for (i in o_year){
  df1 <- filter(df,year == i) %>%
    mutate(scaled_gdppc = log10(gdp_per_capita))
  
  jpeg(paste("plot_",i, ".jpg",sep =""), width = 6, height = 6, units = 'in', res = 300)
  #plot(df1$gdp_per_capita,df1$medals, main = paste("Top 15 Medal Winners in", i), xlab="GDP Per Capita", ylab="Medals")
  p <- ggplot(df1, aes(x = scaled_gdppc, y = medals)) + geom_point() +
    ggtitle(paste("Number of Medals vs Log10 Scaled GDP Per Capita in ",i)) +
    xlab("Scaled GDP Per Capita") +
    ylab("Medals") +
    geom_label_repel(aes(label = country))
  print(p)
  dev.off()
}

jpeg(file="Total_medal_count.jpg", width = 12, height = 6, units = 'in', res = 300)

dft <- arrange(df, desc(medals)) %>% 
  group_by(country) %>% 
  select(medals) %>% 
  mutate(medals = sum(medals)) %>% 
  distinct()

  p<-ggplot(dft, aes(x=reorder(country, -medals), y=medals)) +
  geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 90))+ ggtitle("Countries medal counts 1992-2016") +xlab("Countries") + ylab("Medals")
print(p)
dev.off()

setwd(dirname(getActiveDocumentContext()$path))

## Create tables for exports
write.csv(head(df1), "tables/final_df_preview.csv")
