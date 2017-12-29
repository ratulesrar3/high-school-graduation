# created data extract on the IPUMS data cart
# https://usa.ipums.org/usa-action/variables/group

# clear working environment
rm(list=ls())

# set working directory
setwd("/Users/ratulesrar/Documents/Fifth Year/high-school-graduation/")

# import tidyverse, reshape, and ipumsr to load data
library(tidyverse, warn.conflicts=FALSE)
library(reshape2)
library(ipumsr)
library(directlabels)

# import data file and DDI
ddi <- read_ipums_ddi("usa_00001.xml")
df <- read_ipums_micro(ddi, data_file="usa_00001.dat")

# view code vals for race, education, and state fips code vars
race_codes <- ipums_val_labels(df$RACED)
edu_codes <- ipums_val_labels(df$EDUCD)
st_codes <- ipums_val_labels(df$STATEFIP)

# filter on age, race
# subset those with listed statefip
# drop columns not relevant to analysis
df_filter <- df %>%
  filter(AGE %in% c(20,21,22,23,24,25,26,27,28,29,30),
         RACED %in% c(100,200),
         STATEFIP < 57) %>%
  select(YEAR, STATEFIP, SEX, 
         AGE, RACED, EDUC, 
         EDUCD, MIGPLAC5, MIGPLAC1,
         PERWT)

# filter out GED attainment and missing vals
# create binary graduated variable
# 1 if educational attainment is 12th grade or higher, 0 otherwise
df_filter <- df_filter %>%
  filter(!EDUCD %in% c(64,999)) %>%
  mutate(graduated = if_else(EDUCD > 62, 1, 0))

# group by state, year, and race
# calculate mean of graduation rate, weighted by person weights in survey
df_state <- df_filter %>%
  mutate(year = as.factor(YEAR),
         state = as.factor(STATEFIP),
         race = as.factor(RACED)) %>%
  group_by(YEAR, STATEFIP, RACED) %>%
  summarise(diploma = sum(graduated),
            total = n(),
            grad_rate = weighted.mean(graduated, PERWT))

# create a df for white data
df_white <- left_join(df_state, st_codes, by=c('STATEFIP'='val')) %>%
  filter(RACED == 100)

# melt the data into state name, year, and graduation rate
df_white_melt <- melt(df_white, id.vars=c('lbl','YEAR'), measure.vars='grad_rate')

# create a df for black data
df_black <- left_join(df_state, st_codes, by=c('STATEFIP'='val')) %>%
  filter(RACED == 200)

# melt the data into state name, year, and graduation rate
df_black_melt <- melt(df_black, id.vars=c('lbl','YEAR'), measure.vars='grad_rate')

# create vector of state names to loop through
state_vec <- filter(st_codes, val < 57)$lbl

# initialize empty list to store ggplot objects
plots <- list()

# clean data visualization ggplot theme
theme_dv <- function () {
  theme_bw(base_size=11, base_family='sans') %+replace%
    theme(
      panel.background=element_blank(),
      plot.background=element_rect(fill='gray96', color=NA), 
      legend.background=element_rect(fill='transparent', color=NA),
      legend.key=element_rect(fill='transparent', color=NA))
}

# loop through dataframe to create plot for each state
for (i in 1:51) {
  # initialize df for a state with white grad rate
  df1 <- filter(df_white_melt, lbl == state_vec[i]) %>%
    mutate(Race='White')
  
  # initialize df for a state with black grad rate
  df2 <- filter(df_black_melt, lbl == state_vec[i]) %>%
    mutate(Race='Black')
  
  # bind black and white dataframes together
  df3 <- rbind(df1, df2)
  
  # rename variables and drop superfluous columns
  df4 <- df3 %>%
    mutate(state = lbl,
           grad_rate = value) %>%
    select(state, YEAR, Race, grad_rate)
  
  # create ggplot object 
  p <- df4 %>%
    # make label variable for direct labeling on plot
    mutate(label=if_else(YEAR==max(YEAR), as.character(Race), NA_character_)) %>%
    # use year and grad rate for aesthetics, color based on race
    ggplot(aes(x=YEAR, y=grad_rate*100, color=Race)) +
    geom_line() +
    # add direct label for race
    geom_dl(aes(label=Race), method=list(dl.trans(x=x+0.1, y=y), 
                                         dl.combine("last.points"), cex=0.8)) +
    # set x axis limits
    scale_x_continuous(limits=c(1960, 2012.5)) +
    # paste percent symbol onto y axis ticks
    scale_y_continuous(labels=function(x) {paste0(x, '%')}) +
    # set colors for race
    scale_color_manual(values=c("#000000", "#1696d2")) +
    # add chart title, caption, axes labels
    labs(title=state_vec[i],
         subtitle='Comparing High School Graduation Rates of 20-30 year olds between 1960 - 2010',
         caption='Source: American Community Survey 1960-2010, IPUMS Data Portal', 
         x='Year', y='Percent with High School Diploma') +
    # use clean theming
    theme_dv()
  
  # store plot in list for easy reproduction
  plots[[i]] <- p
  
  # print plot for state i
  # print(p) 
}

