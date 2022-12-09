# load the libraries

library(tidyverse)
library(readxl)
library(janitor)

# read excel sheet 1, 2, 3 into 3 data frames

sheet1 <- read_excel('Library Social Media Stats.xlsx', sheet = 1)
sheet2 <- read_excel('Library Social Media Stats.xlsx', sheet = 2)
sheet3 <- read_excel('Library Social Media Stats.xlsx', sheet = 3)

# get only the month columns - note that final sheet is incomplete
sheet1 <- select(sheet1, 1:13)
sheet2 <- select(sheet2, 1:13)
sheet3 <- select(sheet3, 1:4)

# rename column 1
colnames(sheet1)[1] <- "Metric_full_description"
colnames(sheet2)[1] <- "Metric_full_description"
colnames(sheet3)[1] <- "Metric_full_description"

# wanted to make the column names real dates not Excel date numbers but cannot work out the correct syntax. 
# May have to do it verbosely.
# colnames(sheet1) <- as.character(as.Date(sheet1, origin="1899-12-30"))
# colnames(sheet1) <- excel_numeric_to_date(sheet1, date_system="modern")

# remove totally empty rows
sheet1 <- sheet1[rowSums(is.na(sheet1)) != ncol(sheet1), ]
sheet2 <- sheet2[rowSums(is.na(sheet2)) != ncol(sheet2), ]
sheet3 <- sheet3[rowSums(is.na(sheet3)) != ncol(sheet3), ]

# Create and populate the Platform column sheet 1
sheet1 <- mutate(sheet1, Platform = case_when(
  str_detect(Metric_full_description,"Total") ~ "Total",
  str_detect(Metric_full_description,"Facebook") ~ "Facebook",
  str_detect(Metric_full_description,"Twitter") ~ "Twitter",
  str_detect(Metric_full_description,"Instagram") ~ "Instagram",
  str_detect(Metric_full_description, "YouTube") ~ "YouTube",
  str_detect(Metric_full_description, "SoundCloud") ~ "SoundCloud",
  str_detect(Metric_full_description, "Sound Cloud") ~ "SoundCloud",
  str_detect(Metric_full_description,"Blog") ~ "Blog",
))

# Create and populate the Platform column sheet 2
sheet2 <- mutate(sheet2, Platform = case_when(
  str_detect(Metric_full_description,"Total") ~ "Total",
  str_detect(Metric_full_description,"Facebook") ~ "Facebook",
  str_detect(Metric_full_description,"Twitter") ~ "Twitter",
  str_detect(Metric_full_description,"Instagram") ~ "Instagram",
  str_detect(Metric_full_description, "YouTube") ~ "YouTube",
  str_detect(Metric_full_description, "SoundCloud") ~ "SoundCloud",
  str_detect(Metric_full_description, "Sound Cloud") ~ "SoundCloud",
  str_detect(Metric_full_description,"Blog") ~ "Blog",
))

# Create and populate the Platform column sheet 3
sheet3 <- mutate(sheet3, Platform = case_when(
  str_detect(Metric_full_description,"Total") ~ "Total",
  str_detect(Metric_full_description,"Facebook") ~ "Facebook",
  str_detect(Metric_full_description,"Twitter") ~ "Twitter",
  str_detect(Metric_full_description,"Instagram") ~ "Instagram",
  str_detect(Metric_full_description, "YouTube") ~ "YouTube",
  str_detect(Metric_full_description, "SoundCloud") ~ "SoundCloud",
  str_detect(Metric_full_description, "Sound Cloud") ~ "SoundCloud",
  str_detect(Metric_full_description,"Blog") ~ "Blog",
))

# Wanted to transform the full description to get the metric but found the conditional formulas impossible.
# This does work, to drop everything after the ( and keep what was before it, 
# but only if there is a (, it is NA otherwise, and I couldn't work out how to use the un-transformed value in that case
# sheet1 <- mutate(sheet1, Metric_working = str_sub(Metric_full_description, 0, str_locate(Metric_full_description, "\\(")[, 1]-2 ))

# Create and populate the Metric column sheet 1, (also making Metrics clean)
sheet1 <- mutate(sheet1, Metric = case_when(
  str_detect(Metric_full_description,fixed("engagement", ignore_case=TRUE)) ~ "Engagement",
  str_detect(Metric_full_description,"posts") ~ "Posts",
  str_detect(Metric_full_description,fixed("paid reach", ignore_case=TRUE)) ~ "Paid_reach",
  str_detect(Metric_full_description,fixed("stories reach", ignore_case=TRUE)) ~ "Stories_reach",
  str_detect(Metric_full_description,fixed("reels reach", ignore_case=TRUE)) ~ "Reels_reach",
  str_detect(Metric_full_description,fixed("reach", ignore_case=TRUE)) ~ "Reach",
  str_detect(Metric_full_description,fixed("stories", ignore_case=TRUE)) ~ "Stories",
  str_detect(Metric_full_description,"Reels") ~ "Reels",
  str_detect(Metric_full_description, "video views") ~ "Video_views",
  str_detect(Metric_full_description, "YouTube views") ~ "Video_views",
  str_detect(Metric_full_description, "impressions") ~ "Impressions",
  str_detect(Metric_full_description, "followers") ~ "Followers",
  str_detect(Metric_full_description, "subscribers") ~ "Subscribers",
  str_detect(Metric_full_description, "podcast listens") ~ "Podcast_listens",
  str_detect(Metric_full_description, "podcast plays") ~ "Podcast_listens",
  str_detect(Metric_full_description, "podcasts") ~ "Podcasts",
  str_detect(Metric_full_description, "comments ") ~ "Comments ",
  str_detect(Metric_full_description, "page views") ~ "Page_views",
))

# Create and populate the Metric column sheet 2, (also making Metrics clean)
sheet2 <- mutate(sheet2, Metric = case_when(
  str_detect(Metric_full_description,fixed("engagement", ignore_case=TRUE)) ~ "Engagement",
  str_detect(Metric_full_description,"posts") ~ "Posts",
  str_detect(Metric_full_description,fixed("paid reach", ignore_case=TRUE)) ~ "Paid_reach",
  str_detect(Metric_full_description,fixed("stories reach", ignore_case=TRUE)) ~ "Stories_reach",
  str_detect(Metric_full_description,fixed("reels reach", ignore_case=TRUE)) ~ "Reels_reach",
  str_detect(Metric_full_description,fixed("reach", ignore_case=TRUE)) ~ "Reach",
  str_detect(Metric_full_description,fixed("stories", ignore_case=TRUE)) ~ "Stories",
  str_detect(Metric_full_description,"Reels") ~ "Reels",
  str_detect(Metric_full_description, "video views") ~ "Video_views",
  str_detect(Metric_full_description, "YouTube views") ~ "Video_views",
  str_detect(Metric_full_description, "impressions") ~ "Impressions",
  str_detect(Metric_full_description, "followers") ~ "Followers",
  str_detect(Metric_full_description, "subscribers") ~ "Subscribers",
  str_detect(Metric_full_description, "podcast listens") ~ "Podcast_listens",
  str_detect(Metric_full_description, "podcast plays") ~ "Podcast_listens",
  str_detect(Metric_full_description, "podcasts") ~ "Podcasts",
  str_detect(Metric_full_description, "comments ") ~ "Comments ",
  str_detect(Metric_full_description, "page views") ~ "Page_views",
))

# Create and populate the Metric column sheet 3, (also making Metrics clean)
sheet3 <- mutate(sheet3, Metric = case_when(
  str_detect(Metric_full_description,fixed("engagement", ignore_case=TRUE)) ~ "Engagement",
  str_detect(Metric_full_description,"posts") ~ "Posts",
  str_detect(Metric_full_description,fixed("paid reach", ignore_case=TRUE)) ~ "Paid_reach",
  str_detect(Metric_full_description,fixed("stories reach", ignore_case=TRUE)) ~ "Stories_reach",
  str_detect(Metric_full_description,fixed("reels reach", ignore_case=TRUE)) ~ "Reels_reach",
  str_detect(Metric_full_description,fixed("reach", ignore_case=TRUE)) ~ "Reach",
  str_detect(Metric_full_description,fixed("stories", ignore_case=TRUE)) ~ "Stories",
  str_detect(Metric_full_description,"Reels") ~ "Reels",
  str_detect(Metric_full_description, "video views") ~ "Video_views",
  str_detect(Metric_full_description, "YouTube views") ~ "Video_views",
  str_detect(Metric_full_description, "impressions") ~ "Impressions",
  str_detect(Metric_full_description, "followers") ~ "Followers",
  str_detect(Metric_full_description, "subscribers") ~ "Subscribers",
  str_detect(Metric_full_description, "podcast listens") ~ "Podcast_listens",
  str_detect(Metric_full_description, "podcast plays") ~ "Podcast_listens",
  str_detect(Metric_full_description, "podcasts") ~ "Podcasts",
  str_detect(Metric_full_description, "comments ") ~ "Comments ",
  str_detect(Metric_full_description, "page views") ~ "Page_views",
))


# Make the key and populate it
# no need to clean names in Key as all components are clean
sheet1 <- unite(sheet1,"Key", Platform:Metric, remove = FALSE)
sheet2 <- unite(sheet2,"Key", Platform:Metric, remove = FALSE)
sheet3 <- unite(sheet3,"Key", Platform:Metric, remove = FALSE)

# reorder the columns to bring the key, platform and metric to the front
# this was mainly to troubleshoot duplication and omission
sheet1 <- relocate(sheet1, Key, .before = Metric_full_description)
sheet2 <- relocate(sheet2, Key, .before = Metric_full_description)
sheet3 <- relocate(sheet3, Key, .before = Metric_full_description)
sheet1 <- relocate(sheet1, Platform, .before = Metric_full_description)
sheet2 <- relocate(sheet2, Platform, .before = Metric_full_description)
sheet3 <- relocate(sheet3, Platform, .before = Metric_full_description)
sheet1 <- relocate(sheet1, Metric, .before = Metric_full_description)
sheet2 <- relocate(sheet2, Metric, .before = Metric_full_description)
sheet3 <- relocate(sheet3, Metric, .before = Metric_full_description)

# remove all the Total rows as they are the main source of join duplication, and they are not needed.
sheet1 <- filter(sheet1, Platform != "Total")
sheet2 <- filter(sheet2, Platform != "Total")
sheet3 <- filter(sheet3, Platform != "Total")

# join all three sheets, not using the Metric or Platform from sheets 2 and 3.
sheet1plus2 <- left_join(sheet1, select(sheet2, c(Key,5:16)), by = "Key")
alldata <- left_join(sheet1plus2, select(sheet3, c(Key,5:7)), by = "Key")
# this is another way but no idea how to not use some columns.
# alldata2 <- purrr::reduce(list(sheet1,sheet2,sheet3), dplyr::left_join, by = 'Key')

# make the data long and tidy
alldata <- pivot_longer(alldata, c(5:31), names_to = "Month", values_to = "Value")

# another attempt to try to make the dates dates, thinking it would be easier to do 
# now that they are all in a single column and not column names
# alldata <- excel_numeric_to_date(alldata$Month)
# alldata <- as.character(as.Date(alldata$Month))
# alldata <- as.Date(alldata$Month)


# check contents
head(sheet1)
head(sheet2)
head(sheet3)
head(alldata)
