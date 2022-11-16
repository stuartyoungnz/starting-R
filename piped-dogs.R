# install.packages("tidytuesdayR")
# install.packages("dplyr")
# install.packages("janitor")

library(dplyr)
library(janitor)

tuesdata <- tidytuesdayR::tt_load("2022-02-01")
saveRDS(tuesdata$breed_traits,"breed_traits.rds")

breed_traits <- clean_names(readRDS("breed_traits.rds"))
# glimpse(breed_traits)

breed_traits |> 
  filter(drooling_level == 5) |> 
  arrange(breed)

# arrange(breed_traits, desc(drooling_level), breed)

dogs_that_drool <- breed_traits |> 
  mutate(drool_category = case_when(
    drooling_level <= 2 ~ "Light drool",
    drooling_level == 3 ~ "Medium drool",
    drooling_level >= 4 ~ "Heavy drool",
  )) |> 
  filter (drool_category == "Heavy drool") |> 
  arrange(desc(breed))