# install.packages("tidytuesdayR")
# install.packages("dplyr")
# install.packages("janitor")

library(dplyr)
library(janitor)

# tuesdata <- tidytuesdayR::tt_load("2022-02-01")
# saveRDS(tuesdata$breed_traits,"breed_traits.rds")

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

coat_type_count <- breed_traits |> 
  count(coat_type, sort = TRUE)

coat_type_count
# 'Double' and 'Smooth' are equal most common coat types.

coat_type_grooming_relationship <- breed_traits |>
  group_by(coat_type) |>
  summarise(avg_groom = mean(coat_grooming_frequency)) |>
  arrange(desc(avg_groom))

coat_type_grooming_relationship
# 'Corded' has the highest mean coat grooming frequency

corded <- breed_traits |> 
  filter(coat_type == "Corded") |> 
  count(coat_grooming_frequency)

corded
# there are only 4 dog breeds with Corded coats 
# and one of them does not have a high grooming frequency
  