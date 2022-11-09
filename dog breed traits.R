# install.packages("tidytuesdayR")
# install.packages("dplyr")
# install.packages("janitor")

library(dplyr)
library(janitor)

# tuesdata <- tidytuesdayR::tt_load("2022-02-01")
# saveRDS(tuesdata$breed_traits,"breed_traits.rds")

breed_traits <- readRDS("breed_traits.rds")
# glimpse(breed_traits)
breed_traits <- clean_names(breed_traits)
# glimpse(breed_traits)

breed_traits_mental <- select(breed_traits,breed,'Mental Stimulation Needs')
breed_traits_mental

poodle_traits <- filter(breed_traits,breed == "Poodles")
poodle_traits

select(breed_traits,1,2,6:10)

friendly_droolers <- filter(breed_traits,drooling_level>=4 & affectionate_with_family >=4)
friendly_droolers <- select(friendly_droolers,breed)
friendly_droolers

# homework
friendly_dogs <- filter(breed_traits,affectionate_with_family >=4 & good_with_young_children >=4 & good_with_other_dogs >=4)
friendly_dogs <- select(friendly_dogs,breed)
friendly_dogs