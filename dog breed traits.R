# install.packages("tidytuesdayR")
# install.packages("dplyr")

library(dplyr)

tuesdata <- tidytuesdayR::tt_load("2022-02-01")

breed_traits <- tuesdata$breed_traits

breed_traits_mental <- select(breed_traits,Breed,'Mental Stimulation Needs')

breed_traits_mental

poo_traits_mental <- subset(breed_traits_mental,Breed == "Poodles")

poo_traits_mental

select(breed_traits,1,2,6:10)
