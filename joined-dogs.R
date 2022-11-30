# install.packages("tidytuesdayR")
# install.packages("tidyverse")
# install.packages("janitor")

library(tidyverse)
library(janitor)

# tuesdata <- tidytuesdayR::tt_load("2022-02-01")
# saveRDS(tuesdata,"all_dogs_data.rds")

(all_dogs <- readRDS("all_dogs_data.rds"))

(breed_traits <- all_dogs$breed_traits |> 
  clean_names() |>
    mutate(key = make_clean_names(breed)))

(breed_ranks <- all_dogs$breed_rank |> 
    clean_names() |>
    mutate(key = make_clean_names(breed)))

(breed_ranks_no_breed <- all_dogs$breed_rank |> 
  clean_names() |>
    mutate(key = make_clean_names(breed)) |>
    select(-breed))


(traits_plus_ranks_dirty <- left_join(breed_traits, breed_ranks, by = "breed"))

(traits_plus_ranks <- left_join(breed_traits, breed_ranks_no_breed, by = "key"))

# define family friendly as affectionate + good with kids
(traits_plus_ranks_family <- traits_plus_ranks |>
  mutate(family_friendly = affectionate_with_family + good_with_young_children) |>
  arrange(family_friendly))
# this produces only 3 breeds which are family unfriendly, Plott Hounds, Anatolian Shepherd Dogs, Chihuahuas
# this is not very interesting for a ranking question
# instead answer the question which of the neutral family friendly breeds has the highest rank
(traits_plus_ranks_family_rank <- traits_plus_ranks |>
    mutate(family_friendly = affectionate_with_family + good_with_young_children) |>
    filter(family_friendly == 6) |>
    arrange(x2020_rank))
# of the dogs that are medium on family friendly, the Belgian Malinois has the highest rank in 2020.


# get only the top 20 dogs from 2015 and order by playfullness
(traits_plus_ranks_top20_2015 <- traits_plus_ranks |>
    filter(x2015_rank < 21) |>
    arrange(playfulness_level))
# the breeds with the lowest playfulness are Shih Tzu and Cavalier King Charles Spaniels

# check if there was a change in rank from 2015 to 2018
(traits_plus_ranks_top20_2015_changes <- traits_plus_ranks_top20_2015 |>
    mutate(rank_change = case_when(
      x2015_rank == x2018_rank ~ "No",
      x2015_rank != x2018_rank ~ "Yes",
    )))
# there was no change in the rank from 2015 to 2018 for the Cavalier King Charles Spaniel
# there was a change in rank from 2015 to 2018 for the Shih Tzu