library(tidyverse)
library(readr)

load("data/id.key.rda")

new_ids <- read_csv("data-raw/new.ids.csv", 
                    col_types = cols(alt.id = col_character()))
View(new_ids)

Pcra_id_key <- bind_rows(Pcra_id_key, new_ids)

save(Pcra_id_key, file = "data/id.key.rda")
