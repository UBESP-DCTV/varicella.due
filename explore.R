## Use this script to run exploratory code maybe before to put it into
## the pipeline


# setup -----------------------------------------------------------

library(targets)  # use tar_read(target_name) to load a target anywhere
library(here)
library(tidyverse)
library(future)

# load all your custom functions
list.files(here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()



# Code here below -------------------------------------------------

popolazione <- tar_read(popolazione)
varic_df <- tar_read(varic_df)
gold_date <- tar_read(goldDate)
gold <- tar_read(gold)
varicella <- tar_read(varicella)
example <- tar_read(example)
prepared_for_mixing <- tar_read(prepared_for_mixing)
mixdb <- tar_read(mixdb)

gold_varicella <- prepared_for_mixing

meta_vars <- c("id_medico", "data", "data_n", "sesso", "data_invio",
               "vari_gold", "user_id", "inizio_assistenza", "fine_assistenza",
               "data_elim", "decesso_data", "consenso_pedianet", "min_of_data",
               "max_of_data", "giorni")


example

prepared_for_mixing

str(mixdb[[1]], 1)

attr(mixdb, "meta")[1, c("set", "id_medico", "n_paz", "anno", "notes")]
mixdb[["x"]][[1]]

