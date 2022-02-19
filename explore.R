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

varic_df <- tar_read(varic_df)
popolazione <- tar_read(popolazione)
gold <- tar_read(gold)

varicella <- tar_read(varicella)
example <- tar_read(example)
ready2mix <- tar_read(ready2mix)
mixdb <- tar_read(mixdb)
mixdbs <- tar_read(mixdbs)
embeddingMatrix <- tar_read(embeddingMatrix)




meta_vars <- c("id_medico", "data", "data_n", "sesso", "data_invio",
               "vari_gold", "user_id", "inizio_assistenza", "fine_assistenza",
               "data_elim", "decesso_data", "consenso_pedianet", "min_of_data",
               "max_of_data", "giorni")


str(mixdbs, 1)
attr(mixdbs[[1]], "meta")$set |>
  str(1)

str(mixdb, 1)

attr(mixdb, "meta")[1, c("set", "id_medico", "n_paz", "anno", "notes")]

mixdb[["x"]][[1]]
mixdb[["x"]][[1]] |>
  names()


res <- compose_trvaltest_up_to_year(varicella, 2005)
res <- create_varicella_mixdb(ready2mix)
res <- merge_text_meta_records(example, meta_vars())


str(embeddingMatrix[[1]], 1)
