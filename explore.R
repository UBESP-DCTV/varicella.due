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

adjustesMaxWords <- tar_read(adjustesMaxWords)

testSimpleEmbedding <- tar_read(testSimpleEmbedding)
mixdbs_2007 <- tar_read(mixdbs_2007)

ready2mixes_2011 <- tar_read(ready2mixes_2011)
ready2mixes_2012 <- tar_read(ready2mixes_2012)
ready2mixes_2009 <- tar_read(ready2mixes_2009)
str(ready2mixes_2011)


ready2mixes_204 <- tar_read(ready2mixes_2004)
mixdbs_2004 <- tar_read(mixdbs_2004)
sets_for_keras_2004 <- tar_read(sets_for_keras_2004)
corpusDictionary_2004 <- tar_read(corpusDictionary_2004)


sets_for_keras_2004 <- tar_read(sets_for_keras_2004)
sets_for_keras_2004$train$train$train_y |> colSums()
sets_for_keras_2004$train$validation$validation_y |> colSums()


sets_for_keras_2013 <- tar_read(sets_for_keras_2013)
sets_for_keras_2013$train$train$train_y |> colSums()
sets_for_keras_2013$train$validation$validation_y |> colSums()



sets_for_keras

ready2mixes_2009$notes |> str(1)


ready2mixes_2009[["notes"]] <- unlist(ready2mixes_2009[["notes"]])
ready2mixes_2009[["notes"]] <- limpido::code_num(ready2mixes_2009[["notes"]])
ready2mixes_2009[["notes"]] <- limpido::expand_punctuations(ready2mixes_2009[["notes"]])
res <- limpido::mixdb(
  ready2mixes_2009,
  limpido::meta_vars(
    set, id_medico, n_paz, date, data_n, sesso, data_invio,
    vari_gold, anno, user_id, inizio_assistenza, fine_assistenza,
    data_elim, decesso_data, consenso_pedianet, min_of_data,
    max_of_data, giorni, class
  )
)











test <- ready2mixes_2009[1:2, ]


str(test)

test |>   dplyr::mutate(
  dplyr::across(dplyr::everything(), ~{
    if (all(
      purrr::map_lgl(.x, function(cell) length(unique(cell)) == 1)
    )) {
      unlist(purrr::map(.x, unique))
    }
  })
)





ready2mixes_2012


str(mixdbs_2007, 1)

plot(testSimpleEmbedding$testSimpleEmbedding_d260dac8$params$history)


sets_for_keras <- tar_read(sets_for_keras)
train <- sets_for_keras[[1]]$train$train
validation <- sets_for_keras[[1]]$train$validation

train$train_y[, 1] |> mean()




max_len <- tar_read(maxLen)
max_words <- tar_read(adjustesMaxWords); max_words <- max_words[[1]]
embedding_dim <- tar_read(embeddingDim)
embedding_matrix <- tar_read(embeddingMatrix); embedding_matrix <- embedding_matrix[[1]]

input_do <- tar_read(inputDO)
layers_do <- tar_read(layersDO)


se <- tar_read(testSimpleEmbedding_2008)

p <- plot(se$params$history) |>
  gg_history(se$params$history, se$params)


# Layer 1 =========================================================
# l1_input --------------------------------------------------------
l1_input <- keras::layer_input(shape = c(max_len))

l1_embedding <- l1_input |>
  keras::layer_embedding(
    input_dim = max_words,
    output_dim = embedding_dim,
    input_length = max_len,
    trainable = FALSE,
    weights = list(embedding_matrix),
    name = "l1_embedding"
  ) |>
  # [batch, words, embeddings] --> [batch, max-embeddings]
  keras::layer_global_max_pooling_1d() |>
  keras::layer_batch_normalization(name = "l1_batch-norm") |>
  keras::layer_dropout(input_do, name = "l1_dropout")

# OUTPUT ==========================================================
output <- l1_embedding |>
  keras::layer_dense(
    units = 128,
    activation = "relu",
    name = "fc_out"
  ) |>
  keras::layer_batch_normalization() |>
  keras::layer_dropout(layers_do) |>
  keras::layer_dense(
    units = 2L,
    activation = "sigmoid",
    name = "out_fc")


# MODEL ===========================================================
model <- keras::keras_model(l1_input, output)


# COMPILE =========================================================
model |>
  keras::compile(
    loss      = "binary_crossentropy",
    optimizer = "adam",
    metrics   = "accuracy"
  )

# Run =============================================================
{
  start_time <- lubridate::now()

  history <- keras::fit(
    model,
    # train ------------------------
    x = train[["train_x"]],
    y = train[["train_y"]],
    # validation -------------------
    validation_data = list(
      validation[["validation_x"]],
      validation[["validation_y"]]
    ),
    # learning pace ----------------
    batch_size = 8,
    epochs     = 15,
    verbose = 1
  )

  train_time <- lubridate::now() - start_time
}





finalTraintestIndeces <- tar_read(finalTraintestIndeces)

meta_vars <- c("id_medico", "data", "data_n", "sesso", "data_invio",
               "vari_gold", "user_id", "inizio_assistenza", "fine_assistenza",
               "data_elim", "decesso_data", "consenso_pedianet", "min_of_data",
               "max_of_data", "giorni")


str(mixdbs, 1)
attr(mixdbs[[2]], "meta")$set |>
  table()


a <- mixdbs[[1]]

attr(a, "meta")$set |>
  table()

str(embeddingMatrix, 1)


str(mixdb, 1)

attr(mixdb, "meta")[1, c("set", "id_medico", "n_paz", "anno", "notes")]

mixdb[["x"]][[1]]
mixdb[["x"]][[1]] |>
  names()


res <- compose_trvaltest_up_to_year(varicella, 2005)
res <- create_varicella_mixdb(ready2mix)
res <- merge_text_meta_records(example, meta_vars())


str(embeddingMatrix[[1]], 1)



















mixdb <- merge_text_meta_records(varicellas, meta_vars()) |>
  create_varicella_mixdb()

corpus_dict_size <- limpido::get_dictionary(mixdb) |>
  length()
max_words <- min(corpus_dict_size, 122607L)

embedding_matrix <- limpido:::embedding_mtrx(
  mixdb, fasttextPretrained, embeddingDim, max_words
)

sets_for_keras <- compute_sets_for_keras(mixdb, max_words, maxLen)

adjustesMaxWords <- max_words + 2








maxLen <- tar_read(maxLen)
embeddingDim <- tar_read(embeddingDim)
embeddingMatrix_2008 <- tar_read(embeddingMatrix_2008)


corpusDictionary_2008 <- tar_read(corpusDictionary_2008)
str(corpusDictionary_2008)

  attr(corpusDictionary_2008, "frequencies")[1:20]





last_training_year <- 2008
maxLen <- tar_read(maxLen)
adjustesMaxWords <- tar_read(adjustesMaxWords_2008)
embeddingDim <- tar_read(embeddingDim)
embeddingMatrix <- tar_read(embeddingMatrix)[[1]]
pediaDictSize <- tar_read(pediaDictSize)
corpusDicSize <- tar_read(corpusDicSize_2008)
inputDO <- tar_read(inputDO)
layersDO <- tar_read(layersDO)
preoutputFCunits <- 128
loss <- "binary_crossentropy"
optimizer <- "adam"
metrics <- "accuracy"
sets_for_keras <- tar_read(sets_for_keras_2008)
batchSize <- 8
epochs <- 50




    res <- train_bidirectional_gru(
      # embeddings
      maxLen, adjustesMaxWords,
      embeddingDim, embeddingMatrix,
      pediaDictSize, corpusDicSize,

      # train
      inputDO, layersDO, preoutputFCunits,

      # optimize
      loss, optimizer, metrics,

      # fit
      sets_for_keras, batchSize, epochs,

      # log
      last_year_of_data = last_training_year,
      use_weighted_classes = FALSE,
      is_test = FALSE,
      save_model = FALSE,
      tg = FALSE,
      keras_verbose = 1
    )



