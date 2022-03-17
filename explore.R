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



str(sets_for_keras_2004$train$train, 1)


tar_read(pediaDictSize)









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


    sets <- c(
      "sets_for_keras_2004", "sets_for_keras_2005", "sets_for_keras_2006",
      "sets_for_keras_2007", "sets_for_keras_2008", "sets_for_keras_2009",
      "sets_for_keras_2010", "sets_for_keras_2011", "sets_for_keras_2012",
      "sets_for_keras_2013"
    )

    res <- tibble::tibble(
      year = character(10),
      neg = numeric(10),
      pos = numeric(10)
    )
    for (i in seq_along(sets)) {
      set <- sets[[i]]
      res[[i, 1L]] <- set

      res[i, 2:3] <- tar_read_raw(set)$test$test$validation_y[, 2] |>
        table() |>
        as.list() |>
        setNames(c("neg", "pos"))
    }

performance <- varicella_due |>
  dplyr::filter(type == "test") |>
  dplyr::group_by(year) |>
  dplyr::filter(epoch == max(epoch)) |>
  dplyr::select(year, tp:rec) |>
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::arrange(year) |>
  dplyr::ungroup()

res |>
  dplyr::mutate(
    year = stringr::str_extract(year, "\\d+$") |> as.numeric(),
    true_incidente = pos / (pos + neg)
  ) |>
  dplyr::left_join(performance) |>
  dplyr::mutate(
    estimated_incidence = (tp + fp)/(tp+fp+fn+tn)
  )




library(targets)
library(tidyverse)
library(ROCR)

# varicella <- tar_read(varicella)
# mixdbs_2004 <- tar_read(mixdbs_2004)
eval_plots <- function() {
  n_years <- 10
  res <- tibble::tibble(
    year = numeric(n_years),
    neg = numeric(n_years),
    pos = numeric(n_years),
    incidence = numeric(n_years),
    pred = vector("list", n_years)
  )
  for (i in seq_len(n_years)) {
    year <- 2003 + i
    res[[i, 1L]] <- year

    prob <- targets::tar_read_raw(paste0("testing_probs_", year))
    labels <- targets::tar_read_raw(
      paste0("sets_for_keras_", year)
    )$test$test$validation_y[, 2]
    gc(FALSE)

    res[i, 2:3] <- labels |>
      table() |>
      as.list() |>
      purrr::set_names(c("neg", "pos"))

    res[[i, 4]] <- res[[i, "pos"]] / (res[[i, "pos"]] + res[[i, "neg"]])

    pred <- ROCR::prediction(prob, labels)
    res[[i, "pred"]] <- list(tibble::tibble(
      cutoff = pred@cutoffs[[1]][-1],
      fp = pred@fp[[1]][-1],
      fn = pred@fn[[1]][-1],
      tp = pred@tp[[1]][-1],
      tn = pred@tn[[1]][-1],
      prec = tp / (tp + fp),
      rec = tp / (tp + fn),
      fpr = fp / (tn + fp),
      dist_prc = sqrt((max(rec) - rec)^2 + (max(prec) - prec)^2),
      prod_prc = prec * rec,
      dist_roc = sqrt((min(fpr) - fpr)^2 + (max(rec) - rec)^2),

      estimated_incidence = (tp + fp) / (tp+fp+fn+tn)
    ))
  }
  res_unnested <- res |>
    tidyr::unnest(pred) |>
    dplyr::mutate(
      relerr_inc = estimated_incidence / incidence,
      abserr_inc = estimated_incidence - incidence
    )

  best_prc <- res_unnested |>
    dplyr::group_by(year) |>
    dplyr::filter(dist_prc == min(dist_prc)) |>
    dplyr::ungroup()

  best_prod_prc <- res_unnested |>
    dplyr::group_by(year) |>
    dplyr::filter(prod_prc == max(prod_prc)) |>
    dplyr::ungroup()

  best_roc <- res_unnested |>
    dplyr::group_by(year) |>
    dplyr::filter(dist_roc == min(dist_roc)) |>
    dplyr::ungroup()

  gg_roc <- res_unnested |>
    ggplot2::ggplot(
      ggplot2::aes(x = fpr, y = rec, colour = abserr_inc)
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      data = best_prod_prc,
      size = 5,
      colour = "red"
    ) +
    ggplot2::geom_text(
      data = best_prod_prc,
      ggplot2::aes(
        label = paste0(
          "Abs Inc Err\n",
          100 * round(abserr_inc, 4),
          "%")
      )
    ) +
    ggplot2::facet_wrap(~year, scales = "free")

  gg_prc <- res_unnested |>
    ggplot2::ggplot(
      ggplot2::aes(x = rec, y = prec, colour = abserr_inc)
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(
      data = best_prod_prc,
      size = 5,
      colour = "red"
    ) +
    ggplot2::geom_text(
      data = best_prod_prc,
      ggplot2::aes(
        label = paste0(
          "Abs Inc Err\n",
          100 * round(abserr_inc, 4),
          "%")
      )
    ) +
    ggplot2::facet_wrap(~year, scales = "free") +
    ggplot2::labs(
      title = "Precision-Recall curve on the \"next two-year\" VZV classification task"
    )

  list(
    res = res,
    res_unnested = res_unnested,
    gg_roc = gg_roc,
    gg_prc = gg_prc
  )
}

gg <- eval_plots()

gg$res_unnested |>
  dplyr::mutate(
    year = year |>
      factor(
        levels = 2004:2013,
        labels = paste0("Model: 2004-", 2004:2013)
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = fpr, y = rec, colour = abserr_inc)
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point(
    data = best_prod_prc |>
      dplyr::mutate(
        year = year |>
          factor(
            levels = 2004:2013,
            labels = paste0("Model: 2004-", 2004:2013)
          )
      ),
    size = 5,
    colour = "red"
  ) +
  ggplot2::geom_text(
    data = best_prod_prc |>
      dplyr::mutate(
        year = year |>
          factor(
            levels = 2004:2013,
            labels = paste0("Model: 2004-", 2004:2013)
          )
      ),
    ggplot2::aes(
      label = paste0(
        "Incidence Error\n",
        100 * round(abserr_inc, 4),
        "%")
    ),
    nudge_x = 0.25,
    nudge_y = -0.1
  ) +
  ggplot2::facet_wrap(~year, nrow = 2, scales = "free") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(
    x = "False Positive Rate (AKA 1-Specificity)",
    y = "Recall (AKA Sensibility)",
    colour = "Error in Incidence estimation"
  )



varicella |>
  filter(
    n_paz %in% set$test$test$validation_indeces,
    lubridate::year(date) %in% 2009:2010
  ) |>
  distinct(n_paz, .keep_all = TRUE)



prob2004 <- tar_read(testing_probs_2004)
sets_for_keras_2004 <- tar_read(sets_for_keras_2004)





pred <- prediction(prob2004, sets_for_keras_2004$test$test$validation_y[, 2])

tibble::tibble(
  cutoff = pred@cutoffs[[1]][-1],
  fp = pred@fp[[1]][-1],
  fn = pred@fn[[1]][-1],
  tp = pred@tp[[1]][-1],
  tn = pred@tn[[1]][-1],
  prec = tp / (tp + fp),
  rec = tp / (tp + fn),
  estimated_incidence = (tp + fp) / (tp+fp+fn+tn)
)


perf_prc <- performance(pred, "prec", "rec")
perf_roc <- performance(pred, "tpr", "fpr")
plot(perf_prc, colorize = TRUE)
plot(perf_roc, colorize = TRUE)


as.list(
    nrow(train$train_y) /
      (train$n_class * colSums(train$train_y))
  ) |>
  setNames(c("0", "1"))




testBidirectionalDeepGru__2010 <- tar_read(testBidirectionalDeepGru_2010)



