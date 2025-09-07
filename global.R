# global.R 

# 1. Load necessary libraries
library(tidyverse)
library(text2vec)
library(stringr)
library(lubridate)
library(glmnet)
library(tm)
library(udpipe)
library(textstem)

# 2. Cleaning Functions
clean_text <- function(text) {
  text <- tolower(text)
  text <- gsub("\\\\n|\\\\r|\\\\t", " ", text)
  text <- gsub("[^a-z\\s]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  text <- gsub("\\b[a-z]\\b", "", text)
  text <- gsub("\\s+", " ", text)
  return(text)
}

safe_pos_tagging <- function(words, ud_model) {
  if (length(words) == 0) return(tibble(token = character(), upos = character()))
  text_input <- paste(words, collapse = " ")
  annotation <- udpipe_annotate(ud_model, x = text_input)
  as.data.frame(annotation) %>% select(token, upos)
}

final_buzzword_filter <- function(df) {
  df %>%
    filter(
      !str_detect(term, "\\d"),
      !term %in% c("mpcc", "bmsbl", "gdps", "february", "shannon", "clare", "juncos", "astm"),
      nchar(term) >= 4
    )
}

boost_buckets <- list(
  innovation = c("innovation", "innovative", "groundbreaking", "postgraduate", "emerge", "visionary", "pioneering"),
  growth = c("growth", "scale", "expansion", "multitude", "momentum", "accelerate", "trajectory"),
  positivity = c("welcome", "discretion", "parallel", "resolution", "impeccable", "bms", "astrazeneca", "billingual", "team", "collaborative", "contributions", "multicultural", "inclusive", "empower", "uplift", "supportive", "collaborative"),
  excellence = c("excellence", "spanish", "biomedical", "pharmacovigilance", "robust", "quality", "impactful", "outstanding", "superior", "team")
)

apply_custom_boost <- function(terms_df, boost_buckets, boost_amount = -300) {
  lemmatized_terms <- textstem::lemmatize_words(terms_df$term)
  terms_df %>%
    mutate(
      lemmatized = lemmatized_terms,
      boosted = map_lgl(lemmatized, function(word) {
        any(map_lgl(boost_buckets, ~ word %in% .x))
      }),
      boosted_weight = if_else(boosted, weight + boost_amount, weight)
    ) %>%
    arrange(boosted_weight)
}

# 3. Load datasets
internal_jobs <- read_csv("data/BIIB_15Years_with_buckets.csv") %>%
  rename_with(tolower) %>%
  mutate(
    created = as.Date(created),
    delete.date = as.Date(delete.date),
    source = "internal"
  )

external_jobs <- read_csv("data/Mixed_Companies_2Years_with_buckets.csv") %>%
  rename_with(tolower) %>%
  mutate(
    created = as.Date(created),
    delete.date = as.Date(delete.date),
    source = "external"
  )

offline_df <- bind_rows(internal_jobs, external_jobs) %>%
  mutate(
    description_clean = sapply(job.description, clean_text),
    time_to_fill_days = as.numeric(difftime(delete.date, created, units = "days"))
  ) %>%
  filter(!is.na(time_to_fill_days))

# 4. TF-IDF + Ridge Modeling Functions
generate_tfidf_matrix <- function(texts, ngram_min = 1, ngram_max = 1) {
  it <- itoken(texts, progressbar = FALSE)
  stop_words <- tm::stopwords("en")
  
  vocab <- create_vocabulary(it, ngram = c(ngram_min, ngram_max)) %>%
    prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.8) %>%
    filter(!term %in% stop_words, nchar(term) > 2)
  
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)
  
  tfidf <- TfIdf$new()
  tfidf_matrix <- tfidf$fit_transform(dtm)
  
  list(matrix = tfidf_matrix, vocab = vocab)
}

train_ridge_model <- function(X, y, alpha_value = 1) {
  cv.glmnet(
    X, y,
    alpha = alpha_value,
    nfolds = 5,
    family = "gaussian"
  )
}

predict_user_time_fill <- function(user_text, model, vocab_terms) {
  it <- itoken(list(user_text), progressbar = FALSE)
  vectorizer <- vocab_vectorizer(vocab_terms)
  dtm <- create_dtm(it, vectorizer)
  pred <- predict(model, dtm, s = "lambda.min")
  as.numeric(pred[1])
}

find_predictive_buzzwords <- function(tfidf_vocab, ridge_model, ud_model, top_n = 5) {
  coefs <- as.matrix(coef(ridge_model, s = "lambda.min"))[-1, , drop = FALSE]
  
  tibble(term = tfidf_vocab$term, weight = as.numeric(coefs)) %>%
    filter(!is.na(weight), weight < 0, str_detect(term, "^[a-z]+$"), nchar(term) >= 3) %>%
    arrange(weight) %>%
    { safe_pos_tagging(.$term, ud_model) %>%
      left_join(., by = c("term" = "token")) %>%
      filter(upos %in% c("NOUN", "ADJ")) } %>%
    final_buzzword_filter() %>%
    apply_custom_boost(boost_buckets) %>%
    head(top_n)
}

# 5. Load UDPipe model
if (!exists("ud_model")) {
  model_info <- udpipe_download_model(language = "english-ewt")
  ud_model <- udpipe_load_model(file = model_info$file_model)
}
