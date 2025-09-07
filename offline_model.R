# 1. Load necessary libraries 
library(tidyverse)
library(text2vec)
library(stringr)
library(lubridate)
library(glmnet)
library(tm)
library(udpipe)
library(textstem)

# 2. Load dataset
offline_df <- read_csv("DataScience/Talent_Tempo/offline_df.csv")

# 3. Cleaning Functions
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
  if (length(words) == 0) {
    return(tibble(token = character(), upos = character()))
  }
  text_input <- paste(words, collapse = " ")
  annotation <- udpipe_annotate(ud_model, x = text_input)
  annotation_df <- as.data.frame(annotation)
  annotation_df %>% select(token, upos)
}

# NEW: Final manual filter
final_buzzword_filter <- function(df) {
  df %>%
    filter(
      !str_detect(term, "\\d"),
      !term %in% c("mpcc", "bmsbl", "gdps", "february", "shannon", "clare", "juncos", "astm"),
      nchar(term) >= 4
    )
}

# NEW: Boost buckets
boost_buckets <- list(
  innovation = c("innovation", "innovative", "groundbreaking", "postgraduate", "emerge", "visionary", "pioneering"),
  growth = c("growth", "scale", "expansion", "multitude", "momentum", "accelerate", "trajectory"),
  positivity = c("welcome", "discretion", "parallel", "resolution", "multicultural", "inclusive", "empower", "uplift", "supportive", "collaborative"),
  excellence = c("excellence", "spanish", "biomedical", "pharmacovigilance", "robust", "regard", "impactful", "welcome", "outstanding", "superior", "team")
)

# NEW: Apply custom boost
apply_custom_boost <- function(terms_df, boost_buckets, boost_amount = -300) {
  lemmatized_terms <- textstem::lemmatize_words(terms_df$term)
  
  terms_df <- terms_df %>%
    mutate(
      lemmatized = lemmatized_terms,
      boosted = map_lgl(lemmatized, function(word) {
        any(map_lgl(boost_buckets, ~ word %in% .x))
      }),
      boosted_weight = if_else(boosted, weight + boost_amount, weight)
    ) %>%
    arrange(boosted_weight)
  
  return(terms_df)
}

# 4. Apply cleaning
offline_df$description_clean <- sapply(offline_df$job.description, clean_text)
offline_df <- offline_df %>% filter(!is.na(time_to_fill_days))

# 5. TF-IDF Generator
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
  
  return(list(matrix = tfidf_matrix, vocab = vocab))
}

# 6. Ridge Model Trainer
train_ridge_model <- function(X, y, alpha_value = 1) {
  cv.glmnet(
    X, y,
    alpha = alpha_value,
    nfolds = 5,
    family = "gaussian"
  )
}

# 7. Prediction function
predict_user_time_fill <- function(user_text, model, vocab_terms) {
  it <- itoken(list(user_text), progressbar = FALSE)
  vectorizer <- vocab_vectorizer(vocab_terms)
  dtm <- create_dtm(it, vectorizer)
  pred <- predict(model, dtm, s = "lambda.min")
  return(as.numeric(pred[1]))
}

# 8. External Buzzword Recommender
find_external_buzzwords <- function(user_text, external_vocab, external_model, ud_model, top_n = 20) {
  user_tokens <- str_split(tolower(user_text), "\\s+")[[1]]
  
  coefs <- as.matrix(coef(external_model, s = "lambda.min"))
  coefs <- coefs[-1, , drop = FALSE]
  
  important_terms <- tibble(
    term = external_vocab$term,
    weight = as.numeric(coefs)
  ) %>%
    filter(weight < 0) %>%
    filter(!(term %in% user_tokens)) %>%
    filter(str_detect(term, "^[a-z]+$"), nchar(term) >= 3) %>%
    arrange(weight)
  
  annotation <- safe_pos_tagging(important_terms$term, ud_model)
  
  important_terms <- important_terms %>%
    left_join(annotation, by = c("term" = "token")) %>%
    filter(upos %in% c("NOUN", "ADJ")) %>%
    arrange(weight) %>%
    head(top_n)
  
  return(important_terms)
}

# 9. Predictive Buzzwords from Combined
find_predictive_buzzwords <- function(tfidf_vocab, ridge_model, ud_model, top_n = 20) {
  coefs <- as.matrix(coef(ridge_model, s = "lambda.min"))
  coefs <- coefs[-1, , drop = FALSE]
  
  important_terms <- tibble(
    term = tfidf_vocab$term,
    weight = as.numeric(coefs)
  ) %>%
    filter(!is.na(weight)) %>%
    filter(weight < 0) %>%
    filter(str_detect(term, "^[a-z]+$"), nchar(term) >= 3) %>%
    arrange(weight)
  
  annotation <- safe_pos_tagging(important_terms$term, ud_model)
  
  important_terms <- important_terms %>%
    left_join(annotation, by = c("term" = "token")) %>%
    filter(upos %in% c("NOUN", "ADJ")) %>%
    arrange(weight) %>%
    head(top_n)
  
  return(important_terms)
}

# 10. Load udpipe model if needed
if (!exists("ud_model")) {
  model_info <- udpipe_download_model(language = "english-ewt")
  ud_model <- udpipe_load_model(file = model_info$file_model)
}

# ─────────────────────────────────────
# Main Offline Testing Section
# ─────────────────────────────────────

# Example User Input:
user_input_text <- "Job Description
The MSL provides field-based medical/scientific expertise ensuring understanding of the disease area and Biogen’s products as appropriate. They are integral to building valued constructive partnerships with key stakeholders in the medical community and the healthcare system.  MSLs contribute to company success through both internal and external medical education and medical/scientific information exchange and support.  MSLs utilize their medical/scientific expertise to gather medical insights and to support research/ data generation initiatives.

Acts as a trusted and credible medical science discussion partner by engaging in scientific/medical exchange with HCPs/ stakeholders as appropriate (as per the Biogen Medical whitepaper). Depending upon local requirements and regulations, this may include:

Proactive or reactive focused communication/education within the product label (using non-promotional materials)
Proactive, broad discussion around relevant Disease Areas
Reactive, broader communication which may cover both approved and non-approved products and wider Disease Area literature (medical/scientific exchange)
Legitimate exchange of medical and scientific information during the development of a medicine for the purpose of information gathering from groups such as medical experts/thought leaders (e.g., presentations at symposia)
Exploring new agendas within the MSL community and gathering medical insights to inform MA activities, LCM and investigational compounds

Stays up to date with therapy advances, current research, medical scientific knowledge and detailed knowledge on Biogen products & relevant Disease Areas

May support Medical Research activities (link with Global development functions), internal training and external education, e.g. may be involved in the support and coordination of clinical research activities with CCSLs (Clinical Country and Site Leads) as part of the Country and Site Operations (CSO) Group

Appropriately interacts and collaborate with other internal functions

Provides expertise to HCPs and other stakeholders who wish to present data on Biogen disease areas and related products.

Job holder must respect internal rules and all rules based on the respective legal and ethical (AIFP codex) standards. Any safety related information concerning Biogen products (adverse event / suspicion on adverse event etc.) must report to the Affiliate Safety Designated Staff in accordance with current internal company rules. Any quality related information concerning Biogen products (product complaints / product defects / suspicion on product defects / falsified medicinal products etc.) must reports directly to the Manager, Quality & Governance in accordance with current internal company rules.

Qualifications
BSc; Advanced terminal degree preferred (MD, PhD, PharmD, DNP in medical specialty) or equivalent combination of experience and education
Desire Min 2+ years’ experience in one of the therapeutic areas: Rare diseases, Neurology, Immunology, Dermatology from Medical Affairs (pharmaceutical industry or CRO) OR the pharmaceutical industry OR Medical studies
Desire Post-full-registration experience in hospital practice
Prior experience of clinical trials management and understanding of GCP
Understanding of health economics
Technical knowledge in Neurology, Immunology/Dermatology and/or Hematology
Proven expertise in working with and within local regulations and code of practice"

# 1. Filter jobs
regex_filter <- "MSL"
filtered_jobs <- offline_df %>% filter(str_detect(title, regex(regex_filter, ignore_case = TRUE)))
external_jobs_filtered <- filtered_jobs %>% filter(source == "external")

# 2. Train External Model
external_tfidf <- generate_tfidf_matrix(external_jobs_filtered$description_clean)
external_model <- train_ridge_model(external_tfidf$matrix, external_jobs_filtered$time_to_fill_days)

# 3. Predict User's Time-to-Fill
user_cleaned <- clean_text(user_input_text)
predicted_days <- predict_user_time_fill(user_cleaned, external_model, external_tfidf$vocab)
cat("\nPredicted Time-to-Fill:", round(predicted_days, 1), "days\n")

# 4. External Buzzwords
buzzword_recos <- find_external_buzzwords(user_cleaned, external_tfidf$vocab, external_model, ud_model, top_n = 30)
buzzword_recos <- final_buzzword_filter(buzzword_recos)
buzzword_recos <- apply_custom_boost(buzzword_recos, boost_buckets) %>% head(10)

cat("\nTop Suggested External Buzzwords (Final Polished and Boosted List):\n")
print(buzzword_recos)

# 5. Train Combined Model (Internal + External)
combined_tfidf <- generate_tfidf_matrix(filtered_jobs$description_clean)
combined_model <- train_ridge_model(combined_tfidf$matrix, filtered_jobs$time_to_fill_days, alpha_value = 0)

# 6. Predictive Buzzwords from Combined
predictive_buzzwords <- find_predictive_buzzwords(combined_tfidf$vocab, combined_model, ud_model, top_n = 30)
predictive_buzzwords <- final_buzzword_filter(predictive_buzzwords)
predictive_buzzwords <- apply_custom_boost(predictive_buzzwords, boost_buckets) %>% head(20)

cat("\nTop Predictive Buzzwords From Internal+External Ridge Model (Final Polished and Boosted List):\n")
print(predictive_buzzwords)
