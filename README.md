# Overview
The Talent Tempo hiring tool accepts an internal job description (raw text) input, and outputs the following:
- A prediction of how many days will be required to fill the role.
- A plot showing quantity of similar job postings over time.
- A time to fill trend over the past 6-months (in days).
- Top internal buzzwords relevant to the job posting (that were shown to correlate to faster job fill).
- Top external buzzwords that competitors are using in similar job postings. 

# Datasets
The actual datasets used for this model are redacted for confidentiality reasons, however they can be easily replicated within any organization. The global.R files requires two datasets, one with internal job posting data, and one with external job posting data from companies ideally within the same industry. The relevant columns required in each are as follows:

- job.description column, which features raw job description text
- created column, which provides the date the job posting was created (must be date/datetime data type
- delete.date, which is the date the job posting was removed from the site (our model assumes this is the fill date)
- time_to_fill_days (numeric) — target variable defined by delete.date - created
- title (character) — job title (used for filtering)
- source (character) — e.g., "external" vs "internal"

These two input files should be placed in the data folder within this github, and the file paths within the global.R file (internal_jobs & external_jobs) should be updated to match the .csv file names. 

# Required R Files
### global.R
Global.R loads packages, reads data via relative paths, sources offline_model.R; shared objects for ui/server. Required libraries include tidyverse, text2vec, stringr, lubridate, glmnet, tm, udpipe and textstem. Cleaning functions work through the job description text and prepare it for TF-IDF vectorization. Included is a boost_buckets variable that can be filled with keywords that the author intends to have higher priority in job descriptions (e.g. words like innovation, growth, excellence, etc.). Datasets are imported, cleaned and vectorized before being fed into the training of the ridge regression model. The ridge regression model identifies words and sentence fragments within job descriptions that are predictive of shorter job-fill times. Using the trained model, the predict_user_time_fill and find_predictive_buzzwords functions take the user-provided job description and output a predicted time to fill in addition to key predictive buzzwords that the model identifies as predictive of a faster time to fill the role. 

### offline_model.R
Offline_model.R performs the following workflow: Train text → TF-IDF → penalized regression models to estimate time-to-fill from job descriptions and surface suggested buzzwords that (per the model) are associated with shorter time-to-fill. Also provides helper functions for cleaning, POS-tagging, vectorization, modeling, prediction, and term ranking.

Required inputs
- CSV file: DataScience/Talent_Tempo/offline_df.csv (relative to your working dir)
Required columns (case-sensitive):
- job.description (character) — raw job description text
- time_to_fill_days (numeric) — target variable
- title (character) — job title (used for filtering)
- source (character) — e.g., "external" vs other (used for subset models)
  
If your paths differ, adjust the read_csv() line or use file.path() with your app root.
Packages used:tidyverse, text2vec, stringr, lubridate, glmnet, tm, udpipe, textstem.
Vocabulary thresholds: term_count_min = 10 and doc_proportion_max = 0.8 control sparsity and generality; adjust for smaller/larger datasets.

### server.R
Implements the Shiny server logic for the Talent_Tempo app: validates user input, filters historical postings, trains a TF-IDF + penalized regression model to predict time-to-fill, surfaces internal and external buzzwords, and renders summary cards, tables, and trends.

Data / objects expected in memory from global.R:
- offline_df — a tibble with at least:
- description_clean (character) — cleaned text (see clean_text() in offline_model.R)
- time_to_fill_days (numeric)
- title (character)
- source (character: “internal” / “external”)
- created (POSIXct/Date) — posting timestamp
- ud_model — an udpipe model (e.g., english-ewt) for POS-tagging

Helper functions from offline_model.R: generate_tfidf_matrix(), train_ridge_model(), predict_user_time_fill(), find_predictive_buzzwords(), final_buzzword_filter(), apply_custom_boost()
filter_jobs_by_regex(df, pattern) — a small utility you define (example below)

Packages used (loaded in global.R): shiny, dplyr, stringr, lubridate, ggplot2, DT, zoo (plus modeling deps already used in offline_model.R)

### ui.R
Defines the Shiny user interface for the Talent_Tempo app: inputs to capture a job description and a title filter, and output placeholders for predicted time-to-fill, averages, buzzword tables, and trend plots—with loading spinners.

Packages: shiny, shinycssloaders (UI spinners), and DT (for dataTableOutput)

Header: titlePanel("Talent Tempo Predictor")
Sidebar: inputs (job_desc, job_filter, score_btn)
Main panel:
- 3 KPI cards (predicted, internal avg, external avg), each with withSpinner()
- “Job Postings Over Time” plot with spinner
- “Time-to-Fill Trend Over Time” plot with spinner
Two side-by-side tables: Internal Buzzwords and Missing External Buzzwords, both with spinners

Notes:
dataTableOutput() is from DT; ensure DT is attached (in global.R) or call DT::dataTableOutput() in ui.R.
Spinners (withSpinner(..., type = 4)) require shinycssloaders to be loaded.
Keep IDs in ui.R and server.R in sync; a mismatch will throw “object not found” errors at render time.
For theming, you can add shinythemes::themeSelector() or wrap fluidPage(theme = shinythemes::shinytheme("cosmo"), ...).

# Running the RShiny App
To initialize and launch the app, simply open all four .R files in RStudio and select "Run App". Internal and external job description datasets must be placed in the "data" folder, and the internal path must be specified correctly for the offline_df.csv in the offline_model.R file. This is simply your training set that your ridge model will leverage, and can be a combination of both internal and external datasets. 

# Future Improvements
#### General
- Leverage Elastic Net + proper “Ridge”. Currently the helper defaults to alpha=1 (Lasso) instead of using a grid-search alpha ∈ [0,1] and lambda.
- Split by created month to avoid leakage (train on older, test on newer).
- Memoize TF-IDF + models by job_filter.
- Let users export buzzwords & plots for each job description input.

#### Modeling & explainability:
- Allow bigrams/trigrams through n-gram buckets.
- Add prediction intervals through bootstrap sampling.
- Report top terms with CI across CV folds.
- Maintain a YAML of allow/block/boost lists (versioned), edit in-app for admins.

#### Data & pipeline:
- Add renv::init() and commit renv.lock.
- On app start (or a cron job), build & save tfidf.rds, model.rds, ud_model to avoid per-click training.
- Publish models/vocabs with pins for simple versioning & rollback.
- Log inputs length, filter, latency, and errors (logger, loggit) to a CSV or SQLite.

#### UX/Product:
- Add Δ vs. internal/external benchmarks and colored arrows.
- Show “example” ad; live character counter; highlight matched keywords in the textarea.
- Facet by State/Title/Source and toggle smoothing window.
- Add one-click HTML/PDF report via rmarkdown::render() summarizing prediction, terms, and charts.
