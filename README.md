# Overview
The Talent Tempo hiring tool accepts an internal job description (raw text) input, and outputs the following:
- A prediction of how many days will be required to fill the role.
- A plot showing quantity of similar job postings over time.
- A time to fill trend over the past 6-months (in days).
- Top internal buzzwords relevant to the job posting (that were shown to correlate to faster job fill).
- Top external buzzwords that competitors are using in similar job postings. 

# Datasets
The actual datasets used for this model are redacted for confidentiality reasons, however they can be easily replicated within any organization. The global.R files requires two datasets, one with internal job posting data, and one with external 
job posting data from companies ideally within the same industry. The relevant columns required in each are as follows:

- job.description column, which features raw job description text
- created column, which provides the date the job posting was created
- delete.date, which is the date the job posting was removed from the site (our model assumes this is the fill date)





# Required R Files








# Tips & Suggestions





# Future Improvements



