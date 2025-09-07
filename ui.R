# ui.R 

library(shiny)
library(shinycssloaders)

fluidPage(
  titlePanel("Talent Tempo Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("job_desc", "Paste Job Description:", "", rows = 10),
      textInput("job_filter", "Filter Job Titles by Keyword or Regex:", value = "Manufacturing"),
      
      actionButton("score_btn", "Predict Time-to-Fill", class = "btn-primary")
    ),
    
    mainPanel(
      fluidRow(
        column(4, withSpinner(uiOutput("predicted_score_box"), type = 4)),
        column(4, withSpinner(uiOutput("internal_avg_box"), type = 4)),
        column(4, withSpinner(uiOutput("external_avg_box"), type = 4))
      ),
      
      br(),
      h3("Job Postings Over Time"),
      withSpinner(plotOutput("job_posting_trend"), type = 4),
      
      br(),
      h3("Time-to-Fill Trend Over Time"),
      withSpinner(plotOutput("time_fill_trend"), type = 4),
      
      br(),
      h3("Internal & External Buzzword Insights"),
      
      fluidRow(
        column(6,
               h4("Top Internal Buzzwords (Associated with Faster Hiring)"),
               withSpinner(dataTableOutput("missing_keywords"), type = 4)
        ),
        column(6,
               h4("Top External Buzzwords (Missing from Input)"),
               withSpinner(dataTableOutput("external_gap_table"), type = 4)
        )
      )
    )
  )
)
