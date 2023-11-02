#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(ctrialsgov)

library(tidyr)
library(purrr)

# 0. Midterm scheduling 

# 1. Clean up the table column names X
# 2. Allow multiple brief title keywords X
# 3. Create a histogram of the phase 
# 4. Organize files.
# 5. Fix the Phase plot
# 6. Plot the concurrent studies (adding a feature/capability).

# Steps to adding a feature:
# 1. Specify the feature.
#   - What does it do?
#   - What will be shown?
# 2. Specify the interface
#   - Where should it go?
#   - Does it enhance the user experience?
# 3. Implement the feature without the UI
# 4. Integrate the feature.

source("ct-util.R")
max_num_studies = 1000

con = dbConnect(
  duckdb(
    file.path("..", "..", "duckdb", "ctgov.duckdb"), 
    read_only = TRUE
  )
)
dbListTables(con)
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")


ctgov_load_duckdb_file(file.path("..", "..", "duckdb", "ctgov-derived.duckdb")) #derived data

endpoints = ctgov_query_endpoint()


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Clinical Trials Query"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #          sliderInput("bins",
      #                      "Number of bins:",
      #                      min = 1,
      #                      max = 50,
      #                      value = 30),
      textInput("brief_title_kw", "Brief title keywords"),
      checkboxGroupInput("source_class", 
                         label = h3("Sponsor Type"), 
                         choices = list("Federal" = "FED", 
                                        "Individual" = "INDIV", 
                                        "Industry" = "INDUSTRY",
                                        "Network" = "NETWORK",
                                        "NIH" = "NIH",
                                        "Other" = "OTHER",
                                        "Other gov" = "OTHER_GOV",
                                        "Unknown" = "UNKNOWN")),
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Endpoint Met", plotOutput("endpointPlot")),
        tabPanel("Phase", plotOutput("phase_plot")),
        tabPanel("Concurrent", plotOutput("concurrent_plot"))
      ),
      dataTableOutput("trial_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # browser() #for debugging
    create_phase_histogram_plot(studies, input$brief_title_kw)
  })
  
  output$endpointPlot <- renderPlot({
    create_endpoint_histogram(studies, endpoints, input$brief_title_kw)
  })
  
  
  output$trial_table = renderDataTable({
    si = trimws(unlist(strsplit(input$brief_title_kw, ",")))
    title_kw_search(studies, si) |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date) |>
      head(1000)
  })
  
  # 10/11/2023
  get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret = studies
    }
    # browser()
    if (!is.null(input$source_class)) {
      ret = ret |> 
        filter(source_class %in% !!input$source_class)
        # filter(source_class, si, "brief_title", match_all = TRUE)
    }
    # input$source_class
    
    ret |> 
      head(max_num_studies) |>
      collect()
  })
  
  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })
  
  output$concurrent_plot = renderPlot({
    get_studies() |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      ggplot(aes(x = date, y = count)) +
      geom_line() +
      xlab("Date") +
      ylab("Count") + 
      theme_bw()
  })
  
  output$trial_table = renderDataTable({
    get_studies() |> 
      head(max_num_studies) |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
