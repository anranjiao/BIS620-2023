# Set the directory: 
# the database we created in class is available, is named “ctrialsgov.duckdb” and
# can be found in “../ctrialsgovdb”, relative to the app.R file
# e.g. setwd("/Users/annora/Documents/Yale/Courses/BIS620/HW/bis620.2023/ctgov-query")

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud", repos = "http://cran.us.r-project.org")

library(shiny)
library(shinyWidgets)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(ctrialsgov)
library(tidyr)
library(purrr)
library(maps)
library(ggiraph)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(stringr)

source("ct-util.R")
max_num_studies = 1000

con = dbConnect(
  duckdb(
    file.path("..", "ctrialsgovdb", "ctrialsgov.duckdb"),
    read_only = TRUE
  )
)

dbListTables(con)
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")
facilities = tbl(con, "facilities")
countries = tbl(con, "countries")
facility_investigators = tbl(con, "facility_investigators")
facility_contacts = tbl(con, "facility_contacts")
facility_info = get_facility_info(facilities, facility_investigators, facility_contacts)
outcomes = tbl(con, "outcomes")
outcome_analyses = tbl(con,"outcome_analyses")

ctgov_load_duckdb_file(file.path("..", "ctrialsgovdb", "ctgov-derived.duckdb")) #derived data
endpoints = ctgov_query_endpoint()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  #titlePanel("Clinical Trials Query"),
  navbarPage(
    "Clinical Trials Query",   
    tabPanel("Basic",  
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 #          sliderInput("bins",
                 #                      "Number of bins:",
                 #                      min = 1,
                 #                      max = 50,
                 #                      value = 30),
                 textInput("brief_title_kw", h3("Brief title keywords")),
                 
                 # checkboxGroupInput("source_class", 
                 #                    label = h3("Sponsor Type"), 
                 #                    choices = list("Federal" = "FED", 
                 #                                   "Individual" = "INDIV", 
                 #                                   "Industry" = "INDUSTRY",
                 #                                   "Network" = "NETWORK",
                 #                                   "NIH" = "NIH",
                 #                                   "Other" = "OTHER",
                 #                                   "Other gov" = "OTHER_GOV",
                 #                                   "Unknown" = "UNKNOWN")),
                 ###############################################################
                 # Added by Anran, Problem 3
                 ###############################################################
                 pickerInput("source_class",
                             label = h3("Sponsor Type"),
                             choices = list("Federal" = "FED", 
                                            "Individual" = "INDIV", 
                                            "Industry" = "INDUSTRY",
                                            "Network" = "NETWORK",
                                            "NIH" = "NIH",
                                            "Other" = "OTHER",
                                            "Other gov" = "OTHER_GOV",
                                            "Unknown" = "UNKNOWN"),
                             multiple = TRUE,
                             options = list(`style` = "btn-info",
                                            `actions-box` = TRUE,
                                            `selected-text-format`= "count")),
                 
                 ###############################################################
                 # Added by Min, Feature 5
                 ###############################################################
                 sliderInput("words_freq",
                             "Minimum Frequency of Words:",
                             min = 1,  max = 30, value = 5),
                 sliderInput("words_max",
                             "Maximum Number of Words Displayed:",
                             min = 1,  max = 400,  value = 300),
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   #tabPanel("Plot", plotOutput("distPlot")),
                   tabPanel("Phase", plotOutput("phase_plot")),
                   tabPanel("Concurrent", plotOutput("concurrent_plot")),
                   tabPanel("Endpoint Met", plotOutput("endpointPlot")),
                   
                   ###############################################################
                   # Added by Min, Problem 2 
                   ###############################################################
                   tabPanel("Conditions", plotOutput("conditions_plot")),
                   
                   ###############################################################
                   # Added by Min, Feature 5
                   ###############################################################
                   # Feature 5: In Problem2, we add a new tab that gives a histogram showing 
                   # the conditions that trials in a query are examining. However, there are 
                   # many types of conditions/diseases, and it is hard to show the major types
                   # in a histogram plot. In this feature, I will use a word cloud to show the
                   # main types of conditions/diseases.

                   tabPanel("Word Cloud", plotOutput("plot",  width = "100%", height = 800))
                 ),
                 dataTableOutput("trial_table")
               )
               
             )
    ),
    
    ###############################################################
    # Added by Anran, Feature 1 and 2
    ###############################################################
    navbarMenu("Facilities", 
               
               # Feature 1: Search the facilities, investigators of the facilities, and contacts information and filter 
               # the data on status, country, role of investigators.
               tabPanel("Facility, Investigators, and Contacts", 
                        titlePanel(h4("Many facilities are recruiting! See the information of facilities below.")),
                        fluidRow(
                          column(4,
                                 selectInput("status",
                                             "Status:",
                                             c("All",
                                               unique(as.character(facility_info$status))))
                          ),
                          column(4,
                                 selectInput("country",
                                             "Country:",
                                             c("All",
                                               unique(as.character(facility_info$country))))
                          ),
                          column(4,
                                 selectInput("role",
                                             "Investigator Role:",
                                             c("All",
                                               unique(as.character(facility_info$role))))
                          )
                        ),
                        
                        tabPanel("Facility Status Count", plotOutput("facilityPlot")),
                        DT::dataTableOutput("facility_table")
               ),
               
               # Feature 2: Create a world map of the number of facilities
               tabPanel("World map of facilities", 
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel(h4("Facilities all over the world")),
                            textInput("brief_fac_kw", h5("Input the keywords to search for facilities.")),
                            titlePanel(h5("(e.g. University, Hospital, Cancer Center, Yale)")),
                            checkboxGroupInput("recruit",
                                               label = h5("Status"),
                                               choices = list("Recruiting" = "Recruiting",
                                                              "Available" = "Available",
                                                              "Not yet recruiting" = "Not yet recruiting"
                                                              )),
                            width = 3),
                          mainPanel(
                            #tabPanel("Facilities Map", plotOutput("facilityMap", width = "115%", height = 600))
                            girafeOutput("facilityMap", width = "120%", height = 1000)
                          )),
                        
                        dataTableOutput("fac_table"))
    ),
    ###############################################################
    # Added by Jonathan, Feature 3 and 4
    ###############################################################
    tabPanel("Eligibilities",
             sidebarLayout(
               sidebarPanel(
                 h3("Participant Type"),

                 radioButtons("participant_gender",
                              label = h4("Gender"),
                              choices = list("Male only" = "Male",
                                             "Female only" = "Female",
                                             "Both" = "All")),

                 # pickerInput("participant_gender",
                 #             label = h4("Gender"),
                 #             choices = list("Male only" = "Male",
                 #                            "Female only" = "Female",
                 #                            "Both" = "All"),
                 #             multiple = FALSE),

                 sliderInput("participant_age_range",
                             label = h4("Age Range"),
                             min = 0,
                             max = 100,
                             value = c(5, 15))
               ),

               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Studies", dataTableOutput("elig_table")),
                   tabPanel("Conditions Studied", plotOutput("conditions_eligibility_plot"))
                 ),

                 h3("Detailed Descriptions: "),
                 dataTableOutput("details_table")
               )

             )
    ),
    ###############################################################
    # Added by Min, Feature 6
    ###############################################################
    # In this feature, I create a plot and a table to show the distribution of 
    # p value for outcomes. The outcomes can be filtered by searching key words,
    # by choosing outcome type, or by choosing confidence interval N sides. 
    
    tabPanel("p value",  
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 textInput("description_kw", h3("Description keywords")),
                 pickerInput("outcome_type",
                             label = h3("Outcome Tpye"),
                             choices = c(unique(as.character((outcomes |>collect())$outcome_type))),
                             multiple = TRUE,
                             options = list(`style` = "btn-info",
                                            `actions-box` = TRUE,
                                            `selected-text-format`= "count")),
                 pickerInput("ci_n_sides",
                             label = h3("CI N Sides"),
                             choices = c(unique(as.character((outcome_analyses |>collect())$ci_n_sides))),
                             multiple = TRUE,
                             options = list(`style` = "btn-info",
                                            `actions-box` = TRUE,
                                            `selected-text-format`= "count")),
               ),
               
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("p Value", plotOutput("p_value_plot"))
                 ),
                 dataTableOutput("p_value_table")
               )
               
             )
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$endpointPlot <- renderPlot({
    create_endpoint_histogram(studies, endpoints, input$brief_title_kw)
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
    if (!is.null(input$source_class)) {
      ret = ret |> 
        filter(source_class %in% !!input$source_class)
    }
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
  
  
  ###############################################################
  # Added by Jonathan, Feature 3 and 4
  ###############################################################
  
  get_eligibilities = reactive({
    # browser()
    if (!is.null(input$participant_gender)) {
      ret = eligibilities |>
        filter(gender %in% !!input$participant_gender)
    } else {
      ret = eligibilities
    }
    # browser()
    if (!is.null(input$participant_age_range)) {
      min_age = input$participant_age_range[1]
      max_age = input$participant_age_range[2]
      ret = ret |>
        filter(minimum_age >= min_age) |>
        filter(maximum_age <= max_age)
    }
    ret |>
      # head(max_num_studies) |>
      collect()
  })
  
  output$elig_table = renderDataTable({
    # browser()
    get_eligibilities() |> 
      head(max_num_studies) |>
      create_data_table()
  })
  
  output$details_table = renderDataTable({
    # browser()
    get_eligibilities() |> 
      select(nct_id, description, name, url) |>
      rename(`NCT ID` = nct_id, `Conditions` = name)
  })
  
  output$conditions_eligibility_plot = renderPlot({
    # browser()
    get_eligibilities() |>
      plot_conditions_eligibility()
  })
  
  
  ###############################################################
  # Added by Min, Problem 2
  ###############################################################
  
  # Plot histogram for Problem 2. We only select first 20 types of conditions since
  # there are too many kinds.
  output$conditions_plot = renderPlot({
    left_join(
      get_studies() |> 
        select(nct_id),
      tbl(con, "conditions") |>
        select(nct_id, name) |>
        collect(),
      by = "nct_id") |>
      rename(`NCT ID` = nct_id, `conditions` = name) |>
      plot_conditions_histogram()
  })
  
  ###############################################################
  # Added by Anran, Feature 1 and 2
  ###############################################################
  
  # Get the facilities information with selections
  output$facility_table <- renderDataTable(datatable({
    #browser()
    if (input$status != "All") {
      facility_info <- facility_info[facility_info$status == input$status,]
    }
    if (input$country != "All") {
      facility_info <- facility_info[facility_info$country == input$country,]
    }
    if (input$role != "All") {
      facility_info <- facility_info[facility_info$role == input$role,]
    }
    facility_info |>
      arrange(`nct_id`) |>
      rename(`Facility ID` = facility_id, `NCT ID` = nct_id,
             `Status` = status, `Name` = name, `Country` = country, 
             `State` = state, `Investigator Name` = investigator, `Role` = role,
             `Contact Type` = contact_type, `Contact Name` = contact, `Email` = email) |> 
      collect()
  }))
  
  # Plot the number of facilities with different status
  output$facilityPlot <- renderPlot({
    plot_facility_status(facility_info)
  })
  
  # Plot a world map with the number of facilities
  output$facilityMap <- renderGirafe({
    facilities_wmap = get_facilities() |>
      get_facilities_wmap()
    ggiraph(code = print(plot_facility_map(facilities_wmap)))
  })
  
  # get the basic information of facilities related to the map
  output$fac_table = renderDataTable({
    get_facilities() |> 
      head(max_num_studies) |>
      select(nct_id, id, status, name, city, state, zip, country) |>
      rename(`NCT ID` = nct_id, `Facility ID` = id,
             `Status` = status, `Name` = name, `City` = city, `State` = state, 
             `ZIP Code` = zip, `Country` = country)
  })
  
  # Reactive table
  get_facilities = reactive({
    if (input$brief_fac_kw != "") {
      si = input$brief_fac_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(facilities, si, "name", match_all = TRUE)
    } else {
      ret = facilities
    }
    #browser()
    if (!is.null(input$recruit)) {
      ret = ret |> 
        collect() |>
        filter(status %in% !!input$recruit)
    }
    ret |> 
      collect()
  })
  
  ###############################################################
  # Added by Min, Feature 5 and 6
  ###############################################################
  
  # Plot word cloud for Feature 5
  output$plot <- renderPlot({
    word_results <- get_studies() |> get_word()
    wordcloud(names(word_results), 
              word_results, 
              scale=c(5,1),
              colors=brewer.pal(7,"BrBG"),
              max.words=input$words_max,
              min.freq = input$words_freq,
    )
  })
  
  # Get the outcome information with filter and selections for Feature 6
  get_outcomes = reactive({
    if (input$description_kw != "") {
      si = input$description_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(outcomes, si, "description", match_all = TRUE)
    } else {
      ret = outcomes 
    }
    
    if (!is.null(input$outcome_type)) {
      ret = ret |>
        filter(outcome_type %in% !!input$outcome_type)
    }
    
    if (!is.null(input$ci_n_sides)) {
      ret2 = outcome_analyses |>
        filter(ci_n_sides %in% !!input$ci_n_sides)
    } else {ret2 = outcome_analyses}
    
    
    left_join(ret, ret2 |> filter(!is.na(p_value)), by="nct_id") |>
      head(max_num_studies) |>
      collect()
  })
  
  # Add a table of information of p value for Feature 6
  output$p_value_table = renderDataTable({
    get_outcomes() |>
      select(nct_id, description, p_value, p_value_description) |>
      rename(`NCT ID` = nct_id, `Outcome Description` = description,
             `p Value` = p_value, `p Value Description` = p_value_description)
  })
  
  # Plot p value distribution for Feature 6
  output$p_value_plot = renderPlot({
    p_value_results = get_outcomes() |>
      select(nct_id, description, p_value, p_value_description) 
    p_value_results = p_value_results |>
      mutate(p_value_range = case_when(
        p_value <= 0.001 ~ "p value <= 0.001",
        p_value <= 0.01 ~ "0.001 < p value <= 0.01",
        p_value <= 0.05 ~ "0.01 < p value <= 0.05",
        p_value <= 0.1 ~ "0.05 < p value <= 0.1",
        p_value <= 0.5 ~ "0.1 < p value <= 0.5",
        p_value <= 1.0 ~ "0.5 < p value <= 1.0",
        .default = "p value > 1.0"
      ))
    p_value_results |> plot_p_value_histogram()
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
