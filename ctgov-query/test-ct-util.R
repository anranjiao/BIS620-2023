if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")

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
library(RMySQL)

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

ctgov_load_duckdb_file(file.path("..", "ctrialsgovdb", "ctgov-derived.duckdb")) #derived data

endpoints = ctgov_query_endpoint()

source("ct-util.R")

#test for plot phase histogram
plot_phase_histogram(studies |> head(10) |> collect())


#test for get concurrent trials
studies |> head(10) |> collect() |>
  select(start_date, completion_date) |>
  get_concurrent_trials() |>
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  xlab("Date") +
  ylab("Count") +
  theme_bw()

#test for get trial table
studies |> head(10) |> collect() |>
  create_data_table()

#test for end point histogram
create_endpoint_histogram(studies, endpoints, "NASH")

###############################################################
# Added by Anran, Tests for functions in ct-util.R for Feature 1 & 2
###############################################################

facility_info = get_facility_info(facilities, facility_investigators, facility_contacts)

plot_facility_status(facility_info)

facilities_wmap = get_facilities_wmap(facilities)

plot_facility_map(facilities_wmap)


###############################################################
# Added by Jonathan Feature 3 & 4
###############################################################

#test for eligibility table
eligibilities |> 
  head(10) |>
  select(nct_id, gender, minimum_age, maximum_age, population, criteria) |>
  rename(`NCT ID` = nct_id)


#test for detailed description table
detailed_descriptions |> 
  head(10) |>
  rename(`NCT ID` = nct_id, `Conditions` = name)

#test for conditions count plot
eligibilities |>
  plot_conditions_eligibility()

###############################################################
# Added by Min
###############################################################
data_Problem_2 = left_join(
  studies |> head(1000) |> collect() |> select(nct_id),
  tbl(con, "conditions") |> select(nct_id, name) |> collect(),
  by = "nct_id") |>
  rename(`NCT ID` = nct_id, `conditions` = name)

plot_conditions_histogram(data_Problem_2)

# Feature 5
data_Feature_5 = studies |> head(1000) |> collect()

word_results = get_word(data_Feature_5)

wordcloud(names(word_results), 
          word_results, 
          scale=c(5,1),
          colors=brewer.pal(7,"BrBG"),
          max.words=20,
          min.freq = 5,
)

# Feature 6
data_Feature_6 = left_join(
  outcomes, outcome_analyses |> 
    filter(!is.na(p_value)), by="nct_id") |>
  head(1000) |>
  collect() |>
  select(nct_id, description, p_value, p_value_description) 

data_Feature_6 = data_Feature_6 |>
  mutate(p_value_range = case_when(
    p_value <= 0.001 ~ "p value <= 0.001",
    p_value <= 0.01 ~ "0.001 < p value <= 0.01",
    p_value <= 0.05 ~ "0.01 < p value <= 0.05",
    p_value <= 0.1 ~ "0.05 < p value <= 0.1",
    p_value <= 0.5 ~ "0.1 < p value <= 0.5",
    p_value <= 1.0 ~ "0.5 < p value <= 1.0",
    .default = "p value > 1.0"
  ))

plot_p_value_histogram(data_Feature_6)
