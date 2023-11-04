library(shiny)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(ctrialsgov)

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

ctgov_load_duckdb_file(file.path("..", "..", "duckdb", "ctgov-derived.duckdb")) #derived data

endpoints = ctgov_query_endpoint()

source("ct-util.R")

create_phase_histogram_plot(studies, "NASH") #test

#test for end point histogram


# this was implemented in ct-util.R after confirming it works
# create_endpoint_histogram = function(studies, endpoints, kw) {
#   em = query_kwds(studies, kw, "brief_title", match_all = TRUE) |>
#     select(nct_id) |>
#     collect() |>
#     left_join(endpoints, by = "nct_id")
#   
#   # em |>
#   #   group_by(endpoint_met) |>
#   #   summarize(n = n())
#   
#   ggplot(em, aes(x = endpoint_met)) +
#     geom_col() +
#     scale_y_log10() +
#     theme_bw()
# }
