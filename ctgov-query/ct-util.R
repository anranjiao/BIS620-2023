
# create_phase_histogram_plot = function(studies, brief_title_kw) { #added
#   d = title_kw_search(studies, brief_title_kw) |>
#     head(1000)
#   d$phase[is.na(d$phase)] = "NA"
#   d = d |>
#     select(phase) |>
#     group_by(phase) |>
#     summarize(n = n())
#   ggplot(d, aes(x = phase, y = n)) +
#     geom_col() +
#     theme_bw() +
#     xlab("Phase") +
#     ylab("Count")
# }

create_endpoint_histogram = function(studies, endpoints, kw) {
  em = query_kwds(studies, kw, "brief_title", match_all = TRUE) |>
    select(nct_id) |>
    collect() |>
    left_join(endpoints, by = "nct_id") |>
    group_by(endpoint_met) |>
    summarize(n = n())
  
  ggplot(em, aes(x = endpoint_met, y = n)) +
    geom_col() +
    scale_y_log10() +
    theme_bw()
}


# 10/11/2023
con = dbConnect(
  duckdb(
    file.path("..", "ctrialsgovdb", "ctrialsgov.duckdb"), 
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")

#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords 
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query)) 
}

query_kwds <- function(tbl, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds <- paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query <- paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  
  dplyr::filter(tbl, dplyr::sql(query))
}

title_kw_search = function(studies, kw) {
  query_kwds(studies, kw, "brief_title", match_all = TRUE) |>
    collect()
}

# Create a histogram of the phases returned by a brief title keyword search
# @param d the database table.
# @param brief_title_kw the brief title keywords to look for. This is optional.
plot_phase_histogram = function(x) {
  x$phase[is.na(x$phase)] = "NA"
  x = x |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n()) 
  
  fixed_x_labels <- c("NA", "Not Applicable", "Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Phase 4")
  
  ggplot(x, aes(x = factor(phase, levels = fixed_x_labels), y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count") +
    scale_x_discrete(labels = fixed_x_labels)
}

#' Get the number of concurrent trials for each date in a set of studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
get_concurrent_trials = function(d) {
  # browser()
  # Get all of the unique dates.
  all_dates = d |> 
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |> 
    arrange(value) |>
    na.omit() |> 
    rename(date = value)
  
  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }
  
  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count = 
    map_dbl(
      all_dates$date, 
      ~ .x |> 
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}

plot_concurrent_studies = function(studies) {
  plot(mtcars$mpg, mtcars$cyl)
}


#' @author Anran Jiao
#' @title 
#' @description
#' A short description...
#' @param
#' @return

