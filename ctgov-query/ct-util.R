# Connect to the databases
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
facilities = tbl(con, "facilities")
countries = tbl(con, "countries")
facility_investigators = tbl(con, "facility_investigators")
facility_contacts = tbl(con, "facility_contacts")




eligibilities = tbl(con, "eligibilities")
eligibilities = eligibilities |>
  collect() |>
  select(-id, -sampling_method) |>
  mutate(minimum_age = str_split(minimum_age, " ") |> 
           sapply(`[[`, 1)) |>
  mutate(maximum_age = str_split(maximum_age, " ") |> 
           sapply(`[[`, 1))

eligibilities$minimum_age = as.integer(eligibilities$minimum_age)
eligibilities$maximum_age = as.integer(eligibilities$maximum_age)

eligibilities = eligibilities |>
  na.omit(eligibilities)

conditions = tbl(con, "conditions") |> 
  collect() |> 
  select(-id, -downcase_name)

detailed_descriptions = tbl(con, "detailed_descriptions") |> 
  collect() |> 
  select(-id)

links = tbl(con, "links") |> 
  collect() |> 
  select(nct_id, url, -id)

eligibilities = eligibilities |>
  left_join(detailed_descriptions, by = "nct_id") |>
  left_join(conditions, by = "nct_id") |>
  left_join(links, by = "nct_id")




###############################################################
# Code from class
###############################################################

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

# query_kwds <- function(tbl, kwds, column, ignore_case = TRUE, match_all = FALSE) {
#   kwds <- paste0("%", kwds, "%") |>
#     gsub("'", "''", x = _)
#   if (ignore_case) {
#     like <- " ilike "
#   } else{
#     like <- " like "
#   }
#   query <- paste(
#     paste0(column, like, "'", kwds, "'"),
#     collapse = ifelse(match_all, " AND ", " OR ")
#   )
#   
#   dplyr::filter(tbl, dplyr::sql(query))
# }

title_kw_search = function(studies, kw) {
  query_kwds(studies, kw, "brief_title", match_all = TRUE) |>
    collect()
}

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

###############################################################
# Added by Jonathan, Problem 1
###############################################################
#' Create a histogram of the phases returned by a brief title keyword search
#' @param x the database table.
#' @param brief_title_kw the brief title keywords to look for. This is optional.
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
    scale_x_discrete(labels = fixed_x_labels, drop = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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



###############################################################
# Added by Jonathan, Feature 3 and 4
###############################################################

#' @author Jonathan Lee
#' @title Query a studies  for the selected eligibility criteria
#' @description Shows a table of studies that aligns with the participant type selected
#' @param d the database table.
create_data_table = function(d) {
  d |>
    select(nct_id, gender, minimum_age, maximum_age, population, criteria) |>
    rename(`NCT ID` = nct_id)
}

#' @author Jonathan Lee
#' @title Plot a histogram of the count of conditions investigated for the selected eligibility criteria
#' @description Shows a number of conditions studied in a descending order
#' @param d the database table.
plot_conditions_eligibility = function(d) {
  d |>
    group_by(name) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    head(10) |>
    ggplot(aes(x = reorder(name, -n), y = n)) +
    geom_col() +
    xlab("Conditions") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

###############################################################
# Added by Anran, Feature 1 and 2
###############################################################

#' @author Anran Jiao
#' @title The information of facilities, the investigators, and contacts
#' @description
#' Feature 1: Search the facilities, investigators of the facilities, and contacts information and filter 
#' the data on status, country, role of investigators.
#' @param Tables: facilities, facility_investigators, facility_contacts
#' @return A joined table facility_info with investigators of the facilities, and contacts information.
get_facility_info = function(facilities, facility_investigators, facility_contacts){
  Facilities = facilities |> 
    rename(`facility_id` = id) |>
    select(facility_id, nct_id, status, name,  country, state)
  
  Facility_investigators = facility_investigators |>
    mutate(investigator = name) |>
    select(facility_id, investigator, role)
  
  Facility_contacts = facility_contacts |>
    mutate(contact = name) |>
    select(facility_id, contact, contact_type, email)
  
  # Checked the nct_id matches : identical(facility_info[['nct_id.x']],facility_info[['nct_id.y']])
  # keep columns with known status
  facility_info = Facilities |> 
    filter(!is.na(status)) |>
    left_join(Facility_investigators, by = "facility_id") |> 
    filter(!is.na(investigator)) |>
    left_join(Facility_contacts,  by = "facility_id") |>
    filter(!is.na(contact)) |>
    collect()
  return(facility_info)
}

#' @param d the database table.
#' @description 
#' Plot the number of facilities with different status
plot_facility_status = function(d) {
  d = d |>
    select(status) |>
    group_by(status) |>
    summarize(n = n()) 
  
  fixed_x_labels <- c("Recruiting", "Not yet recruiting", "Available")
  
  ggplot(d, aes(x = factor(status, levels = fixed_x_labels), y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Status") +
    ylab("Count") +
    scale_x_discrete(labels = fixed_x_labels)
}


#' @author Anran Jiao
#' @title The world map of the facilities
#' @description
#' Feature 2: Create a world map of the number of facilities
#' @param facilities
#' @return A table of the number of facilities joined with world map data
# Reference: https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/ 
get_facilities_wmap = function(facilities) {
  facilities_map = facilities |>
    mutate(facility_id = id) |>
    select(facility_id, status, name,  country, state) |>
    group_by(country) |>
    summarize(num = n()) 
  
  wmap = ggplot2::map_data('world')
  wmap = fortify(wmap) |>
    mutate(country = region) |>
    select(long, lat, group, country)
  
  # facilities |> distinct(country) |> summarise(n = n()) # 223
  # wmap |> distinct(country) |> summarise(n = n()) # 252
  
  # change the names in wmap
  # > "Congo, The Democratic Republic of the" %in% wmap$name
  # [1] FALSE
  # > "Democratic Republic of the Congo" %in% wmap$name
  # [1] TRUE
  original = c("Democratic Republic of the Congo", "Ivory Coast", "Iran", "North Korea", "South Korea",
               "Laos", "Moldova", "Palestine",  "UK", "USA", "Brunei", "Sint Maarten", "Russia", "Syria",
               "Tobago", "Antigua", "Barbuda", "Nevis","Saint Kitts", "Libya","Virgin Islands", "Vatican", 
               "Micronesia", "Grenadines","Saint Vincent", "French Southern and Antarctic Lands")
  new = c("Congo, The Democratic Republic of the", "Côte D'Ivoire", "Iran, Islamic Republic of", 
          "Korea, Democratic People's Republic of", "Korea, Republic of", "Lao People's Democratic Republic",
          "Moldova, Republic of", "Palestinian Territory, occupied", "United Kingdom", "United States", "Brunei Darussalam", 
          "Saint Martin", "Russian Federation", "Syrian Arab Republic","Trinidad and Tobago", "Antigua and Barbuda", 
          "Antigua and Barbuda", "Saint Kitts and Nevis","Saint Kitts and Nevis", "Libyan Arab Jamahiriya", 
          "Virgin Islands (U.S.)", "Holy See (Vatican City State)", "Federated States of Micronesia", 
          "Saint Vincent and the Grenadines", "Saint Vincent and the Grenadines", "Antarctic") 
  
  for (i in 1:length(original)){
    wmap$country[wmap$country == original[i]] <- new[i]
  }
  
  facilities_wmap = facilities_map |> 
    collect() |> 
    left_join(wmap, by = "country")
  facilities_wmap$num = log10(facilities_wmap$num)
  return(facilities_wmap)
}

#' @param d the database table.
#' @description 
#' Plot a world map with the number of facilities
plot_facility_map = function(d) {
  d = d |> mutate(`log10(#Facilities)` = num)
  ggplot() + 
    geom_polygon_interactive(data = subset(d, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = `log10(#Facilities)` , group = group, 
                                 tooltip = sprintf("%s<br/>%s", country, 10^num))) + 
    scale_fill_gradientn(colours = brewer.pal(8, "PuBuGn"), na.value = 'gray50') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          strip.background = element_rect(fill = 'white', colour = 'white'))
}

###############################################################
# Added by Min, Problem 2
###############################################################

#' @author Min Zhu
#' @title The condition histogram of Problem 2
#' @description Create a histogram of the conditions returned by a brief title keyword search
#' @param x the database table.
#' @param brief_title_kw the brief title keywords to look for. This is optional.
#' @return a histogram of the conditions
plot_conditions_histogram = function(x) {
  x$conditions[is.na(x$conditions)] = "NA"
  x = x |>
    select(conditions) |>
    group_by(conditions) |>
    summarize(n = n()) |>
    arrange(desc(n)) |> 
    mutate(conditions = factor(conditions, levels = conditions)) |>
    head(20)
  
  ggplot(x, aes(x = conditions, y = n)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Conditions") +
    ylab("Count")
}

###############################################################
# Added by Min, Feature 5 and 6
###############################################################

#' @author Min Zhu
#' @title get_word function for Feature 5
#' @description get the word counts from a column of database 
#' @param x the database table
#' @return word counts of the database table
#' @references https://shiny.posit.co/r/gallery/start-simple/word-cloud/
get_word <- function(x){
  isolate({
      condition_results = left_join(
        x |> 
          select(nct_id),
        tbl(con, "conditions") |>
          select(nct_id, name) |>
          collect(),
        by = "nct_id")
      
      condition_results = condition_results$name |> 
        VectorSource() |>
        Corpus() |>
        tm_map(content_transformer(tolower)) |>
        tm_map(removePunctuation) |>
        tm_map(removeNumbers) |>
        tm_map(removeWords, c(stopwords("SMART"), "the", "and", "but")) |>
        TermDocumentMatrix(control = list(minWordLength = 1)) |>
        as.matrix() |>
        rowSums() |>
        sort(decreasing = TRUE)
  })
}

#' @author Min Zhu
#' @title The p value distribution histogram of Feature 6
#' @description Create a histogram of the p value distribution returned by a table of p value 
#' @param x the p value table.
#' @return a histogram of p value distribution
plot_p_value_histogram = function(x) {
  x = x |>
    select(p_value_range) |>
    group_by(p_value_range) |>
    summarize(n = n()) |>
    arrange(desc(n)) |> 
    mutate(p_value_range = factor(p_value_range, levels = p_value_range))
  
  ggplot(x, aes(x = p_value_range, y = n)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("p Value Range") +
    ylab("Count")
}
