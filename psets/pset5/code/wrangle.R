library(httr2)
library(tidyverse)
library(jsonlite)
library(janitor)
library(purrr)
library(lubridate)
source("census-key.R")
source("funcs.R")

## Read-in population
api <- "https://api.census.gov/data/2021/pep/population"
request <- request(api) |>  
  req_url_query(get = I("POP_2020,POP_2021,NAME"), 
                `for` = I("state:*"), 
                key = census_key) 
response <- request |> req_perform() 

population <- resp_body_json(response, simplifyVector = TRUE) |>
  janitor::row_to_names(1) |>
  as_tibble() |>
  select(-state) |> 
  rename(state_name = NAME) |> 
  pivot_longer(-state_name, names_to = "year", values_to = "population") |>
  mutate(year = str_remove(year, "POP_")) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]) |>
  mutate(state = case_when(
    state_name == "District of Columbia" ~ "DC", 
    state_name == "Puerto Rico" ~ "PR",
    .default = state))

## Read-in regions
url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions <- fromJSON(url, simplifyDataFrame = FALSE) |>
  map_df(function(x){
    data.frame(state_name = x$states, region = x$region, region_name = x$region_name)
  }) |>
  mutate(region = factor(as.numeric(region))) |>
  mutate(region_name = ifelse(nchar(region_name) > 50, "NY,NJ,PR,USVI", region_name))

## Join tables
population <- left_join(population, regions, by = "state_name")

## Download data
cases_raw <- get_cdc_data("https://data.cdc.gov/resource/pwn4-m3yp.json")
hosp_raw <- get_cdc_data("https://data.cdc.gov/resource/39z2-9zu6.json")
deaths_raw <- get_cdc_data("https://data.cdc.gov/resource/r8kw-7aab.json")
vax_raw <- get_cdc_data("https://data.cdc.gov/resource/rh2h-3yt2.json")

## wrangle cases
cases <- cases_raw |> mutate(cases = parse_number(new_cases), 
                             date = as_date(ymd_hms(end_date))) |>  
  filter(state %in% population$state) |>
  mutate(mmwr_week = epiweek(date), mmwr_year = epiyear(date)) |> 
  select(state, mmwr_year, mmwr_week, cases) |>
  arrange(state, mmwr_year, mmwr_week)

