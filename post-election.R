library(tidyverse)
library(rvest)
library(ggrepel)
res <- read_csv("data/state_vote_differences.csv") |> 
  mutate(Vote_Difference = (2*(Candidate=="Harris")-1)*Vote_Difference) |>
  mutate(State = str_replace(State, "(\\d+)(st|nd|rd|th) District", "CD-\\1")) |>
  rename(state = State, margin = Vote_Difference) |>
  mutate(margin = margin/100) |>
  select(state, margin) 

url <- "https://projects.fivethirtyeight.com/polls/data/president_polls.csv"
raw_dat <- read_csv(url)

dat <- raw_dat |> filter(!hypothetical & population != "v") |>
  mutate(population = factor(population, levels = c("lv", "rv", "a"))) |>
  mutate(start_date = mdy(start_date),
         end_date = mdy(end_date))|>
  group_by(question_id) |> 
  mutate(n = n()) |>
  ungroup() |>
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump")) |>
  select(poll_id, question_id,  n, candidate_name, state, pollster, start_date, end_date,
         numeric_grade, sample_size, population, pct) |>
  mutate(candidate_name = str_remove(candidate_name, "\\w+\\s")) |>
  pivot_wider(names_from = candidate_name, values_from = pct) |>
  mutate(spread = (Harris - Trump)/100)  |>
  arrange(population, n) |>
  group_by(poll_id) |>
  slice(1) |>
  ungroup() |>
  select(-n, -question_id, -poll_id)  

polls <- filter(dat, !is.na(state))

url <- "https://state.1keydata.com/state-electoral-votes.php"
h <- read_html(url) |>
  html_table() 
tmp <- data.frame(state = c("Maine CD-1", "Maine CD-2", "Nebraska CD-2",  
                            "District of Columbia"), 
                  electoral_votes = c(1, 1, 1, 3))

ev <- h[[4]] |> 
  janitor::row_to_names(1) |> select(-Rank) |>
  setNames(c("state", "electoral_votes")) |>
  mutate(electoral_votes = as.numeric(electoral_votes)) |>
  bind_rows(tmp) |>
  mutate(electoral_votes = case_when(state == "Maine" ~ electoral_votes - 2,
                                     state == "Nebraska" ~ electoral_votes - 1,
                                     TRUE ~ electoral_votes))
library(gsheet)
sheet_url <- "https://docs.google.com/spreadsheets/d/1D-edaVHTnZNhVU840EPUhz3Cgd7m39Urx7HM8Pq6Pus/edit?gid=29622862"
raw_res_2020 <- gsheet2tbl(sheet_url) 

res_2020 <- raw_res_2020[,c(1,4)] |>  
  janitor::row_to_names(1) |>
  filter(State %in% ev$state) |>
  rename(state = State, party = `P.S.`) |>
  mutate(party = str_remove(party, "\\+")) |>
  bind_rows(data.frame(state = c("Maine CD-1", "Maine CD-2", "Nebraska CD-2", "District of Columbia"), party = c("D", "R", "D", "D")))


results <- polls |>
  filter(start_date > make_date(2024, 7, 21)) |>
  mutate(numeric_grade = replace_na(numeric_grade, 0)) |>
  mutate(week = floor(as.numeric(difftime(max(end_date), end_date, units = "week")))) |>
  group_by(state, week, pollster) |>
  summarize(spread = mean(spread), population = first(population), numeric_grade = mean(numeric_grade),
            .groups = "drop") |>
  group_by(state, week) |>
  arrange(population, desc(numeric_grade)) |>
  mutate(rank = row_number()) |>
  slice(1:10) |>
  summarize(avg = mean(spread), sd = sd(spread), n = n(), .groups = "drop") |>
  ungroup() |> 
  group_by(state) |>
  arrange(week) |>
  slice(1) |>
  ungroup() |>
  left_join(ev, by = "state") |>
  arrange(abs(avg))

tmp <- data.frame(state = c(state.name, "Maine CD-1", "Maine CD-2", "Nebraska CD-2", "District of Columbia"), 
                  abb = c(state.abb, "ME-1", "ME-2","NE-2","DC"))
results |> mutate(upper = ifelse(n >= 5, avg + qt(0.975, n - 1)*sd/sqrt(n), NA),
                  lower = ifelse(n >= 5, avg - qt(0.975, n - 1)*sd/sqrt(n), NA)) |>
  left_join(res, by = "state") |> 
  left_join(tmp, by = "state") |>
  ggplot(aes(avg*100, margin*100 - avg*100)) + geom_point() + geom_text_repel(aes(label = abb), size = 4, max.overlaps = 15)  +theme_bw()
  geom_errorbar(aes(xmin = lower*100, xmax = upper*100), color = "grey") +
  geom_point()+
  geom_abline() +
  geom_text_repel(aes(label = abb), size = 2, max.overlaps = 15) 
  geom_abline() +
  theme_bw() +
  ylim(-40, 40) +
  xlim(-40, 40) +
  labs(x = "Poll average (Harris - Trump)", y = "Current result", title = "2024 election results versus poll prediction",
       caption = "95% confidence interval included when 5 or more polls.")

  ggsave(filename ="~/Desktop/2024-result.png", width = 4.5, height = 4)

harris_start <- anti_join(res_2020, results, by = "state") |> 
  left_join(ev, by = "state") |>
  filter(party == "D") |>
  summarize(start  = sum(electoral_votes)) |> pull(start)



mu <- 0
tau <- 0.02
bias_sd <- 0.035
harris_ev <- replicate(100, {
  results |> mutate(sigma = sd/sqrt(n),
                    #few polls means not in play so don't shrink
                    B = ifelse(n >= 5, sigma^2 / (sigma^2 + tau^2), 0),
                    posterior_mean = B*mu + (1 - B)*avg,
                    posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2)),
                    result = rnorm(length(posterior_mean), 
                                   posterior_mean, sqrt(posterior_se^2 + bias_sd^2)),
                    harris = ifelse(result > 0, electoral_votes, 0)) |> 
    summarize(harris = sum(harris)) |> 
    pull(harris)
}) + harris_start
mean(harris_ev  > 269)
mean(harris_ev )
hist(harris_ev, breaks = 100:400)
quantile(harris_ev, c(0.1, 0.9))
