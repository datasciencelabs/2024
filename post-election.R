library(tidyverse)
library(rvest)
library(ggrepel)
url <- "https://projects.fivethirtyeight.com/polls/data/president_polls.csv"
raw_dat <- read_csv(url)

dat <- raw_dat |> filter(population != "v") |>
  mutate(population = factor(population, levels = c("lv", "rv", "a"))) |>
  mutate(start_date = mdy(start_date),
         end_date = mdy(end_date)) |>
  filter(start_date > make_date(2024, 7, 21)) |>
  group_by(question_id) |> 
  mutate(n = n()) |>
  ungroup() |>
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump")) |>
  select(poll_id, question_id,  n, candidate_name, state, pollster, start_date, end_date,
         numeric_grade, sample_size, population, hypothetical, pct) |>
  mutate(candidate_name = str_remove(candidate_name, "\\w+\\s")) |>
  pivot_wider(names_from = candidate_name, values_from = pct) |>
  mutate(spread = (Harris - Trump)/100)  |>
  arrange(population, hypothetical, n) |>
  group_by(poll_id) |>
  slice(1) |>
  ungroup() |>
  select(-n, -question_id, -poll_id)  

polls <- filter(dat, !is.na(state))

url <- "https://en.wikipedia.org/w/index.php?title=2020_United_States_presidential_election&oldid=1255928649#External_links"
h <- read_html(url) |>
  html_table() 

res_2020 <- h[[30]][-c(1, 58, 59) ,c(1, 2, 4, 5, 7, 20)] |> 
  setNames(c("state", "biden", "ev1", "trump", "ev2", "total")) |>
  mutate(ev = ifelse(ev1 == "–", ev2, ev1)) |>
  select(-ev1, -ev2) |>
  mutate(state = str_remove_all(state, "\\[\\w*\\]")) |>
  mutate(state = str_remove_all(state, "Tooltip.*$")) |> 
  mutate(state = str_remove_all(state, "\\s†")) |>
  mutate(state = str_replace_all(state, "NE", "Nebraska CD")) |>
  mutate(state = str_replace_all(state, "ME", "Maine CD")) |>
  mutate(across(-state, parse_number)) |>
  mutate(margin = biden/total - trump/total) |>
  select(state, margin)

## 2024 electoral votes
url <- "https://state.1keydata.com/state-electoral-votes.php"
tmp <- data.frame(state = c("Maine CD-1", "Maine CD-2", "Nebraska CD-1", "Nebraska CD-2", "Nebraska CD-3",
                            "District of Columbia"), 
                  electoral_votes = c(1, 1, 1, 1, 1, 3))
ev <- read_html(url) |>
  html_table() |>
  _[[4]] |>
  janitor::row_to_names(1) |> select(-Rank) |>
  setNames(c("state", "electoral_votes")) |>
  mutate(electoral_votes = as.numeric(electoral_votes)) |>
  bind_rows(tmp) |>
  mutate(electoral_votes = case_when(state == "Maine" ~ electoral_votes - 2,
                                     state == "Nebraska" ~ 2,
                                     TRUE ~ electoral_votes))


results <- polls |>
  filter(!is.na(numeric_grade)) |>
  mutate(numeric_grade = replace_na(numeric_grade, 0)) |>
  mutate(week = floor(as.numeric(difftime(max(end_date), end_date, units = "week")))) |>
  group_by(state, week, pollster) |>
  summarize(spread = mean(spread), population = first(population), numeric_grade = mean(numeric_grade), hypothetical = first(hypothetical),
            .groups = "drop") |> 
  group_by(state, week) |> 
  summarize(avg = mean(spread), sd = sd(spread), n = n(), .groups = "drop") |>
  ungroup() |> 
  group_by(state) |>
  arrange(week) |>
  slice(1) |>
  ungroup() |>
  mutate(sd = ifelse(n < 5, median(sd[n > 5]), sd)) |>
  arrange(abs(avg)) 


## Bayesian shirnk towards previous year:
## tmp is 2020 results with 2024 ev
tmp <- res_2020 |> left_join(ev, by = "state") |> setNames(c("state", "margin_2020", "ev"))
tau <- 0.05
results <- results |> right_join(tmp, by = "state") |>
  mutate(sigma = sd/sqrt(n), 
         B = sigma^2/(sigma^2 + tau^2),
         posterior_mean = B*margin_2020 + (1 - B)*avg,
         posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2))) |> 
  mutate(posterior_mean = ifelse(is.na(posterior_mean), margin_2020, posterior_mean),
         posterior_se = ifelse(is.na(posterior_se), tau, posterior_se))


## Simulation
global_bias <- 0.02
harris_ev <- replicate(10000, {
  results |> 
    mutate(result = rnorm(length(posterior_mean), 
                          posterior_mean, sqrt(posterior_se^2 + local_bias^2)) + 
             rnorm(1, 0, bias_sd),
           harris = ifelse(result > 0, ev, 0)) |> 
    summarize(harris = sum(harris)) |> 
    pull(harris)
}) 
plot(table(harris_ev))
hist(harris_ev, breaks = seq(150,450,5))
mean(harris_ev  > 269)
mean(harris_ev)

## 80 % credible interval
quantile(harris_ev, c(0.10, 0.90))


## comparison to election night results
res <- read_csv("data/state_vote_differences.csv") |> 
  mutate(Vote_Difference = (2*(Candidate=="Harris") - 1)*Vote_Difference) |>
  mutate(State = str_replace(State, "(\\d+)(st|nd|rd|th) District", "CD-\\1")) |>
  rename(state = State, margin = Vote_Difference) |>
  mutate(margin = margin/100) |>
  select(state, margin) 

tmp <- data.frame(state = c(state.name, "Maine CD-1", "Maine CD-2", "Nebraska CD-1", "Nebraska CD-2", "Nebraska CD-3", "District of Columbia"), 
                  abb = c(state.abb, "ME-1", "ME-2","NE-1", "NE-2","NE-3", "DC"))
results |> mutate(upper = ifelse(n >= 5, avg + qt(0.975, n - 1)*sd/sqrt(n), NA),
                  lower = ifelse(n >= 5, avg - qt(0.975, n - 1)*sd/sqrt(n), NA)) |>
  left_join(res, by = "state") |> 
  left_join(tmp, by = "state") |>
  ggplot(aes(avg*100, margin*100)) +
  geom_errorbar(aes(xmin = lower*100, xmax = upper*100), color = "grey") +
  geom_point() +
  geom_abline() +
  geom_text_repel(aes(label = abb), size = 2, max.overlaps = 15) +
  geom_abline() +
  theme_bw() +
  ylim(-40, 40) +
  xlim(-40, 40) +
  labs(x = "Poll average (Harris - Trump)", y = "Current result", title = "2024 election results versus poll prediction",
       caption = "95% confidence interval included when 5 or more polls.")



### pollster bias
polls |>
  filter(pollster != "Dartmouth Poll") |>
  filter(start_date > make_date(2024, 7, 21)) |>
  mutate(numeric_grade = replace_na(numeric_grade, 0)) |>
  mutate(week = floor(as.numeric(difftime(max(end_date), end_date, units = "week")))) |>
  group_by(state, week, pollster) |>
  summarize(spread = mean(spread), population = first(population), numeric_grade = mean(numeric_grade),
            .groups = "drop") |> 
  filter(week <= 1) |>
  left_join(res, by = "state") |> 
  left_join(tmp, by = "state") |> 
  mutate(bias = (margin - spread)*100) |> 
  mutate(pollster = str_remove(pollster, "/.*$")) |>
  mutate(pollster = str_remove(pollster, "for\\s.*$")) |>
  mutate(pollster = reorder(pollster, bias, mean)) |>
  ggplot(aes(pollster, bias, label = abb)) +
  geom_text(size = 2) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Predicted minus observed Trump margin", x = "Pollster", y = "Bias")

  
