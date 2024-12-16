make_table <- function(tests, hosp_mort, 
                       start_date = first_day, 
                       end_date = last_complete_day, 
                       type = "Molecular+Antigen"){
  
  tmp <- select(hosp_mort, date, HospitCOV19, IncMueSalud, CamasICU, mort_week_avg)
  
  ret <- tests |>
    filter(testType == "Molecular+Antigens") |>
    left_join(tmp, by = "date") |>
    filter(date >= start_date & date <= end_date) |>
    select(date, IncMueSalud, mort_week_avg, CamasICU, HospitCOV19, cases_rate_daily, cases_rate, cases, cases_week_avg, tests_total, tests_total_week) |>
    mutate(across(c(cases_rate_daily, cases_rate), ~ paste0(round(.x*100), "%"))) |>
    mutate(cases_week_avg = round(cases_week_avg), mort_week_avg = round(mort_week_avg,1)) |>
    setNames(c("Date", "Deaths", "Death weekly average", "ICU", "Hospitaliztions", "Daily positivity", "Weekly positivity", "Daily cases", "Cases weekly average", "Tests", "Weekly tests")) |>
    arrange(desc(Date)) |>
    DT::datatable()
  
 return(ret)
}


plot_positivity <- function(tests, 
                           start_date = first_day, 
                           end_date = last_day){
  

  dat <- tests |>
    filter(testType == "Molecular+Antigens" &
             date >= start_date & date <= end_date) 
  
  
  ret <- dat |>
    ggplot(aes(date, cases_rate_daily*100)) + 
    geom_point(alpha = 0.65, show.legend = FALSE) +
    ylab("Positivity") +
    xlab("Date") +
    theme_bw() +
    geom_line(aes(y = cases_rate*100), linewidth = 0.80) +
    scale_x_date(date_labels = "%b%Y") +
    ylim(c(0,40)) 
  
  return(ret)
}


plot_positivity_2 <- function(tests, 
                            start_date = first_day, 
                            end_date = last_day){
  
  
  dat <- tests |>
    filter(testType == "Molecular+Antigens" &
             date >= start_date & date <= end_date) |>
      mutate(hover_text = paste0(
        format(date, "%B %e, %y"), "<br>",
        "Cases: ", cases, "<br>",
        "Tests: ", tests_total, "<br>",
        "Daily Positivity Rate: ", round(cases_rate_daily * 100, 2), "%<br>",
        "Cumulative Positivity Rate: ", round(cases_rate * 100, 2), "%"
      ))
  
  ret <- dat |>
    ggplot(aes(date, cases_rate_daily*100, text = hover_text)) + 
    geom_point(alpha = 0.65, show.legend = FALSE) +
    ylab("Positivity") +
    xlab("Date") +
    theme_bw() +
    geom_line(aes(y = cases_rate*100), size = 0.80, show.legend = FALSE) +
    scale_x_date(date_labels = "%b%Y") +
    ylim(c(0,40))
  
  ret <- ggplotly(ret, tooltip = "text")
  ret
  return(ret)
}


