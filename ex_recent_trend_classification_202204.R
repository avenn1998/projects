library(tidycensus)
library(fst)
library(jsonlite)
library(httr)
library(snakecase)
library(ggformula)
library(tidytext)
library(janitor) 
library(tidyverse)
library(data.table)
library(tidycensus)
library(mgcv)
library(gratia)
library(lubridate) 
library(Rnssp)

#read Rnssp profile - ESSENCE
myProfile <- Credentials$new(
  username = askme("Enter your username: "),
  password = askme()
)


r_state <- function(path){
  
  lag <- 3
  
  endDate <- Sys.Date() - lag
  startDate <- as.Date("2020-04-30")
  
  date_seq <- seq(startDate, endDate, length.out = 3)
  
  labelEndDate <- format(lubridate::floor_date(Sys.Date(), unit = "1 week") - 1, "%B %d, %Y")
  labelStartDate <- "May 01, 2020"
  
  cat <- c('cdc%20covid-specific%20dd%20v1','cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2')
  cat_display <- c("CDC COVID-Specific DD v1", "CLI CC with CLI DD and Coronavirus DD v2")
  
  out <- list()
  
  idx <- 1
  
  for(i in 1:length(cat)){
    for(j in 1:(length(date_seq)-1)){
      
      startDate <- (date_seq[j]+1) %>% format('%d%b%Y')
      endDate <- (date_seq[j+1]) %>% format('%d%b%Y')
      
      url <- paste0("https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=", endDate, "&percentParam=ccddCategory&datasource=va_hosp&startDate=", startDate, "&medicalGroupingSystem=essencesyndromes&userId=2362&aqtTarget=TimeSeries&ccddCategory=", cat[i], "&geographySystem=hospitalstate&detector=probrepswitch&timeResolution=daily&hasBeenE=1&stratVal=&multiStratVal=geography&graphOnly=true&numSeries=2&graphOptions=multipleSmall&seriesPerYear=false&nonZeroComposite=false&removeZeroSeries=false&startMonth=January")
      out[[idx]] <- pluck(myProfile$get_api_data(url), "timeSeriesData") %>% mutate(ccdd_category = cat_display[i])
      idx <- idx + 1
    }
  }
  
  out <- do.call(rbind, out)
  
  ccdd_long <- out %>%
    select(date, 
           ccdd_category, 
           state = hospitalstate_display,
           data_count = dataCount,
           all_count = allCount,
           percent = count, 
           expected, 
           levels, 
           color) %>%
    mutate(color = ifelse(color == "grey", "blue", color)) %>%
    mutate(date = as.Date(date)) %>%
    mutate(ccdd_category = to_any_case(ccdd_category, case = "snake")) %>%
    arrange(date, state, ccdd_category) %>%
    mutate(ccdd_category = str_replace_all(ccdd_category, "v_1", "v1"),
           ccdd_category = str_replace_all(ccdd_category, "v_2", "v2")) %>%
    filter(!str_detect(state, "Samoa|Guam|Virgin Islands|Hawaii|OTHER|Puerto|Maryland")) # Maryland still down as of April 2022
  
  # > CLI by hospital state---------------------------------------------------------------------------------------------
  cli_states_by_time <- ccdd_long %>%
    filter(ccdd_category == "cli_cc_with_cli_dd_and_coronavirus_dd_v2") %>%
    arrange(state, date) %>%
    select(
      date, 
      state,
      data_count, 
      all_count,
      percent,
      color
    ) %>%
    nest(data = -state) %>%
    mutate(
      model = map(.x = data, .f = function (.x) {
        
        sparsity_ratio90 <- .x %>%
          arrange(date) %>%
          slice_tail(n = 90) %>%
          mutate(
            nonzero_obs = sum(data_count != 0),
            sp_ratio = n() / nonzero_obs
          ) %>%
          pull(sp_ratio) %>%
          unique()
        
        sparsity_ratio <- .x %>%
          arrange(date) %>%
          filter(date >= max(date) %m-% years(1) + 1) %>%
          mutate(
            nonzero_obs = sum(data_count != 0),
            sp_ratio = n() / nonzero_obs
          ) %>%
          pull(sp_ratio) %>%
          unique()
        
        if (sparsity_ratio < 5 & sparsity_ratio90 < 5) {
          
          .y <- .x %>%
            select(
              date,
              data_count,
              all_count
            ) %>%
            mutate(date = as.double(date))
          
          .sp_knots <- floor(nrow(.x) / 21)
          
          .gam_out <- gam(cbind(data_count, all_count - data_count) ~ s(as.double(date), bs = "ad", k = .sp_knots), family = binomial, data = .x, method = "REML")
          
          if (.gam_out$converged) {
            
            .deriv <- derivatives(.gam_out, newdata = .y, n = nrow(.y), level = 0.95, type = "central")
            
            data.frame(
              fitted = .gam_out$fitted.values * 100,
              sp_ratio90 = sparsity_ratio90,
              sp_ratio = sparsity_ratio
            ) %>%
              bind_cols(.deriv) %>%
              rename(deriv = derivative)
            
          } else {
            
            data.frame(
              fitted = rep(NA, nrow(.x))
            ) %>%
              mutate(
                sp_ratio90 = sparsity_ratio90,
                sp_ratio = sparsity_ratio, 
                lower = NA,
                deriv = NA,
                upper = NA
              )
            
          }
          
          
        } else {
          
          data.frame(
            fitted = rep(NA, nrow(.x))
          ) %>%
            mutate(
              sp_ratio90 = sparsity_ratio90,
              sp_ratio = sparsity_ratio,
              lower = NA,
              deriv = NA,
              upper = NA
            )
          
        }
        
      })
    ) %>%
    unnest(c(model, data)) %>%
    group_by(state) %>%
    mutate(
      row = row_number(),
      trajectory = case_when(
        is.na(sp_ratio90) ~ "Sparse",
        sp_ratio90 >= 5 | sp_ratio >= 5 ~ "Sparse",
        deriv > 0 & lower > 0 ~ "Increasing",
        deriv < 0 & upper < 0 ~ "Decreasing",
        deriv > 0 & lower < 0 ~ "Stable",
        deriv < 0 & upper > 0 ~ "Stable",
        is.na(deriv) ~ "Sparse",
        TRUE ~ "Stable"
      )
    ) %>%
    group_by(state, grp = with(rle(trajectory), rep(seq_along(lengths), lengths))) %>%
    mutate(
      period_total = max(seq_along(grp)),
      counter = seq_along(grp),
      start_date = min(date)
    ) %>%
    ungroup() %>%
    mutate(trajectory = factor(trajectory, levels = c("Increasing", "Stable", "Decreasing", "Sparse")))
  
  cli_increasing <- cli_states_by_time %>%
    filter(date == max(date)) %>%
    filter(trajectory == "Increasing") %>%
    mutate(
      `Recent Trend` = trajectory, 
      category = "CLI Chief Complaint plus Dx"
    )
  
  cli_decreasing <- cli_states_by_time %>%
    filter(date == max(date)) %>%
    filter(trajectory == "Decreasing") %>%
    mutate(
      `Recent Trend` = trajectory, 
      category = "CLI Chief Complaint plus Dx"
    )
  
  cli_stable <- cli_states_by_time %>%
    filter(date == max(date)) %>%
    filter(trajectory == "Stable") %>%
    mutate(
      `Recent Trend` = trajectory, 
      category = "CLI Chief Complaint plus Dx"
    )
  
  cli_sparse <- cli_states_by_time %>%
    filter(date == max(date)) %>%
    filter(trajectory == "Sparse") %>%
    mutate(
      `Recent Trend` = trajectory, 
      category = "CLI Chief Complaint plus Dx"
    )
  
  # COVID-DD by hospital state------------------------------------------------------------------------------------------
  covid_states_by_time <- ccdd_long %>%
    filter(ccdd_category == "cdc_covid_specific_dd_v1") %>%
    arrange(state, date) %>%
    select(date, 
           state,
           data_count, 
           all_count,
           percent,
           color) %>%
    nest(data = -state) %>%
    mutate(
      model = map(.x = data, .f = function (.x) {
        
        sparsity_ratio90 <- .x %>%
          arrange(date) %>%
          slice_tail(n = 90) %>%
          mutate(
            nonzero_obs = sum(data_count != 0),
            sp_ratio = n() / nonzero_obs
          ) %>%
          pull(sp_ratio) %>%
          unique()
        
        sparsity_ratio <- .x %>%
          arrange(date) %>%
          filter(date >= max(date) %m-% years(1) + 1) %>%
          mutate(
            nonzero_obs = sum(data_count != 0),
            sp_ratio = n() / nonzero_obs
          ) %>%
          pull(sp_ratio) %>%
          unique()
        
        if (sparsity_ratio < 5 & sparsity_ratio90 < 5) {
          
          .y <- .x %>%
            select(
              date,
              data_count,
              all_count
            ) %>%
            mutate(date = as.double(date))
          
          .sp_knots <- floor(nrow(.x) / 21)
          
          .gam_out <- gam(cbind(data_count, all_count - data_count) ~ s(as.double(date), bs = "ad", k = .sp_knots), family = binomial, data = .x, method = "REML")
          
          if (.gam_out$converged) {
            
            .deriv <- derivatives(.gam_out, newdata = .y, n = nrow(.y), level = 0.95, type = "central")
            
            data.frame(
              fitted = .gam_out$fitted.values * 100,
              sp_ratio90 = sparsity_ratio90,
              sp_ratio = sparsity_ratio
            ) %>%
              bind_cols(.deriv) %>%
              rename(deriv = derivative)
            
          } else {
            
            data.frame(
              fitted = rep(NA, nrow(.x))
            ) %>%
              mutate(
                sp_ratio90 = sparsity_ratio90,
                sp_ratio = sparsity_ratio, 
                lower = NA,
                deriv = NA,
                upper = NA
              )
            
          }
          
          
        } else {
          
          data.frame(
            fitted = rep(NA, nrow(.x))
          ) %>%
            mutate(
              sp_ratio90 = sparsity_ratio90,
              sp_ratio = sparsity_ratio,
              lower = NA,
              deriv = NA,
              upper = NA
            )
          
        }
        
      })
    ) %>%
    unnest(c(model, data)) %>%
    group_by(state) %>%
    mutate(
      row = row_number(),
      trajectory = case_when(
        is.na(sp_ratio90) ~ "Sparse",
        sp_ratio90 >= 5 | sp_ratio >= 5 ~ "Sparse",
        deriv > 0 & lower > 0 ~ "Increasing",
        deriv < 0 & upper < 0 ~ "Decreasing",
        deriv > 0 & lower < 0 ~ "Stable",
        deriv < 0 & upper > 0 ~ "Stable",
        is.na(deriv) ~ "Sparse",
        TRUE ~ "Stable"
      )
    ) %>%
    group_by(state, grp = with(rle(trajectory), rep(seq_along(lengths), lengths))) %>%
    mutate(
      period_total = max(seq_along(grp)),
      counter = seq_along(grp),
      start_date = min(date)
    ) %>%
    ungroup() %>%
    mutate(trajectory = factor(trajectory, levels = c("Increasing", "Stable", "Decreasing", "Sparse")))
  
  covid_increasing <- covid_states_by_time %>%
    filter(date == max(date)) %>%
    filter(trajectory == "Increasing") %>%
    mutate(
      `Recent Trend` = trajectory, 
      category = "COVID-Specific Dx"
    )
  
  covid_decreasing <- covid_states_by_time %>%
    filter(date == max(date)) %>%
    filter(trajectory == "Decreasing") %>%
    mutate(
      `Recent Trend` = trajectory, 
      category = "COVID-Specific Dx"
    )
  
  covid_stable <- covid_states_by_time %>%
    filter(date == max(date)) %>%
    filter(trajectory == "Stable") %>%
    mutate(
      `Recent Trend` = trajectory, 
      category = "COVID-Specific Dx"
    )
  
  covid_sparse <- covid_states_by_time %>%
    filter(date == max(date)) %>%
    filter(trajectory == "Sparse") %>%
    mutate(
      `Recent Trend` = trajectory, 
      category = "COVID-Specific Dx"
    )
  
  main_state_table <- bind_rows(
    cli_increasing, 
    cli_decreasing, 
    cli_stable, 
    cli_sparse, 
    covid_increasing, 
    covid_decreasing, 
    covid_stable,
    covid_sparse
  ) %>%
    select(
      State = state,
      `Recent Trend` = trajectory, 
      Category = category
    ) %>%
    mutate(
      HHSRegion = case_when(
        State %in% c("Massachusetts", "Connecticut", "Maine", "New Hampshire", "Rhode Island", "Vermont") ~ "HHS Region 1",
        State %in% c("New Jersey", "New York") ~ "HHS Region 2",
        State %in% c("Delaware", "Maryland", "Pennsylvania", "Virginia", "West Virginia", "District of Columbia") ~ "HHS Region 3",
        State %in% c("Alabama", "Florida", "Georgia", "Kentucky", "Mississippi", "North Carolina", "South Carolina", "Tennessee") ~ "HHS Region 4",
        State %in% c("Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin") ~ "HHS Region 5",
        State %in% c("Arkansas", "Louisiana", "New Mexico", "Oklahoma", "Texas") ~ "HHS Region 6",
        State %in% c("Iowa", "Kansas", "Missouri", "Nebraska") ~ "HHS Region 7",
        State %in% c("Colorado", "Montana", "North Dakota", "Utah", "Wyoming", "South Dakota") ~ "HHS Region 8",
        State %in% c("Arizona", "California", "Nevada") ~ "HHS Region 9",
        State %in% c("Alaska", "Idaho", "Oregon", "Washington") ~ "HHS Region 10")
    )
  
  return(main_state_table)
  
}
