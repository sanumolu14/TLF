#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("gt")

library(dplyr)
library(tidyr)
library(gt)

adams_path <- path.expand("C:/Users/suvar/Rworkdr")
adsl <- get(load(paste0(adams_path, "/admiral_adsl.rda"))) %>% 
filter(TRT01P != "")

adsl <- adsl %>%  mutate("TRT01PN" = case_when( 
                                                #TRT01P == "Screen Failure" ~ 1,
                                                TRT01P == "Placebo" ~ 1,
                                                TRT01P == "Xanomeline High Dose" ~ 2,
                                                TRT01P == "Xanomeline Low Dose" ~ 3
                                              ))

adsl_overall <- adsl %>% filter(TRT01P != "Screen Failure") %>% 
  mutate(TRT01P = "Overall", TRT01PN = 4)
adsl_pre <- bind_rows(adsl, adsl_overall)

denom <- adsl_pre %>% 
  group_by(TRT01P, TRT01PN) %>%
  summarise(denom = n_distinct(USUBJID), .groups = "drop")

function_name <- function(out, variable) {
  var_name <- deparse(substitute(variable))
  adsl_final <- adsl_pre %>%
    select(USUBJID, TRT01P, TRT01PN, {{ variable }})
  num <- adsl_final %>%
    group_by(TRT01P, TRT01PN) %>%
    summarise(num = sum({{ variable }} == "Y", na.rm = TRUE), .groups = "drop")
  prep <- left_join(num, denom, by = c("TRT01P", "TRT01PN")) %>%
    mutate(
      pct = round(num / denom * 100, 2),
      calc = case_when(
        is.na(num) | num == 0 ~ "O",
        num == denom ~ paste0(num, " (100%)"),
        TRUE ~ paste0(num, " (", pct, "%)")),
      col = paste0("t", TRT01PN))
  
  final_out <- prep %>%
  select(col, calc) %>%
    pivot_wider(names_from = col, values_from = calc) %>%
    mutate(
      Population = var_name,
      Statistic = "n (%)") %>%
    relocate(Population, Statistic)
  
  assign(deparse(substitute(out)), final_out, envir = .GlobalEnv)}

function_name(out01, SAFFL)
#function_name(out02, DLTEVLFL)
#function_name(out03, PKEVLFL)

#final <- bind_rows(out01, out02, out03) %>% 
final <- out01 %>% 
select(Population, Statistic, t1, t2, t3, t4)

final %>%
  gt() %>%
  cols_label(
    t1 = "Placebo",
    t2 = "Xanomeline High Dose",
    t3 = "Xanomeline Low Dose",
    #t4 = "Drug 4",
    t4 = "Overall"
  ) %>% 
  tab_header(
       title = md(" ** Table 14.1.3 Subject Assignment to Analysis Populations ** " )
            ) %>%
tab_options(
        table.font.names = "Courier, monospace"
        )
