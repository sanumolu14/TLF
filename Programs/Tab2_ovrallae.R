#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("gt")
#install.packages("gtsummary")
#install.packages("sassy")
# install.packages("tern")
library(dplyr)
library(tidyr)
library(gt)
library(gtsummary)
library(tern)



aesi_vars <- c("FATAL", "SER", "SERWD", "SERDSM", "RELSER", "WD", "DSM", "REL", "RELWD", "RELDSM", "SEV")

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae
ds <- pharmaversesdtm::ds

??ltransmute

# adsl <- adsl %>%
#   left_join(ds %>% filter(DSTERM == "ADVERSE EVENT") %>%  rename(DCSREAS = DSTERM ) %>% select(USUBJID, DCSREAS), by = "USUBJID")
#   
# ds <- ds %>% filter(DSTERM == "ADVERSE EVENT") %>% mutate(DCSREAS = DSTERM) %>% distinct(USUBJID, .keep_all = TRUE)

adsl <- adsl %>% 
        left_join( ds %>% filter(DSTERM == "ADVERSE EVENT") %>% mutate(DCSREAS = DSTERM) %>% distinct(USUBJID, .keep_all = TRUE),
        by = "USUBJID"
        )

# Check whether the function exists
# find("count_patients_with_event")

# Layout for variables from adsl dataset.
lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  count_patients_with_event(
    "USUBJID",
    filters = c("DTHFL" = "Y"),
    denom = "N_col",
    .labels = c(count_fraction = "Total number of deaths")
  ) %>%
  count_patients_with_event(
    "USUBJID",
    filters = c("DCSREAS" = "ADVERSE EVENT"),
    denom = "N_col",
    .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
    table_names = "tot_wd"
  )

result_adsl <- build_table(lyt_adsl, df = adsl, alt_counts_df = adsl)

# Layout for variables from adae dataset.
lyt_adae <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  analyze_num_patients(
    vars = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Total number of AEs"
    ),
    .formats = list(unique = format_count_fraction_fixed_dp, nonunique = "xx"),
    show_labels = "hidden"
  ) %>%
  count_patients_with_flags(
    "USUBJID",
    flag_variables = aesi_vars,
    denom = "N_col",
    var_labels = "Total number of patients with at least one",
    show_labels = "visible"
  )



result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

# Combine tables.
col_info(result_adsl) <- col_info(result_adae)
result <- rbind(
  result_adae[1:2, ],
  result_adsl,
  result_adae[3:nrow(result_adae), ]
)

result