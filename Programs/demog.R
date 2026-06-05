##install.packages("flextable")

## ---------Libraries-------------##
library(flextable)
library(tidyverse) ## dplyr already included in tidyverse package
library(dplyr)
####-------------Clear environment--------------##

rm(list = ls(all.names = TRUE))

base_path <- "C:/Users/suvar/Rworkdr"
out_path <- "C:/Users/suvar/Rworkdr/TLF/outputs"



adsl <-  get(load(file.path(base_path, "admiral_adsl.rda"))) 
class(adsl)

adsl <- adsl %>%
  filter(SAFFL == "Y") %>%
  mutate(TRT01P = gsub(" ", "_", TRT01P))



bign <- adsl %>%
  group_by(TRT01P)%>%
  summarise(N=n_distinct(USUBJID), .group="drop")

create_freq <- function(data, var, label){
  data %>%
    mutate(var= .data[[var]]) %>%
    group_by(TRT01P, var) %>%
    summarise(n=n_distinct(USUBJID), .group="drop") %>%
    left_join(bign, by=c("TRT01P")) %>%
    mutate(
      n= ifelse(is.na(n), 0, n),
      pct=((n/N)*100),
      cnt=sprintf("%d (%.1f%%)", n, pct),
      row_text = var,   # values (M/F)
      category = label # header (Sex)
    ) %>%
    select (TRT01P, category, row_text, cnt)
}

agegr <- create_freq(adsl, "AGEGR1", "Age group")
sex <- create_freq(adsl, "SEX", "Sex")
race <- create_freq(adsl, "RACE", "Race")
ethnic <- create_freq(adsl, "ETHNIC", "Ethnic")

agegr <- agegr %>% mutate(row_text = paste0(row_text, " years"))

create_transpose <- function(data) {
  pivot_wider (data,
               id_cols=c("category","row_text"),
               values_from= cnt,
               names_from= TRT01P,
               values_fill = "0 (0.0%)"
  ) %>% 
    
    mutate(
      order= case_when (
        category == "Age group" & row_text == "18-64 years" ~ 1,
        category == "Age group" & row_text == ">64 years" ~ 2,
        # category == "Age group" & row_text == "40-49 years" ~ 3,
        # category == "Age group" & row_text == "50-65 years" ~ 4,
        # category == "Age group" & row_text == ">65 years" ~ 5,
        
        category == "Sex" & row_text == "M" ~ 6,
        category == "Sex" & row_text == "F" ~ 7,
        
        category == "Race" & row_text == "WHITE" ~ 8,
        category == "Race" & row_text == "BLACK OR AFRICAN AMERICAN" ~ 9,
        category == "Race" & row_text == "ASIAN" ~ 10,
        category == "Race" & row_text == "UNKNOWN" ~ 11,
        
        category == "Ethnic" & row_text == "HISPANIC OR LATINO" ~ 12,
        category == "Ethnic" & row_text == "NOT HISPANIC OR LATINO" ~ 13,
        category == "Ethnic" & row_text == "UNKNOWN" ~ 14,
        
        TRUE ~99
      )         
    ) %>%
    arrange(category, order)
}

agegr2 <- create_transpose(agegr)

sex2 <- create_transpose(sex)
race2 <- create_transpose(race)
ethnic2 <- create_transpose(ethnic)

age_stat <- adsl %>%
  group_by(TRT01P) %>%
  summarise(
    n = sum(!is.na(AGE)),
    mean= mean (AGE, na.rm=TRUE),
    sd=sd(AGE, na.rm=TRUE),
    med=median(AGE, na.rm=TRUE),
    min=min(AGE, na.rm=TRUE),
    max=max(AGE, na.rm=TRUE)
  ) %>%
  mutate(
    category="AGE",
    n= as.character(n),
    mean_sd= sprintf("%.1f (%.2f)", mean, sd),
    median= sprintf("%.1f", med),
    min_max= sprintf("(%d, %d)", min, max)
  ) %>%
  select(TRT01P, category, n, mean_sd, median, min_max)

age_stat2 <- pivot_longer (age_stat,
                           cols = c(n, mean_sd, median, min_max),
                           names_to ="row_text",
                           values_to= c("cnt")
)
age_stat3 <- create_transpose(age_stat2)  

sex2 <- sex2 %>% mutate(row_text = str_replace_all(
  row_text,
  c(
    "F" = "Female" ,
    "M" = "Male" 
  )
)
)


tbl <- bind_rows (
  data.frame(category="Age group", row_text="Age group", Placebo="",Xanomeline_High_Dose="",Xanomeline_Low_Dose=""), agegr2,
  data.frame(category="Age", row_text="Age", Placebo="",Xanomeline_High_Dose="",Xanomeline_Low_Dose=""), age_stat3,
  data.frame(category="Sex", row_text="Sex", Placebo="",Xanomeline_High_Dose="",Xanomeline_Low_Dose=""), sex2,
  data.frame(category="Race", row_text="Race", Placebo="",Xanomeline_High_Dose="",Xanomeline_Low_Dose=""), race2,
  data.frame(category="Ethnic", row_text="Ethnic", Placebo="",Xanomeline_High_Dose="",Xanomeline_Low_Dose=""), ethnic2
) %>%  
  mutate(
    row_text=ifelse(row_text == category, row_text, paste0("  ", row_text))
  ) %>%
  select(-category, -order)

tbl <- tbl %>% mutate(row_text = str_replace_all(
                      row_text,
                      c(
                        "mean_sd" = "mean (sd)",
                        "min_max" = "(Min, Max)"
                       )
                      )
                    )
# Convert to flextable
ft <- flextable(tbl)

#to get the bign value in header
bign_vec <- setNames (bign$N, bign$TRT01P)


# Remove ugly column name
ft <- set_header_labels(
  ft,
  row_text = "",
  Placebo = paste0("Placebo \n(N=", bign_vec["Placebo"], ")"),
  Xanomeline_High_Dose = paste0("Xanomeline High Dose\n(N=", bign_vec["Xanomeline_High_Dose"], ")"),
  Xanomeline_Low_Dose = paste0("Xanomeline Low Dose\n(N=", bign_vec["Xanomeline_Low_Dose"], ")")
  
)
# Bold header
ft <- bold(ft, part = "header")

# Align columns
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- align(ft, j= 2:4, align = "center", part = "all")

# Set column widths
ft <- width(ft, j=1, width = 3.5)
ft <- width(ft, j = 2:4, width = 1.5)

# Add indentation for subcategories
# ft <- padding(ft, i = ~ grepl("years", row_text), j = 1, padding.left = 20)

# Add title
ft <- add_header_lines (
  ft,
  "Table 14.1.1: Demographic Summary by Treatment"
)   
# Add footnotes
ft <- add_footer_lines (ft, c(
  "Source: ADSL Dataset",
  "Percentages are based on N"
))    
# Improve font
ft <- fontsize(ft, size = 9)

# Wrap text
ft <- width(ft, j=1, width = 3)

# Borders (SAS-like look)
ft <- border_remove(ft)
ft <- hline_top(ft, border = officer :: fp_border (width = 1))
ft <- hline(ft, part = "header", border = officer :: fp_border(width = 1))
ft <- hline_bottom(ft, border = officer :: fp_border (width = 1))

# Save RTF
save_as_rtf(ft, path = file.path(out_path, "t_demog.rtf"))
