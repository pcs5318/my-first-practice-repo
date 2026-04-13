# =========================================
# US Armed Forces - Correct Final Script
# =========================================

library(tidyverse)
library(googlesheets4)
library(rvest)

gs4_deauth()

sheet_url <- "https://docs.google.com/spreadsheets/d/19xQnI1cBh6Jkw7eP8YQuuicMlVDF7Gr-nXCb5qbwb_E"

# ------------------------------------------------
# 1. Import Sheet (use real header row)
# ------------------------------------------------

armed_raw <- read_sheet(sheet_url)

# If first row accidentally became data, remove it
if(armed_raw$Pay_Grade[1] == "Pay Grade"){
  armed_raw <- armed_raw[-1, ]
}

# ------------------------------------------------
# 2. Tidy Data
# ------------------------------------------------

armed_tidy <- armed_raw %>%
  pivot_longer(
    cols = -Pay_Grade,
    names_to = c("Branch","Sex"),
    names_sep = " ",
    values_to = "Count"
  ) %>%
  drop_na(Count) %>%
  mutate(
    Count = as.numeric(Count),
    Pay_Grade = str_remove_all(Pay_Grade, "-"),
    Pay_Grade = str_trim(toupper(Pay_Grade)),
    Branch = str_trim(Branch)
  )

# ------------------------------------------------
# 3. Scrape Rank Table Properly
# ------------------------------------------------

rank_page <- read_html("https://neilhatfield.github.io/Stat184_PayGradeRanks.html")

rank_table <- rank_page %>%
  html_element("table") %>%
  html_table(fill = TRUE)

# Remove repeated header rows inside table
rank_table <- rank_table %>%
  filter(!`Pay Grade` %in% c("Pay Grade", NA))

colnames(rank_table)[1] <- "Pay_Grade"

rank_lookup <- rank_table %>%
  pivot_longer(
    cols = -Pay_Grade,
    names_to = "Branch",
    values_to = "Rank"
  ) %>%
  mutate(
    Pay_Grade = str_remove_all(Pay_Grade, "-"),
    Pay_Grade = str_trim(toupper(Pay_Grade)),
    Branch = str_trim(Branch)
  )

# ------------------------------------------------
# 4. Merge
# ------------------------------------------------

armed_final <- armed_tidy %>%
  left_join(rank_lookup, by = c("Pay_Grade","Branch")) %>%
  select(Branch, Pay_Grade, Sex, Rank, Count) %>%
  arrange(Branch, Pay_Grade, Sex)

armed_final
