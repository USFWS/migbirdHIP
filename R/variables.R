# Define variables that change with each hunting season

# Current hunting season (the year when the season opens)
REF_CURRENT_SEASON <- "2025"

# Regular expression for a birth date in the MM/DD/YYYY format, allowing months from
# 01-12, days from 01-31, and years from 1926 to 2027
REGEX_BIRTHDATE <-
  paste0(
    "^(0[1-9]|1[0-2])\\/(0[1-9]|[12][0-9]|3[01])\\/(19[1-9][0-9]|20[0-1][0-9]|",
    "202[0-7])$")

# migbirdHIP package versions and their intended HIP season
REF_RELEASES <-
  c("dev" = "1.2.0",
    "dev" = "1.2.1",
    "dev" = "1.2.2",
    "dev" = "1.2.3",
    `2021-2022` = "1.2.4",
    "dev" = "1.2.5",
    "dev" = "1.2.6",
    `2022-2023` = "1.2.7",
    `2023-2024` = "1.2.8",
    `2024-2025` = "1.3.0",
    "dev" = "1.4.0",
    "dev" = "1.4.1",
    "dev" = "1.4.2",
    "dev" = "1.4.3",
    "dev" = "1.4.4",
    "dev" = "1.4.5",
    "dev" = "1.4.6",
    "dev" = "1.4.7",
    "dev" = "1.4.8",
    "dev" = "1.4.9",
    "dev" = "1.4.10",
    "dev" = "1.4.11",
    "dev" = "1.4.12",
    `2025-2026` = "1.4.13"
  )
