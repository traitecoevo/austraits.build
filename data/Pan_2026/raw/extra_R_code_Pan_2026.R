# Writes data/Pan_2026/data.csv from the authors' Zenodo data file (raw xlsx), adding derived
# columns so their values sit alongside the data. Run from the repo root.
# Values are read from the xlsx at full precision (a Save As csv from Excel rounds them to the
# digits displayed).
#   - seed_provenance: seed source locality and native seed supplier for each species, from
#     Table S2 of the primary reference.

library(dplyr)
library(readr)
library(readxl)

data <- read_excel("data/Pan_2026/raw/Pan_2026 New Phytologist NPH-MS-2026-55394 DATA.xlsx",
                   sheet = "Table1")

data <- data |>
  mutate(
    seed_provenance = case_when(
      Species == "Eucalyptus porosa" ~ "Yorke Peninsula, SA (Nindethana)",
      Species == "Eucalyptus paniculata" ~ "Nowra, NSW (Seedworld Australia Pty Ltd)",
      Species == "Eucalyptus leucoxylon leucoxylon" ~ "Kapunda, SA (Nindethana)",
      Species == "Eucalyptus tricarpa" ~ "Whroo, Vic (Australian Tree Seed Centre)",
      Species == "Eucalyptus largiflorens" ~ "Lake Albacutya, Vic (Australian Tree Seed Centre)",
      Species == "Eucalyptus moluccana" ~ "Hunter Valley, NSW (Seedworld Australia Pty Ltd)",
      Species == "Eucalyptus formanii formanii" ~ "no seed source information (Nindethana)",
      Species == "Eucalyptus foecunda foecunda" ~ "Yanchep, WA (Nindethana)",
      Species == "Eucalyptus regnans" ~ "Vic (Seedworld Australia Pty Ltd)",
      Species == "Eucalyptus nitida" ~ "cultivated (Nindethana)",
      Species == "Eucalyptus viridis" ~ "Dubbo, NSW (Australian Tree Seed Centre)",
      Species == "Eucalyptus populnea" ~ "no seed source information (Ole Lantana’s Seed Store)",
      Species == "Eucalyptus neglecta" ~ "cultivated (Nindethana)",
      Species == "Eucalyptus propinqua" ~ "Northern NSW (Seedworld Australia Pty Ltd)",
      Species == "Eucalyptus stellulata" ~ "Guthega, NSW (Australian Tree Seed Centre)",
      Species == "Eucalyptus stricta" ~ "no seed source information (Nindethana)",
      Species == "Eucalyptus longirostrata" ~ "no seed source information (Queensland Native Seeds)",
      Species == "Eucalyptus grandis" ~ "Brisbane, Qld (Australian Seed Company)",
      Species == "Eucalyptus dawsonii" ~ "Denman, NSW (Australian Seed Company)",
      Species == "Eucalyptus orgadophila" ~ "no seed source information (Queensland Native Seeds)",
      Species == "Eucalyptus cneorifolia" ~ "Boston Island, SA (Australian Seed Company)",
      Species == "Eucalyptus bakeri" ~ "no seed source information (Queensland Native Seeds)"
    )
  )

stopifnot(!any(is.na(data$seed_provenance)))

write_csv(data, "data/Pan_2026/data.csv", na = "")
