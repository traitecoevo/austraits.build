# Writes data/Hallam_1970/data.csv from raw/trait_data.csv, adding derived columns so
# their values sit alongside the data. Run from the repo root.
#   - source_rank: 1 = Appendix I, 2 = text / Table 3, 3 = Tables 2 and 4. custom_R_code
#     keeps the best-ranked row per taxon x location x trait x leaf type.
#   - basis_of_record: from the Appendix I collection codes (herb_coll_nos), using the
#     paper's code key (Appendix I, p.372). Field: State-prefixed field numbers (WA, NT,
#     SA, Q, N, NSW), Carr, DA and Scrymgeour numbers, and numbers followed by a State
#     initial (minor field collections). Captive_cultivated: botanic garden / plantation
#     suffixes (Cb, Wt, Wl, S, Ch, M, Cu), glasshouse- or seed-grown material, VC, FTBD,
#     and WA86 (planted street trees, Port Hedland). Both -> "field captive_cultivated".
#     Rows with no collection code (text, Tables 2 and 4) default to field.
#   - voucher_MEL / voucher_PERTH: record_number split by holding herbarium. The paper
#     houses its vouchers at MEL; the Scrymgeour and Gardner specimens are at PERTH.

library(dplyr)
library(readr)

data <- read_csv("data/Hallam_1970/raw/trait_data.csv", col_types = cols(.default = "c"))

field_codes <- "(^|; )(WA|NT|N|SA|Q|NSW|DA)[0-9]|(^|; )Carr |Scrymgeour|[0-9](V|T|Q|NSW|SA)($|;)|QSn|NSWSn"
cultivated_codes <- "[0-9](Cb|Wt|Wl|S|Ch|M|Cu)($|;|,|[.])|(Wt|Wl|Cu|Cb)Sn|[Gg]lasshouse|[Ss]eed|\\bVC\\b|FTBD|WA86"
perth_vouchers <- "Scrymgeour|Gardner"

data <- data |>
  mutate(
    source_rank = case_when(
      grepl("^Appendix", source_section) ~ 1,
      grepl("^Table [24]", source_section) ~ 3,
      TRUE ~ 2
    ),
    from_field = grepl(field_codes, herb_coll_nos),
    from_cultivated = grepl(cultivated_codes, herb_coll_nos),
    basis_of_record = case_when(
      location_name %in% c("Port Hedland, WA", "Waite Institute Arboretum, Adelaide, SA") ~ "captive_cultivated",
      from_field & from_cultivated ~ "field captive_cultivated",
      from_cultivated ~ "captive_cultivated",
      TRUE ~ "field"
    ),
    voucher_MEL = if_else(!is.na(record_number) & !grepl(perth_vouchers, record_number), record_number, NA_character_),
    voucher_PERTH = if_else(grepl(perth_vouchers, record_number), record_number, NA_character_)
  ) |>
  select(-from_field, -from_cultivated) |>
  relocate(voucher_MEL, voucher_PERTH, basis_of_record, .after = herb_coll_nos) |>
  relocate(source_rank, .after = source_section)

write_csv(data, "data/Hallam_1970/data.csv", na = "")
