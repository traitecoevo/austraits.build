# Writes data/Anderegg_2020/data.csv from the author's raw/TraitsAll_branch_20200701.csv
# (also at https://github.com/leanderegg/EucTraits/tree/master/data). Run from the repo root.
#   - The author file has CR-only line endings; data.csv is written with LF.
#   - Drops the unnamed leading row-number column and the six unnamed, entirely empty
#     columns (between WD and Stem_Di, and between Flag_Area and Tot_dry).
#   - Every other column and value is copied unchanged (read as character, so no rounding).

library(readr)

lines <- strsplit(read_file("data/Anderegg_2020/raw/TraitsAll_branch_20200701.csv"), "\r\n|\r|\n")[[1]]

data <- read_csv(I(paste(lines, collapse = "\n")), col_types = cols(.default = "c"),
                 na = character(), trim_ws = FALSE, name_repair = "minimal")

unnamed <- names(data) == ""
stopifnot(all(sapply(data[unnamed][-1], function(x) all(x == ""))))
data <- data[!unnamed]

write_csv(data, "data/Anderegg_2020/data.csv", na = "", eol = "\n")
