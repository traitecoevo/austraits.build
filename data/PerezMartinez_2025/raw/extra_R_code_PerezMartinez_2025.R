# Writes data/PerezMartinez_2025/data.csv from the authors' tree-level file
# raw/CCAThornhillTraitsretrievedplussampling&ClimateFinal.csv. Run from the repo root.
#   - The author file is Latin-1 encoded with CRLF line endings; data.csv is written as UTF-8 with LF.
#   - Drops the unnamed leading row-number column.
#   - Every other column and value is copied unchanged (read as character, so no rounding).

library(readr)

data <- read_csv("data/PerezMartinez_2025/raw/CCAThornhillTraitsretrievedplussampling&ClimateFinal.csv",
                 col_types = cols(.default = "c"), locale = locale(encoding = "latin1"),
                 na = character(), trim_ws = FALSE, name_repair = "minimal")

stopifnot(names(data)[1] == "", identical(data[[1]], as.character(seq_len(nrow(data)))))
data <- data[-1]

write_csv(data, "data/PerezMartinez_2025/data.csv", na = "", eol = "\n")
