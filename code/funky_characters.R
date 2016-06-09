#find funky characters in numeric trait values

require(readr)

a <- remake::make('austraits')$data

defs <- read_csv('config/definitions_traits.csv')

numeric <- defs[defs$type == 'numeric',]$trait_name

b <- a[a$trait_name %in% numeric,]
unique(b$trait_name)

reg <- "[^[:digit:][:space:]\\.\\-]" # anything that isn't a number, . or -

c <- b[grepl(reg, b$value),]



