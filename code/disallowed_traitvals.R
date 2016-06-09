require(yaml)
require(plyr)

disallowed <- function(df) { # df is remake::make('austraits')$data

constraints <- yaml.load_file('config/categorical_trait_constraints.yml') 
x <- data.frame()
my.list <- vector("list", length(constraints))

for(i in 1:length((constraints))) {

  trait <- names(constraints)[i]
  
  df_trait <- df[df$trait_name == trait,]
  
  disallowed_vals <- df_trait[!df_trait$value %in% constraints[[i]],]
  disallowed_vals <- disallowed_vals[!is.na(disallowed_vals$value),]

  my.list[[i]] <- disallowed_vals

  }

x <- rbind(x, do.call(rbind, my.list))

x <- unique(x[,c('study','trait_name','value')])

return(x)

}

disallowed_traitvals <- disallowed(remake::make('austraits')$data)

