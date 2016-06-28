require(plyr)
require(readr)
require(remake)
require(reshape2)
require(knitr)
require(scales)

#setwd('../')

# make 'austraits' if it isn't already loaded
if(!exists('austraits')) {
  austraits <- remake::make('austraits')
    } else {
        print('austraits loaded')
}

# calculates coefficient of variation
CV <- function(x){
  sqrt(var(x))/mean(x)
}

definitions_traits_numeric <- subset(read_csv('config/definitions_traits.csv'), type == "numeric")

########

CV <- function(x){
  sqrt(var(x))/mean(x)
}

# does dataset contain numeric data?

has_numeric <- function(df) {
  any(unique(df$trait_name) %in% definitions_traits_numeric$trait_name)
}


## SUMMARY TABLE FUNCTIONS

summ <- function(df) {
  
  ddply(df, .(trait_name), summarise, 
        units = paste0(unique(unit)), 
        N.records = length(value), 
        N.species = length(unique(species_name))
  )
  
}

# format data for dotchart and pair plots

format_data <- function(id) { 
  
#  id <- deparse(substitute(id))
  study_data <- subset(austraits$data, study == id)
#  study_metadata <- subset(austraits$metadata, dataset_id == id)
  study_data <- study_data[!is.na(study_data$value),]
  
  definitions_traits_numeric <- subset(read_csv('config/definitions_traits.csv'), type == "numeric")
  data_all <- austraits$data

  study_traits <- unique(study_data[study_data$trait_name %in% unique(definitions_traits_numeric$trait_name),]$trait_name) # numeric traits from our target study please
  study_traits_alldata <- data_all[data_all$trait_name %in% study_traits,] # and pull out all austraits data for those traits
  study_traits_alldata <- study_traits_alldata[!is.na(study_traits_alldata$value),]
  
  # aggregate to mean vals per species/trait across all data
  #study_traits_alldata <- study_traits_alldata[!study_traits_alldata$study == id,]  # remove target dataset
  study_traits_alldata$value <- as.numeric(study_traits_alldata$value)
  
  study_traits_alldata <- ddply(study_traits_alldata, .(species_name, trait_name, unit), summarise, 
                                trait_mean = mean(value), trait_CV = CV(value))
  # study_traits_alldata <- study_traits_alldata[study_traits_alldata$trait_CV < 0.5,] # remove records with unrealistic intraspecific variation
  study_traits_alldata <- study_traits_alldata[!is.na(study_traits_alldata$trait_mean),]
  study_traits_alldata_wide <- dcast(study_traits_alldata, species_name ~ trait_name, value.var = 'trait_mean', fun.aggregate=function(x) paste(x, collapse = ", "))
  study_traits_alldata_wide$target <- 'all data'
  
  # add in non-aggregated data from target study
  study_data_numeric <- study_data[study_data$trait_name %in% unique(definitions_traits_numeric$trait_name),]
  study_data_numeric$seq <- with(study_data_numeric, ave(value, species_name, trait_name, study, FUN = seq_along)) #adds a sequence number for multiple values within datasets so I can cast long to wide without aggregating
  study_data_numeric_wide <- dcast(study_data_numeric, study + seq + species_name ~ trait_name, value.var = 'value', fun.aggregate=function(x) as.character(x)[1])
  study_data_numeric_wide$seq <- NULL
  study_data_numeric_wide$study <- NULL
  study_data_numeric_wide$target <- paste(id)
  
  # put it all together
  formatted <- rbind(study_data_numeric_wide,study_traits_alldata_wide)
  
  if(ncol(formatted) > 3) { # lapply breaks if there's only one column to apply a function to
    formatted[,2:(ncol(formatted)-1)] <- lapply(formatted[,2:(ncol(formatted)-1)], as.numeric)
  } else {
    formatted[,2] <- as.numeric(formatted[,2])
  }
  
  formatted$target <- as.factor(formatted$target)
  
  return(formatted)
  
}





pairwise_panel <- function(id, df) {
  
  if(ncol(df) > 2) { # catch studies with only one numeric trait (can't be plotted) 
    # this used be > 3 for no good reason. changed to 2 but might cause a bug somewhere one day?
    
    panel.hist <- function(x, ...) {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5) )
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
    }
    
    col.rainbow <- rainbow(2:3)
    palette(col.rainbow)
    
    if(ncol(df) %in% c(3:7)) { # break df into two if there are too many traits to plot pairwise
      
      pairs(log10(df[,2:(ncol(df)-1)]), panel = panel.smooth,
            cex = 2, pch = 24, bg = df$target,
            diag.panel = panel.hist, cex.labels = 2, font.labels = 2,
            main = paste('Pairwise plots for', id, sep = " "))
      
     } else { 
       
       if(ncol(df) %in% c(8:ncol(df))) {
        
      df1 <- df[,1:7]
      df2 <- df[,c(1,8:ncol(df))]
      
      pairs(log10(df1[,2:ncol(df1)]), panel = panel.smooth,
            cex = 2, pch = 24, bg = df$target,
            diag.panel = panel.hist, cex.labels = 2, font.labels = 2,
            main = paste('(1) Pairwise plots for', id, sep = " "))
      
      pairs(log10(df2[,2:(ncol(df2)-1)]), panel = panel.smooth,
            cex = 2, pch = 24, bg = df$target,
            diag.panel = panel.hist, cex.labels = 2, font.labels = 2,
            main = paste('(2) Log10 pairwise plots for', id, sep = " "))
      }
      
    }
      
  } else {
    
    print('Dataset contains only 1 trait. Unable to plot pairwise diagnostic')
  }
  
}  

function(id, df) {
  
  # id <- deparse(substitute(id))
  
  if(ncol(df) > 3){
    panel_dims <- ceiling(sqrt(length(df[,2:(ncol(df)-1)])))
  } else {
    panel_dims <- 1
  }
  
  # col.rainbow <- rainbow(2:3)
  #  palette(col.rainbow)
  
  par(mfrow=c(panel_dims,panel_dims),
      oma = c(2,1,0,1) + 0.1,
      mar = c(2,1,2,1) + 0.1
      ,bg = 'white'
  )
  
  for(i in 2:(ncol(df)-1)) {
    dotchart(log10(df[,i]), gcolor = par(df$target), groups = df$target, color = df$target, main = colnames(df)[i], lcolor = 'white', pch=20)
  }
  
}

