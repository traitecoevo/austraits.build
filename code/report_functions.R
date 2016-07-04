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
 # study_traits_alldata <- study_traits_alldata[!study_traits_alldata$study == id,]  # remove target dataset
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
  
  palette(c(rgb(0.19,0.19,0.19, alpha = 0.5),'blue'))
  
  if(ncol(df) > 2) {
    
    panel.hist <- function(x, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5) )
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
    }
    
    col.rainbow <- rainbow(2:3)
    palette(col.rainbow)
    
    if(ncol(df) %in% c(3:7)) {
      
      pairs(log10(df[,2:(ncol(df)-1)]), panel = panel.smooth,
            cex = 2, pch = 21, bg = df$target,
            diag.panel = panel.hist, cex.labels = 2, font.labels = 2)
      
    } else { 
      
      if(ncol(df) %in% c(8:ncol(df))) {
        
#        df1 <- df[,1:6]
#        df2 <- df[,c(1,2,7:ncol(df))]
        
        size <- (ncol(df)-2)/2
        df1 <- df[,2:(size+1)]
        df2 <- df[,(size+2):(ncol(df)-1)]           
        
        pairs(log10(df1), panel = panel.smooth,
              cex = 2, pch = 21, bg = df$target,
              diag.panel = panel.hist, cex.labels = 2, font.labels = 2,
              main = '(1)')
        
        pairs(log10(df2), panel = panel.smooth,
              cex = 2, pch = 21, bg = df$target,
              diag.panel = panel.hist, cex.labels = 2, font.labels = 2,
              main = '(2)')
        
      }
      
    }
    
  } else {
    
    print('Dataset contains only 1 trait. Unable to plot pairwise diagnostic')
  }
  
}



dotcharts <- function(id, df) {
  
  palette(c(rgb(0.19,0.19,0.19, alpha = 0.3),rgb(1,0,0,alpha = 0.4)))
  
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





pairwise_panel2 <- function(id, df) {
  
  
  if(ncol(df) > 3) {
    
    panel.hist <- function(x, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5) )
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, ...)
    }
    
    col.rainbow <- rainbow(2:3)
    palette(col.rainbow)
    
    if(ncol(df) %in% c(3:7)) {
      
      palette(c(rgb(0.19,0.19,0.19, alpha = 0.2),rgb(1,0,0,alpha = 0.6)))
      
      pairs(log10(df[,2:(ncol(df)-1)]), 
            cex = 2, pch = 20, bg = df$target,
            diag.panel = panel.hist, cex.labels = 2, font.labels = 2, col = df$target)
      
    } else { 
      
      if(ncol(df) %in% c(8:ncol(df))) {
        
        #        df1 <- df[,1:6]
        #        df2 <- df[,c(1,2,7:ncol(df))]
        
        size <- (ncol(df)-2)/2
        df1 <- df[,2:(size+1)]
        df2 <- df[,(size+2):(ncol(df)-1)]           
        
        palette(c(rgb(0.19,0.19,0.19, alpha = 0.2),rgb(1,0,0,alpha = 0.6)))
        
        pairs(log10(df1),
              cex = 2, pch = 20, bg = df$target,
              diag.panel = panel.hist, cex.labels = 2, font.labels = 2, col = df$target,
              main = '(1)')
        
        pairs(log10(df2), 
              cex = 2, pch = 20, bg = df$target,
              diag.panel = panel.hist, cex.labels = 2, font.labels = 2, col = df$target,
              main = ('1'))
        
      }
      
    }
    
  } else {
    
    print('Dataset contains only 1 trait. Unable to plot pairwise diagnostic')
  }
  
}
