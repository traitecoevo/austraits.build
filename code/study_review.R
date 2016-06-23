require(plyr)
require(readr)
require(remake)
require(reshape2)

#setwd('../')
austraits <- remake::make('austraits')

# calculates coefficient of variation
CV <- function(x){
  sqrt(var(x))/mean(x)
}

definitions_traits_numeric <- subset(read_csv('config/definitions_traits.csv'), type == "numeric")

########

## SUMMARY TABLE FUNCTIONS

summary_table <- function() {
  
  ddply(study_data, .(trait_name), summarise, 
        units = paste0(unique(unit)), 
        N.records = length(value), 
        N.species = length(unique(species_name))
        )

}

blah <- study_data %>%
         group_by(trait_name) %>%
         summarise(N.records = length(value), N.species = length(unique(species_name)), unit = paste0(unique(unit)))

blah <- summarise(group_by(study_data, trait_name), N.records = length(value), N.species = length(unique(species_name)))

######### 

## TRAIT-WISE DIAGNOSTICS

numeric_data <- data_all[data_all$trait_name %in% definitions_traits_numeric$trait_name,]
numeric_data$value <- as.numeric(numeric_data$value)

x <- 1

for(i in x) {
  
  target_trait <- numeric_data[numeric_data$trait_name == unique(numeric_data$trait_name)[i],]
  
  dotchart(log10(target_trait$value), 
           groups = as.factor(target_trait$study), 
           color = as.factor(target_trait$study), 
           main = unique(target_trait$trait_name), 
           lcolor = 'white',
           cex = 0.8,
           cex.axis = 0.2)
  
  x <- x + 1
}

#########

## STUDY-WISE DIAGNOSTIC PLOT FUNCTIONS ##

format_data <- function(id) { 

  id <- deparse(substitute(id))
  study_data <- subset(austraits$data, study == id)
  study_metadata <- subset(austraits$metadata, dataset_id == id)
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
  all <- rbind(study_data_numeric_wide,study_traits_alldata_wide)

  if(ncol(all) > 3) { # lapply breaks if there's only one column to apply a function to
    all[,2:(ncol(all)-1)] <- lapply(all[,2:(ncol(all)-1)], as.numeric)
  } else {
    all[,2] <- as.numeric(all[,2])
  }
  
  all$target <- as.factor(all$target)

  return(all)
   
}


pairwise_panel <- function(id) {
  
  if(ncol(all) > 3) {
    
    id <- deparse(substitute(id))
    
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
    
    pairs(log10(all[,2:(ncol(all)-1)]), panel = panel.smooth,
          cex = 1, pch = 24, bg = all$target,
          diag.panel = panel.hist, cex.labels = 1, font.labels = 1,
          main = paste('Pairwise plots for', id, sep = " "))
  } else {
    
    print('Dataset contains only 1 trait. Unable to plot pairwise diagnostic')
  }
  
}  
 
dotcharts <- function(id) {
  
  id <- deparse(substitute(id))
 
  if(ncol(all) > 3){
    panel_dims <- ceiling(sqrt(length(all[,2:(ncol(all)-1)])))
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
  
  for(i in 2:(ncol(all)-1)) {
    dotchart(log10(all[,i]), groups = all$target, color = all$target, main = colnames(all)[i], lcolor = 'white')
  }

}


all <- format_data(dataset_066)
pairwise_panel(dataset_066)
dotcharts(dataset_066)
  