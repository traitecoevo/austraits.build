
pallette1 <- function(){
  c('red', 'seagreen3', 'steelblue3', 'yellow2')
}

# calculates coefficient of variation
CV <- function(x){
  sqrt(var(x))/mean(x)
}

## SUMMARY TABLE FUNCTIONS

summ <- function(df) {
  df %>% group_by(trait_name) %>%
    summarise(
        units = paste0(unique(unit)), 
        N.records = length(value), 
        N.species = length(unique(species_name))
        )
}

# format data for dotchart and pair plots

format_data <- function(id, study_data, austraits, definitions_traits_numeric) {
  
  study_data <- subset(austraits$data, study == id)
  study_data <- study_data[!is.na(study_data$value),]
  
  data_all <- austraits$data
  
  study_traits <- unique(study_data[study_data$trait_name %in% unique(definitions_traits_numeric$trait_name),]$trait_name) # numeric traits from our target study please
  study_traits_alldata <- data_all[data_all$trait_name %in% study_traits,] # and pull out all austraits data for those traits
  study_traits_alldata <- study_traits_alldata[!is.na(study_traits_alldata$value),]
  
  # aggregate to mean vals per species/trait across all data
  study_traits_alldata$value <- as.numeric(study_traits_alldata$value)
  
  study_traits_alldata <- study_traits_alldata %>%
    group_by(species_name, trait_name, unit) %>%
    summarise(
      trait_mean = mean(value),
      trait_CV = CV(value)) %>%
    ungroup()
  
  # study_traits_alldata <- study_traits_alldata[study_traits_alldata$trait_CV < 0.5,] # remove records with unrealistic intraspecific variation
  study_traits_alldata <- study_traits_alldata[!is.na(study_traits_alldata$trait_mean),]
  study_traits_alldata_wide <- study_traits_alldata %>% 
    select(- trait_CV, - unit) %>%
    spread(key = trait_name, value = trait_mean) %>%
    mutate(target = 'all_data')

  # add in non-aggregated data from target study
  study_data_numeric <- study_data[study_data$trait_name %in% unique(definitions_traits_numeric$trait_name),]
  study_data_numeric$seq <- with(study_data_numeric, ave(value, species_name, trait_name, study, FUN = seq_along)) #adds a sequence number for multiple values within datasets so I can spread long to wide without aggregating
  study_data_numeric_wide <- study_data_numeric %>%
    select(species_name, trait_name, value, seq) %>%
    spread(key = trait_name, value = value) %>%    
    select(- seq) %>%
    mutate(target = paste(id))

  # put it all together
  formatted <- rbind(study_data_numeric_wide,study_traits_alldata_wide)
  
  # make the trait data numeric class
  if(ncol(formatted) > 3) { # lapply breaks if there's only one column to apply a function to
    formatted[,2:(ncol(formatted)-1)] <- lapply(formatted[,2:(ncol(formatted)-1)], as.numeric)
  } else {
    formatted[[2]] <- as.numeric(formatted[[2]])
  }
  
  formatted <- arrange(formatted, target)
  formatted$target <- as.factor(formatted$target)
  
  return(formatted)
  
}


dotchart_single <- function(trait, id, data_all, heights=c(1,4)) {

  data_trait <- data_all %>%
                filter(trait_name == trait) %>%
                mutate(value=as.numeric(value)) %>%
                filter(!is.na(value)) %>%
                arrange(study)

  data_summary <- data_trait %>%
        group_by(study) %>%
        summarize(
          np = length(trait_name)) %>%
        ungroup() %>%
        mutate(
          spread = sqrt(np)/sum(sqrt(np)),
          y_av = cumsum(spread)-0.5*spread,
          col=rep(c("b", "c", "d"), length.out=length(study))
          )
  data_summary$col[data_summary$study == id] <- "a"

  data_join <-
      full_join(filter(data_trait, trait_name == trait_name), data_summary, by="study") %>%
      mutate(y = y_av + runif(length(y_av), -0.45, 0.45)*spread)

  Xlab <- paste0(gsub("_", " ", data_join$trait_name), " (", data_join$unit, ")")[1]

  # Histogram at top
  p1 <- ggplot(data_join, aes(x=value)) +
          geom_histogram(aes(y = ..density..), color="darkgrey", fill="darkgrey") +
          geom_density(color="black") +
          xlab("") + ylab("All data") +
          scale_x_log10(
                          breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +

          theme(legend.position = "none",
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks.y=element_blank(),
              axis.text=element_blank(),
              panel.background = element_blank(),
              plot.margin= unit(c(1, 1, -1, 1), "lines"))

  # Dotchart with plot by study
  p2 <- ggplot(data_join, aes(x = value, y = y, colour = col)) +
          geom_point(alpha = 0.6) +
          scale_colour_manual(values=pallette1())  +
          scale_x_log10(
                          breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
          scale_y_continuous(breaks=data_summary$y_av, labels=data_summary$study) +
          xlab(Xlab) + ylab("By study") +
          theme(legend.position = "none",
                axis.title.x = element_text(),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                plot.margin= unit(c(0, 1, 1, 1), "lines"))

  # Fix width of second plot to be same as bottom using ggplot_table
  p1b <- ggplot_gtable(ggplot_build(p1))
  p2b <- ggplot_gtable(ggplot_build(p2))
  p1b$widths[2:3] <- p2b$widths[2:3]
  grid.arrange(p1b, p2b, nrow=2, widths=c(1), heights=heights)
}

pairwise_panel <- function(id, df) {
  
  
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
              main = '(2)')
        
      }
      
    }
    
  } else {
    
    print('Dataset contains only 1 trait. Unable to plot pairwise diagnostic')
  }
  
}


flags <- function(id, austraits, definitions_traits_numeric) {

  all <- austraits$data
  
  numeric_data <- unique(all[all$trait_name %in% unique(definitions_traits_numeric$trait_name),]$trait_name)
  numeric_data <- all[all$trait_name %in% numeric_data,] # and pull out all austraits data
  numeric_data <- numeric_data[!numeric_data$value_type %in% c('min', 'lower_quantile'),] # remove min and lower quantile records for range traits (e.g. plant_height, leaf_width)
  numeric_data <- numeric_data[!numeric_data$value == 0,]
  numeric_data_ <- numeric_data[!duplicated(numeric_data[,c('species_name', 'value')]),]
  
  numeric_data.CV <- numeric_data %>% 
    dplyr::group_by(species_name, trait_name, value_type) %>%
    dplyr::summarise(CV = CV(as.numeric(value)),
                     mean = mean(as.numeric(value)),
                     fractional_range = max(as.numeric(value)) / min(as.numeric(value)),
                     count = length(value)) 
  
  numeric_data.CV <- na.omit(numeric_data.CV)
  
  divergent_records.CV <- merge(subset(numeric_data.CV, CV > 0.6), numeric_data, by = c("species_name", "trait_name", "value_type"))
  divergent_records.range <- merge(subset(numeric_data.CV, fractional_range > 10), numeric_data, by = c("species_name", "trait_name", "value_type"))
  
  flagged <- divergent_records.range[divergent_records.range$species_name %in% subset(all, study == id)$species_name &
                                    divergent_records.range$trait_name %in% subset(all, study == id)$trait_name,]
  flagged$CV <- NULL
  flagged$mean <- NULL
  flagged$count <- NULL
  flagged$replicates <- NULL
  flagged$precision <- NULL
  flagged$methodology_ids <- NULL
  flagged$fractional_range <- round(flagged$fractional_range, 2)
 
  return(flagged)
   
}



md_link <- function(text, link) {
  sprintf("[%s](%s)", text, link)
}
md_link_doi <- function(doi) {
  md_link(doi, paste0("http://doi.org/", doi))
}

## returns up to 80 nice colours, generated using
## http://tools.medialab.sciences-po.fr/iwanthue/
nice_colours <- function(n=80) {
  cols <- c(
    "#75954F", "#D455E9", "#E34423", "#4CAAE1", "#451431", "#5DE737", "#DC9B94",
    "#DC3788", "#E0A732", "#67D4C1", "#5F75E2", "#1A3125", "#65E689", "#A8313C",
    "#8D6F96", "#5F3819", "#D8CFE4", "#BDE640", "#DAD799", "#D981DD", "#61AD34",
    "#B8784B", "#892870", "#445662", "#493670", "#3CA374", "#E56C7F", "#5F978F",
    "#BAE684", "#DB732A", "#7148A8", "#867927", "#918C68", "#98A730", "#DDA5D2",
    "#456C9C", "#2B5024", "#E4D742", "#D3CAB6", "#946661", "#9B66E3", "#AA3BA2",
    "#A98FE1", "#9AD3E8", "#5F8FE0", "#DF3565", "#D5AC81", "#6AE4AE", "#652326",
    "#575640", "#2D6659", "#26294A", "#DA66AB", "#E24849", "#4A58A3", "#9F3A59",
    "#71E764", "#CF7A99", "#3B7A24", "#AA9FA9", "#DD39C0", "#604458", "#C7C568",
    "#98A6DA", "#DDAB5F", "#96341B", "#AED9A8", "#55DBE7", "#57B15C", "#B9E0D5",
    "#638294", "#D16F5E", "#504E1A", "#342724", "#64916A", "#975EA8", "#9D641E",
    "#59A2BB", "#7A3660", "#64C32A")
  cols[seq_len(n)]
}

is_wholenumber <- function(x, tol=.Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

axis_log10 <- function(side=1, horiz=FALSE, labels=TRUE,
                       wholenumbers=TRUE, labelends=TRUE, las=1) {
  fg <- par("fg")
  if (side == 1 | side == 3) {
    r <- par("usr")[1:2]  #upper and lower limits of x-axis
  } else {
    r <- par("usr")[3:4]  #upper and lower limits of y-axis
  }

  at <- pretty(r)
  if (!labelends) {
    at <- at[at > r[1] & at < r[2]]
  }
  if (wholenumbers) {
    at <- at[is_wholenumber(at)]
  }

  if (labels) {
    lab <- do.call(expression, lapply(at, function(i) bquote(10^.(i))))
    axis(side, at=10^at, lab, col=if (horiz) fg else NA,
         col.ticks=fg, las=las)
  } else {
    axis(side, at=10^at, FALSE, col=if (horiz) fg else NA,
         col.ticks=fg, las=las)
  }
}
