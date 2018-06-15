
# Use ggap to make maps
# Use ggrepel package to ensure site points are well-spaced

set_ggmap_cache <- function() {
  # set cache for ggmap - https://rdrr.io/cran/ggmap/man/file_drawer.html
  # Define using "rprojroot" package
  cache <- file.path(rprojroot::find_root("remake.yml"), "downloads/ggmap")
  if(!file.exists(cache))
    dir.create(cache, recursive = TRUE)
  options(ggmap.file_drawer = cache)
}

# Expand a box to give padding around it
pad_box <- function(bbox, p=0.1, min_deg = 0.5) {
  new_box <- bbox
  new_box[1] <- bbox[1] - max(p*(bbox[3]-bbox[1]), min_deg)
  new_box[3] <- bbox[3] + max(p*(bbox[3]-bbox[1]), min_deg)
  new_box[2] <- bbox[2] - max(p*(bbox[4]-bbox[2]), min_deg)
  new_box[4] <- bbox[4] + max(p*(bbox[4]-bbox[2]), min_deg)
  new_box
}

# Calculate area of bounding box
area_box <- function(bbox) {
  as.numeric((bbox[3]-bbox[1])*(bbox[4]-bbox[2]))
}

# calculate suitable zoom value, based on area of bounding box
# Adds some padding to box to ensure has some area
# for Australia, box are ranges from 1 (minimum with padding, to 1350)
# Zoom values at this scale go from 5 to 10
get_zoom<- function(bbox, zoom_max=10) {
  box_area <- bbox %>% pad_box() %>% area_box()
  cuts <- 10^seq(0, 3.5, length.out = 6)
  zoom_max + 1 - (box_area >= cuts) %>% which() %>% max()
}

# Download data and make a mapr for specified sites
make_map <- function(bbox, sites) {
  sitesMap <- ggmap::get_stamenmap(bbox = bbox %>% pad_box(), zoom = get_zoom(bbox), maptype="terrain")

  ggmap::ggmap(sitesMap) +
    labs(x = "Longitude", y = "Latitude") +
    geom_point(data = sites, aes(x=`longitude (deg)`, y = `latitude (deg)`), colour = "red") +
    ggrepel::geom_text_repel(data = sites, aes(x=`longitude (deg)`, y = `latitude (deg)`, label = site_name))

}
