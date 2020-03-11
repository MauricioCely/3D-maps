# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Name:     CASES-99 EXPERIMENTAL SITE ANIMATION
# Author:   Iván Mauricio Cely Toro
# Date:     11-03-2020
# e-mail:   mauriciocelytoro@hotmail.com
# Version:  0.0
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# =============== REQUIRED PACKAGES ===============
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(rayshader)
library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(ggforce)
library(ggmap)
library(tweenr)
library(pals)


# *************** Tower  ***************
towers = data.frame(lat = c(37.64855, 37.64893, 37.64766, 37.64907, 37.64999, 37.64587, 37.64969),
                    lon = c(-96.73610, -96.73507, -96.73620, -96.73703, -96.73870, -96.73640, -96.73302),
                    z = c(55, rep(10,6)),
                    name = c("Tower", paste("station", 1:6)))

# *************** Theme for the map ***************
themeval = theme(panel.border = element_blank(), 
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 axis.line = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(), 
                 axis.text.y = element_blank(), 
                 legend.key = element_blank(),
                 plot.margin = unit(c(0.5, 0, 0, 0), "cm"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# =============== SITE DESCRIPTION ===============
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Function to convert google map to raster object
ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}


# *************** Image map CASES-99 ***************

load(file="~/Documents/Doutorado/Research/Linhares/Data/satellite_LN/Linhares_map_Z17.rda")

### Map -> Raster (Three bands)
CASES_map <- ggmap_rast(map = Linhares_map)

CASES_map <- 
  as.data.frame(CASES_map, xy = TRUE) %>% 
  dplyr::rename(R = layer.1,
                G = layer.2,
                B = layer.3) %>% 
  mutate(color = rgb(R/254, G/255, B/254))

# CONVERT COLOR TO HEIGHT
# https://www.maptiler.com/blog/2019/03/rgb-encoded-elevation-data-in-maptiler-cloud.html

CASES_map <-
  CASES_map %>% mutate(z = 10 - ((R * 254 * 255 + G * 254 + B) * 0.0000006))

### Palette color
library(pals)
palette <- c(rev(brewer.rdylgn(10)[c(1,2,3,4,5,9,10)]))


### For labels
bounce_title_time <- tween(data = list(c(0,0),c(20,30)), n = 50, ease = "elastic-out") %>% 
  bind_cols() %>% rename(tower = V1, plant=V2)

### For towers
bounce_towers <- tween(data = list(c(0,0),c(50,30)), n = 60, ease = "elastic-out") %>% 
  bind_cols() %>% rename(tower = V1, stations=V2)

### EFFECTS
ease_effects <- 
data.frame(time = 1:360,
           tower = c(rep(0,60), bounce_towers$tower, rep(50, 240)),
           stations = c(rep(0,60), bounce_towers$stations, rep(50, 240)),
           label_tower = c(rep(0,150), bounce_title_time$tower, rep(30,160)),
           label_stations = c(rep(0,150), bounce_title_time$plant, rep(20,160))) 



for(i in 60:360) {
  
  site <-
    ggplot() +
    # geom_point(data = CASES_map,aes(x = x, y = y, color = color)) +
    # scale_color_identity() +
    ggforce::geom_circle(data = towers %>% filter(name == "Tower"), 
                         aes(x0=lon, y0=lat, r = 0.00005, fill = ease_effects$tower[i])) +
    ggforce::geom_circle(data = towers %>% filter(name != "Tower"), 
                         aes(x0=lon, y0=lat, r = 0.00005, fill = ease_effects$stations[i])) +
    # geom_text(data = towers, aes(x=lon, y=lat, label = name)) +
    coord_fixed(ratio = 1, expand = F) +
    scale_fill_gradientn(colours = palette, limits = c(0,80), breaks = seq(0, 80, 20), name = "Altitude (m)\n",
                         guide = guide_colorbar(title.position = "bottom", label.position = "right")) +
    labs(x= "Longitude", y = "Latitude") +
    theme(legend.position = "left") +
    #theme_void() +
    themeval
  
  rgl::rgl.clear()
  
  #Generate animation option 1
  
  ggheight = plot_gg(site, multicore = TRUE, raytrace=F,sunangle = 35, #reduce_size = 0.1,
                     height_aes = "fill", shadow_intensity = 0.3, scale = 300, offset_edges = FALSE,
                     width=8,height=7, soliddepth = -20, save_height_matrix = TRUE, 
                     background = "#afceff", shadowcolor= "#4f463c", windowsize=c(1600,1200))
  
  render_camera(phi=20,theta=45+i,fov=70,zoom=0.35)
  #render_camera(phi=50,theta=0,fov=70,zoom=0.45)
  # if (i >= 41 & i < 85) {
  #   
  #   render_label(clear_previous = TRUE)
  #   
  #   render_label(ggheight, "Tower", x=140,y=165, z=bounce_title_time$tower[i-40], textcolor = "black",textsize = 2,
  #                dashed = T, linecolor = "darkred", offset = 60)
  #   
  #   render_label(ggheight, "Power Plant", x=100,y=90,z=bounce_title_time$plant[i-40], textcolor = "black",textsize = 2,
  #                dashed = T, linecolor = "darkred", offset = 20)
  # }
  # 
  # if (i >= 85) {
  #   render_label(clear_previous = TRUE)
  #   
  #   render_label(ggheight, "Tower", x=140,y=165, z=bounce_title_time$tower[45], textcolor = "black",textsize = 2,
  #                dashed = T, linecolor = "darkred", offset = 60)
  #   
  #   render_label(ggheight, "Power Plant", x=100,y=90,z=bounce_title_time$plant[45], textcolor = "black",textsize = 2,
  #                dashed = T, linecolor = "darkred", offset = 20)
  # }
  
  render_snapshot(filename = paste0("~/Desktop/Video_LN/prova2/",glue::glue("snow{i}")),
                  title_text = "LINHARES EXPERIMENTAL SITE", 
                  title_color = "white", title_bar_color = "darkgreen",
                  vignette = TRUE, 
                  title_font = "Helvetica", gravity = "North")
  
}

png_files <- sprintf("~/Desktop/Video_LN/prova/snow%01d.png", 1:139)

av::av_encode_video(input = rep(png_files, 2), output = "~/Desktop/Video_LN/prova/Option_HQ.mp4", framerate = 20,
                    codec = "libx264",  vfilter = "pad=ceil(iw/2)*2:ceil(ih/2)*2")



