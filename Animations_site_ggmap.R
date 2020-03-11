library(rayshader)
library(raster)
library(sf)
library(ggplot2)
library(dplyr)

localtif = raster::raster("/home/mauricio/Documents/Doutorado/Research/Linhares/3D-plot/Elevation/s20_w040_3arc_v1.tif")

e <- extent(-40, -39.75,-19.7, -19.45)

#e <- extent(-45, -33, -22.5, 17)
#e <- extent(-85, -34, -60, 15)
localtif <- crop(localtif, e)	

values(localtif)[is.na(values(localtif))] <- 0

localtif <- rasterToPoints(localtif) 

localtif <- as.data.frame(localtif)

colnames(localtif)[1] <- "lon"
colnames(localtif)[2] <- "lat"
colnames(localtif)[3] <- "grf"



# *************** Tower  ***************
Linhares = data.frame(lon = c(-39.80083, -39.80153, -39.80110,-39.80055, -39.80010),
                      lat = c(-19.53137, -19.53352, -19.53363,-19.53377, -19.53389),
                      z = c(140, rep(30, 4)))

# *************** Power plant  ***************
powerplant <- data.frame(lon = c(-39.80226, -39.79942,-39.79981, -39.80071, -39.80090, -39.80166, -39.80146,-39.80264),
                         lat = c(-19.53268, -19.53343,-19.53474, -19.53453, -19.53521, -19.53502, -19.53431,-19.53399),
                         z = rep(13.8, 8))

# *************** Power incinerator  ***************

incinerator <- data.frame(lon = c(-39.80153, -39.80110,-39.80055, -39.80010),
                          lat = c(-19.53352, -19.53363,-19.53377, -19.53389),
                          z = rep(30, 4))

# *************** Fake Surface  ***************
library(gstat)

fake_raster <- 
  raster(nrows = 1000, ncols = 1000, res = 0.0001, xmn = -39.805, xmx = -39.795, ymn = -19.54, ymx = -19.53)

library(gstat)

x <- 1:100 # x coordinates
y <- 1:100 # y coordinates
dat <- expand.grid(x=x,y=y) # create data frame with all combinations
dat$z <- 1 # initialize z variable
coordinates(dat) <- ~x+y # set coordinates
gridded(dat) <- TRUE # specify data is gridded
g <- gstat(id='z',formula=z~1,model=vgm(psill=5,model="Gau",range=5),data=dat,dummy=TRUE,beta=10,maxdist=20,nmax=10) # create gstat object
dat <- data.frame(predict(g,newdata=dat,nsim=1)) # simulate random field data

localtif <- 
  setValues(fake_raster, dat$sim1)

#plot(localtif)

localtif <- rasterToPoints(localtif) 

localtif <- as.data.frame(localtif)

colnames(localtif)[1] <- "lon"
colnames(localtif)[2] <- "lat"
colnames(localtif)[3] <- "grf"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# =============== SITE DESCRIPTION ===============
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggnewscale)
library(pals)
library(rgdal)

# *************** Roads  ***************

roads <- 
  readOGR("~/Documents/Doutorado/Research/Linhares/3D-plot/Roads/streets/roads-line.shp")

roads <- fortify(roads, region='id')

themeval = theme(panel.border = element_blank(), 
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 axis.line = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(), 
                 axis.text.y = element_blank(), 
                 legend.key = element_blank(),
                 plot.margin = unit(c(0.5, 0, 0, 0), "cm"))

site <- 
  ggplot()+
  geom_tile(data=localtif, aes(x=lon, y=lat, fill=grf), alpha=0.9, size= 0.1, color = NA) +
  geom_contour(data = localtif, aes(x=lon, y=lat,z = grf, color = stat(level)), color = "black", breaks = seq(0,20,5)) +
  geom_path(data = roads, aes(x = long, y = lat, group = id), size = 4, color = "gray80") + 
  geom_polygon(data = powerplant, aes(x=lon, y=lat, fill  = z), fill = alpha("gray",alpha = 40), size = .1) +
  # geom_rect(data = Linhares, aes(xmin=-39.80087, xmax=-39.80080, ymin=-19.53141, ymax=-19.53135, fill = z),
  #           color = "gray50", size = .1) +
  geom_rect(data = Linhares, aes(xmin=lon-0.00004, xmax=lon+0.00004, ymin=lat-0.00004, ymax=lat+0.00004, fill = z),
            color = "gray50", size = .1) +
  # geom_rect(data = incinerator, aes(xmin=lon-0.00004, xmax=lon+0.00004, ymin=lat-0.00004, ymax=lat+0.00004, fill = z),
  #           color = "gray50", size = .1) +
  scale_fill_gradientn(colours = rev(brewer.rdylgn(10)[c(2:5, 10)]), limits = c(0,150)) +
  #scale_fill_gradient2(low = "#006837", mid = "#FEE08B", high = "#D7191C", limits = c(0,150)) +
  #scale_fill_viridis_c(limits = c(0,150), guide = F) +
  coord_fixed(ratio = 1, expand = F, xlim = c(-39.8045, -39.79758), ylim = c(-19.53756, -19.53024)) +
  labs(x= "Longitude (º)", y="Latitude (º)", fill = "Altitude (m)") +
  themeval

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# =============== SITE DESCRIPTION ===============
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# convert google map to raster object
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


# *************** Image map Linhares ***************

load(file="~/Documents/Doutorado/Research/Linhares/Data/satellite_LN/Linhares_map_Z17.rda")

### Map -> Raster (Three bands)
LN.rast <- ggmap_rast(map = Linhares_map)

LN.rast <- 
  as.data.frame(LN.rast, xy = TRUE) %>% 
  dplyr::rename(R = layer.1,
                G = layer.2,
                B = layer.3) %>% 
  mutate(color = rgb(R/254, G/255, B/254))

# CONVERT COLOR TO HEIGHT
# https://www.maptiler.com/blog/2019/03/rgb-encoded-elevation-data-in-maptiler-cloud.html

LN.rast <-
  LN.rast %>% mutate(z = 10 - ((R * 254 * 255 + G * 254 + B) * 0.0000006))

### Palette color
library(pals)
palette <- c(brewer.rdylgn(10)[c(3,5,9,10)], rev(kovesi.linear_grey_0_100_c0(60)[1:40]))

site <-
  ggplot() +
  # geom_point(data = LN.rast,aes(x = x, y = y, color = color)) +
  # scale_color_identity() +
  #geom_contour(data = LN.rast, aes(x=x, y=y,z = z, color = stat(level)), color = "#1A9850", breaks = seq(0,10,5), size = .3) +
  #geom_path(data = roads, aes(x = long, y = lat, group = id), size = 4, color = "gray80") + 
  geom_polygon(data = powerplant, aes(x=lon, y=lat, fill  = z),  size = .1) +
  #geom_rect(data = Linhares, aes(xmin=lon-0.00004, xmax=lon+0.00004, ymin=lat-0.00004, ymax=lat+0.00004, fill = z), size = .1) +
  ggforce::geom_circle(data = Linhares, aes(x0=lon, y0=lat, r = 0.00005, fill = z)) +
  coord_fixed(ratio = 1, expand = F, xlim = c(-39.80369, -39.79797), ylim = c(-19.53596, -19.53024)) +
  scale_fill_gradientn(colours = palette, limits = c(0,150), breaks = seq(0, 150, 20), name = "Altitude (m)\n",
                      guide = guide_colorbar(title.position = "bottom", label.position = "right")) +
  labs(x= "Longitude", y = "Latitude") +
  theme(legend.position = "left") +
  #theme_void() +
  themeval


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# =============== RENDER OPTION A ===============
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


filename_movie = tempfile()
#Generate animation option 1

ggheight = plot_gg(site, multicore = TRUE, raytrace=F,sunangle = 35, reduce_size = 0.1,
                   height_aes = "fill", shadow_intensity = 0.3, scale = 300, offset_edges = FALSE,
                   width=8,height=7, soliddepth = -20, save_height_matrix = TRUE, 
                   background = "#afceff", shadowcolor= "#4f463c", windowsize=c(1600,1200))

  bounce_title_time <- tween(data = list(c(0,0),c(20,30)), n = 45, ease = "bounce-in") %>% 
    bind_cols() %>% rename(tower = V1, plant=V2)
  
  
  # render_label(ggheight, "Tower", x=1000,y=900,z=300, textcolor = "white",textsize = 2,
  #              dashed = T, linecolor = "darkred", offset = 200)
  # 
  # render_label(ggheight, "Power Plant", x=1000,y=900,z=300, textcolor = "white",textsize = 2,
  #              dashed = T, linecolor = "darkred", offset = 200)
  
    for(i in 1:360) {
    render_camera(phi=20,theta=45+i,fov=70,zoom=0.35)
    #render_camera(phi=50,theta=0,fov=70,zoom=0.45)
    if (i >= 41 & i < 85) {
    
      render_label(clear_previous = TRUE)
      
      render_label(ggheight, "Tower", x=140,y=165, z=bounce_title_time$tower[i-40], textcolor = "black",textsize = 2,
                   dashed = T, linecolor = "darkred", offset = 60)
      
      render_label(ggheight, "Power Plant", x=100,y=90,z=bounce_title_time$plant[i-40], textcolor = "black",textsize = 2,
                   dashed = T, linecolor = "darkred", offset = 20)
    }
      
    if (i >= 85) {
      render_label(clear_previous = TRUE)
      
      render_label(ggheight, "Tower", x=140,y=165, z=bounce_title_time$tower[45], textcolor = "black",textsize = 2,
                   dashed = T, linecolor = "darkred", offset = 60)
      
      render_label(ggheight, "Power Plant", x=100,y=90,z=bounce_title_time$plant[45], textcolor = "black",textsize = 2,
                   dashed = T, linecolor = "darkred", offset = 20)
    }
    
      render_snapshot(filename = paste0("~/Desktop/Video_LN/prova/",glue::glue("snow{i}")),
                      title_text = "LINHARES EXPERIMENTAL SITE", 
                      title_color = "white", title_bar_color = "darkgreen",
                      vignette = TRUE, 
                      title_font = "Helvetica", gravity = "North")
      
    }
  
  png_files <- sprintf("~/Desktop/Video_LN/prova/snow%01d.png", 1:139)
  
  av::av_encode_video(input = rep(png_files, 2), output = "~/Desktop/Video_LN/prova/Option_HQ.mp4", framerate = 20,
                      codec = "libx264",  vfilter = "pad=ceil(iw/2)*2:ceil(ih/2)*2")
  








#Add label
render_label(clear_previous = TRUE)

render_label(ggheight, "Tower", x=1060,y=1650,z=300, textcolor = "white",  textsize = 2, dashed = T,linecolor = "darkred",  offset = 200)

render_label(ggheight, "Power Plant", x=1000,y=900,z=300, textcolor = "white",textsize = 2,dashed = T, linecolor = "darkred", offset = 200)


for(i in 1:360) {
  render_camera(phi=40,theta=0+i,fov=70,zoom=0.45)
  render_snapshot(filename = paste0("~/Desktop/Video_LN/",glue::glue("snow{i}")),
                  title_text = "LINHARES EXPERIMENTAL SITE", 
                  title_color = "white", title_bar_color = "darkgreen",
                  vignette = TRUE, 
                  title_font = "Helvetica", gravity = "North")
  # render_depth(focus= 0.75,
  #              title_text = "Linhares experimental site", 
  #              title_size = 35,
  #              filename = glue::glue("snow{i}"))
}

png_files <- sprintf("~/Desktop/Video_LN/snow%01d.png", 1:360)

rgl::rgl.close()

library(av)

setwd("/home/mauricio/Desktop/GIF/")

#input <- mixedsort(sort(list.files("~/Desktop/GIF/", pattern = "*.png")))

av::av_encode_video(input = rep(png_files, 2), output = "~/Desktop/Video_LN/Option_HQ.mp4", framerate = 30,
                    codec = "libx264",  vfilter = "pad=ceil(iw/2)*2:ceil(ih/2)*2")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# =============== RENDER OPTION B ===============
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Generate animation option 2
#filename_movie = tempfile()

phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = -90 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))


ggheight = plot_gg(site, multicore = TRUE, raytrace=TRUE,sunangle = 30, 
                   height_aes = "fill", shadow_intensity = 0.3, scale = 300,
                   width=8,height=7, soliddepth = -100, save_height_matrix = TRUE, 
                   background = "#f5e9dc", shadowcolor= "#4f463c", windowsize=c(1600,900))


#Add label
render_label(clear_previous = TRUE)

render_label(ggheight, "Tower", x=1050,y=1650,z=300, textsize = 1.5, dashed = T,linecolor = "darkred",  offset = 200)

render_label(ggheight, "Power Plant", x=1000,y=900,z=300, textsize = 1.5, dashed = T, linecolor = "darkred", offset = 200)

# for (i in 1:length(phivecfull)) {
#   render_camera(phi=phivecfull[i],theta=thetavec[i]+90 ,zoom=zoomvecfull[i], fov = 70)
# }


#Un-comment the following to run
render_movie(filename = "~/Desktop/Video_LN/option_oscillate_B.mp4", type = "custom", title_text = "Linhares experimental site", 
             frames = 720*2,  phi = c(rep(phivecfull,2), rev(rep(phivecfull,2))),
             zoom = c(rep(zoomvecfull,2), rev(rep(zoomvecfull,2))),
             theta = c(rep(thetavec,2), rev(rep(thetavec,2))))
rgl::rgl.close()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# =============== RENDER OPTION HIGH QUALITY ===============
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ggheight = plot_gg(site, multicore = TRUE, raytrace=TRUE,sunangle = 190, #reduce_size = 0.1,
                   height_aes = "fill", shadow_intensity = 0.3, scale = 300,offset_edges = FALSE,
                   width=8,height=7, soliddepth = -10, save_height_matrix = TRUE, 
                   background = "gray60", shadowcolor= "#4f463c", windowsize=c(1600,900))

render_camera(phi=50,theta=0,fov=70,zoom=0.45)


#Add label
render_label(clear_previous = TRUE)

render_label(ggheight, "Tower", x=1060,y=1650,z=300, textcolor = "white",  textsize = 2, dashed = T,linecolor = "darkred",  offset = 200)

render_label(ggheight, "Power Plant", x=1000,y=900,z=300, textcolor = "white",textsize = 2,dashed = T, linecolor = "darkred", offset = 200)


for(i in 1:360) {
  render_camera(phi=40,theta=0+i,fov=55,zoom=0.45)
  render_highquality(filename = paste0("~/Desktop/Video_LN/",glue::glue("snow{i}")),
                     ground_material = rayrender::diffuse(color="#000000", noise=1/10,noisecolor = "#654321"),
                     lightdirection = c(190), title_text = "LINHARES EXPERIMENTAL SITE", 
                     title_color = "white", title_bar_color = "darkgreen",
                     title_font = "Helvetica")
  
  # render_snapshot(filename = paste0("~/Desktop/Video_LN/",glue::glue("snow{i}")),
  #                 title_text = "LINHARES EXPERIMENTAL SITE", 
  #                 title_color = "white", title_bar_color = "darkgreen", 
  #                 vignette = TRUE, 
  #                 title_font = "Helvetica", gravity = "North")
  # render_depth(focus= 0.75,
  #              title_text = "Linhares experimental site", 
  #              title_size = 35,
  #              filename = glue::glue("snow{i}"))
}

              png_files <- sprintf("~/Desktop/Video_LN/snow%01d.png", 1:360)

rgl::rgl.close()

library(av)

setwd("/home/mauricio/Desktop/GIF/")

#input <- mixedsort(sort(list.files("~/Desktop/GIF/", pattern = "*.png")))

av::av_encode_video(input = rep(png_files, 2), output = "~/Desktop/Video_LN/Option_3.mp4", framerate = 30,
                    codec = "libx264",  vfilter = "pad=ceil(iw/2)*2:ceil(ih/2)*2")


