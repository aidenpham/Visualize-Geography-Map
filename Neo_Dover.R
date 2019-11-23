#Call the required libraries
library(rgdal)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)

#Data of Dover city was chosen. Setting up the directory:
lcp_dir = 'D:/Year 2 Quarter 1/EPA1315 Data Analytics/Neo Lab/Dover/'
los_dir = 'D:/Year 2 Quarter 1/EPA1315 Data Analytics/Neo Lab/E07000108/'

#Read and plot Dover map:
lsoas_link = paste(lcp_dir, "shapefiles/Dover_lsoa11.shp", sep = "")
lsoas = readOGR(lsoas_link)
plot(lsoas)

# Read file and plot Railway Tunnel in Dover:
los_dir_rwy = paste(los_dir, "RailwayTunnel.shp", sep = "")
rwy_tun = readOGR(los_dir_rwy)

plot(rwy_tun)
proj4string(rwy_tun)

#Read and plot named places in Dover:
los_dir_namp = paste(los_dir, "NamedPlace.shp", sep = "")
namp = readOGR(los_dir_namp)

plot(namp)

#Modify transparency of the map:
plot(lsoas, col = rgb(0, 0.4, 0.8, alpha = 0.3))
plot(lsoas, col = rgb(0, 0.4, 0.8, alpha = 0.9))

#Show the axes:
plot(lsoas, col = rgb(0, 0.4, 0.8, alpha = 0.3), axes = TRUE)

#Add a title:
plot(lsoas, col = rgb(0, 0.4, 0.8, alpha = 0.3), main = "Dover Polygons", axes = TRUE)

#Use ggplot:
ggplot() +
  geom_path(data = lsoas, aes(x = long, y = lat, group = group)) +
  labs(title = "Dover - using ggplot")

#Fix the coordinates:
ggplot() +
  geom_path(data = lsoas, aes(x = long, y = lat, group = group)) + 
  coord_fixed() + 
  labs(title = "My awesome ggplot map of Dover",
       subtitle = "My awesome subtitle",
       x = "", y = "")

#Remove the axes:
ggplot() +
  geom_path(data = lsoas, aes(x = long, y = lat, group = group)) + 
  coord_fixed() + 
  labs(title = "My awesome ggplot map of Dover",
       subtitle = "My awesome subtitle",
       x = "", y = "") +
  theme_void()

#Transform Coordinate Reference System:
proj4string(lsoas)
lsoas_transform <- spTransform(lsoas, CRS("+proj=longlat +init=epsg:4326"))
proj4string(lsoas_transform)

ggplot() +
  geom_path(data = lsoas_transform, aes(x = long, y = lat, group = group)) + 
  coord_fixed() + 
  labs(title = "My awesome ggplot map of Dover",
       subtitle = "My awesome subtitle",
       x = "", y = "")

#Add the tunnel layer on top of polygon layer:
ggl = ggplot(data = lsoas) +
  geom_path(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(x = long, y = lat, group = group)) +
  coord_fixed() + 
  labs(title = "My awesome ggplot map of Dover",
       subtitle = "My awesome subtitle",
       x = "", y = "") 

ggl +
  geom_path(data = rwy_tun, aes(x = long, y = lat, group = group))

#Adjust the color for better visibility:
ggl = ggplot(data = lsoas) +
  geom_path(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = rgb(0, 0, 0.3, alpha = 0.1)) +
  coord_fixed() + 
  labs(title = "My awesome ggplot map of Dover",
       subtitle = "My awesome subtitle",
       x = "", y = "") + 
  theme_void()

ggl +
  geom_path(data = rwy_tun, aes(x = long, y = lat, group = group), color = "black")

#Display colors:
display.brewer.all()
names(wes_palettes)
wes = wes_palette(n=3, name="GrandBudapest1")
wes

#Read more data from OS Pack:
tidW = readOGR(paste(los_dir, "TidalWater.shp", sep = ""))
tidB = readOGR(paste(los_dir, 'TidalBoundary.shp', sep = ""))
funS = readOGR(paste(los_dir, 'FunctionalSite.shp', sep = ""))
road = readOGR(paste(los_dir, 'Road.shp', sep = ""))

#Add all layers:
ggplot() +
  # Add tidal water (remove boundary lines for the polygons)
  geom_polygon(data = tidW, aes(x = long, y = lat, group = group), fill = "#618A98", size = 0) + 
  # Add tidal boundaries
  geom_polygon(data = tidB, aes(x = long, y = lat, group = group), color = "#D5E3D8") + 
  # Add LSOAs
  geom_polygon(data = lsoas, aes(x = long, y = lat, group = group), fill = "#F9DA95", color = "#F9DA95", size = 0) +
  # Add roads
  geom_path(data = road, aes(x = long, y = lat, group = group), color = "#AE4B16", size = 0.1) + 
  # Add functional sites (remove boundary lines for the polygons)
  geom_polygon(data = funS, aes(x = long, y = lat, group = group), color = "#787064", size = 0) + 
  # Impose same size for units across axes
  coord_fixed() + 
  # Add your titles
  labs(title = "Map of Dover",
       subtitle = "Tidal water & boundary, Road 
       and Functional Sites",
       x = "", y = "") +
  theme_void()

#Save map:
ggsave("dover_polygons_all.png")
ggsave("dover_polygons_all.pdf")
ggsave(ggl, file = "dover_railway_tunels.png", dpi = 1080)

#Set the path of population data:
tab_path = 'D:/Year 2 Quarter 1/EPA1315 Data Analytics/Neo Lab/Population/'

#Read file and display table:
tab_path_file = paste(tab_path, "dover_pop.csv", sep = "")
lsoa_orig_sub = read.csv(tab_path_file, header = TRUE, sep = ",")
 
lsoa_orig_sub$Total = rowSums(lsoa_orig_sub[,-1])

head(lsoa_orig_sub)

head(lsoas@data)

#Merge 2 tables:
library(rgeos)
library(maptools)
lsoas_df = fortify(lsoas, region = "LSOA11CD")
head(lsoas_df)

merged_Data = merge(lsoas_df, lsoa_orig_sub, by.x = "id", by.y = "LSOA11CD")

head(merged_Data)

#Plot the chorophleth:
ggplot() +
  # Add tidal water (remove boundary lines for the polygons)
  geom_polygon(data = tidW, aes(x = long, y = lat, group = group), fill = "#618A98", size = 0) + 
  # Add tidal boundaries
  geom_polygon(data = tidB, aes(x = long, y = lat, group = group), color = "#D5E3D8") + 
  # Add LSOAs but merged with population this time
  geom_polygon(data = merged_Data, aes(x = long, y = lat, group = group, fill = Total), color = "white", size = 0.25) +
  # Impose same size for units across axes
  coord_fixed() + 
  # Add your titles
  labs(title = "Dover Map",
       subtitle = "Population Density",
       x = "", y = "") +
  theme_void()

ggsave("Dover_Population.pdf")

#Spatial manipulation
#Centroid:
cent = gCentroid(lsoas, byid=TRUE)
plot(cent)

#Buffer:
proj4string(namp)
buf = gBuffer(namp, width = 500, byid = TRUE)
head(buf@data)
namp_df = data.frame(namp)
head(namp_df)

ggplot() +
  # Add tidal water (remove boundary lines for the polygons)
  geom_polygon(data = tidW, aes(x = long, y = lat, group = group), fill = "#618A98", size = 0) + 
  # Add tidal boundaries
  geom_polygon(data = tidB, aes(x = long, y = lat, group = group), color = "#D5E3D8") + 
  # Add LSOAs but merged with population this time
  geom_polygon(data = buf, aes(x = long, y = lat, group = group), fill = "#618A98", color = "white", size = 0.25, alpha = 0.3) +
  geom_point(data = namp_df, aes(x = coords.x1, y = coords.x2, fill = classifica), color = "#618A98", size = 0.1) +
  # Impose same size for units across axes
  coord_fixed() + 
  # Add your titles
  labs(title = "Dover Map",
       subtitle = "Dover",
       x = "", y = "") +
  theme_void()