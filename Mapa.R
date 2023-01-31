library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
Sur_America     <- st_read ("SHP/SurAmerica.shp")  
SurAmerica_utm  <- st_transform(Sur_America ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Amazonas     <- st_read ("SHP/Cuenca Amazonas.geojson")  
Amazona  <- st_transform(Amazonas ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))







bio        <- getData("worldclim", var = "bio", res=2.5)
Prec_SA    <- crop(bio, SurAmerica_utm)
Prec_SA    <- Prec_SA <- mask(Prec_SA,SurAmerica_utm)
plot(Prec)
Prec = Prec_SA$bio12

Prec_data_frame = as.data.frame(Prec, xy =TRUE) %>% drop_na()

colores2<- c("#8D1A01",   "#dc2f02",  "#DCEEEE", "#48cae4", "#0C8FE1", "#0C8FE1", "#137FB7","#137FB7","#08306B","#08306B")
colores2<- c("#911805", "#DB471D", "#E46355", "#FCBDB5", "#DBEEF2", "#78C8E3", "#0B90E1", "#1780B1", "#08326C")

library(elevatr)
elev = get_elev_raster(SurAmerica_utm , z=4)

Poligo_alt    <- crop(elev, SurAmerica_utm )                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, SurAmerica_utm )


slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

library(ggnewscale)

library(dplyr)
library(scales)
bivariate_color_scale <- tibble(
  "3 - 3" = "#E46355", # high inequality, high income
  "2 - 3" = "#DB471D",
  "1 - 3" = "#911805", # low inequality, high income
  "3 - 2" = "#78C8E3",
  "2 - 2" = "#DBEEF2", # medium inequality, medium income
  "1 - 2" = "#FCBDB5",
  "3 - 1" = "#08326C", # high inequality, low income
  "2 - 1" = "#1780B1",
  "1 - 1" = "#0B90E1" # low inequality, low income
) %>%
  gather("group", "fill")

bivariate_color_scale %<>%
  separate(group, into = c("gini", "mean"), sep = " - ") %>%
  mutate(gini = as.integer(gini),
         mean = as.integer(mean))

legend <-ggplot() +
  geom_tile(data = bivariate_color_scale, mapping = aes(
    x = gini, y = mean, fill = fill)) +
  scale_fill_identity() +
  labs(x = "manor Precipitación??????",
       y = "mayor Precipitación ??????") +
  theme( axis.title = element_text(size = 6),
         axis.title.x=element_text(color="black"),
         axis.text = element_blank(),
         panel.background = element_rect(fill = "white"),
         axis.title.y=element_text(color="black")) +
  
  coord_fixed()
legend
legend.grob <- ggplotGrob(legend)


Mapa=ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+ scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data=Prec_data_frame, aes(x = x, y =  y, fill = bio12),  alpha=0.6) +
  scale_fill_gradientn(colours = colores2, 
                       
                       breaks = c(0,500,1000,1500,2000,2500,3000, 5000,10000),
                       na.value = 'white',
                       name='Precipitación \n(mm)')+
  geom_sf(data = SurAmerica_utm , fill=NA, color="gray30", size=0.8)+
  geom_sf(data = Amazona , fill=NA, color="black", size=1)+
  coord_sf(xlim = c(-82, -40), ylim = c(-23 ,12)) +
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect( color = "grey20", fill = NA, size = 1),
    axis.text.y  = element_text(angle = 90,face="bold", color="black",family="serif",size=11),
    axis.text.x  = element_text(face="bold", color="black", size=11,family="serif"),
    legend.position = c(0.85,0.85),
    panel.grid.major = element_line(color = "black", linetype = "dashed", size=0.01)
    
  )+
  guides(fill = guide_legend(
    title = "Precipitación \n(mm/anual)",
    
    nrow = 9,
    keywidth = 1.75,
    keyheight = 0.5,
    
    title.position = "top",
    override.aes = list(alpha = 1)
  ))+
  labs(x = '', y = '',  title="")

library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 25), ylim = c(0, 25), expand = FALSE) +
  
  
  draw_plot(Mapa , width = 25, height = 25,x = 0, y = 0)+
  draw_plot(legend.grob , width = 4, height = 4,x = 1.5, y = 3.5)


ggsave(plot=Expo ,"Mapa de precipitacion.png",units = "cm",width = 25, #alto
       height = 25, #ancho
       dpi=1200)
