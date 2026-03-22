### Combined Map and datasets plots###
# By Nyssa Silbiger ###



# load libraries ####
library(here)
library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
library(wesanderson)
library(ggmap)
library(sf)
library(osmdata)
library(units)
library(ggrepel)
library(ggspatial)
library(broom)
library(ggstream)

## Datasets plots first------------------
## read in data -----
data<- read_csv(here("Data","bar_graph_data_entry.csv"))

pal <- wes_palette("Zissou1", 15, type = "continuous")

data_p<-ggplot(data, aes(year, log(number_sites+1)*bar_value, 
                 linetype = gump_criobe)) +
  geom_area(aes(fill = dataset), color = "black")+
  geom_segment(aes(y = 0, yend = 0, x = 1980, xend = 2025), size = 1)+
  geom_vline(xintercept = 2020, lty = 2)+
  annotate(geom = "text", x = 1990, y = 20,label = "MCR", size = 5)+
  annotate(geom = "text", x = 1990, y = -20, label = "SNO CORAIL", size = 5)+
  labs(y = "Number of Sites",
       x = "Year",
       fill = "")+
  scale_fill_manual(values = pal)+
  scale_linetype_manual(values = c(1,1))+
  guides(linetype = "none")+
  theme_classic()+
  theme(
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold"),
    # axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))


### Map plot next-------------
## MCR Data
# Lagoon temperature points

MCR_Temp<-read_csv(here("Data","LTER","Lagoon_Therm_Locations_update.csv")) %>%
  rename(longitude = "X", latitude = "Y") %>%
  select(longitude, latitude, Name) %>%
  mutate(site = "MCR",
         category = "Temperature")

# LTER standard sites
MCR_all<-read_csv(here("Data","LTER","LTERSites.csv"))

## read in the map data ########

criobe<-read_csv(here("Data","GPS_CRIOBE.csv"))

# extract distinct points

criobe_clean<-criobe %>%
  select(latitude, longitude, variable) %>%
  distinct() # remove replicates


# extract the unique names of things sampled
params<-unique(criobe_clean$variable)

# put these into categories 
criobe_mapdata<-criobe_clean %>%
  mutate(category = case_when(variable %in% c("Coral","Coral","Reef 3D model",
                                              "Macro invertebrate","Reef rugosity") ~ "Benthic",
                              variable %in% c("Fish")~"Fish",
                              variable %in% c("Oxygen","pH","Conductivity","Salinity","fluorimetry",
                                              "Phosphate","nitrates","nitrites","carbonates","Silica",
                                              "Ammonium","Turbidity")~"Biogeochemical",
                              variable == "Temperature"~"Temperature",
                              variable %in% c("Significant height","Significant period")~"Physical Oceanography")) %>%
  select(!variable) %>%
  distinct() %>%
  drop_na() %>%
  mutate(site = "SNO CORAIL")


# Bring the MCR and criobe data together

mapdata<-criobe_mapdata %>%
  bind_rows(MCR_Temp) %>%
  bind_rows(MCR_all)


stations<-tibble(site  =c("Gump Research Station","CRIOBE"), # -17.49556,-149.87278
                 latitude  =c(-17.49048,-17.5198), 
                 longitude = c(-149.82637,-149.8522)) 

stations_sf<-st_as_sf(stations,
                      coords = c("longitude", "latitude"),  # specify coordinate columns
                      crs = 4326                 # WGS84 latitude/longitude CRS)
)

# 1) Get Mo'orea polygon from OSM and pad it by ~3 km so tiles include the coast
moorea_poly <- getbb("Moorea, Windward Islands, French Polynesia",
                     format_out = "sf_polygon")

stopifnot(!is.null(moorea_poly))
moorea_sf <- st_as_sf(moorea_poly)

# Buffer in meters (work in Web Mercator to buffer in planar meters)
moorea_buf <- moorea_sf |>
  st_transform(3857) |>
  st_buffer(set_units(3000, "m")) |>   # change 3000 for more/less padding
  st_transform(4326)

# 2) Build bbox (left, bottom, right, top) guaranteed numeric & ordered
bb <- st_bbox(moorea_buf)
bbox <- c(
  left   = unname(bb["xmin"]),
  bottom = unname(bb["ymin"]),
  right  = unname(bb["xmax"]),
  top    = unname(bb["ymax"])
)

# 3) Clip to Web Mercator lat limits (+/-85.0511) to satisfy tile math
bbox["bottom"] <- max(bbox["bottom"], -85.0511)
bbox["top"]    <- min(bbox["top"],     85.0511)

# 4) Safety swaps if a numeric issue ever inverts the box
if (bbox["left"] > bbox["right"])  bbox[c("left","right")]   <- bbox[c("right","left")]
if (bbox["bottom"] > bbox["top"])  bbox[c("bottom","top")]   <- bbox[c("top","bottom")]

# ---- Basemap via ggmap (Stamen; no API key required) ----
basemap <- get_stadiamap(
  bbox = bbox,
  zoom = 12,                      # tweak if tiles look too coarse/fine
  maptype = "stamen_terrain_background"
)


# ---- Coastline from OpenStreetMap  ----
coast_q <- opq(bbox = bbox) |>
  add_osm_feature(key = "natural", value = "coastline")

coast_sf <- osmdata_sf(coast_q)

# Use only line features; occasionally OSM returns none—handle gracefully
coast_lines <- coast_sf$osm_lines
if (is.null(coast_lines) || nrow(coast_lines) == 0) {
  warning("No OSM coastline lines returned in this bbox; map will show basemap only.")
}

# ---- Plot: ggmap + sf overlay ----
#
set.seed(42)  # reproducible jitter

sites_sf <- st_as_sf(
  mapdata ,
  coords = c("longitude", "latitude"),  # specify coordinate columns
  crs = 4326                 # WGS84 latitude/longitude CRS
)

# Jitter ~100 m in both x/y (Web Mercator meters), then back to WGS84
sites_jit <- sites_sf |>
  st_transform(3857) |>
  st_jitter(amount = c(500, 500)) |>  # ~500 m; 
  st_transform(4326)

# Build a small df to draw leader lines (true -> jittered)
orig_df <- sites_sf |> st_drop_geometry() |> bind_cols(as.data.frame(st_coordinates(sites_sf))) |>
  rename(lon0 = X, lat0 = Y)
jit_df  <- sites_jit |> st_drop_geometry() |> bind_cols(as.data.frame(st_coordinates(sites_jit))) |>
  rename(lon1 = X, lat1 = Y)
seg_df  <- orig_df |> left_join(jit_df, by = names(st_drop_geometry(sites_sf)))

# Plot: jittered points, leader lines back to true locations, and label repulsion
labs_df <- sites_jit |> st_drop_geometry() |> bind_cols(as.data.frame(st_coordinates(sites_jit))) |>
  rename(lon = X, lat = Y)

colors<-c("#3a9ab2","#93bbb3","#dccb4e","#ed6603","#f11b00")

map<-ggmap(basemap) +
  {
    if (!is.null(coast_lines) && nrow(coast_lines) > 0)
      geom_sf(
        data = coast_lines |> st_transform(4326),
        inherit.aes = FALSE,
        linewidth = 0.6
      )
  }  +
  geom_sf(data = sites_jit, inherit.aes = FALSE, size = 2.5, 
          aes(fill = category, shape = site), alpha = 0.5)+ 
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = c(21,22))+
  labs(x = "Longtitude",
       y = "Latitude",
       fill = "Data Category",
       shape = "Research Program")+
  theme_classic(base_size = 12) +
  theme(
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size=12),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey")
  ) +
  coord_sf(
    xlim = c(bbox["left"], bbox["right"]),
    ylim = c(bbox["bottom"], bbox["top"]),
    expand = FALSE
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, alpha = 1)))+
  annotation_scale(location = "bl",  # "bl" for bottom left, other options: "br", "tl", "tr"
                   width_hint = 0.2) + # Suggested proportion of the plot area the scale bar occupies
  annotation_north_arrow(location = "tr", # "tr" for top right, other options: "br", "tl", "bl"
                         which_north = "true", 
                         width = unit(0.75, "cm"),
                         height = unit(0.75, "cm") # Always points to true north
  ) 

### bring everything together ######-----
data_p+map +plot_annotation(tag_levels = "A")
ggsave(here("Output","combined.pdf"), width = 12,height = 8)
