require(dplyr)
require(tidyverse)
require(sf)
require(geosphere)
require(RColorBrewer)

ctrl_rds_file <- read_rds("~/Downloads/17_000.rds")
t00X_rds_file <- read_rds("~/Downloads/17_001.rds")
ctrl_coords_file <- read.csv("~/Downloads/17_000_lat_lon.csv") %>% dplyr::select(-LU_INDEX) %>% mutate(ctrl_index = row_number())
t00X_coords_file <- read.csv("~/Downloads/17_001_lat_lon.csv") %>% mutate(t00X_index = row_number())

###############

states <- st_read("./geography/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
physiographic <- st_make_valid(st_read("./geography/Physiographicdi/physio_shp/physio.shp"))
selected_states <- states %>% 
  filter(NAME %in% c("Ohio", "Kentucky", "North Carolina", "Virginia", "West Virginia", "Tennessee"))
clipped_physiographic <- st_intersection(physiographic, selected_states)
box_coords <- data.frame(
  long = c(-82.878, -79.425, -79.652, -82.991, -82.878),
  lat = c(38.967, 38.830, 36.141, 36.274, 38.967))
box_polygon <- st_polygon(list(cbind(box_coords$long, box_coords$lat))) %>% 
  st_sfc(crs = st_crs(clipped_physiographic))
inside_regions <- st_intersection(clipped_physiographic, box_polygon) #WHERE THE STATES AND THE BOX OVERLAP.
outside_regions <- st_difference(clipped_physiographic, box_polygon) #OUTSIDE WHERE THE STATES AND BOX OVERLAP
inside_colors <- colorRampPalette(brewer.pal(9, "Greens"))(length(unique(inside_regions$PROVINCE))) #MAKE IT GREEN INSIDE.
outside_colors <- colorRampPalette(brewer.pal(9, "Blues"))(length(unique(outside_regions$PROVINCE))) #AND BLUE OUTSIDE.
inside_palette <- setNames(inside_colors, unique(inside_regions$PROVINCE)) #MAKE THE KEY FOR INSIDE THE BOX.
outside_palette <- setNames(outside_colors, unique(outside_regions$PROVINCE)) #AND THE KEY FOR OUTSIDE THE BOX.
outside_regions <- outside_regions %>% mutate(color = outside_palette[PROVINCE]) %>%
  group_by(color) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()
inside_regions <- inside_regions %>% mutate(color = inside_palette[PROVINCE]) %>%
  group_by(color) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()
combined_regions <- bind_rows(
  inside_regions,
  outside_regions)
selected_states <- st_transform(selected_states, crs = 4326)
combined_regions <- st_transform(combined_regions, crs = 4326)

standardized_theme <- theme(legend.text = element_text(size = 10), legend.title = element_text(size = 12), legend.key.width = unit(1.5, 'cm'),
                            plot.title = element_text(hjust = 0.5, face="bold"))


#####################

U <- ctrl_rds_file[, , "U"]
V <- ctrl_rds_file[, , "V"]
VPD <- as.data.frame(ctrl_rds_file[, , "VPD"])

days <- floor(nrow(VPD) / 24)

VPD_trimmed <- VPD[1:(n_days * 24), ]
VPD_array <- array(as.matrix(VPD_trimmed), dim = c(24, n_days, ncol(VPD)))
VPD_daily_avg <- apply(VPD_array, c(2, 3), mean)
VPD_daily_avg_df <- as.data.frame(VPD_daily_avg)
colnames(VPD_daily_avg_df) <- colnames(VPD)
vpd_long <- VPD_daily_avg_df %>%
  mutate(Day = 1:n()) %>%
  pivot_longer(-Day, names_to = "Pixel", values_to = "VPD") %>% 
  mutate(ctrl_index = as.integer(gsub("Pixel", "", Pixel)))
vpd_long <- full_join(vpd_long, ctrl_coords_file, by = "ctrl_index")

require(scico)
require(gganimate)
require(gifski)
require(transformr)
require(av)

daily_vpd <- ggplot() +
  geom_point(data = vpd_long, aes(x = Longitude, y = Latitude, color = VPD), shape = 15, size = 1.6) +
  scale_color_scico(palette = "berlin", name = expression("VPD (Pa)")) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  ggtitle("VPD - Day: {frame_time}") +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  transition_time(Day)


animate(daily_vpd, renderer = av_renderer("~/Desktop/VPD_timeseries.mp4"), width = 800, height = 500, fps = 10)






##########################
CTRL_VPD <- as.data.frame(ctrl_rds_file[, , "VPD"])
CTRL_VPD$timestep <- seq_len(nrow(CTRL_VPD))
CTRL_VPD_melted <- CTRL_VPD %>% pivot_longer(cols = !timestep, names_to = "Pixel", values_to = "CTRL_VPD") %>% 
  mutate(ctrl_index = as.integer(gsub("Pixel", "", Pixel)))
CTRL_hourly_VPD <- full_join(CTRL_VPD_melted, ctrl_coords_file, by = "ctrl_index")
rm(CTRL_VPD, CTRL_VPD_melted, ctrl_rds_file, ctrl_coords_file)

t00X_VPD <- as.data.frame(t00X_rds_file[, , "VPD"])
t00X_VPD$timestep <- seq_len(nrow(t00X_VPD))
t00X_VPD_melted <- t00X_VPD %>% pivot_longer(cols = !timestep, names_to = "Pixel", values_to = "t00X_VPD") %>% 
  mutate(t00X_index = as.integer(gsub("Pixel", "", Pixel)))
t00X_hourly_VPD <- full_join(t00X_VPD_melted, t00X_coords_file, by = "t00X_index")
rm(t00X_VPD, t00X_VPD_melted, t00X_rds_file, t00X_coords_file)

CTRL_hourly_VPD <- CTRL_hourly_VPD %>% select(timestep, CTRL_VPD, Latitude, Longitude)
t00X_hourly_VPD <- t00X_hourly_VPD %>% select(timestep, t00X_VPD, Latitude, Longitude, LU_INDEX)

complete_df <- full_join(CTRL_hourly_VPD, t00X_hourly_VPD, by = c("timestep", "Latitude", "Longitude"))
rm(CTRL_hourly_VPD, t00X_hourly_VPD)
complete_df <- complete_df %>% mutate(delta_VPD = t00X_VPD - CTRL_VPD)
complete_df2 <- complete_df %>% filter(delta_VPD < 20000, LU_INDEX == 4)
range(complete_df2$delta_VPD)
rm(complete_df)


hourly_vpd <- ggplot() +
  geom_point(data = complete_df2, aes(x = Longitude, y = Latitude, color = delta_VPD), shape = 15, size = 1.6) +
  scale_color_scico(palette = "vik", name = expression("VPD (Pa)")) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  ggtitle("VPD - Hourly: {frame_time}") +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  transition_time(timestep)


animate(hourly_vpd, renderer = av_renderer("~/Desktop/VPD_hourly_timeseries.mp4"), width = 800, height = 500, fps = 8, nframes = 5881)




#####
