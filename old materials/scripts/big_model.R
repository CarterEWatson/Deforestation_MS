require(tidyverse)
require(MuMIn)
require(geosphere)
require(sf)

lu_coords <- sprintf("./trial_coords/17_%03d_lat_lon.csv", 1:7)
acsvs_17 <- lapply(lu_coords, read.csv)

csv0 <- acsvs_17[[1]] %>% mutate(t00X_index = row_number())
csv1 <- acsvs_17[[1]] %>% mutate(t00X_index = row_number())
csv2 <- acsvs_17[[2]] %>% mutate(t00X_index = row_number())
csv3 <- acsvs_17[[3]] %>% mutate(t00X_index = row_number())
csv4 <- acsvs_17[[4]] %>% mutate(t00X_index = row_number())
csv5 <- acsvs_17[[5]] %>% mutate(t00X_index = row_number())
csv6 <- acsvs_17[[6]] %>% mutate(t00X_index = row_number())
csv7 <- acsvs_17[[7]] %>% mutate(t00X_index = row_number())

lu_16_t001 <- csv1 %>% filter(LU_INDEX == 16) %>% filter(!(t00X_index == 2390)) %>%
  select(Longitude, Latitude)
lu_16_t002 <- csv2 %>% filter(LU_INDEX == 16) %>% filter(!(t00X_index == 2381)) %>%
  select(Longitude, Latitude)
lu_16_t003 <- csv3 %>% filter(LU_INDEX == 16) %>% filter(!(t00X_index == 2379)) %>%
  select(Longitude, Latitude)
lu_16_t004 <- csv4 %>% filter(LU_INDEX == 16) %>% filter(!(t00X_index == 2381)) %>%
  select(Longitude, Latitude)
lu_16_t005 <- csv5 %>% filter(LU_INDEX == 16) %>%
  select(Longitude, Latitude)
lu_16_t006 <- csv6 %>% filter(LU_INDEX == 16) %>% filter(!(t00X_index == 2389)) %>%
  select(Longitude, Latitude)

csv1$distance_to_LU16_meters <- apply(csv1[, c("Longitude", "Latitude")], 1, function(coord) {
  min(distHaversine(coord, lu_16_t001))
})
csv2$distance_to_LU16_meters <- apply(csv2[, c("Longitude", "Latitude")], 1, function(coord) {
  min(distHaversine(coord, lu_16_t002))
})
csv3$distance_to_LU16_meters <- apply(csv3[, c("Longitude", "Latitude")], 1, function(coord) {
  min(distHaversine(coord, lu_16_t003))
})
csv4$distance_to_LU16_meters <- apply(csv4[, c("Longitude", "Latitude")], 1, function(coord) {
  min(distHaversine(coord, lu_16_t004))
})
csv5$distance_to_LU16_meters <- apply(csv5[, c("Longitude", "Latitude")], 1, function(coord) {
  min(distHaversine(coord, lu_16_t005))
})
csv6$distance_to_LU16_meters <- apply(csv6[, c("Longitude", "Latitude")], 1, function(coord) {
  min(distHaversine(coord, lu_16_t006))
})

csv0 <- csv0 %>% rowwise() %>% mutate(distance_to_LU16_meters = 26000)
csv7 <- csv7 %>% rowwise() %>% mutate(distance_to_LU16_meters = 26000)

csv0$Treatment = "Scenario.0"
csv1$Treatment = "Scenario.1"
csv2$Treatment = "Scenario.2"
csv3$Treatment = "Scenario.3"
csv4$Treatment = "Scenario.4"
csv5$Treatment = "Scenario.5"
csv6$Treatment = "Scenario.6"
csv7$Treatment = "Scenario.7"

distance_df <- bind_rows(csv0, csv1, csv2, csv3, csv4, csv5, csv6, csv7) %>%
  select(Latitude, Longitude, distance_to_LU16_meters, Treatment)
###

ctrl_17 <- read.csv("./CSVs/July2025_17.001_all_points.csv") %>% filter(Mean_CTRL_PSN > 2)
ctrl_18 <- read.csv("./CSVs/July2025_18.001_all_points.csv") %>% filter(Mean_CTRL_PSN > 2)

t17_names <- sprintf("./CSVs/July2025_17.%03d_deciduous_points_only.csv", 1:7)
csvs_17 <- lapply(t17_names, read.csv)

t18_names <- sprintf("./CSVs/July2025_18.%03d_deciduous_points_only.csv", 1:6)
csvs_18 <- lapply(t18_names, read.csv)

vpd_17 <- list()
vpd_18 <- list()
for (i in 1:7) {
  vpd_17[[i]] <- csvs_17[[i]] %>% select(Latitude, Longitude, Mean_t00X_VPD)
  colnames(vpd_17[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
for (i in 1:6) {
  vpd_18[[i]] <- csvs_18[[i]] %>% select(Latitude, Longitude, Mean_t00X_VPD)
  colnames(vpd_18[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
vpd_17_merged <- ctrl_17 %>% select(Latitude, Longitude, Mean_CTRL_VPD)
colnames(vpd_17_merged) <- c("Latitude", "Longitude", paste0("Scenario.0"))
vpd_18_merged <- ctrl_18 %>% select(Latitude, Longitude, Mean_CTRL_VPD)
colnames(vpd_18_merged) <- c("Latitude", "Longitude", paste0("Scenario.0"))
for (i in 1:7) {
  vpd_17_merged <- full_join(vpd_17_merged, vpd_17[[i]], by = c("Latitude", "Longitude"))
}
for (i in 1:6) {
  vpd_18_merged <- full_join(vpd_18_merged, vpd_18[[i]], by = c("Latitude", "Longitude"))
}
vpd_17_merged <- vpd_17_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "VPD")
vpd_18_merged <- vpd_18_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "VPD")


sm_17 <- list()
sm_18 <- list()
for (i in 1:7) {
  sm_17[[i]] <- csvs_17[[i]] %>% select(Latitude, Longitude, Mean_t00X_SMOIS)
  colnames(sm_17[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
for (i in 1:6) {
  sm_18[[i]] <- csvs_18[[i]] %>% select(Latitude, Longitude, Mean_t00X_SMOIS)
  colnames(sm_18[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
sm_17_merged <- ctrl_17 %>% select(Latitude, Longitude, Mean_CTRL_SMOIS)
colnames(sm_17_merged) <- c("Latitude", "Longitude", paste0("Scenario.0"))
sm_18_merged <- ctrl_18 %>% select(Latitude, Longitude, Mean_CTRL_SMOIS)
colnames(sm_18_merged) <- c("Latitude", "Longitude", paste0("Scenario.0"))
for (i in 1:7) {
  sm_17_merged <- full_join(sm_17_merged, sm_17[[i]], by = c("Latitude", "Longitude"))
}
for (i in 1:6) {
  sm_18_merged <- full_join(sm_18_merged, sm_18[[i]], by = c("Latitude", "Longitude"))
}
sm_17_merged <- sm_17_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "SM")
sm_18_merged <- sm_18_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "SM")


par_17 <- list()
par_18 <- list()
for (i in 1:7) {
  par_17[[i]] <- csvs_17[[i]] %>% select(Latitude, Longitude, Mean_t00X_APAR)
  colnames(par_17[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
for (i in 1:6) {
  par_18[[i]] <- csvs_18[[i]] %>% select(Latitude, Longitude, Mean_t00X_APAR)
  colnames(par_18[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
par_17_merged <- ctrl_17 %>% select(Latitude, Longitude, Mean_CTRL_APAR)
colnames(par_17_merged) <- c("Latitude", "Longitude", paste0("Scenario.0"))
par_18_merged <- ctrl_18 %>% select(Latitude, Longitude, Mean_CTRL_APAR)
colnames(par_18_merged) <- c("Latitude", "Longitude", paste0("Scenario.0"))
for (i in 1:7) {
  par_17_merged <- full_join(par_17_merged, par_17[[i]], by = c("Latitude", "Longitude"))
}
for (i in 1:6) {
  par_18_merged <- full_join(par_18_merged, par_18[[i]], by = c("Latitude", "Longitude"))
}
par_17_merged <- par_17_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "PAR")
par_18_merged <- par_18_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "PAR")


gpp_17 <- list()
gpp_18 <- list()
for (i in 1:7) {
  gpp_17[[i]] <- csvs_17[[i]] %>% select(Latitude, Longitude, Mean_t00X_PSN)
  colnames(gpp_17[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
for (i in 1:6) {
  gpp_18[[i]] <- csvs_18[[i]] %>% select(Latitude, Longitude, Mean_t00X_PSN)
  colnames(gpp_18[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
gpp_17_merged <- ctrl_17 %>% select(Latitude, Longitude, Mean_CTRL_PSN)
colnames(gpp_17_merged) <- c("Latitude", "Longitude", paste0("Scenario.0"))
gpp_18_merged <- ctrl_18 %>% select(Latitude, Longitude, Mean_CTRL_PSN)
colnames(gpp_18_merged) <- c("Latitude", "Longitude", paste0("Scenario.0"))
for (i in 1:7) {
  gpp_17_merged <- full_join(gpp_17_merged, gpp_17[[i]], by = c("Latitude", "Longitude"))
}
for (i in 1:6) {
  gpp_18_merged <- full_join(gpp_18_merged, gpp_18[[i]], by = c("Latitude", "Longitude"))
}
gpp_17_merged <- gpp_17_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "GPP")
gpp_18_merged <- gpp_18_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "GPP")



merge1_17 <- full_join(vpd_17_merged, sm_17_merged, by = c("Latitude", "Longitude", "Treatment"))
merge2_17 <- full_join(merge1_17, par_17_merged, by = c("Latitude", "Longitude", "Treatment"))
merge3_17 <- full_join(merge2_17, gpp_17_merged, by = c("Latitude", "Longitude", "Treatment"))
big_df_17 <- full_join(merge3_17, distance_df, by = c("Latitude", "Longitude", "Treatment"))

merge1_18 <- full_join(vpd_18_merged, sm_18_merged, by = c("Latitude", "Longitude", "Treatment"))
merge2_18 <- full_join(merge1_18, par_18_merged, by = c("Latitude", "Longitude", "Treatment"))
merge3_18 <- full_join(merge2_18, gpp_18_merged, by = c("Latitude", "Longitude", "Treatment"))
big_df_18 <- full_join(merge3_18, distance_df, by = c("Latitude", "Longitude", "Treatment"))

calculate_polar <- function(lat, lon) {
  r <- distHaversine(c(-81.21869, 37.57799), c(lon, lat))
  theta <- atan2(lat - 37.57799, lon - -81.21869)
  return(c(r, theta))}

big_df_17 <- big_df_17 %>% rowwise() %>%
  mutate(polar = list(calculate_polar(Latitude, Longitude)),
         r = polar[1],
         theta = polar[2]) %>%
  select(-polar)

big_df_18 <- big_df_18 %>% rowwise() %>%
  mutate(polar = list(calculate_polar(Latitude, Longitude)),
         r = polar[1],
         theta = polar[2]) %>%
  select(-polar)


big_df_17$theta <- big_df_17$theta * (180/pi)
big_df_18$theta <- big_df_18$theta * (180/pi)

##ifelse(Treatment %in% c(Scenario.1, Scenario.2, Scenario.3, Scenario.4, Scenario.5, Scenario.6), filter(distance_to_LU16_meters < 25000), NA)

big_df_17_pt2 <- big_df_17 %>% filter(Treatment %in% c("Scenario.0", "Scenario.7") | (Treatment %in% c("Scenario.1", "Scenario.2", "Scenario.3", "Scenario.4", "Scenario.5", "Scenario.6") & distance_to_LU16_meters < 25000))
big_df_18_pt2 <- big_df_18 %>% filter(Treatment %in% c("Scenario.0", "Scenario.7") | (Treatment %in% c("Scenario.1", "Scenario.2", "Scenario.3", "Scenario.4", "Scenario.5", "Scenario.6") & distance_to_LU16_meters < 25000))


big_df_17_pt2$year <- 2017
big_df_18_pt2$year <- 2018

full_df <- rbind(big_df_17_pt2, big_df_18_pt2)



############
mask003 <- data.frame(long = c(-80.13846, -80.17999, -81.60565, -82.27844, -82.25146, -80.82397, -80.13846),
                      lat = c(38.33989, 37.80225, 36.78333, 36.80656, 37.34526, 38.37086, 38.33989))
mask004 <- data.frame(long = c(-80.13846, -80.18411, -81.46793, -82.2084, -82.17819, -80.89255, -80.13846),
                      lat = c(38.33989, 37.74847, 36.83211, 36.85827, 37.4508, 38.37374, 38.33989))
mask003_sf <- st_polygon(list(as.matrix(mask003))) %>% st_sfc(crs = 4326)
mask004_sf <- st_polygon(list(as.matrix(mask004))) %>% st_sfc(crs = 4326)
full_sf <- st_as_sf(full_df, coords = c("Longitude", "Latitude"), crs = 4326)
inside_003 <- st_within(full_sf, mask003_sf, sparse = FALSE)[,1]
inside_004 <- st_within(full_sf, mask004_sf, sparse = FALSE)[,1]

keep_point <- case_when(full_sf$Treatment == "Scenario.3" ~ !inside_003,
                        full_sf$Treatment == "Scenario.4" ~ !inside_004,
                        TRUE ~ TRUE)

full_sf <- full_sf %>% ungroup()

filtered_df <- full_sf[keep_point, ] %>% mutate(Longitude = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) %>% st_drop_geometry()
############

filtered_df <- filtered_df %>% mutate(Treatment = recode(Treatment, "Scenario.0" = "CTRL", "Scenario.1" = "001", "Scenario.2" = "002",  "Scenario.3" = "003",  "Scenario.4" = "004",  "Scenario.5" = "005",  "Scenario.6" = "006", "Scenario.7" = "CTRL_Replicate"))
filtered_df$Treatment <- relevel(factor(filtered_df$Treatment), ref = "CTRL")

filtered_df2 <- filtered_df %>% filter(GPP > 2)
filtered_df2 <- filtered_df2 %>% select(-r, -theta)



filtered_df_001 <- filtered_df2 %>% filter(Treatment == "CTRL", year == 2017) %>% select(-year, -Treatment)
filtered_df_001_sf <- st_as_sf(filtered_df_001, coords = c("Longitude", "Latitude"), crs = 4326)
plot(filtered_df_001_sf)









##ADD IN SOIL TYPE & ELEVATION
##ACTUALLY, DO I NEED TO? GPP ISN'T DIRECTLY READING EITHER, RATHER THEY ARE FELT VIA VPD, SM, AND APAR.

mod0 <- lm(data = filtered_df2, GPP ~ .)
mod1 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR)
mod2 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year)
mod3 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment)
#mod4 <- lm(data = filtered_df2, GPP ~ Treatment * theta * year * distance_to_LU16_meters)
#mod5 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + distance_to_LU16_meters * theta * Treatment * year)
#mod6 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + distance_to_LU16_meters:theta:Treatment:year)
#mod7 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + distance_to_LU16_meters * theta * Treatment)
#mod8 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + r * theta * Treatment * year)
#mod9 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + distance_to_LU16_meters * r * theta * Treatment * year)
mod10 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + Latitude * Longitude * Treatment * year)

#mod11 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + Latitude * Longitude * distance_to_LU16_meters * r * theta * Treatment * year)
#mod12 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + Latitude * Longitude:distance_to_LU16_meters * r * theta * Treatment * year)

#mod13 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + Latitude:Longitude:distance_to_LU16_meters:r:theta * Treatment * year)
mod14 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment * distance_to_LU16_meters * year)

#mod15 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + Latitude * Longitude * distance_to_LU16_meters * theta * Treatment * year)

#mod16 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + Latitude * Longitude * distance_to_LU16_meters * Treatment * year + theta * Treatment)

### WINNER ###
mod17 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment * distance_to_LU16_meters + Latitude * Longitude * distance_to_LU16_meters * Treatment * year)
##############


#mod18 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + theta * distance_to_LU16_meters * Treatment * year)
#mod19 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + Latitude * Longitude * distance_to_LU16_meters * Treatment * year:theta)
mod20 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year * Treatment + Latitude * Longitude * distance_to_LU16_meters * Treatment * year)
mod21 <- lm(data = filtered_df2, GPP ~ (VPD * SM * PAR + Latitude * Longitude) * distance_to_LU16_meters * Treatment * year)

#AIC(mod0, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod14, mod15, mod16, mod17, mod18, mod19, mod20, mod21)

mod17_summary <- summary(mod17)
mod22 <- lm(data = filtered_df2, GPP ~ VPD * SM * PAR * year + Latitude * Longitude * year)
mod22_summary <- summary(mod22)


mod23 <- lm(data = filtered_df2, GPP ~ .)
summary(mod23)


##MOD17 IS GOOD##
# (1) Use stepwise model prediction
# (2) Ford: Making figures demonstrating how deforestation changes climate space in which remaining forest pixels reside.
# (3) Carter: Make stepwise model (#dredge), send Ford spreadsheets for ALL remaining deciduous forest pixels and all scenarios, add all ctrl and ctrl_replicate pixels. Work on delta videos across ALL timesteps.


#write.csv(filtered_df2, "~/Desktop/aug10_new_df.csv")

coefs <- as.data.frame(mod17_summary$coefficients)
colnames(coefs) <- c("Estimate", "StdError", "tvalue", "pvalue")
coefs_significant <- coefs %>% filter(pvalue < 0.1)
#write.csv(coefs_significant, "aug10_mod17_significant.csv")

filtered_df2$GPP_predicted_mod17 <- predict(mod17)
filtered_df2$GPP_predicted_mod22 <- predict(mod22)
filtered_df2$GPP_predicted_mod23 <- predict(mod23)



ggplot(filtered_df2, aes(x = SM, y = GPP, color = PAR)) +
  geom_point() +
  facet_grid(year ~ Treatment)


lm_df <- filtered_df2 %>% group_by(Treatment, year) %>% summarise(slope = coef(lm(GPP_predicted_mod23 ~ GPP))[2], intercept = coef(lm(GPP_predicted_mod23 ~ GPP))[1], .groups = "drop")

ggplot(filtered_df2, aes(x = GPP, y = GPP_predicted_mod23, color = distance_to_LU16_meters)) +
  geom_point(alpha = 0.3) +
  scale_color_scico(palette = "berlin") +
  geom_smooth(method = "lm", color = "green") +
  facet_grid(year ~ Treatment) +
  labs(x = "Observed GPP", y = "Predicted GPP") +
  theme_bw() +
  coord_fixed() +
  geom_text(data = lm_df, aes(x = Inf, y = -Inf, label = paste0("y = ", round(slope, 2), "x + ", round(intercept, 2))), hjust = 1.1, vjust = -1.1, inherit.aes = FALSE)


worse_lm_df <- filtered_df2 %>% group_by(Treatment, year) %>% summarise(slope = coef(lm(GPP_predicted_mod22 ~ GPP))[2], intercept = coef(lm(GPP_predicted_mod22 ~ GPP))[1], .groups = "drop")

ggplot(filtered_df2, aes(x = GPP, y = GPP_predicted_mod22, color = distance_to_LU16_meters)) +
  geom_point(alpha = 0.3) +
  scale_color_scico(palette = "berlin") +
  geom_smooth(method = "lm", color = "green") +
  facet_grid(year ~ Treatment) +
  labs(x = "Observed GPP", y = "Predicted GPP") +
  theme_bw() +
  coord_fixed() +
  geom_text(data = worse_lm_df, aes(x = Inf, y = -Inf, label = paste0("y = ", round(slope, 2), "x + ", round(intercept, 2))), hjust = 1.1, vjust = -1.1, inherit.aes = FALSE)








