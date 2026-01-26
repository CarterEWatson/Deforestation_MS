






partial_data <- full_data %>% filter(Year == 2018, Treatment == "Scenario.6")

summary(lm(partial_data$X.GPP ~ partial_data$normalized_ctrl_GPP))

results_of_all_regressions <- read.csv("~/Desktop/full_data_normalized_ctrls_deltas.csv")

results_of_all_regressions <- results_of_all_regressions %>% mutate(P.val = as.numeric(gsub("^<", "", P.val)))

results_of_all_regressions <- results_of_all_regressions %>% mutate(inv_p_val = log10(1/P.val))


ggplot(results_of_all_regressions, aes(x = Scenario, y = factor(Year), color = Slope, size = log(1/P.val))) +
  geom_point() + 
  facet_wrap(~Variable)





ggplot(partial_data, aes(x = normalized_ctrl_SM, y = X.SM, color = "red")) +
  geom_point(alpha=0.2) +
  theme_bw()



test_plot <- ggplot()

for (v in unique(results_of_all_regressions$Variable)) {
  df <- results_of_all_regressions %>% filter(Variable == v)
  test_plot <- test_plot + new_scale_color() + geom_point(data = df, aes(x = Scenario, y = factor(Year), color = Slope, size = inv_p_val)) + scale_color_viridis_c(name = paste("Slope:", v))}

test_plot + facet_wrap(~Variable) + theme_bw() + theme(legend.position = "bottom")








```{r}
all_sm <- c(ctrl_17$Mean_CTRL_SMOIS, ctrl_18$Mean_CTRL_SMOIS)

sm_range <- range(all_sm, na.rm = TRUE)
smois_17 <- ggplot() +
  geom_point(data = ctrl_17, aes(x = Longitude, y = Latitude, color = Mean_CTRL_SMOIS), shape = 15, size = 1) +
  scale_color_scico(palette = "oslo", name = expression("SM (m3/m3)"), limits = sm_range) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  ggtitle("SM") +
  labs(x = NULL, y = "2017") +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  standardized_theme +
  theme(axis.title = element_text(size = 16, face = "bold"), plot.title = element_text(size = 16))
smois_18 <- ggplot() +
  geom_point(data = ctrl_18, aes(x = Longitude, y = Latitude, color = Mean_CTRL_SMOIS), shape = 15, size = 1) +
  scale_color_scico(palette = "oslo", name = expression("SM (m3/m3)"), limits = sm_range) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  labs(x = NULL, y = "2018") +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  standardized_theme +
  theme(axis.title = element_text(size = 16, face = "bold"))
smois_plot_sheet <- ggarrange(smois_17, smois_18, nrow = 2, ncol = 1, align = "hv", common.legend = TRUE, legend = "bottom")

all_vpd <- c(ctrl_17$Mean_CTRL_VPD, ctrl_18$Mean_CTRL_VPD)
vpd_range <- range(all_vpd, na.rm = TRUE)
vpd_17 <- ggplot() +
  geom_point(data = ctrl_17, aes(x = Longitude, y = Latitude, color = Mean_CTRL_VPD), shape = 15, size = 1) +
  scale_color_scico(palette = "oslo", name = expression("VPD (Pa)"), limits = vpd_range) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  ggtitle("VPD") +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  standardized_theme +
  theme(axis.title = element_text(size = 12, face = "bold"), plot.title = element_text(size = 16))
vpd_18 <- ggplot() +
  geom_point(data = ctrl_18, aes(x = Longitude, y = Latitude, color = Mean_CTRL_VPD), shape = 15, size = 1) +
  scale_color_scico(palette = "oslo", name = expression("VPD (Pa)"), limits = vpd_range) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  standardized_theme +
  theme(axis.title = element_text(size = 12, face = "bold"))
vpd_plot_sheet <- ggarrange(vpd_17, vpd_18, nrow = 2, ncol = 1, align = "hv", common.legend = TRUE, legend = "bottom")

all_par <- c(ctrl_17$Mean_CTRL_APAR, ctrl_18$Mean_CTRL_APAR)
par_range <- range(all_par, na.rm = TRUE)
par_17 <- ggplot() +
  geom_point(data = ctrl_17, aes(x = Longitude, y = Latitude, color = Mean_CTRL_APAR), shape = 15, size = 1) +
  scale_color_scico(palette = "oslo", name = expression("APAR (W/m2)"), limits = par_range) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  ggtitle("APAR") +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  standardized_theme +
  theme(axis.title = element_text(size = 12, face = "bold"), plot.title = element_text(size = 16))
par_18 <- ggplot() +
  geom_point(data = ctrl_18, aes(x = Longitude, y = Latitude, color = Mean_CTRL_APAR), shape = 15, size = 1) +
  scale_color_scico(palette = "oslo", name = expression("APAR (W/m2)"), limits = par_range) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  standardized_theme +
  theme(axis.title = element_text(size = 12, face = "bold"))
par_plot_sheet <- ggarrange(par_17, par_18, nrow = 2, ncol = 1, align = "hv", common.legend = TRUE, legend = "bottom")

all_gpp <- c(ctrl_17$Mean_CTRL_PSN, ctrl_18$Mean_CTRL_PSN)
gpp_range <- range(all_gpp, na.rm = TRUE)
gpp_17 <- ggplot() +
  geom_point(data = ctrl_17, aes(x = Longitude, y = Latitude, color = Mean_CTRL_PSN), shape = 15, size = 1) +
  scale_color_scico(palette = "oslo", name = expression("PSN (umol co2/m2/s)"), limits = gpp_range) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  ggtitle("PSN") +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  standardized_theme +
  theme(axis.title = element_text(size = 12, face = "bold"), plot.title = element_text(size = 16))
gpp_18 <- ggplot() +
  geom_point(data = ctrl_18, aes(x = Longitude, y = Latitude, color = Mean_CTRL_PSN), shape = 15, size = 1) +
  scale_color_scico(palette = "oslo", name = expression("GPP (ADD)"), limits = gpp_range) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  standardized_theme +
  theme(axis.title = element_text(size = 12, face = "bold"))
gpp_plot_sheet <- ggarrange(gpp_17, gpp_18, nrow = 2, ncol = 1, align = "hv", common.legend = TRUE, legend = "bottom")

fig_4_plot_sheet <- ggarrange(smois_plot_sheet, vpd_plot_sheet, par_plot_sheet, gpp_plot_sheet, nrow = 1, ncol = 4, align = "hv")

ggsave("fig_4_plot_sheet.pdf", fig_4_plot_sheet, width = 22, height = 10, units = "in")

rm(gpp_17, gpp_18, vpd_17, vpd_18, smois_17, smois_18, par_17, par_18, gpp_plot_sheet, vpd_plot_sheet, par_plot_sheet, smois_plot_sheet, fig_5_plot_sheet)

```





## New Figures 5+6)

#Make a 10 panel, theta on top, with all the gamms; if you had to drop one set, it would be precip – reference to supplement would probably be fine. One second thought, use Fig. 3.19 instead of GPP attenuation, and/or put the VPD and GPP thetas into one 4 panel figure. These distance/direction figures probably may not be critical enough to include in the main text though.

```{r}
#####
lu_coords <- sprintf("./trial_coords/17_%03d_lat_lon.csv", 1:7)
acsvs_17 <- lapply(lu_coords, read.csv)

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


calculate_polar <- function(lat, lon) {
  r <- distHaversine(c(-81.21869, 37.57799), c(lon, lat))
  theta <- atan2(lat - 37.57799, lon - -81.21869)
  return(c(r, theta))}

csv7 <- csv7 %>% rowwise() %>%
  mutate(polar = list(calculate_polar(Latitude, Longitude)),
         distance_to_LU16_meters = polar[1],
         theta = polar[2]) %>%
  select(-polar, -theta)

csv1$Treatment = "Scenario.1"
csv2$Treatment = "Scenario.2"
csv3$Treatment = "Scenario.3"
csv4$Treatment = "Scenario.4"
csv5$Treatment = "Scenario.5"
csv6$Treatment = "Scenario.6"
csv7$Treatment = "Scenario.7"

distance_df <- bind_rows(csv1, csv2, csv3, csv4, csv5, csv6, csv7) %>%
  select(Latitude, Longitude, distance_to_LU16_meters, Treatment)
###

t17_names <- sprintf("./CSVs/July2025_17.%03d_deciduous_points_only.csv", 1:7)
csvs_17 <- lapply(t17_names, read.csv)
angle_rad_2017 <- 34.348

t18_names <- sprintf("./CSVs/July2025_18.%03d_deciduous_points_only.csv", 1:6)
csvs_18 <- lapply(t18_names, read.csv)
angle_rad_2018 <- 49.458

vpd_17 <- list()
vpd_18 <- list()
for (i in 1:7) {
  vpd_17[[i]] <- csvs_17[[i]] %>% select(Latitude, Longitude, delta_VPD_t00X)
  colnames(vpd_17[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
for (i in 1:6) {
  vpd_18[[i]] <- csvs_18[[i]] %>% select(Latitude, Longitude, delta_VPD_t00X)
  colnames(vpd_18[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
vpd_17_merged <- vpd_17[[1]]
vpd_18_merged <- vpd_18[[1]]
for (i in 2:7) {
  vpd_17_merged <- full_join(vpd_17_merged, vpd_17[[i]], by = c("Latitude", "Longitude"))
}
for (i in 2:6) {
  vpd_18_merged <- full_join(vpd_18_merged, vpd_18[[i]], by = c("Latitude", "Longitude"))
}
vpd_17_merged <- vpd_17_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "VPD")
vpd_18_merged <- vpd_18_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "VPD")








sm_17 <- list()
sm_18 <- list()
for (i in 1:7) {
  sm_17[[i]] <- csvs_17[[i]] %>% select(Latitude, Longitude, delta_SMOIS_t00X)
  colnames(sm_17[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
for (i in 1:6) {
  sm_18[[i]] <- csvs_18[[i]] %>% select(Latitude, Longitude, delta_SMOIS_t00X)
  colnames(sm_18[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
sm_17_merged <- sm_17[[1]]
sm_18_merged <- sm_18[[1]]
for (i in 2:7) {
  sm_17_merged <- full_join(sm_17_merged, sm_17[[i]], by = c("Latitude", "Longitude"))
}
for (i in 2:6) {
  sm_18_merged <- full_join(sm_18_merged, sm_18[[i]], by = c("Latitude", "Longitude"))
}
sm_17_merged <- sm_17_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "SM")
sm_18_merged <- sm_18_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "SM")


par_17 <- list()
par_18 <- list()
for (i in 1:7) {
  par_17[[i]] <- csvs_17[[i]] %>% select(Latitude, Longitude, delta_APAR_t00X)
  colnames(par_17[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
for (i in 1:6) {
  par_18[[i]] <- csvs_18[[i]] %>% select(Latitude, Longitude, delta_APAR_t00X)
  colnames(par_18[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
par_17_merged <- par_17[[1]]
par_18_merged <- par_18[[1]]
for (i in 2:7) {
  par_17_merged <- full_join(par_17_merged, par_17[[i]], by = c("Latitude", "Longitude"))
}
for (i in 2:6) {
  par_18_merged <- full_join(par_18_merged, par_18[[i]], by = c("Latitude", "Longitude"))
}
par_17_merged <- par_17_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "PAR")
par_18_merged <- par_18_merged %>%
  pivot_longer(cols = starts_with("Scenario"), names_to = "Treatment", values_to = "PAR")


gpp_17 <- list()
gpp_18 <- list()
for (i in 1:7) {
  gpp_17[[i]] <- csvs_17[[i]] %>% select(Latitude, Longitude, delta_PSN_t00X)
  colnames(gpp_17[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
for (i in 1:6) {
  gpp_18[[i]] <- csvs_18[[i]] %>% select(Latitude, Longitude, delta_PSN_t00X)
  colnames(gpp_18[[i]]) <- c("Latitude", "Longitude", paste0("Scenario.", i))
}
gpp_17_merged <- gpp_17[[1]]
gpp_18_merged <- gpp_18[[1]]
for (i in 2:7) {
  gpp_17_merged <- full_join(gpp_17_merged, gpp_17[[i]], by = c("Latitude", "Longitude"))
}
for (i in 2:6) {
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
big_df_17 <- big_df_17 %>% mutate(Abs.VPD = abs(VPD), Abs.SM = abs(SM), Abs.PAR = abs(PAR), Abs.GPP = abs(GPP))
big_df_18 <- big_df_18 %>% mutate(Abs.VPD = abs(VPD), Abs.SM = abs(SM), Abs.PAR = abs(PAR), Abs.GPP = abs(GPP))

big_df_17 <- big_df_17 %>% mutate(Treatment = recode(Treatment, "Scenario.1" = "001", "Scenario.2" = "002",  "Scenario.3" = "003",  "Scenario.4" = "004",  "Scenario.5" = "005",  "Scenario.6" = "006", "Scenario.7" = "CTRL_Replicate"))
big_df_18 <- big_df_18 %>% mutate(Treatment = recode(Treatment, "Scenario.1" = "001", "Scenario.2" = "002",  "Scenario.3" = "003",  "Scenario.4" = "004",  "Scenario.5" = "005",  "Scenario.6" = "006", "Scenario.7" = "CTRL_Replicate"))


big_df_17_pt2 <- big_df_17 %>% filter(distance_to_LU16_meters < 25000)
big_df_18_pt2 <- big_df_18 %>% filter(distance_to_LU16_meters < 25000)


VPD_17_theta <- ggplot(big_df_17_pt2, aes(x = theta, y = VPD, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(aes(yintercept = 0, linetype = "Reference Line"), color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = angle_rad_2017, linetype = "Prevailing Wind Direction"), color = "limegreen", size = 1.5) +
  geom_vline(aes(xintercept = angle_rad_2017 - 180, linetype = "Leeward"), color = "blue", size = 1.5) +
  scale_linetype_manual(values = c("Reference Line" = "dashed", "Prevailing Wind Direction" = "dashed", "Leeward" = "dashed")) +
  labs(x = expression(theta * " (DEGREES)"), y = expression(Delta * "Avg. VPD (Pa)"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("VPD, 2017")
VPD_18_theta <- ggplot(big_df_18_pt2, aes(x = theta, y = VPD, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(aes(yintercept = 0, linetype = "Reference Line"), color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = angle_rad_2018, linetype = "Prevailing Wind Direction"), color = "limegreen", linewidth = 1.5) +
  geom_vline(aes(xintercept = angle_rad_2018 - 180, linetype = "Leeward"), color = "blue", linewidth = 1.5) +
  scale_linetype_manual(values = c("Reference Line" = "dashed", "Prevailing Wind Direction" = "dashed", "Leeward" = "dashed")) +
  labs(x = expression(theta * " (DEGREES)"), y = expression(Delta * "Avg. VPD (Pa)"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("VPD, 2018")
GPP_17_theta <- ggplot(big_df_17_pt2, aes(x = theta, y = GPP, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(aes(yintercept = 0, linetype = "Reference Line"), color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = angle_rad_2017, linetype = "Prevailing Wind Direction"), color = "limegreen", size = 1.5) +
  geom_vline(aes(xintercept = angle_rad_2017 - 180, linetype = "Leeward"), color = "blue", size = 1.5) +
  scale_linetype_manual(values = c("Reference Line" = "dashed", "Prevailing Wind Direction" = "dashed", "Leeward" = "dashed")) +
  labs(x = expression(theta * " (DEGREES)"), y = expression(Delta * "Avg. GPP " * "(" * g ~ C ~ m^{-2} ~ hr^{-1} * ")"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("GPP, 2017")
GPP_18_theta <- ggplot(big_df_18_pt2, aes(x = theta, y = GPP, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(aes(yintercept = 0, linetype = "Reference Line"), color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = angle_rad_2018, linetype = "Prevailing Wind Direction"), color = "limegreen", linewidth = 1.5) +
  geom_vline(aes(xintercept = angle_rad_2018 - 180, linetype = "Leeward"), color = "blue", linewidth = 1.5) +
  scale_linetype_manual(values = c("Reference Line" = "dashed", "Prevailing Wind Direction" = "dashed", "Leeward" = "dashed")) +
  labs(x = expression(theta * " (DEGREES)"), y = expression(Delta * "Avg. GPP " * "(" * g ~ C ~ m^{-2} ~ hr^{-1} * ")"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("GPP, 2018")
VPD_17_abs_distance <- ggplot(big_df_17_pt2, aes(x = distance_to_LU16_meters, y = Abs.VPD, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Distance (m)", y = expression("abs(" * Delta * "Avg. VPD) (Pa)"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("VPD 2017") +
  guides(color = guide_legend(nrow = 1))
VPD_18_abs_distance <- ggplot(big_df_18_pt2, aes(x = distance_to_LU16_meters, y = Abs.VPD, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Distance (m)", y = expression("abs(" * Delta * "Avg. VPD) (Pa)"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("VPD 2018")
GPP_17_abs_distance <- ggplot(big_df_17_pt2, aes(x = distance_to_LU16_meters, y = Abs.GPP, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Distance (m)", y = expression("abs(" * Delta * "Avg. GPP) " * "(" * g ~ C ~ m^{-2} ~ hr^{-1} * ")"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("GPP 2017")
GPP_18_abs_distance <- ggplot(big_df_18_pt2, aes(x = distance_to_LU16_meters, y = Abs.GPP, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Distance (m)", y = expression("abs(" * Delta * "Avg. GPP) " * "(" * g ~ C ~ m^{-2} ~ hr^{-1} * ")"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("GPP 2018")
PAR_17_abs_distance <- ggplot(big_df_17_pt2, aes(x = distance_to_LU16_meters, y = Abs.PAR, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Distance (m)", y = expression("abs(" * Delta * "Avg. PAR) " * "(" * W * m^{-2} * ")"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("PAR 2017")
PAR_18_abs_distance <- ggplot(big_df_18_pt2, aes(x = distance_to_LU16_meters, y = Abs.PAR, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Distance (m)", y = expression("abs(" * Delta * "Avg. PAR) " * "(" * W * m^{-2} * ")"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("PAR 2018")
SM_17_abs_distance <- ggplot(big_df_17_pt2, aes(x = distance_to_LU16_meters, y = Abs.SM, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Distance (m)", y = expression("abs(" * Delta * "Avg. SM) " * "(" * m^{3} * m^{-3} * ")"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("SM 2017")
SM_18_abs_distance <- ggplot(big_df_18_pt2, aes(x = distance_to_LU16_meters, y = Abs.SM, color = Treatment)) +
  geom_smooth(size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Distance (m)", y = expression("abs(" * Delta * "Avg. SM) " * "(" * m^{3} * m^{-3} * ")"), color = "", linetype = "") +
  theme_test() +
  standardized_theme +
  ggtitle("SM 2018")


big_df_17_pt2$year <- "2017"
big_df_18_pt2$year <- "2018"

full_df <- rbind(big_df_17_pt2, big_df_18_pt2)


mod <- lm(data = full_df, GPP ~ VPD * SM * PAR + distance_to_LU16_meters + theta + Treatment * year + Treatment:VPD + Treatment:SM + Treatment:PAR)
summary(mod)


### TWO SEPARATE GRAPHS:
quad <- ggarrange(VPD_17_theta, VPD_18_theta, GPP_17_theta, GPP_18_theta, nrow = 1, ncol = 4, align = "hv", labels = c("(a)", "(b)", "(c)", "(d)"),
                  label.x = c(0, 0, 0, 0), label.y = c(1, 1, 1, 1), common.legend = TRUE, legend = "bottom")
ggsave("quad2.pdf", quad, width = 15, height = 4, units = "in")

octa <- ggarrange(VPD_17_abs_distance, VPD_18_abs_distance, GPP_17_abs_distance, GPP_18_abs_distance, PAR_17_abs_distance, PAR_18_abs_distance, SM_17_abs_distance, SM_18_abs_distance, nrow = 2, ncol = 4, align = "hv", labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)"),
                  label.x = c(0, 0, 0, 0, 0, 0, 0, 0), label.y = c(1, 1, 1, 1, 1, 1, 1, 1), common.legend = TRUE, legend = "bottom")
ggsave("octa2.pdf", octa, width = 15, height = 8, units = "in")
```






LU_graphable.2 <- LU_graphable.2 %>%
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(2,3,5), 1, LU_INDEX)) %>% 
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(7,8,9,10,11,12,13,14,15,17), 6, LU_INDEX)) %>% 
  mutate(LU_INDEX = recode(LU_INDEX, '1' = "Different Forest Types", '4' = "Deciduous Broadleaf Forest", '6' = "Other LU Types", '16' = "Barren or Sparsely Vegetated"))


LU_graphable.3 <- LU_graphable.3 %>%
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(2,3,5), 1, LU_INDEX)) %>% 
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(7,8,9,10,11,12,13,14,15,17), 6, LU_INDEX)) %>% 
  mutate(LU_INDEX = recode(LU_INDEX, '1' = "Different Forest Types", '4' = "Deciduous Broadleaf Forest", '6' = "Other LU Types", '16' = "Barren or Sparsely Vegetated"))


LU_graphable.4 <- LU_graphable.4 %>%
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(2,3,5), 1, LU_INDEX)) %>% 
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(7,8,9,10,11,12,13,14,15,17), 6, LU_INDEX)) %>% 
  mutate(LU_INDEX = recode(LU_INDEX, '1' = "Different Forest Types", '4' = "Deciduous Broadleaf Forest", '6' = "Other LU Types", '16' = "Barren or Sparsely Vegetated"))


LU_graphable.5 <- LU_graphable.5 %>%
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(2,3,5), 1, LU_INDEX)) %>% 
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(7,8,9,10,11,12,13,14,15,17), 6, LU_INDEX)) %>% 
  mutate(LU_INDEX = recode(LU_INDEX, '1' = "Different Forest Types", '4' = "Deciduous Broadleaf Forest", '6' = "Other LU Types", '16' = "Barren or Sparsely Vegetated"))


LU_graphable.6 <- LU_graphable.6 %>%
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(2,3,5), 1, LU_INDEX)) %>% 
  mutate(LU_INDEX = ifelse(LU_INDEX %in% c(7,8,9,10,11,12,13,14,15,17), 6, LU_INDEX)) %>% 
  mutate(LU_INDEX = recode(LU_INDEX, '1' = "Different Forest Types", '4' = "Deciduous Broadleaf Forest", '6' = "Other LU Types", '16' = "Barren or Sparsely Vegetated"))






t001 <- ggplot() +
  geom_point(data = LU_graphable.1, aes(x = lon, y = lat, color = LU_INDEX), shape = 15) +
  scale_color_manual(values = c("Different Forest Types" = "#4361EE",
                                "Deciduous Broadleaf Forest" = "#80ED99",
                                "Other LU Types" = "#F28482", 
                                "Barren or Sparsely Vegetated" = "black"), name = NULL) +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  ggtitle("#001") +
  labs(x = "", y = "") +
  theme(plot.margin = margin(5, 10, 5, 10), legend.text = element_text(size = 16)) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 12)))
table(LU_graphable.1$LU_INDEX)


t002 <- ggplot() +
  geom_point(data = LU_graphable.2, aes(x = lon, y = lat, color = LU_INDEX), shape = 15) +
  scale_color_manual(values = c("Different Forest Types" = "#4361EE",
                                "Deciduous Broadleaf Forest" = "#80ED99",
                                "Other LU Types" = "#F28482", 
                                "Barren or Sparsely Vegetated" = "black"), name = "Land Use Type") +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  ggtitle("#002") +
  labs(x = "", y = "") +
  theme(plot.margin = margin(5, 10, 5, 10)) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE)
table(LU_graphable.2$LU_INDEX)


t003 <- ggplot() +
  geom_point(data = LU_graphable.3, aes(x = lon, y = lat, color = LU_INDEX), shape = 15) +
  scale_color_manual(values = c("Different Forest Types" = "#4361EE",
                                "Deciduous Broadleaf Forest" = "#80ED99",
                                "Other LU Types" = "#F28482", 
                                "Barren or Sparsely Vegetated" = "black"), name = "Land Use Type") +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  ggtitle("#003") +
  labs(x = "", y = "") +
  theme(plot.margin = margin(5, 10, 5, 10)) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE)
table(LU_graphable.3$LU_INDEX)


t004 <- ggplot() +
  geom_point(data = LU_graphable.4, aes(x = lon, y = lat, color = LU_INDEX), shape = 15) +
  scale_color_manual(values = c("Different Forest Types" = "#4361EE",
                                "Deciduous Broadleaf Forest" = "#80ED99",
                                "Other LU Types" = "#F28482", 
                                "Barren or Sparsely Vegetated" = "black"), name = "Land Use Type") +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  ggtitle("#004") +
  labs(x = "", y = "") +
  theme(plot.margin = margin(5, 10, 5, 10)) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE)
table(LU_graphable.4$LU_INDEX)


t005 <- ggplot() +
  geom_point(data = LU_graphable.5, aes(x = lon, y = lat, color = LU_INDEX), shape = 15) +
  scale_color_manual(values = c("Different Forest Types" = "#4361EE",
                                "Deciduous Broadleaf Forest" = "#80ED99",
                                "Other LU Types" = "#F28482", 
                                "Barren or Sparsely Vegetated" = "black"), name = "Land Use Type") +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  ggtitle("#005") +
  labs(x = "", y = "") +
  theme(plot.margin = margin(5, 10, 5, 10)) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE)
table(LU_graphable.5$LU_INDEX)


t006 <- ggplot() +
  geom_point(data = LU_graphable.6, aes(x = lon, y = lat, color = LU_INDEX), shape = 15) +
  scale_color_manual(values = c("Different Forest Types" = "#4361EE",
                                "Deciduous Broadleaf Forest" = "#80ED99",
                                "Other LU Types" = "#F28482", 
                                "Barren or Sparsely Vegetated" = "black"), name = "Land Use Type") +
  geom_sf(data = combined_regions, color = "black", linewidth = 0.1, alpha = 0) + 
  geom_sf(data = selected_states, fill = "NA", color = "black", linewidth = 0.2) +
  theme_test() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  ggtitle("#006") +
  labs(x = "", y = "") +
  theme(plot.margin = margin(5, 10, 5, 10)) +
  coord_sf(xlim = c(-83.4, -79), ylim = c(36, 39.1), expand = FALSE)
table(LU_graphable.6$LU_INDEX)

fig3 <- ggarrange(t001, t002, t003, t004, t005, t006, ncol = 6, nrow = 1, common.legend = TRUE, legend = "bottom")
ggsave("fig3_plot_sheet.pdf", fig3, width = 30, height = 5, units = "in")
rm(nc_17_001, nc_17_002, nc_17_003, nc_17_004, nc_17_005, nc_17_006, LU_graphable.1, LU_graphable.2, LU_graphable.3, LU_graphable.4, LU_graphable.5, LU_graphable.6)
