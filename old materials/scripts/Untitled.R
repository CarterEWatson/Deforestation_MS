ctrl_rds_file <- read_rds("~/Downloads/17_000.rds")
ctrl_coords_file <- read.csv("~/Downloads/17_000_lat_lon.csv") %>% dplyr::select(-LU_INDEX) %>% mutate(ctrl_index = row_number())
ctrl_breakpoints <- data.frame(lat = numeric(), lon = numeric(), ctrl_index = numeric(), slope1 = numeric(), bp = numeric(), slope2 = numeric(),
                               adjR2.segmented = numeric(), se.bp.segmented = numeric(), slope.lm = numeric(), adjR2.lm = numeric())

str(ctrl_rds_file)
dimnames(ctrl_rds_file)[[3]]

APAR <- ctrl_rds_file[, , "APAR"]
PSN <- ctrl_rds_file[, , "PSN"]
SM <- ctrl_rds_file[, , "SMOIS"]
VPD <- ctrl_rds_file[, , "VPD"]

APAR_mean_hourly <- rowMeans(APAR, na.rm = TRUE)
PSN_mean_hourly <- rowMeans(PSN, na.rm = TRUE)
SM_mean_hourly <- rowMeans(SM, na.rm = TRUE)
VPD_mean_hourly <- rowMeans(VPD, na.rm = TRUE)

test <- data.frame(APAR = APAR_mean_hourly, PSN = PSN_mean_hourly, SM = SM_mean_hourly, VPD = VPD_mean_hourly)

lin_mod1 <- lm(data = test, PSN ~ APAR + SM)
lin_mod2 <- lm(data = test, PSN ~ APAR * SM * VPD)
summary(lin_mod2)
AIC(lin_mod1)
AIC(lin_mod2)


ggplot(test, aes(x = APAR, y = PSN, color = SM)) +
  geom_point() + 
  theme_bw()