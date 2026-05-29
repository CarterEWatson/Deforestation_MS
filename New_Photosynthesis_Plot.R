PCO2 <- 39.5     ## CO2 in Pa (39.5 Pa = 395 ppm from MP table)
Psfc <- 100000   ## Surface air pressure in Pa (100000 Pa = 1 atm)
C3psn <- 1.0     ## C3 photosynthesis (1.0 for C3, 0.0 for C4)
PO2 <- 20900     ## Oxygen partial pressure in Pa (from MP table)
KC25 <- 30.0       ## Michaelis constant for CO2 at 25°C in Pa (from MP table)
KC10 <- 2.1        ## Michaelis Q10 for CO2 (from MP table)
KO25 <- 30000      ## Michaelis constant for O2 at 25°C in Pa (from MP table)
KO10 <- 1.2        ## Michaelis Q10 for O2 (from MP table)
temp_range <- seq(0, 50, by = 1)  ## From 0 to 50°C... shouldn't be any warmer than that.
Sppf <- 4.6 * 1:3 *40 ## as high as 280, but for illustrative purposes, 120 W/m^2 is as high as we need to go
QE25 <- 0.060      
Beta_sm<-1:3/3 ## ranges from no moisture stress to 1/3 to 2/3 of maximum moisture stress


par(mar = c(4, 4, 2, 2),mfcol=c(1,1),lwd=2)
plot(NULL,
     xlab = "Temperature (°C)", ylab = "C Assim Rate (umol CO2/m2/s)",
     ylim = c(0, 45),xlim=c(0,50))

lty_values=3:1
lwd_values=1:3*2
##################################
####### Light Limited Rate #######
##################################

light_limited <- function(Je, PCO2init, PcCO2, C3psn) {
  ((max(PCO2init - PcCO2, 0) * Je) / (PCO2init + 2*PcCO2)) * C3psn + Je * (1 - C3psn)}

light_limited_values <- numeric(length(temp_range))

light_limited_df_old <- data.frame(Temp = temp_range)
light_limited_df_new <- data.frame(Temp = temp_range)

for(h in 1:length(Sppf)){
  for (i in 1:length(temp_range)) {
    Je <- Sppf[h] * QE25
    Temp <- temp_range[i]
    KC <- KC25 * KC10^((Temp - 25) / 10)
    KO <- KO25 * KO10^((Temp - 25) / 10)
    PcCO2 <- 0.5 * (KC / KO) * PO2 * 0.21
    PCO2init <- 0.7 * PCO2 * C3psn + 0.4 * PCO2 * (1 - C3psn)
    light_limited_values[i] <- light_limited(Je, PCO2init, PcCO2, C3psn)}
  light_limited_df_old[[paste0("LIGHT_",Sppf[h]/4.6)]] <- light_limited_values
  light_limited_df_new[[paste0("LIGHT_",Sppf[h]/4.6)]] <- light_limited_values
}

#light_limited_df_old <- light_limited_df %>% pivot_longer(cols = c("40", "80", "120"), names_to = "Light", values_to = "Light_Limitation") %>% mutate(params = "old")
#light_limited_df_new <- light_limited_df %>% pivot_longer(cols = c("40", "80", "120"), names_to = "Light", values_to = "Light_Limitation") %>% mutate(params = "new")

light_limited_df_old <- light_limited_df_old %>% mutate(params = "old")
light_limited_df_new <- light_limited_df_new %>% mutate(params = "new")


light_limited_df <- full_join(light_limited_df_old, light_limited_df_new, by = c("Temp", "params", "LIGHT_40", "LIGHT_80", "LIGHT_120"))


## ORIGINAL PARAMETERS ##
old_Vcmax25 <- 60
old_Vcmax10 <- 2.4
new_Vcmax25 <- 43
new_Vcmax10 <- 3.4


####################################
##### Rubisco Limited Rate #####
####################################

rub_limited <- function(Vcmax, PCO2init, PcCO2, Fwc, C3psn) {
  ((max(PCO2init - PcCO2, 0) * Vcmax) / (PCO2init + Fwc)) * C3psn + Vcmax * (1 - C3psn)}

old_rub_limited_values <- numeric(length(temp_range))
new_rub_limited_values <- numeric(length(temp_range))

old_rub_limited_df <- data.frame(Temp = temp_range)
new_rub_limited_df <- data.frame(Temp = temp_range)

for(h in 1:length(Beta_sm)){
  
  for (i in 1:length(temp_range)) {
    Temp <- temp_range[i]
    KC <- KC25 * KC10^((Temp - 25) / 10)
    KO <- KO25 * KO10^((Temp - 25) / 10)
    PcCO2 <- 0.5 * (KC / KO) * PO2 * 0.21
    PCO2init <- 0.7 * PCO2 * C3psn + 0.4 * PCO2 * (1 - C3psn)
    Fwc <- KC * (1 + (PO2 / KO))
    old_Vcmax <- old_Vcmax25 * 1 * Beta_sm[h] * ((old_Vcmax10^((Temp - 25) / 10)) / (1 + exp((-2.2 * 10^5 + 710 * (Temp + 273.16)) / (8.314 * (Temp + 273.16)))))
    new_Vcmax <- new_Vcmax25 * 1 * Beta_sm[h] * ((new_Vcmax10^((Temp - 25) / 10)) / (1 + exp((-2.2 * 10^5 + 710 * (Temp + 273.16)) / (8.314 * (Temp + 273.16)))))
    old_rub_limited_values[i] <- rub_limited(old_Vcmax, PCO2init, PcCO2, Fwc, C3psn)
    new_rub_limited_values[i] <- rub_limited(new_Vcmax, PCO2init, PcCO2, Fwc, C3psn)}
  old_rub_limited_df[[paste0("RUB_SM_",round(Beta_sm[h],2))]] <- old_rub_limited_values
  new_rub_limited_df[[paste0("RUB_SM_",round(Beta_sm[h],2))]] <- new_rub_limited_values
}

#old_rub_limited_df <- old_rub_limited_df %>% pivot_longer(cols = c("0.33", "0.67", "1"), names_to = "Beta_sm", values_to = "Rub_Limitation") %>% mutate(params = "old")
#new_rub_limited_df <- new_rub_limited_df %>% pivot_longer(cols = c("0.33", "0.67", "1"), names_to = "Beta_sm", values_to = "Rub_Limitation") %>% mutate(params = "new")

old_rub_limited_df <- old_rub_limited_df %>% mutate(params = "old")
new_rub_limited_df <- new_rub_limited_df %>% mutate(params = "new")


rub_limited_df <- full_join(old_rub_limited_df, new_rub_limited_df, by = c("Temp", "params", "RUB_SM_0.33", "RUB_SM_0.67", "RUB_SM_1"))


###################################
##### Export Limited Rate #####
###################################
old_exp_limited_values <- numeric(length(temp_range))
new_exp_limited_values <- numeric(length(temp_range))
old_exp_limited_df <- data.frame(Temp = temp_range)
new_exp_limited_df <- data.frame(Temp = temp_range)

export_limited <- function(Vcmax, PCO2init, Psfc, C3psn) {
  0.5 * Vcmax * C3psn + 4000 * Vcmax * (PCO2init/Psfc) * (1-C3psn)}

for(h in 1:length(Beta_sm)){
  
  for (i in 1:length(temp_range)) {
    Temp <- temp_range[i]
    KC <- KC25 * KC10^((Temp - 25) / 10)
    KO <- KO25 * KO10^((Temp - 25) / 10)
    PcCO2 <- 0.5 * (KC / KO) * PO2 * 0.21
    PCO2init <- 0.7 * PCO2 * C3psn + 0.4 * PCO2 * (1 - C3psn)
    Fwc <- KC * (1 + (PO2 / KO))
    old_Vcmax <- old_Vcmax25 * Beta_sm[h] * 0.9 * ((old_Vcmax10^((Temp - 25) / 10)) / (1 + exp((-2.2 * 10^5 + 710 * (Temp + 273.16)) / (8.314 * (Temp + 273.16)))))
    new_Vcmax <- new_Vcmax25 * Beta_sm[h] * 0.9 * ((new_Vcmax10^((Temp - 25) / 10)) / (1 + exp((-2.2 * 10^5 + 710 * (Temp + 273.16)) / (8.314 * (Temp + 273.16)))))
    old_exp_limited_values[i] <- export_limited(old_Vcmax, PCO2init, Psfc, C3psn)
    new_exp_limited_values[i] <- export_limited(new_Vcmax, PCO2init, Psfc, C3psn)}
  old_exp_limited_df[[paste0("EXP_SM_",round(Beta_sm[h],2))]] <- old_exp_limited_values
  new_exp_limited_df[[paste0("EXP_SM_",round(Beta_sm[h],2))]] <- new_exp_limited_values
}

#old_exp_limited_df <- old_exp_limited_df %>% pivot_longer(cols = c("0.33", "0.67", "1"), names_to = "Beta_sm", values_to = "Exp_Limitation") %>% mutate(params = "old")
#new_exp_limited_df <- new_exp_limited_df %>% pivot_longer(cols = c("0.33", "0.67", "1"), names_to = "Beta_sm", values_to = "Exp_Limitation") %>% mutate(params = "new")

old_exp_limited_df <- old_exp_limited_df %>% mutate(params = "old")
new_exp_limited_df <- new_exp_limited_df %>% mutate(params = "new")

exp_limited_df <- full_join(old_exp_limited_df, new_exp_limited_df, by = c("Temp", "params", "EXP_SM_0.33", "EXP_SM_0.67", "EXP_SM_1"))




photosynthetic_rate_df <- left_join(rub_limited_df, exp_limited_df, by = c("Temp", "params"))
photosynthetic_rate_df <- left_join(photosynthetic_rate_df, light_limited_df, by = c("Temp", "params"))

ggplot(data = photosynthetic_rate_df) +
  geom_line(aes(x = Temp, y = RUB_SM_0.33), col = "red") +
  geom_line(aes(x = Temp, y = RUB_SM_0.67), col = "red") +
  geom_line(aes(x = Temp, y = RUB_SM_1), col = "red") +
  geom_line(aes(x = Temp, y = EXP_SM_0.33), col = "blue") +
  geom_line(aes(x = Temp, y = EXP_SM_0.67), col = "blue") +
  geom_line(aes(x = Temp, y = EXP_SM_1), col = "blue") +
  geom_line(aes(x = Temp, y = LIGHT_40, linewidth = 1), col = "green") +
  geom_line(aes(x = Temp, y = LIGHT_80, linewidth = 2), col = "green") +
  geom_line(aes(x = Temp, y = LIGHT_120, linewidth = 3), col = "green") +
  facet_grid(~params) +
  theme_bw()

photosynthetic_rate_df2 <- photosynthetic_rate_df %>% pivot_longer(cols = c(RUB_SM_0.33, RUB_SM_0.67, RUB_SM_1), names_to = "SM_rubisco", values_to = "Rubisco_Limited") %>%
  pivot_longer(cols = c(EXP_SM_0.33, EXP_SM_0.67, EXP_SM_1), names_to = "SM_export", values_to = "Export_Limited") %>%
  pivot_longer(cols = c(LIGHT_40, LIGHT_80, LIGHT_120), names_to = "LIGHT", values_to = "Light_Limited") %>%
  mutate(LIGHT = ifelse(LIGHT == "LIGHT_120", 120, LIGHT), LIGHT = ifelse(LIGHT == "LIGHT_80", 80, LIGHT), LIGHT = ifelse(LIGHT == "LIGHT_40", 40, LIGHT),
         SM_rubisco = ifelse(SM_rubisco == "RUB_SM_0.33", 0.33, SM_rubisco), SM_rubisco = ifelse(SM_rubisco == "RUB_SM_0.67", 0.67, SM_rubisco), SM_rubisco = ifelse(SM_rubisco == "RUB_SM_1", 1, SM_rubisco),
         SM_export = ifelse(SM_export == "EXP_SM_0.33", 0.33, SM_export), SM_export = ifelse(SM_export == "EXP_SM_0.67", 0.67, SM_export), SM_export = ifelse(SM_export == "EXP_SM_1", 1, SM_export),
         params = ifelse(params == "old", "Old Params", params), params = ifelse(params == "new", "New Params", params))

#18.785523 18.8
#13.462958 13.5

old_x_segment <- data.frame(x = c(0, 25), y = c(18.785523, 18.785523), params = "Old Params")
old_y_segment <- data.frame(x = c(25, 25), y = c(0, 18.785523), params = "Old Params")
old_point <- data.frame(x = 25, y = 18.785523, params = "Old Params")

new_x_segment <- data.frame(x = c(0, 25), y = c(13.462958, 13.462958), params = "New Params")
new_y_segment <- data.frame(x = c(25, 25), y = c(0, 13.462958), params = "New Params")
new_point <- data.frame(x = 25, y = 13.462958, params = "New Params")

photosynthetic_rate_df2 <- photosynthetic_rate_df2 %>% mutate(LIGHT = factor(LIGHT, levels = c(40, 80, 120)), params = factor(params, levels = c("Old Params", "New Params")))

rm (exp_limited_df, light_limited_df, light_limited_df_new, light_limited_df_old, new_exp_limited_df, new_rub_limited_df, old_exp_limited_df, old_rub_limited_df, photosynthetic_rate_df, rub_limited_df, Beta_sm, C3psn, Fwc, h, i, Je, KC, KC10, KC25, KO, KO10, KO25, light_limited_values, lty_values, lwd_values, new_exp_limited_values, new_rub_limited_values, new_Vcmax, new_Vcmax10, new_Vcmax25, old_exp_limited_values, old_rub_limited_values, old_Vcmax, old_Vcmax10, old_Vcmax25, PcCO2, PCO2, PCO2init, PO2, Psfc, QE25, Sppf, Temp, temp_range, export_limited, light_limited, rub_limited)

new_fig2 <- ggplot(data = photosynthetic_rate_df2) +
  geom_line(aes(x = Temp, y = Light_Limited, linewidth = LIGHT, color = "Light Limited")) +
  geom_line(aes(x = Temp, y = Export_Limited, linetype = SM_export, color = "Export Limited"), linewidth = 1.5) +
  geom_line(aes(x = Temp, y = Rubisco_Limited, linetype = SM_rubisco, color = "Rubisco Limited"), linewidth = 1.5) +
  geom_line(data = old_x_segment, aes(x = x, y = y)) +
  geom_line(data = old_y_segment, aes(x = x, y = y)) +
  geom_point(data = old_point, aes(x = x, y = y), size = 3.5, color = "purple") +
  geom_line(data = new_x_segment, aes(x = x, y = y)) +
  geom_line(data = new_y_segment, aes(x = x, y = y)) +
  geom_point(data = new_point, aes(x = x, y = y), size = 3.5, color = "purple") +
  scale_linetype_manual(values = c("0.33" = "dotted", "0.67" = "twodash", "1" = "solid")) +
  scale_linewidth_manual(values = c("40" = 1.5, "80" = 3, "120" = 4.5)) +
  facet_grid(~params) +
  theme_bw() +
  labs(color = NULL, linewidth = NULL, linetype = NULL, x = "Temperature (°C)", y = expression(mu * "mol CO"[2] * " m"^{-2} * " s"^{-1})) +
  theme(legend.position = "bottom", legend.text = element_text(size = 14), axis.title = element_text(size = 12), axis.text = element_text(size = 10), strip.text = element_text(size = 14))

ggsave("~/Desktop/new_fig2.pdf", new_fig2, width = 10, height = 5.5, units = "in")




