library('mgcv')
library('brms')
library('ggplot2')
library('schoenberg')
library(lubridate)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())

######## Loading Metrics

load("data/BC_metrics.RData")
load("data/HC_metrics.RData")
load("data/CC_metrics.RData")
load("data/CalC_metrics.RData")

# Humboldt Current

### Mean_intensity and wind speed
HC_meanInt_wind_spd<- gam(mean_intensity ~ s(mean_speed) + s(mean_wind), data = HC_metrics, method = "REML")
summary(HC_meanInt_wind_spd)
plot(BC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(BC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(BC_meanInt_wind_spd, pages = 1, scheme = 2, unconditional = TRUE)

# fit the model
# predict.gam() is used to generate predictions and standard errors
HC_metrics<- HC_metrics %>%
  ungroup
HC_pred <- as.data.frame(predict(BC_meanInt_wind_spd, se.fit = TRUE, unconditional = TRUE))
HC_pred <- transform(HC_pred,
                     upper = fit + (2 * se.fit),
                     lower = fit - (2 * se.fit)) %>% 
  ungroup()
tester <- cbind(HC_pred,HC_metrics) 


ggplot(tester,  aes(x = year, y = mean_intensity)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  facet_wrap(~season)
theme_bw()

####Total count and wind direction
HC_totalC_wind_dir<- gam(total_count ~ s(mean_speed) + s(mean_wind), data = HC_metrics, method = "REML")
summary(HC_totalC_wind_dir)
plot(HC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(HC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(HC_totalC_wind_dir, pages = 1, scheme = 2, unconditional = TRUE)


# fit the model
# predict.gam() is used to generate predictions and standard errors
HC_metrics<- HC_metrics %>%
  ungroup
HC_pred <- as.data.frame(predict(HC_totalC_wind_dir, se.fit = TRUE, unconditional = TRUE))
HC_pred <- transform(HC_pred,
                     upper = fit + (2 * se.fit),
                     lower = fit - (2 * se.fit)) %>% 
  ungroup()
tester <- cbind(HC_pred,HC_metrics) 


ggplot(tester,  aes(x = year, y = total_count)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  facet_wrap(~season)
theme_bw()
#########################################################################################################################################

# Canary Current

### Mean_intensity and wind speed
CC_meanInt_wind_spd<- gam(mean_intensity ~ s(mean_speed) + s(mean_wind), data = CC_metrics, method = "REML")
summary(CC_meanInt_wind_spd)
plot(CC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(CC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(CC_meanInt_wind_spd, pages = 1, scheme = 2, unconditional = TRUE)

# fit the model
# predict.gam() is used to generate predictions and standard errors
CC_metrics<- CC_metrics %>%
  ungroup
CC_pred <- as.data.frame(predict(CC_meanInt_wind_spd, se.fit = TRUE, unconditional = TRUE))
CC_pred <- transform(CC_pred,
                     upper = fit + (2 * se.fit),
                     lower = fit - (2 * se.fit)) %>% 
  ungroup()
tester <- cbind(CC_pred,CC_metrics) 


ggplot(tester,  aes(x = year, y = mean_intensity)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  facet_wrap(~season)
theme_bw()

####Total count and wind direction
CC_totalC_wind_dir<- gam(total_count ~ s(mean_speed) + s(mean_wind), data = CC_metrics, method = "REML")
summary(CC_totalC_wind_dir)
plot(CC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(CC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(CC_totalC_wind_dir, pages = 1, scheme = 2, unconditional = TRUE)


# fit the model
# predict.gam() is used to generate predictions and standard errors
CC_metrics<- CC_metrics %>%
  ungroup
CC_pred <- as.data.frame(predict(CC_totalC_wind_dir, se.fit = TRUE, unconditional = TRUE))
CC_pred <- transform(CC_pred,
                     upper = fit + (2 * se.fit),
                     lower = fit - (2 * se.fit)) %>% 
  ungroup()
tester <- cbind(CC_pred,CC_metrics) 


ggplot(tester,  aes(x = year, y = total_count)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  facet_wrap(~season)
theme_bw()
#########################################################################################################################################


# California Current

### Mean_intensity and wind speed
CalC_meanInt_wind_spd<- gam(mean_intensity ~ s(mean_speed) + s(mean_wind), data = CalC_metrics, method = "REML")
summary(CalC_meanInt_wind_spd)
plot(CalC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(CalC_meanInt_wind_spd, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(CalC_meanInt_wind_spd, pages = 1, scheme = 2, unconditional = TRUE)

# fit the model
# predict.gam() is used to generate predictions and standard errors
CalC_metrics<- CalC_metrics %>%
  ungroup
CalC_pred <- as.data.frame(predict(CalC_meanInt_wind_spd, se.fit = TRUE, unconditional = TRUE))
CalC_pred <- transform(CalC_pred,
                     upper = fit + (2 * se.fit),
                     lower = fit - (2 * se.fit)) %>% 
  ungroup()
tester <- cbind(CalC_pred,CalC_metrics) 


ggplot(tester,  aes(x = year, y = mean_intensity)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  facet_wrap(~season)
theme_bw()

####Total count and wind direction
CalC_totalC_wind_dir<- gam(total_count ~ s(mean_speed) + s(mean_wind), data = CalC_metrics, method = "REML")
summary(CalC_totalC_wind_dir)
plot(CalC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, residuals = TRUE) # show partial residuals
plot(CalC_totalC_wind_dir, pages = 1, scheme = 1, shade = TRUE, shade.col = 2, seWithMean = TRUE) # `with intercept' CIs
plot(CalC_totalC_wind_dir, pages = 1, scheme = 2, unconditional = TRUE)


# fit the model
# predict.gam() is used to generate predictions and standard errors
CalC_metrics<- CalC_metrics %>%
  ungroup
CalC_pred <- as.data.frame(predict(CalC_totalC_wind_dir, se.fit = TRUE, unconditional = TRUE))
CalC_pred <- transform(CC_pred,
                     upper = fit + (2 * se.fit),
                     lower = fit - (2 * se.fit)) %>% 
  ungroup()
tester <- cbind(CalC_pred,CalC_metrics) 


ggplot(tester,  aes(x = year, y = total_count)) +
  geom_jitter(shape = 5, width = 0.05) +
  geom_point(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.4) +
  geom_line(aes(y = fit)) +
  facet_wrap(~season)
theme_bw()




