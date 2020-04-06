library(tidyverse)

load("data/BC_metrics.RData")
load("data/HC_metrics.RData")
load("data/CC_metrics.RData")
load("data/CalC_metrics.RData")


total_count <- BC_metrics %>%
  group_by(season) %>%
  mutate(slope = lm(total_count ~ year)$coefficients[2])



totCnt_lm <- glm(mean_intensity ~ year * season, data = CalC_metrics, family = gaussian())
summary(totCnt_lm)




anova_func <- function(df){
  sites_aov <- aov(total_count ~ year * season, data = df)
  return(sites_aov)
}

BC_meanInt_ano<- anova_func(df = BC_metrics)
summary(BC_meanInt_ano)
HC_meanInt_ano<- anova_func(df = HC_metrics)
summary(HC_meanInt_ano)
CC_meanInt_ano<- anova_func(df = CC_metrics)
summary(CC_meanInt_ano)
CalC_meanInt_ano<- anova_func(df = CalC_metrics)
summary(CalC_meanInt_ano)


ggplot(data = BC_metrics, aes(x = year, y = total_count))+
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~season)

test <- lm(total_count ~ season *year, data = BC_metrics)
summary(test)
