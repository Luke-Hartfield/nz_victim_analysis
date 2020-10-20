# Packages
require(ggplot2)
require(dplyr)
require(tidyr)
require(ggpubr)
require(kableExtra)

#---------------PREPROCESS---------------
# Load data
load(file = "project_data.RData")

# Replace 'C' with NA
census2018[census2018 == "C"] <- NA

# Clean for merging
linking_codes <- area_conversions %>% select(c("SA12018_code", "MB2013_code", ))
police$Year.Month <- gsub(".* ", "\\2", police$Year.Month)

# Filter police by 2018
police_2018 <- police %>% filter(Year.Month == 2018)

# Merge police
area_police <- merge(x = linking_codes, y = police_2018, by.x = "MB2013_code", 
                     by.y = "Meshblock")

# Merge census
merged_full <- merge(x = area_police, y = census2018, by.x = "SA12018_code", 
                     by.y = "Area_code")

# Get attributes of interest
model_df <- merged_full %>% select(c("SA12018_code",
                                      "Territorial.Authority", 
                                      "Victimisations",
                                      "Census_2018_Work_and_labour_force_status_3_Unemployed_CURP_15years_and_over",
                                      "Census_2018_Total_personal_income_Median_CURP_15years_and_over",
                                      "Census_2018_usually_resident_population_count",
                                      "Census_2018_median_age_CURP"))

# Tidy names
model_df <- model_df %>% 
  rename(unemploy = Census_2018_Work_and_labour_force_status_3_Unemployed_CURP_15years_and_over ,
         med_inc = Census_2018_Total_personal_income_Median_CURP_15years_and_over,
         population = Census_2018_usually_resident_population_count,
         med_age = Census_2018_median_age_CURP)

# Impute missing using mean
model_df$unemploy <- as.numeric(model_df$unemploy)
model_df$med_inc <- as.numeric(model_df$med_inc)
model_df$med_age <- as.numeric(model_df$med_age)
model_df$unemploy[is.na(model_df$unemploy)] = mean(model_df$unemploy, na.rm=TRUE)
model_df$med_inc[is.na(model_df$med_inc)] = mean(model_df$med_inc, na.rm=TRUE)
model_df$med_age[is.na(model_df$med_age)] = mean(model_df$med_age, na.rm=TRUE)

# Get population per territory
pop_by_SA <- model_df[!duplicated(model_df$SA12018_code), ]
pop_by_SA <- pop_by_SA %>% group_by(Territorial.Authority) %>% 
  summarise(pop = sum(population)) 

# Sum by territory
model_df <- model_df %>% group_by(Territorial.Authority) %>% 
  summarise(victims = sum(Victimisations), 
            mean_UR = mean(unemploy),
            mean_income = mean(med_inc),
            mean_age= mean(med_age))                         

# Merge together
model_df <- merge(x = model_df, y = pop_by_SA, by = "Territorial.Authority")

# Get percent of territory victimized 
model_df$victims_by_pop <- (model_df$victims/model_df$pop)*100

#---------------EXPLORATORY ANALYSIS STARTS HERE---------------
# Plot relationships
a <- ggplot(model_df, aes(x=mean_UR, y=victims_by_pop)) + geom_point()+
        geom_smooth(method = "lm") +
        labs(x="Mean Unemployment Rate (%)", y="Victimsation Rate (%)")

b <- ggplot(model_df, aes(x=mean_income, y=victims_by_pop)) + geom_point()+
        geom_smooth(method = "lm") +
        labs(x="Mean Income ($NZD)", y="Victimsation Rate (%)")

c <- ggplot(model_df, aes(x=mean_age, y=victims_by_pop)) + geom_point()+
        geom_smooth(method = "lm") +
        labs(x="Mean Age (Years)", y="Victimsation Rate (%)")

ggarrange(a, b, c, ncol = 2, nrow = 2)

# Plot log relationships
a_log <- ggplot(model_df, aes(x=log(mean_UR), y=victims_by_pop)) + geom_point()+ 
          geom_smooth(method = "lm") +
          labs(x="Mean Unemployment Rate (%)", y="log(Victimsation Rate)")

b_log <- ggplot(model_df, aes(x=log(mean_income), y=victims_by_pop)) + geom_point()+
          geom_smooth(method = "lm") +
          labs(x="Mean Income ($NZD)", y="Victimsation Rate (%)")

c_log <- ggplot(model_df, aes(x=log(mean_age), y=victims_by_pop)) + geom_point()+
          geom_smooth(method = "lm") +
          labs(x="Mean Age (Years)", y="log(Victimsation Rate)")

ggarrange(a_log, b_log, c_log, ncol = 2, nrow = 2)

# Log convert 
model_df$log_mean_income <- log(model_df$mean_income)
model_df$log_mean_UR <- log(model_df$mean_UR)
model_df$log_mean_age <- log(model_df$mean_age)

# Function to remove outliers
remove_outliers <- function(x, na.rm = TRUE) {
  q <- quantile(x, probs=c(.25, .75))
  limits <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (q[1] - limits)] <- NA
  y[x > (q[2] + limits)] <- NA
  y
}

# Remove outliers
model_df$log_mean_UR <- remove_outliers(model_df$log_mean_UR)
model_df$log_mean_age <- remove_outliers(model_df$log_mean_age)

#---------------FIT FIRST MODEL---------------

# Fit model, exclude some influential points
lm.fit <- lm(victims_by_pop ~ log_mean_UR*log_mean_age, data=model_df[c(-51,-50,-14),])

# Diagnostic Plots
par(mfrow=c(2,2))
plot(lm.fit)

# Model summary
summary(lm.fit)

# Get model CI
coef <- as.data.frame(summary(lm.fit)$coefficients)
model_CI <- merge(x = coef, y = confint(lm.fit), by.x = 0, by.y = 0) %>%
                  select(-`Std. Error`, -`t value`)

# Clean CI df
model_CI <- model_CI %>% rename(Variable = Row.names, `P-value` = `Pr(>|t|)`,
                                `Lower CI` = `2.5 %`, `Upper CI` = `97.5 %`)
model_CI$Variable <- c("Intercept", "log(mean age)", "log(mean unemployment rate)",
                       "Interaction Term")

kable(model_CI[-1,], digits=2) %>%
  kable_styling()


#---------------FIT SECOND MODEL---------------
# Fit model, exclude some influential points
lm.age <- lm(victims_by_pop ~ log_mean_age, data=model_df[c(-51),])

# Diagnostic Plots
par(mfrow=c(2,2))
plot(lm.age)

# Model summary
summary(lm.age)

# Get model CI
coef.age <- as.data.frame(summary(lm.age)$coefficients)
model_CI.age <- merge(x = coef.age, y = confint(lm.age), by.x = 0, by.y = 0) %>%
  select(-`Std. Error`, -`t value`)

# Clean CI df
model_CI.age <- model_CI.age %>% rename(Variable = Row.names, `P-value` = `Pr(>|t|)`,
                                  `Lower CI` = `2.5 %`, `Upper CI` = `97.5 %`)
model_CI.age$Variable <- c("Intercept", "log(mean age)")

kable(model_CI.age[-1,], digits = 2) %>%
  kable_styling()
