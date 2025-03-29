# script for project Sex-ED
# See https://github.com/gbarbalat/evol-MHEDvisits-Sex

# header ----

rm(list=ls())

start_time <- Sys.time()

library(fixest)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(purrr)
library(ggplot2)

today <- format(Sys.Date(), "%Y-%m-%d")

# for future plotting
coefplot_fixest <- function(model) { 
  plot <- coefplot(model, 
                   keep = "20*",#"year*.+",
                   main = "",
                   xlab = "Year",
                   ylab = "Coefficient Estimate", 
                   dict = c("year2011:pas_sexeM" = "2011",
                            "year2012:pas_sexeM" = "2012",
                            "year2013:pas_sexeM" = "2013",
                            "year2014:pas_sexeM" = "2014",
                            "year2015:pas_sexeM" = "2015",
                            "year2016:pas_sexeM" = "2016",
                            "year2017:pas_sexeM" = "2017",
                            "year2018:pas_sexeM" = "2018",
                            "year2019:pas_sexeM" = "2019",
                            "year2020:pas_sexeM" = "2020",
                            "year2021:pas_sexeM" = "2021",
                            "year2022:pas_sexeM" = "2022",
                            "year2023:pas_sexeM" = "2023"
                            ),
                   angle = 45)
}

#load ----
path <- "C:/Users/Guillaume/Desktop/All projects/Databases INSEE OSCOUR SURSAUD etc/SURSAUD/"
filename <- "SURSAUD_nbPassages_An_2024_VF.csv"
df_Sex_ED_raw <- read.csv(paste0(path,filename), sep=";",encoding="UTF-8",fileEncoding="latin1")
str(df_Sex_ED_raw)

# look at the data ----
# Trsf "." to 0 and "inferieur a 5" to ?
df_Sex_ED <- df_Sex_ED_raw %>%
  mutate(across(starts_with("nb"), 
                ~case_when(
                  . == "." ~ "0",
                  . == "inferieur a 5" ~ "4",
                  TRUE ~ .
                ))) %>%
  mutate(across(starts_with("nb"), as.numeric))

#do not process observations with less than 5 visits
df_Sex_ED <- df_Sex_ED %>%
  mutate(across(starts_with("nb"), ~replace(., . < 5, NA)))

str(df_Sex_ED)
table(df_Sex_ED$annee_entree)
length(unique(df_Sex_ED$rge_code_etablissement))
"" %in% df_Sex_ED$rge_code_etablissement
length(unique(df_Sex_ED$Libelle))
length(unique(df_Sex_ED$code_insee))
table(df_Sex_ED$pas_sexe)

# Select columns starting with "nb"
columns_to_plot <- df_Sex_ED %>%
  select(starts_with("nb"))

# Use map to create histograms for each column
histograms <- map(names(columns_to_plot), function(col_name) {
  ggplot(df_Sex_ED, aes_string(x = col_name)) +
    geom_histogram(fill = "blue", color = "black") +
    labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency") +
    theme_minimal()
})

# Display the histograms
histograms


# Calculate summary statistics for each column
summaries <- map_dfr(columns_to_plot, ~ {
  tibble(
    Mean = mean(.x, na.rm = TRUE),
    Median = median(.x, na.rm = TRUE),
    Min = min(.x, na.rm = TRUE),
    Max = max(.x, na.rm = TRUE),
    SD = sd(.x, na.rm = TRUE)
  )
}, .id = "Column")

# View the summaries
print(summaries)

#subset of interest ----
since_when <- 2010
ref_year <- 2010
varOI <- "nb_F20F29_SS" # "nb_F10F19_SS" "nb_F20F29_SS"
varOI1 <- "nb_F40F49_SS"
varOI2 <- "nb_F30F39_SS" 

df_Sex_ED <- df_Sex_ED_raw %>%
  mutate(across(starts_with("nb"), 
                ~case_when(
                  . == "." ~ "0",
                  . == "inferieur a 5" ~ "3",
                  TRUE ~ .
                ))) %>%
  mutate(across(starts_with("nb"), as.numeric)) %>%
  mutate(across(starts_with("nb"), ~replace(., . < 5, NA))) %>%
  mutate(outcome=!!sym(varOI)/nb_TCC_SS)

df_Sex_ED_subset <- df_Sex_ED %>%
  filter(Clage == "18-64 ans") %>% #18-64 ans; 65 ans ou +; Moins de 18 ans
  filter(annee_entree>=since_when) %>%
  filter(pas_sexe != "I") %>%
  mutate(year=factor(annee_entree))

# sum of all visits ----
{
    summary_data <- df_Sex_ED_subset %>%
    group_by(year,pas_sexe ) %>%#
    summarise(
      sum_outcome = sum(nb_TCC_SS, na.rm = TRUE)
    )
  
    Nhosp_year <- df_Sex_ED_subset %>%
      select(annee_entree, pas_sexe, rge_code_etablissement, nb_TCC_SS) %>%
      #na.omit() %>%
      group_by(annee_entree, pas_sexe, rge_code_etablissement) %>%
      summarise(
        sum_outcome = sum(nb_TCC_SS, na.rm = TRUE)
      ) %>%
      group_by(annee_entree, pas_sexe) %>%
      summarise(Sum=sum(length(rge_code_etablissement))) %>%
      arrange(annee_entree)
    
  # Create the plot
  ggplot(summary_data, aes(x = year, y = sum_outcome, color = pas_sexe, group = pas_sexe)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Mean Outcome", color = "Sex") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  
}

# sum of visits for varOI ----
{
 summary_data <- df_Sex_ED_subset %>%
    group_by(year, pas_sexe) %>%
    summarise(
      sum_outcome = sum(!!sym(varOI), na.rm = TRUE)
    )

  # Create the plot
  ggplot(summary_data, aes(x = year, y = sum_outcome, color = pas_sexe, group = pas_sexe)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Mean Outcome", color = "Sex") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  
}

# rate of visits for varOI ----
{
  # Calculate mean and standard error for each group
  summary_data <- df_Sex_ED_subset %>%
    group_by(year, pas_sexe) %>%
    summarise(
      mean_outcome = mean(outcome, na.rm = TRUE),
      sd_outcome = sd(outcome, na.rm = TRUE) ,
      se_outcome = sd(outcome, na.rm = TRUE) / sqrt(n()),
      median_outcome = median(outcome, na.rm = TRUE),
      IQR_outcome = IQR(outcome, na.rm = TRUE)
    ) 
  mean(summary_data$mean_outcome)
  
  #Create the plot
  ggplot(summary_data, aes(x = year, y = mean_outcome, color = pas_sexe, group = pas_sexe)) +
    geom_line() +
    geom_point() +
    #geom_errorbar(aes(ymin = mean_outcome - sd_outcome, ymax = mean_outcome + sd_outcome), width = 0.2) +
    geom_errorbar(aes(ymin = median_outcome - IQR_outcome, ymax = median_outcome + IQR_outcome), width = 0.2) +
    labs(x = "Year", y = "Mean Outcome", color = "Sex") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")

}

# fe modeling ----
df_Sex_ED_subset$year <- relevel(df_Sex_ED_subset$year, ref = as.character(ref_year))
{
#check distributions
hist(df_Sex_ED_subset$outcome)
hist(log(df_Sex_ED_subset$outcome))
hist(df_Sex_ED_subset$nb_TCC_SS)
hist(log(df_Sex_ED_subset$nb_TCC_SS))

model <- feols(
  log(outcome) ~ year * pas_sexe + log(nb_TCC_SS) |
    rge_code_etablissement + year,
  data = df_Sex_ED_subset,
  cluster = ~rge_code_etablissement 
)

summary(model) %>% print
coefplot_fixest(model)

#residual checks
pearson_res <- resid(model, type="pearson")
plot(fitted(model),pearson_res)
qqnorm(pearson_res); qqline(pearson_res)  # Check normality
#NOT DONE: acf
# residuals <- resid(model)
# residuals_df <- data.frame(year =  df_Sex_ED_subset %>% select(outcome,nb_TCC_SS,year, pas_sexe) %>% 
#                              mutate(outcome=log(outcome),
#                                     nb_TCC_SS=log(nb_TCC_SS)) %>%
#                              na.omit%>%select(year) %>% unlist, 
#                            residual = residuals)
# residuals_df <- residuals_df[order(residuals_df$year), ]
# acf(residuals_df$residual, lag.max = 10, main = "ACF of Residuals")
}


# Mood+Anx sum visits ----

df_Sex_ED <- df_Sex_ED_raw %>%
    mutate(across(starts_with("nb"), 
                  ~case_when(
                    . == "." ~ "0",
                    . == "inferieur a 5" ~ "3",
                    TRUE ~ .
                  ))) %>%
    mutate(across(starts_with("nb"), as.numeric)) %>%
    mutate(across(starts_with("nb"), ~replace(., . < 5, NA))) %>%
    mutate(outcome=(!!sym(varOI1)+!!sym(varOI2))/nb_TCC_SS)
  
df_Sex_ED_subset <- df_Sex_ED %>%
    filter(Clage == "18-64 ans") %>%
    filter(pas_sexe != "I") %>%
    mutate(year=factor(annee_entree)) %>%
    filter(annee_entree>=since_when)

{
  
  summary_data <- df_Sex_ED_subset %>%
    group_by(year, pas_sexe) %>%
    summarise(
      sum_outcome = sum(!!sym(varOI1)+!!sym(varOI2), na.rm = TRUE)
    )
  
  # Create the plot
  ggplot(summary_data, aes(x = year, y = sum_outcome, color = pas_sexe, group = pas_sexe)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Mean Outcome", color = "Sex") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  
}

# Mood+Anx rate visits ----
{
  # Calculate mean and standard error for each group
  summary_data <- df_Sex_ED_subset %>%
    group_by(year, pas_sexe) %>%
    summarise(
      mean_outcome = mean(outcome, na.rm = TRUE),
      se_outcome = sd(outcome, na.rm = TRUE) / sqrt(n())
    )
  
  mean(summary_data$mean_outcome)
  
  
  #Create the plot
  ggplot(summary_data, aes(x = year, y = mean_outcome, color = pas_sexe, group = pas_sexe)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = mean_outcome - se_outcome, ymax = mean_outcome + se_outcome), width = 0.2) +
    labs(x = "Year", y = "Mean Outcome", color = "Sex") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  
}

# Mood+Anx modeling ----
df_Sex_ED_subset$year <- relevel(df_Sex_ED_subset$year, ref = as.character(ref_year))

{
  #hist of variables
  hist(df_Sex_ED_subset$outcome)
  hist(log(df_Sex_ED_subset$outcome))
  hist(df_Sex_ED_subset$nb_TCC_SS)
  hist(log(df_Sex_ED_subset$nb_TCC_SS))

  model <- feols(
    log(outcome) ~ year * pas_sexe + log(nb_TCC_SS) | 
      rge_code_etablissement + year,
    data = df_Sex_ED_subset,
    cluster = ~rge_code_etablissement #
  )
  summary(model)
  coefplot(model)
  
  #check residuals
  pearson_res <- resid(model, type="pearson")
  plot(fitted(model),pearson_res)
  qqnorm(pearson_res); qqline(pearson_res)  # Check normality
  
  #acf NOT DONE
  # residuals <- resid(model)
  # residuals_df <- data.frame(year =  df_Sex_ED_subset %>% select(outcome,nb_TCC_SS,year, pas_sexe
  # ) %>% 
  #   na.omit%>%select(year) %>% unlist, 
  # residual = residuals)
  # residuals_df <- residuals_df[order(residuals_df$year), ]
  # acf(residuals_df$residual, lag.max = 10, main = "ACF of Residuals")
}
