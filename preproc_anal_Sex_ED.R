# script for project Sex-ED
# See https://github.com/gbarbalat/

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
#plot
coefplot_fixest <- function(model) { 
  plot <- coefplot(model, 
                   keep = "year*.+",
                   main = "Interaction Effect: Year and Sex",
                   xlab = "Year",
                   ylab = "Coefficient Estimate")
}

#load ----
path <- "C:/Users/Guillaume/Desktop/All projects/Databases INSEE OSCOUR SURSAUD etc/SURSAUD/"
filename <- "SURSAUD_nbPassages_An_2024_VF.csv"
df_Sex_ED_raw <- read.csv(paste0(path,filename), sep=";",encoding="UTF-8",fileEncoding="latin1")
str(df_Sex_ED_raw)

# Trsf "." to 0 and "inferieur a 5" to ? ----
df_Sex_ED <- df_Sex_ED_raw %>%
  mutate(across(starts_with("nb"), 
                ~case_when(
                  . == "." ~ "0",
                  . == "inferieur a 5" ~ "4",
                  TRUE ~ .
                ))) %>%
  mutate(across(starts_with("nb"), as.numeric))
str(df_Sex_ED)

df_ED <- df_Sex_ED_raw %>%
  mutate(across(starts_with("nb"), 
                ~case_when(
                  . == "." ~ "0",
                  . == "inferieur a 5" ~ "4",
                  TRUE ~ .
                ))) %>%
  mutate(across(starts_with("nb"), as.numeric)) %>%
  group_by(Libelle,annee_entree,Clage) %>%
  summarise(N=sum(nb_F10F19_SS))
head(df_ED)

# range of dpd variable ----
table(df_Sex_ED$annee_entree)
length(unique(df_Sex_ED$rge_code_etablissement))
"" %in% df_Sex_ED$rge_code_etablissement
length(unique(df_Sex_ED$Libelle))
length(unique(df_Sex_ED$code_insee))
#No missing in rge_code_etablissement so we'll use rge_code_etablissement as FE

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

# subset of interest (by age)----
df_Sex_ED_subset <- df_Sex_ED %>%
  filter(Clage == "18-64 ans") %>%
  filter(pas_sexe != "I") %>%
  mutate(year=factor(annee_entree))
df_Sex_ED_subset$year <- relevel(df_Sex_ED_subset$year, ref = "2019")

df_ED_subset <- df_ED %>%
  filter(Clage == "18-64 ans") %>%
  #filter(pas_sexe != "I") %>%
  mutate(year=factor(annee_entree))
df_ED_subset$year <- relevel(df_ED_subset$year, ref = "2019")

#Poisson Sex ----
model <- fepois(
  nb_F40F49_SS ~ year * pas_sexe + nb_TCC_SS | #
    rge_code_etablissement , #
  data = df_Sex_ED_subset, 
  offset = ~log(nb_TCC_SS), 
  cluster = ~rge_code_etablissement
)
summary(model)
coefplot_fixest(model)

model <- fepois(
  nb_F30F39_SS ~ year * pas_sexe + nb_TCC_SS | #
    rge_code_etablissement , #
  data = df_Sex_ED_subset, 
  offset = ~log(nb_TCC_SS), 
  cluster = ~rge_code_etablissement
)
summary(model)
coefplot_fixest(model)

model <- fepois(
  nb_F20F29_SS ~ year * pas_sexe + nb_TCC_SS | #
    rge_code_etablissement , #
  data = df_Sex_ED_subset, 
  offset = ~log(nb_TCC_SS), 
  cluster = ~rge_code_etablissement
)
summary(model)
coefplot_fixest(model)

model <- fepois(
  nb_F10F19_SS ~ year * pas_sexe + nb_TCC_SS   | #
    rge_code_etablissement , #
  data = df_Sex_ED_subset, 
  offset =~  log(nb_TCC_SS), 
  cluster = ~rge_code_etablissement
)
summary(model)
coefplot_fixest(model)

model <- feglm(
  nb_F10F19_SS ~ year * pas_sexe + nb_TCC_SS   | #
    rge_code_etablissement , #
  data = df_Sex_ED_subset, 
  offset =~  log(nb_TCC_SS), 
  cluster = ~rge_code_etablissement,
  family="quasipoisson"
)
summary(model)
coefplot_fixest(model)
