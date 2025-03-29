# evol-MHEDvisits-Sex

This seeks to analyse mental health-related ED visits by sex for the years 2010-2023 in France.  
Yearly temporal resolution. Data is at the hospital level.  
Disorders: substance use, psychotic, and affective (mood and anxiety) disorders.  
We'll use the fixest R package to implement a fixed effect approach
- fixed effect linear regression of log-rate ED visits related to mental health diagnoses (principal and associated diagnoses)
- We'll investigate the interaction of Year by Sex taking year 2010 as a reference
- Hospitals and Years as fe
- sex and log-total ED presentation as covariates
- cluster-robust standard error
