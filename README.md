# evol-MHEDvisits-Sex

This seeks to analyse mental health-related ED visits and inpatient admissions by sex for the years 2010-2023 in France.  
Yearly temporal resolution. Data is at the hospital level.  
Disorders: substance use, psychotic, mood, anxiety disorders.  
We'll use the fixest R package to emply a fixed effect approach
- hospitals as fe
- cluster-robust standard error
- We'll investigate the interaction of Year by Sex taking pre-COVID year (2019) as a reference
- including total ED presentation as both a covariate and an offset
- count poisson regression (quasi-poisson also possible with fixest)
- particularity 1: fixest removes observations (and FE) with only zero outcomes
- particularity 2: count lower than five are not numbered so we'll take a number e.g. 3. Zeros are documented. 
