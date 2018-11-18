---
title: "Supplement 1"
subtitle: "Descriptive statistics for the whole cohort"
author: "Peter Kamerman and Prinisha Pillay"
date: "17 November 2018"
---



----

This script generates descriptive statistics for variables collected at baseline (visit day: 0, visit_number: 1) for the whole cohort, irrespective of whether they went on to develop sensory neuropathy (SN) or not.

The following data columns were not analysed:

- `pain` and `pain score`: related to SN only, therefore not relevant at baseline when everyone was free from SN.  

- `*_record`: inconsistent patient records.  

- `hba1c_percent` and `vitaminB12_pmol.l`: These data were only used to classify individuals as having diabetes or vitamin B12 deficiency when cleaning the data _(see:clean-data.R)_.  

- `ID`, `visit_number`, `visit_day`, `hivsn_present`, `visit_months`: provide sorting and grouping information only. 

----

# Import data

```r
data <- read_rds('data-clean/clean_data.rds') %>% 
    # Filter for visit one
    filter(visit_number == 1) %>% 
    # Remove columns that won't be analysed
    select(-starts_with('pain'), -ends_with('_record'), 
           -visit_number, -visit_months, 
           -hba1c_percent, -vitaminB12_pmol.l) 
```

# Inspect data

```r
# Dimensions 
dim(data) 
```

```
## [1] 83 17
```

```r
# Column names
names(data)
```

```
##  [1] "ID"                     "visit_day"             
##  [3] "age_years"              "mass_kg"               
##  [5] "height_m"               "sex"                   
##  [7] "hivsn_present"          "CD4_cell.ul"           
##  [9] "viral_load_copies.ml"   "consumes_alcohol"      
## [11] "alcohol_units.week"     "TB_current"            
## [13] "pyridoxine_prophylaxis" "rifafour_treatment"    
## [15] "ARV_regimen"            "diabetic_hba1c"        
## [17] "vitaminB12_deficiency"
```

```r
# Head and tail
head(data)
```

```
## # A tibble: 6 x 17
##   ID    visit_day age_years mass_kg height_m sex   hivsn_present
##   <chr>     <int>     <dbl>   <dbl>    <dbl> <fct> <fct>        
## 1 001           0        59    41.4     1.56 F     no           
## 2 002           0        23    70.2     1.56 F     no           
## 3 003           0        27    75       1.64 M     no           
## 4 004           0        26    68.8     1.74 M     no           
## 5 005           0        37   107       1.6  F     no           
## 6 006           0        34    85.5     1.53 F     no           
## # ... with 10 more variables: CD4_cell.ul <dbl>,
## #   viral_load_copies.ml <dbl>, consumes_alcohol <fct>,
## #   alcohol_units.week <int>, TB_current <fct>,
## #   pyridoxine_prophylaxis <fct>, rifafour_treatment <fct>,
## #   ARV_regimen <fct>, diabetic_hba1c <fct>, vitaminB12_deficiency <fct>
```

```r
tail(data)
```

```
## # A tibble: 6 x 17
##   ID    visit_day age_years mass_kg height_m sex   hivsn_present
##   <chr>     <int>     <dbl>   <dbl>    <dbl> <fct> <fct>        
## 1 078           0        29    54       1.53 F     no           
## 2 079           0        44    58.8     1.52 F     no           
## 3 080           0        50    53.2     1.51 F     no           
## 4 081           0        43    49.5     1.62 F     no           
## 5 082           0        40    82.8     1.62 F     no           
## 6 083           0        46    73.3     1.66 F     no           
## # ... with 10 more variables: CD4_cell.ul <dbl>,
## #   viral_load_copies.ml <dbl>, consumes_alcohol <fct>,
## #   alcohol_units.week <int>, TB_current <fct>,
## #   pyridoxine_prophylaxis <fct>, rifafour_treatment <fct>,
## #   ARV_regimen <fct>, diabetic_hba1c <fct>, vitaminB12_deficiency <fct>
```

```r
# Data structure
glimpse(data)
```

```
## Observations: 83
## Variables: 17
## $ ID                     <chr> "001", "002", "003", "004", "005", "006...
## $ visit_day              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ age_years              <dbl> 59, 23, 27, 26, 37, 34, 44, 34, 32, 29,...
## $ mass_kg                <dbl> 41.4, 70.2, 75.0, 68.8, 107.0, 85.5, 12...
## $ height_m               <dbl> 1.56, 1.56, 1.64, 1.74, 1.60, 1.53, 1.6...
## $ sex                    <fct> F, F, M, M, F, F, F, F, F, M, M, M, M, ...
## $ hivsn_present          <fct> no, no, no, no, no, no, no, no, no, no,...
## $ CD4_cell.ul            <dbl> 35, 285, 28, 270, 310, 247, 439, 311, 1...
## $ viral_load_copies.ml   <dbl> 6.103804, 5.041393, 5.181844, 2.484300,...
## $ consumes_alcohol       <fct> no, no, no, no, yes, no, no, no, no, no...
## $ alcohol_units.week     <int> 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 6, 9,...
## $ TB_current             <fct> no, no, yes, no, no, no, no, no, no, no...
## $ pyridoxine_prophylaxis <fct> no, no, yes, no, no, no, no, no, no, no...
## $ rifafour_treatment     <fct> no, no, yes, no, no, no, no, no, no, no...
## $ ARV_regimen            <fct> TDF_FTC_EFV, TDF_FTC_EFV, TDF_FTC_EFV, ...
## $ diabetic_hba1c         <fct> no, no, no, no, no, no, no, no, no, no,...
## $ vitaminB12_deficiency  <fct> no, no, no, no, no, no, no, no, no, no,...
```

----

# Analyses

## Age

```r
# Tabular summary
data %>% 
    select(age_years) %>% 
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 1 
## 
## ── Variable type:numeric ───────────────────────────────────────────────────────────────
##   variable missing complete  n  mean   sd p0 p25 p50 p75 p100     hist
##  age_years       0       83 83 38.67 9.09 21  32  39  44   59 ▂▅▇▆▇▃▂▂
```

```r
# 95% bootstrap confidence interval of the mean age
## Method = BCa, Resamples = 10000
set.seed(1234)
groupwiseMean(age_years ~ 1, 
              data = data, 
              R = 10000, 
              traditional = FALSE, 
              boot = TRUE, 
              bca = TRUE)[c(2:3, 5, 6, 7)]
```

```
##    n Mean Conf.level Bca.lower Bca.upper
## 1 83 38.7       0.95      36.8      40.6
```

```r
# Plot
data %>%
    ggplot(data = .) +
    aes(y = age_years, 
        x = 'All patients') +
    geom_boxplot() +
    geom_jitter(height = 0) +
    labs(title = 'Age at recruitment',
         y = 'Age (years)',
         x = 'All participants') +
    theme(axis.text.x = element_blank())
```

<img src="./figures/suppl-01-descriptive-whole-cohort/age-1.png" width="480" style="display: block; margin: auto;" />

## Body mass

```r
# Tabular summary
data %>%
    select(mass_kg) %>% 
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 1 
## 
## ── Variable type:numeric ───────────────────────────────────────────────────────────────
##  variable missing complete  n  mean    sd   p0 p25  p50   p75  p100
##   mass_kg       0       83 83 66.11 15.97 41.4  54 64.2 75.85 121.4
##      hist
##  ▆▇▆▅▃▁▁▁
```

```r
# 95% bootstrap confidence interval of thew mean age
## Method = BCa, Resamples = 10000
set.seed(1234)
groupwiseMean(mass_kg ~ 1, 
              data = data, 
              R = 10000, 
              traditional = FALSE, 
              boot = TRUE, 
              bca = TRUE)[c(2:3, 5, 6, 7)]
```

```
##    n Mean Conf.level Bca.lower Bca.upper
## 1 83 66.1       0.95      62.8      69.7
```

```r
# Plot
data %>%
    ggplot(data = .) +
    aes(y = mass_kg, 
        x = 'All patients') +
    geom_boxplot() +
    geom_jitter(height = 0) +
    labs(title = 'Body mass at recruitment',
         y = 'Body mass (kg)') +
    theme(axis.text.x = element_blank())
```

<img src="./figures/suppl-01-descriptive-whole-cohort/mass-1.png" width="480" style="display: block; margin: auto;" />

## Height
Expect height to show sex difference, so analyse separately for males and females.

```r
# Tabular summary
data %>%
    group_by(sex) %>% 
    select(height_m, sex) %>% 
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 2 
##  group variables: sex 
## 
## ── Variable type:numeric ───────────────────────────────────────────────────────────────
##  sex variable missing complete  n mean    sd   p0  p25  p50  p75 p100
##    F height_m       0       46 46 1.58 0.072 1.46 1.53 1.56 1.63 1.76
##    M height_m       0       37 37 1.7  0.046 1.63 1.65 1.71 1.74 1.79
##      hist
##  ▃▇▅▂▅▃▂▁
##  ▇▁▆▃▅▃▃▁
```

```r
# 95% bootstrap confidence interval of the mean height
## Method = BCa, Resamples = 10000
set.seed(1234)
groupwiseMean(age_years ~ sex, 
              data = data, 
              R = 10000, 
              traditional = FALSE, 
              boot = TRUE, 
              bca = TRUE)[c(1:3, 5, 6, 7)]
```

```
##   sex  n Mean Conf.level Bca.lower Bca.upper
## 1   F 46 37.7       0.95      34.9      40.5
## 2   M 37 39.9       0.95      37.3      42.6
```

```r
# Plots 
data %>%
    ggplot(data = .) +
    aes(y = height_m, 
        x = sex) +
    geom_boxplot() +
    geom_jitter(height = 0) +
    labs(title = 'Height at recruitment, grouped by sex',
         y = 'Height (m)',
         x = 'Sex')
```

<img src="./figures/suppl-01-descriptive-whole-cohort/height-1.png" width="480" style="display: block; margin: auto;" />

## Sex

```r
# Tabular summary
data %>%
    select(sex) %>% 
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 1 
## 
## ── Variable type:factor ────────────────────────────────────────────────────────────────
##  variable missing complete  n n_unique          top_counts ordered
##       sex       0       83 83        2 F: 46, M: 37, NA: 0   FALSE
```

```r
# 95% bootstrap confidence interval of the proportion of females
## Method = BCa, Resamples = 10000
set.seed(1234)
boot.ci(boot(data = data, 
             statistic = function(d, i){mean(d[i, 'sex'] == 'F')}, 
             R = 10000, 
             stype = 'i'), 
        type = 'bca') %>% 
    data_frame(n = nrow(filter(data, !is.na(sex))), 
      Proportion = round(.$t0, 3), 
      Conf.level = 0.95, 
      Bca.lower = round(.$bca[[4]], 3),
      Bca.upper = round(.$bca[[5]], 3)) %>%
    .[1, -1] %>% 
    as.data.frame()
```

```
##    n Proportion Conf.level Bca.lower Bca.upper
## 1 83      0.554       0.95     0.434     0.651
```

```r
# Plot
data %>%
    ggplot(data = .) +
    aes(x = 'All participants',
        fill = sex) +
    geom_bar(position = 'fill') + 
    geom_hline(yintercept = 0.5,
               linetype = 2) +
    labs(title = 'Sex ratio at recruitment',
         subtitle = '(All participants)',
         y = 'Proportion') +
    scale_fill_grey(name = 'Sex') +
    theme(axis.text.x = element_blank())
```

<img src="./figures/suppl-01-descriptive-whole-cohort/sex-1.png" width="480" style="display: block; margin: auto;" />

## CD4 T-cell count

```r
# Tabular summary
data %>%
    select(CD4_cell.ul) %>%  
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 1 
## 
## ── Variable type:numeric ───────────────────────────────────────────────────────────────
##     variable missing complete  n   mean    sd p0 p25 p50 p75 p100     hist
##  CD4_cell.ul       0       83 83 243.46 180.9  1 122 234 317  867 ▇▇▇▃▂▁▁▁
```

```r
# 95% bootstrap confidence interval of the median CD4 T-cell count
## Method = BCa, Resamples = 10000
set.seed(1234)
groupwiseMedian(CD4_cell.ul ~ 1, 
                data = data, 
                R = 10000,
                boot = TRUE, 
                bca = TRUE)[c(2:3, 5, 6, 7)]
```

```
##    n Median Conf.level Bca.lower Bca.upper
## 1 83    234       0.95       151       285
```

```r
# Plot
data %>%
    ggplot(data = .) +
    aes(y = CD4_cell.ul, 
        x = 'All patients') +
    geom_boxplot() +
    geom_jitter(height = 0) +
    labs(title = 'CD4 T-cell count at recruitment',
         y = expression(paste('CD4 T-cell count (cells.', mu, l^-1, ')'))) +
    theme(axis.text.x = element_blank())
```

<img src="./figures/suppl-01-descriptive-whole-cohort/cd4-1.png" width="480" style="display: block; margin: auto;" />

## Viral load

```r
# Tabular summary
data %>%
    select(viral_load_copies.ml) %>%  
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 1 
## 
## ── Variable type:numeric ───────────────────────────────────────────────────────────────
##              variable missing complete  n mean   sd  p0  p25  p50  p75
##  viral_load_copies.ml       6       77 83 3.42 1.35 1.7 2.21 3.02 4.23
##  p100     hist
##  6.51 ▇▅▆▂▅▂▁▂
```

```r
# 95% bootstrap confidence interval of the median viral load
## Method = BCa, Resamples = 10000
set.seed(1234)
groupwiseMedian(viral_load_copies.ml ~ 1, 
                data = data[!is.na(data$viral_load_copies.ml), ], # Remove <NA>
                R = 10000,
                boot = TRUE, 
                bca = TRUE)[c(2:3, 5, 6, 7)]
```

```
##    n Median Conf.level Bca.lower Bca.upper
## 1 77   3.02       0.95      2.78      3.38
```

```r
# Plot
data %>%
    filter(!is.na(viral_load_copies.ml)) %>%
    ggplot(data = .) +
    aes(y = viral_load_copies.ml, 
        x = 'All patients') +
    geom_boxplot() +
    geom_jitter(height = 0) +
    labs(title = 'Viral load at recruitment',
         y = expression(paste('log' [10], ' viral load (copies.ml' ^-1, ')')))  +
    theme(axis.text.x = element_blank())
```

<img src="./figures/suppl-01-descriptive-whole-cohort/viral_load-1.png" width="480" style="display: block; margin: auto;" />

## Alcohol

```r
# Tabular summary
data %>%
    select(alcohol_units.week) %>%
    mutate(drinks_alcohol = case_when(
        alcohol_units.week >= 1 ~ 'Yes',
        alcohol_units.week == 0 ~ 'No'
    )) %>% 
    mutate(drinks_alcohol = factor(drinks_alcohol)) %>% 
    group_by(drinks_alcohol) %>% 
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 2 
##  group variables: drinks_alcohol 
## 
## ── Variable type:integer ───────────────────────────────────────────────────────────────
##  drinks_alcohol           variable missing complete  n  mean   sd p0 p25
##              No alcohol_units.week       0       67 67  0    0     0   0
##             Yes alcohol_units.week       0       16 16 10.56 7.58  3   4
##  p50 p75 p100     hist
##    0   0    0 ▁▁▁▇▁▁▁▁
##    9  15   29 ▇▃▂▂▁▁▁▁
```

```r
# 95% bootstrap confidence interval of the median alcohol consumption
## Method = BCa, Resamples = 10000
set.seed(1234)
groupwiseMedian(alcohol_units.week ~ 1, 
                data = data[data$alcohol_units.week > 0, ], # Remove none drinkers
                R = 10000, 
                boot = TRUE, 
                bca = TRUE)[c(2:3, 5, 6, 7)]
```

```
##    n Median Conf.level Bca.lower Bca.upper
## 1 16      9       0.95         4        12
```

```r
# Plot
data %>%
    filter(alcohol_units.week > 0) %>% 
    ggplot(data = .) +
    aes(x = 'Participants who drink\n(n=16)',
        y = alcohol_units.week) +
    geom_boxplot() +
    geom_jitter(height = 0) +
    labs(title = 'Alcohol consumption at recruitment',
         y = expression(paste('Alcohol consumption (units.week' ^-1, ')'))) +
    theme(axis.text.x = element_blank())
```

<img src="./figures/suppl-01-descriptive-whole-cohort/alcohol-1.png" width="480" style="display: block; margin: auto;" />

## TB
_Note: Treatment policy was to start some patients, irrespective of TB diagnosis, on TB treatment. Therefore current TB infection and treatment for TB analysed separately._

### Currently infected with TB

```r
# Tabular summary
data %>%
    select(TB_current) %>%  
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 1 
## 
## ── Variable type:factor ────────────────────────────────────────────────────────────────
##    variable missing complete  n n_unique             top_counts ordered
##  TB_current       0       83 83        2 no: 67, yes: 16, NA: 0   FALSE
```

```r
# 95% bootstrap confidence interval of the proportion with TB
## Method = BCa, Resamples = 10000
set.seed(1234)
boot.ci(boot(data = data, 
             statistic = function(d, i){mean(d[i, 'TB_current'] == 'yes')}, 
             R = 10000, 
             stype = 'i'), 
        type = 'bca') %>% 
    data_frame(n = nrow(filter(data, !is.na(TB_current))),
      Proportion = round(.$t0, 3), 
      Conf.level = 0.95, 
      Bca.lower = round(.$bca[[4]], 3),
      Bca.upper = round(.$bca[[5]], 3)) %>%
    .[1, -1] %>% 
    as.data.frame()
```

```
##    n Proportion Conf.level Bca.lower Bca.upper
## 1 83      0.193       0.95     0.108     0.277
```

```r
# Plot
data %>%
    mutate(TB_current = str_to_title(TB_current)) %>% 
    ggplot(data = .) +
    aes(x = 'All participants',
        fill = TB_current) +
    geom_bar(position = 'fill') + 
    geom_hline(yintercept = 0.5,
               linetype = 2) +
    labs(title = 'Current TB at recruitment',
         y = 'Proportion') +
    scale_fill_grey(name = 'Infected') +
    theme(axis.text.x = element_blank())
```

<img src="./figures/suppl-01-descriptive-whole-cohort/tb_current-1.png" width="480" style="display: block; margin: auto;" />

### Currently receiving TB treatment?
Treatment consisted of rifafour and pyridoxine (prophylaxis). Therefore only need to analyse rifafour data. Data coded as _'No'_ (not being treated), _'Yes'_ (being treated for active TB), and _'Prophylaxis'_ (being treated prophylactically for TB).

```r
# Double-check matching between rifafour and pyridoxine columns 
unique(data$rifafour_treatment == data$pyridoxine_prophylaxis)
```

```
## [1] TRUE
```

```r
# Tabular summary
data %>%
    group_by(pyridoxine_prophylaxis) %>% 
    select(rifafour_treatment, pyridoxine_prophylaxis) %>%  
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 2 
##  group variables: pyridoxine_prophylaxis 
## 
## ── Variable type:factor ────────────────────────────────────────────────────────────────
##  pyridoxine_prophylaxis           variable missing complete  n n_unique
##                      no rifafour_treatment       0       66 66        1
##                     yes rifafour_treatment       0       15 15        1
##             prophylaxis rifafour_treatment       0        2  2        1
##                     top_counts ordered
##  no: 66, yes: 0, pro: 0, NA: 0   FALSE
##  yes: 15, no: 0, pro: 0, NA: 0   FALSE
##   pro: 2, no: 0, yes: 0, NA: 0   FALSE
```

```r
# Proportion on prophylaxis treatment
## Too low to analyse separately
round(mean(data$rifafour_treatment == 'prophylaxis'), 3)
```

```
## [1] 0.024
```

```r
## ...so collapse 'yes' and 'prophylaxis'
data_tb <- data %>% 
    mutate(rifafour_treatment = fct_collapse(rifafour_treatment,
                                             yes = c('yes', 'prophylaxis')))

# 95% bootstrap confidence interval of the proportion on TB treatment
## Method = BCa, Resamples = 10000
set.seed(1234)
boot.ci(boot(data = data_tb, 
             statistic = function(d, i){mean(d[i, 'rifafour_treatment'] == 'yes')}, 
             R = 10000, 
             stype = 'i'), 
        type = 'bca') %>% 
    data_frame(n = nrow(filter(data_tb, !is.na(rifafour_treatment))), 
      Proportion = round(.$t0, 3), 
      Conf.level = 0.95, 
      Bca.lower = round(.$bca[[4]], 3),
      Bca.upper = round(.$bca[[5]], 3)) %>%
    .[1, -1] %>% 
    as.data.frame()
```

```
##    n Proportion Conf.level Bca.lower Bca.upper
## 1 83      0.205       0.95      0.12     0.289
```

```r
# Plot
data %>%
    mutate(rifafour_treatment = str_to_title(rifafour_treatment),
           rifafour_treatment = factor(rifafour_treatment,
                                       levels = c('No', 'Yes',
                                                  'Prophylaxis'),
                                       ordered = TRUE)) %>% 
    ggplot(data = .) +
    aes(x = 'All participants',
        fill = rifafour_treatment) +
    geom_bar(position = 'fill') + 
    geom_hline(yintercept = 0.5,
               linetype = 2) +
    labs(title = 'Being treated for TB at recruitment',
         y = 'Proportion') +
    scale_fill_grey(name = 'Treatment') +
    theme(axis.text.x = element_blank())
```

<img src="./figures/suppl-01-descriptive-whole-cohort/tb_rifafour-1.png" width="480" style="display: block; margin: auto;" />

## Diabetes
Classified as diabetic based on `data$hba1c_percent` > 7%. No participants were diabetic.

```r
# Tabular summary
data %>%
    select(diabetic_hba1c) %>%  
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 1 
## 
## ── Variable type:factor ────────────────────────────────────────────────────────────────
##        variable missing complete  n n_unique            top_counts ordered
##  diabetic_hba1c       2       81 83        1 no: 81, NA: 2, yes: 0   FALSE
```

## Vitamin B12 deficiency
Classed as B12 deficient based on `data$vitaminB12_pmol.l` < 141 pmol/l. Only one participant had a deficiency.

```r
# Tabular summary
data %>%
    select(vitaminB12_deficiency) %>%  
    skim()
```

```
## Skim summary statistics
##  n obs: 83 
##  n variables: 1 
## 
## ── Variable type:factor ────────────────────────────────────────────────────────────────
##               variable missing complete  n n_unique            top_counts
##  vitaminB12_deficiency       3       80 83        2 no: 79, NA: 3, yes: 1
##  ordered
##    FALSE
```

----

# Session information

```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS  10.14
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] bindrcpp_0.2.2   skimr_1.0.3      boot_1.3-20      rcompanion_2.0.0
##  [5] forcats_0.3.0    stringr_1.3.1    dplyr_0.7.8      purrr_0.2.5     
##  [9] readr_1.1.1      tidyr_0.8.2      tibble_1.4.2     ggplot2_3.1.0   
## [13] tidyverse_1.2.1  magrittr_1.5    
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.3.1         jsonlite_1.5       splines_3.5.1     
##  [4] BSDA_1.2.0         modelr_0.1.2       assertthat_0.2.0  
##  [7] expm_0.999-3       stats4_3.5.1       coin_1.2-2        
## [10] cellranger_1.1.0   yaml_2.2.0         pillar_1.3.0      
## [13] backports_1.1.2    lattice_0.20-38    glue_1.3.0        
## [16] digest_0.6.18      rvest_0.3.2        colorspace_1.3-2  
## [19] sandwich_2.5-0     htmltools_0.3.6    Matrix_1.2-15     
## [22] plyr_1.8.4         pkgconfig_2.0.2    broom_0.5.0       
## [25] haven_1.1.2        EMT_1.1            mvtnorm_1.0-8     
## [28] scales_1.0.0       manipulate_1.0.1   TH.data_1.0-9     
## [31] withr_2.1.2.9000   lazyeval_0.2.1     cli_1.0.1         
## [34] survival_2.43-1    crayon_1.3.4       readxl_1.1.0      
## [37] evaluate_0.12      fansi_0.4.0        nlme_3.1-137      
## [40] MASS_7.3-51.1      xml2_1.2.0         foreign_0.8-71    
## [43] class_7.3-14       tools_3.5.1        hms_0.4.2         
## [46] multcomp_1.4-8     munsell_0.5.0      compiler_3.5.1    
## [49] e1071_1.7-0        multcompView_0.1-7 rlang_0.3.0.1     
## [52] grid_3.5.1         rstudioapi_0.8     labeling_0.3      
## [55] rmarkdown_1.10     DescTools_0.99.26  gtable_0.2.0      
## [58] codetools_0.2-15   R6_2.3.0           zoo_1.8-4         
## [61] lubridate_1.7.4    knitr_1.20         utf8_1.1.4        
## [64] bindr_0.1.1        nortest_1.0-4      rprojroot_1.3-2   
## [67] modeltools_0.2-22  stringi_1.2.4      Rcpp_1.0.0        
## [70] tidyselect_0.2.5   lmtest_0.9-36
```
