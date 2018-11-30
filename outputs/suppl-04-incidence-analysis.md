---
title: "Supplement 4"
subtitle: 'HIV-SN incidence analysis'
author: 'Peter Kamerman and Prinisha Pillay'
date: "30 November 2018"
---



----

# Import data

```r
data <- read_rds('data-cleaned/clean_data.rds') %>% 
    # Select columns 
    select(ID, visit_number, visit_day, 
           hivsn_present, pain) 
```

# Process data

## Estimate SN onset date
We estimated the date of SN onset (_'approximate day'_) as the mid-point between two successive visits when a participant was found to have transitioned between SN:no to SN:yes. Otherwise, _'approximate day'_ was taken to be the visit_day.

```r
### Create filter of participants who developed SN 
sn_filter <- unique(data$ID[data$hivsn_present == 'yes'])

# SN:yes group
approx_sn <- data %>%
    # Filter for participants that developed SN
    filter(ID %in% sn_filter) %>%
    # Group by ID
    group_by(ID) %>%
    # Approximate days
    mutate(approximate_day = ifelse(visit_number == 1,
                                    yes = 0,
                                    no = ifelse(hivsn_present == 'yes',
                                                yes = (visit_day + lag(visit_day)) / 2,
                                                no = visit_day)),
           approximate_day = as.integer(round(approximate_day))) %>%
    # Add person years
    mutate(visit_year = visit_day / 365.25,
           approximate_year = approximate_day / 365.25)

# SN:no group
approx_no_sn <- data %>%
    # Filter for participants that did not develop SN
    filter(!ID %in% sn_filter) %>%
    # Group by ID
    group_by(ID) %>%
    # approximate days (because no SN developed, use visit_day)
    mutate(approximate_day = visit_day,
           approximate_day = as.integer(round(approximate_day))) %>%
    # Add person years
    mutate(visit_year = visit_day / 365.25,
           approximate_year = approximate_day / 365.25)

# Put approx_* data frames together
data <- bind_rows(approx_sn, approx_no_sn)
```

## Cumulative incidence data
Extract 3 and 6 month periods for cumulative incidence.  
**Note: Data were censored at last screening within each 3/6 month period.**

```r
## Generate time filters
months_6 <- round(lubridate::dyears(0.5) / lubridate::ddays(1)) 
months_3 <- round(lubridate::dyears(0.25) / lubridate::ddays(1))
    
# Extract SN:yes within 6-month period
data_6months <- data %>%
    # Filter values within 6 months
    filter(visit_day <= months_6) %>%
    # Group by ID 
    group_by(ID) %>%
    # Filter max visit_day
    filter(visit_day == max(visit_day))

# Extract SN:yes within first 3-month period
data_3months <- data %>%
    # Filter values within 6 months
    filter(visit_day <= months_3) %>%
    # Group by ID 
    group_by(ID) %>%
    # Filter max visit_day
    filter(visit_day == max(visit_day))

# Extract SN:yes within second 3-month period
data_3to6months <-  data_6months %>%
    # Filter out those with SN with first 3 months
    filter(!ID %in% data_3months[data_3months$hivsn_present == 'yes', ]$ID) 
```

## Incidence rate data
Only need full 6-month period

```r
# Extract SN:yes for full period
df_6months <- data %>%
    # Group by ID 
    group_by(ID) %>%
    # Filter max visit_day
    filter(visit_day == max(visit_day))
```

## Survival analysis data

```r
# Create a data frame of SN:yes patients
data_sn.yes <- data %>%
    # Select columns
    select(ID, 
           visit_number, 
           visit_day, 
           visit_year,
           approximate_day,
           approximate_year,
           hivsn_present,
           pain) %>%
    # Add counting columns
    ## SN:yes and SN:no
    mutate(sn_count = ifelse(hivsn_present == 'yes',
                             yes = 1,
                             no = 0)) %>%
    # Create counting index to identify when SN first develop
    ## Sum sn_count
    group_by(ID) %>%
    mutate(sn_count = cumsum(sn_count)) %>%
    # Filter by sn_count == 1 to get when SN first developed
    filter(sn_count == 1) %>%
    # Remove sn_count
    select(-sn_count)

# Create data frame of SN:no patients
data_sn.no <- data %>%
    # Select columns
    select(ID, 
           visit_number, 
           visit_day, 
           visit_year,
           approximate_day,
           approximate_year,
           hivsn_present,
           pain) %>%
    # Filter out SN:yes patient with the filter created earlier
    filter(!ID %in% sn_filter) %>%
    # Filter out 'repeats' (only the data at the last visit)
    group_by(ID) %>%
    mutate(max_visits = max(visit_number)) %>%
    filter(visit_number == max_visits) %>%
    select(-max_visits) 

# Merge the SN:yes and SN:no data frames
data_surv <- data_sn.yes %>%
    full_join(data_sn.no) %>%
    # New column with recoded hivsn_present data for survival analysis
    mutate(hivsn_coded = ifelse(hivsn_present == 'yes',
                                yes = 1, # SN:yes
                                no = 0)) # SN:no

# Add a new column with painful SN (but only if they have SN)
data_surv <- data_surv %>%
    mutate(hivsn_painful = ifelse(hivsn_present == 'yes' & pain == 'yes',
                                  yes = 'yes',
                                  no = ifelse(hivsn_present == 'yes' &
                                                  pain == 'no',
                                              yes = 'no',
                                              no = NA))) 
```

## Modelling data

```r
# Identify and extract information on SN development (at anytime) 
# by looking at the presence of SN at the final visit
data_model <- data %>%
    select(ID, visit_number, hivsn_present) %>%
    group_by(ID) %>%
    mutate(max_visit = max(visit_number)) %>%
    filter(visit_number == max_visit) %>%
    select(ID, hivsn_present) %>%
    rename(sn = hivsn_present)

# Join data_sn to data
data_model <- read_rds('data-cleaned/clean_data.rds') %>%
    left_join(data_model) %>% 
    select(-hivsn_present)

# Restrict data to the baseline visit (visit 1)
data_model %<>%
    filter(visit_number == 1)

# Clean-up df_model data frame for analyses
## These data will be used for all the baseline charateristic analyses.
data_model %<>%
    # Recode rifafour 'prophylaxis' to 'yes'
    mutate(rifafour_treatment = as.character(rifafour_treatment),
           rifafour_treatment = ifelse(rifafour_treatment == 'prophylaxis',
                                       yes = 'yes',
                                       no = rifafour_treatment),
           rifafour_treatment = factor(rifafour_treatment)) %>%
    # Select data that will be modelled
    select(sn, age_years, sex, mass_kg, height_m, CD4_cell.ul,
           viral_load_copies.ml, alcohol_units.week, TB_current,
           rifafour_treatment) 
```
---

# Analysis

## Cumulative incidence

Cumulative incidence measures the number of new cases per person in the population over a defined period of time (i.e., a fixed follow-up period). Therefore, to calculate cumulative incidence we defined a fixed 6-month follow-up period, and also subdivided this period into a 1^st^ and 2^nd^ 3-month period of this follow-up. To standardize the periods of follow-up, data were cleaned such that only visits falling into the indicated periods ((0-3 months], (3-6 months], (0-6 months]) were used to define SN status. As such, participant's whose last clinic visit occurred after 91 days (end of first 3-month interval) or 182 days (end of 6-month interval), and who were found to have new-onset SN at this visit, were recorded as SN:no over the 3 or 6-month period of follow-up. This conservative strategy may have lead to a slight under-estimation of the cumulative incidence of SN.  

## Boostrap function

```r
## Formula for cases / person year
cases_boot <- function(data, i){
    foo <- data[i, ]
    tab <- table(foo$hivsn_present)
    prop <- round(prop.table(tab) * 100)
    case <- prop * 10
    case[2]
}
```

### Full six-month period

```r
# Tabulate SN:yes/SN:no
tab_sn <- table(data_6months$hivsn_present)

# Calculate proportions, convert to percent
prop_sn <- round(prop.table(tab_sn) * 100) 

# Bootstrap cases per 1000 patients
## Method: BCa, resamples: 1999
cases_ci <- boot.ci(boot.out = boot(data = data_6months, 
                                    statistic = cases_boot, 
                                    R = 1999, 
                                    stype = 'i'),
                    type = 'bca')

# Create summary table
as.data.frame(tab_sn) %>%
    rename(Count = Freq) %>%
    left_join(as.data.frame(prop_sn)) %>%
    rename(Percentage = Freq,
           sn_present = Var1) %>%
    bind_cols(`Cases/1000 patients` = c('', paste(cases_ci$t0))) %>%
    bind_cols(`Conf.interval` = c('', paste(cases_ci$bca[4],' - ', cases_ci$bca[5])))
```

```
##   sn_present Count Percentage Cases/1000 patients Conf.interval
## 1         no    66         80                                  
## 2        yes    17         20                 200   120  -  290
```

### First 3-month period

```r
# Tabulate SN:yes/SN:no
tab_sn <- table(data_3months$hivsn_present)

# Calculate proportions, convert to percent
prop_sn <- round(prop.table(tab_sn) * 100) 

# Bootstrap cases per 1000 patients
## Method: BCa, resamples: 1999
cases_ci <- boot.ci(boot.out = boot(data = data_3months, 
                                    statistic = cases_boot, 
                                    R = 1999, 
                                    stype = 'i'),
                    type = 'bca')

# Create summary table
as.data.frame(tab_sn) %>%
    rename(Count = Freq) %>%
    left_join(as.data.frame(prop_sn)) %>%
    rename(Percentage = Freq,
           sn_present = Var1) %>%
    bind_cols(`Cases/1000 patients` = c('', paste(cases_ci$t0))) %>%
    bind_cols(`Conf.interval` = c('', paste0(cases_ci$bca[4],' to ', cases_ci$bca[5])))
```

```
##   sn_present Count Percentage Cases/1000 patients Conf.interval
## 1         no    76         92                                  
## 2        yes     7          8                  80     20 to 140
```

## Second 3-month period

```r
# Tabulate SN:yes/SN:no
tab_sn <- table(data_3to6months$hivsn_present)

# Calculate proportions, convert to percent
prop_sn <- round(prop.table(tab_sn) * 100) 

# Bootstrap cases per 1000 patients
## Method: BCa, resamples: 1999
cases_ci <- boot.ci(boot.out = boot(data = data_3to6months, 
                                    statistic = cases_boot, 
                                    R = 1999, 
                                    stype = 'i'),
                    type = 'bca')

# Create summary table
as.data.frame(tab_sn) %>%
    rename(Count = Freq) %>%
    left_join(as.data.frame(prop_sn)) %>%
    rename(Percentage = Freq,
           sn_present = Var1) %>%
    bind_cols(`Cases/1000 patients` = c('', paste(cases_ci$t0))) %>%
    bind_cols(`Conf.interval` = c('', paste0(cases_ci$bca[4],' to ', cases_ci$bca[5])))
```

```
##   sn_present Count Percentage Cases/1000 patients Conf.interval
## 1         no    66         87                                  
## 2        yes    10         13                 130     50 to 210
```

# Incidence rate (person years)

Incidence rate is a measure of the number of new cases per unit of time. We did not have extact dates of SN onset to define the per patient unit of time, and nor was there uniform spacing between clinic visits. We therefore chose to calculate an approximate SN onset time, arbitrarily defined as the number of days between the first neuropathy screening and the mid-point between the visit when neuropathy was detected and the preceeding visit. For participants who did not develop SN, the date of censoring was defined by the number of days between the first neuropathy screening and the last screening (study exit).

## Boostrap function

```r
## Formula for cases / person year
boot_df <- function(data, i){
    foo <- data[i, ]
    cases <- sum(foo$hivsn_present == 'yes')
    person_time <- sum(foo$approximate_year)
    cases / person_time
}
```

## Six-month period

```r
# Bootstrap confidence intervals
df_ci <- boot.ci(boot.out = boot(data = df_6months,
                                 statistic = boot_df,
                                 R = 1999,
                                 stype = 'i'),
                 type = 'bca')

data_frame(`Cases/person.year` = round(df_ci$t0, 2),
           Conf.interval = paste0(round(df_ci$bca[4], 2), ' to ',
                                 round(df_ci$bca[5], 2)))
```

```
## # A tibble: 1 x 2
##   `Cases/person.year` Conf.interval
##                 <dbl> <chr>        
## 1                0.47 0.29 to 0.69
```

# Survival curves

## Plot a basic Kaplan-Meyer survival curve

```r
# Basic Kaplan-Meyer (KM)
## No predictors (~ 1)
km_basic <- survfit(Surv(time = approximate_day, 
                         event = hivsn_coded) ~ 1, 
                    data = data_surv)

# Summary of KM fit
km_basic
```

```
## Call: survfit(formula = Surv(time = approximate_day, event = hivsn_coded) ~ 
##     1, data = data_surv)
## 
##       n  events  median 0.95LCL 0.95UCL 
##      83      20      NA      NA      NA
```

```r
summary(km_basic)
```

```
## Call: survfit(formula = Surv(time = approximate_day, event = hivsn_coded) ~ 
##     1, data = data_surv)
## 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##    14     83       1    0.988  0.0120        0.965        1.000
##    22     82       1    0.976  0.0168        0.943        1.000
##    23     81       1    0.964  0.0205        0.925        1.000
##    28     80       2    0.940  0.0261        0.890        0.992
##    31     78       1    0.928  0.0284        0.874        0.985
##    46     77       1    0.916  0.0305        0.858        0.977
##    47     76       1    0.904  0.0324        0.842        0.969
##    51     75       1    0.892  0.0341        0.827        0.961
##    52     74       1    0.880  0.0357        0.812        0.952
##    53     73       1    0.867  0.0372        0.798        0.944
##    55     72       1    0.855  0.0386        0.783        0.935
##    56     71       1    0.843  0.0399        0.769        0.925
##    64     70       1    0.831  0.0411        0.755        0.916
##    73     69       1    0.819  0.0422        0.741        0.906
##    84     68       1    0.807  0.0433        0.727        0.897
##   104     67       1    0.795  0.0443        0.713        0.887
##   132     66       1    0.783  0.0452        0.699        0.877
##   143     65       1    0.771  0.0461        0.686        0.867
##   144     64       1    0.759  0.0469        0.672        0.857
```

```r
# Plot
ggsurvplot(km_basic,
           conf.int = TRUE,
           fun = 'event',
           risk.table = 'abs_pct', 
           cumcensor = TRUE,
           cumevents = TRUE, 
           ggtheme = theme_bw(), 
           tables.theme = theme(plot.title = element_text(size = 12,
                                                          face = 'bold'),
                                panel.border = element_blank(),
                                panel.grid.major.x = element_blank(),
                                axis.title = element_blank(),
                                axis.text.x = element_blank(),
                                axis.ticks = element_blank()),
           fontsize = 4,
           tables.height = 0.1,
           legend = 'none',
           palette = '#0072B2', 
           ylim = c(0, 1),
           xlab = 'Days', 
           title = 'Kaplan-Meier Survival Curve: SN vs Days')
```

<img src="figures/suppl-04-incidence-analysis/km_basic-1.png" style="display: block; margin: auto;" />

```r
# Publication plot
## New plot theme
theme_new <- function(){
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(size = 22,
                                 colour = '#000000'),
        axis.title = element_text(size = 24,
                                  colour = '#000000'),
        panel.grid.major.x = element_line(size = 1,
                                          colour = '#CCCCCC'),
        panel.border = element_rect(size = 1.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())
    }

## Plot
gg_plot <- ggsurvplot(km_basic,
                      fun = 'event',
                      conf.int = TRUE,
                      conf.int.fill = '#666666',
                      risk.table = 'abs_pct', 
                      cumcensor = TRUE,
                      cumevents = TRUE,
                      tables.theme = theme(plot.title = element_text(size = 22,
                                                                     face = 'bold'),
                                           panel.border = element_blank(),
                                           panel.grid.major.x = element_blank(),
                                           axis.title = element_blank(),
                                           axis.text = element_blank(),
                                           axis.ticks = element_blank()),
                      fontsize = 7,
                      risk.table.height = 0.08,
                      cumevents.height = 0.08,
                      cumcensor.height = 0.08,
                      xlab = 'Days', 
                      ylab = 'Cumulative proportion\n',
                      ylim = c(0, 1),
                      palette = c('#000000'),
                      legend = 'none',
                      ggtheme = theme_new())

## Save
ggsave(filename = 'figures/survival-curve.png', 
       plot = print(gg_plot), 
       width = 12.7, 
       height = 12)
```

## Plot a basic KM curve conditioned on painful SN
In addition we performed a log-rank test to asssess for differences between painful and non-painful SN strata. 

```r
# Incidence ~ pain
km_pain <- survfit(Surv(time = approximate_day, 
                        event = hivsn_coded) ~ pain,
                   data = data_surv)

# Summary
km_pain
```

```
## Call: survfit(formula = Surv(time = approximate_day, event = hivsn_coded) ~ 
##     pain, data = data_surv)
## 
##           n events median 0.95LCL 0.95UCL
## pain=no  65     11     NA      NA      NA
## pain=yes 18      9    104      55      NA
```

```r
summary(km_pain)
```

```
## Call: survfit(formula = Surv(time = approximate_day, event = hivsn_coded) ~ 
##     pain, data = data_surv)
## 
##                 pain=no 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##    23     65       1    0.985  0.0153        0.955        1.000
##    28     64       1    0.969  0.0214        0.928        1.000
##    31     63       1    0.954  0.0260        0.904        1.000
##    47     62       1    0.938  0.0298        0.882        0.999
##    51     61       1    0.923  0.0331        0.861        0.990
##    53     60       1    0.908  0.0359        0.840        0.981
##    56     59       1    0.892  0.0384        0.820        0.971
##    64     58       1    0.877  0.0407        0.801        0.961
##   132     57       1    0.862  0.0428        0.782        0.950
##   143     56       1    0.846  0.0448        0.763        0.939
##   144     55       1    0.831  0.0465        0.744        0.927
## 
##                 pain=yes 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##    14     18       1    0.944  0.0540        0.844        1.000
##    22     17       1    0.889  0.0741        0.755        1.000
##    28     16       1    0.833  0.0878        0.678        1.000
##    46     15       1    0.778  0.0980        0.608        0.996
##    52     14       1    0.722  0.1056        0.542        0.962
##    55     13       1    0.667  0.1111        0.481        0.924
##    73     12       1    0.611  0.1149        0.423        0.883
##    84     11       1    0.556  0.1171        0.368        0.840
##   104     10       1    0.500  0.1179        0.315        0.794
```

```r
# Log-rank test
survdiff(Surv(time = visit_day, 
              event = hivsn_coded) ~ pain,
         data = data_surv)
```

```
## Call:
## survdiff(formula = Surv(time = visit_day, event = hivsn_coded) ~ 
##     pain, data = data_surv)
## 
##           N Observed Expected (O-E)^2/E (O-E)^2/V
## pain=no  65       11    16.68      1.93      11.9
## pain=yes 18        9     3.32      9.72      11.9
## 
##  Chisq= 11.9  on 1 degrees of freedom, p= 6e-04
```

```r
# Plot
ggsurvplot(km_pain,
           conf.int = TRUE,
           fun = 'event',
           risk.table = 'abs_pct', 
           cumcensor = TRUE,
           cumevents = TRUE, 
           surv.median.line = 'hv', 
           pval = TRUE, 
           ggtheme = theme_bw(), 
           tables.theme = theme(plot.title = element_text(size = 12,
                                                          face = 'bold'),
                                panel.border = element_blank(),
                                panel.grid.major.x = element_blank(),
                                axis.title = element_blank(),
                                axis.text.x = element_blank(),
                                axis.ticks = element_blank()),
           fontsize = 4,
           tables.height = 0.14,
           legend = 'none',
           legend.labs = c('Non-painful SN', 'Painful SN'), 
           palette = c('#0072B2', '#D55E00'), 
           ylim = c(0, 1),
           xlab = 'Days', 
           title = 'Kaplan-Meier Survival Curve: SN vs Days')
```

<img src="figures/suppl-04-incidence-analysis/km_pain-1.png" style="display: block; margin: auto;" />

# Multivariable modeling

In an exploratory analysis using Cox proportional hazard models (not shown here) various predictors violated assumptions of the model (e.g., proportional hazard, linearity, no influence points). Therefore we used logistic regression modelling, with visit 1 charateristics as predictors of SN onset. 

**Note: Only complete datasets used (n = 77)**

## Model data

The model does not include:

1. `diabetes_hba1c` because nobody had HbA1c > 7%.  

2. `vitaminB12_deficiency` because only one individual had a deficiency.  

3. `mass_kg` because it is likely to be co-linear with `height_m`.  

4. `consumes_alcohol` because this information was deemed to be encoded in `alcohol_units.week`.

## Model selection

### Check height vs mass assumption

```r
# Double-check presumed 'mass_kg' vs 'height_m' relationship before proceeding
ggplot(data = data_model) +
    aes(x = height_m,
        y = mass_kg,
        colour = sex) +
    geom_point() +
    geom_smooth(method = 'lm') +
    scale_colour_manual(values = c('#000000', '#999999')) +
    labs(title = 'Relationship between body mass and height') +
    theme_bw()
```

<img src="figures/suppl-04-incidence-analysis/model_selection-1.png" style="display: block; margin: auto;" />

```r
# Males
with(data_model[data_model$sex == 'M', ], cor.test(height_m, mass_kg))
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  height_m and mass_kg
## t = 0.99029, df = 35, p-value = 0.3288
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.1679090  0.4642755
## sample estimates:
##      cor 
## 0.165092
```

```r
# Females
with(data_model[data_model$sex == 'F', ], cor.test(height_m, mass_kg))
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  height_m and mass_kg
## t = 4.7005, df = 44, p-value = 2.575e-05
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.3459427 0.7436566
## sample estimates:
##       cor 
## 0.5781765
```

The two variables are related in females, but not in males. We decided to omit `weight_kg` in favour of `height_m` because of this relationship, and the stronger historical association between height and SN.

## Lasso regression

We chose to use LASSO (least absolute shrinkage and selection operator) for variable selection. LASSO is a regression analysis method that performs both variable selection and regularization in order to enhance the prediction accuracy and interpretability of the statistical model it produces.

The process involves performing a 10-fold cross validation to find the optimal _lambda_ (penalization parameter). And then running the analysis and extracting the model based on the best _lambda_. 

### Cross-validation to estimate alpha and lambda

```r
# Remove mass_kg
data_model %<>%
    select(-mass_kg)

cvafit <- cva.glmnet(sn ~ ., 
                     data = data_model, 
                     family = 'binomial')

# Print CVA object
print(cvafit)
```

```
## Call:
## cva.glmnet.formula(formula = sn ~ ., data = data_model, family = "binomial")
## 
## Model fitting options:
##     Sparse model matrix: FALSE
##     Use model.frame: FALSE
##     Alpha values: 0 0.001 0.008 0.027 0.064 0.125 0.216 0.343 0.512 0.729 1
##     Number of crossvalidation folds for lambda: 10
```

```r
# Plot CVA object log(lambda) vs CV loss (deviance) for each value of alpha
plot(cvafit)
```

<img src="figures/suppl-04-incidence-analysis/cv_lamda-1.png" style="display: block; margin: auto;" />

```r
# Plot minimum CV loss (deviance) for each value of alpha
minlossplot(cvafit)
```

<img src="figures/suppl-04-incidence-analysis/cv_lamda-2.png" style="display: block; margin: auto;" />

```r
# Print model at each of the four lowest CV loss alphas
## alpha = 0.216
plot(cvafit$modlist[[7]])
title('alpha = 0.216', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/cv_lamda-3.png" style="display: block; margin: auto;" />

```r
## alpha = 0.343
plot(cvafit$modlist[[8]])
title('alpha = 0.343', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/cv_lamda-4.png" style="display: block; margin: auto;" />

```r
## alpha = 0.512
plot(cvafit$modlist[[9]])
title('alpha = 0.512', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/cv_lamda-5.png" style="display: block; margin: auto;" />

```r
## Alpha = 0.729
plot(cvafit$modlist[[10]])
title('alpha = 0.729', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/cv_lamda-6.png" style="display: block; margin: auto;" />

```r
## and alpha = 1
plot(cvafit$modlist[[11]])
title('alpha = 1.0', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/cv_lamda-7.png" style="display: block; margin: auto;" />

```r
# Extract best lambda for each alpha (lambda.1se)
## alpha = 0.216
a216 <- cvafit$modlist[[7]]$lambda.1se
## alpha = 0.343
a343 <- cvafit$modlist[[8]]$lambda.1se
## alpha = 0.512
a512 <- cvafit$modlist[[9]]$lambda.1se
## Alpha = 0.729
a729 <- cvafit$modlist[[10]]$lambda.1se
## and alpha = 1
a100 <- cvafit$modlist[[11]]$lambda.1se
```

### Fit the models using CV alphas and best lambdas

```r
# Fit the models for each alpha
## alpha = 0.216
model_a216 <- glmnet(sn ~ .,
                     data = data_model, 
                     alpha = 0.216,
                     family = 'binomial')

## alpha = 0.343
model_a343 <- glmnet(sn ~ .,
                     data = data_model, 
                     alpha = 0.343,
                     family = 'binomial')

## alpha = 0.512
model_a512 <- glmnet(sn ~ .,
                     data = data_model, 
                     alpha = 0.512,
                     family = 'binomial')

## alpha = 0.729
model_a729 <- glmnet(sn ~ .,
                     data = data_model, 
                     alpha = 0.729,
                     family = 'binomial')

## alpha = 1.0
model_a100 <- glmnet(sn ~ .,
                     data = data_model, 
                     alpha = 1,
                     family = 'binomial')

# Plot and get coefficients for each model
## alpha = 0.216
plot(model_a216,
     xvar = 'lambda',
     label = TRUE)
abline(v = log(a216))
title('alpha = 0.216', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/model_fit-1.png" style="display: block; margin: auto;" />

```r
coef(model_a216, s = a216)
```

```
## 12 x 1 sparse Matrix of class "dgCMatrix"
##                                  1
## (Intercept)           -5.868271933
## age_years              0.005959892
## sexF                   .          
## sexM                   .          
## height_m               2.884865711
## CD4_cell.ul            .          
## viral_load_copies.ml   .          
## alcohol_units.week    -0.006712765
## TB_currentno          -0.332727110
## TB_currentyes          0.332745908
## rifafour_treatmentno   .          
## rifafour_treatmentyes  .
```

```r
## alpha = 0.343
plot(model_a343,
     xvar = 'lambda',
     label = TRUE)
abline(v = log(a343))
title('alpha = 0.343', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/model_fit-2.png" style="display: block; margin: auto;" />

```r
coef(model_a343, s = a343)
```

```
## 12 x 1 sparse Matrix of class "dgCMatrix"
##                                  1
## (Intercept)           -6.874854081
## age_years              0.005083932
## sexF                   .          
## sexM                   .          
## height_m               3.519414238
## CD4_cell.ul            .          
## viral_load_copies.ml   .          
## alcohol_units.week    -0.006075501
## TB_currentno          -0.342617370
## TB_currentyes          0.342512903
## rifafour_treatmentno   .          
## rifafour_treatmentyes  .
```

```r
## alpha = 0.512
plot(model_a512,
     xvar = 'lambda',
     label = TRUE)
abline(v = log(a512))
title('alpha = 0.512', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/model_fit-3.png" style="display: block; margin: auto;" />

```r
coef(model_a512, s = a512)
```

```
## 12 x 1 sparse Matrix of class "dgCMatrix"
##                                  1
## (Intercept)           -7.727807354
## age_years              0.003078953
## sexF                   .          
## sexM                   .          
## height_m               4.079043177
## CD4_cell.ul            .          
## viral_load_copies.ml   .          
## alcohol_units.week    -0.003859431
## TB_currentno          -0.334707205
## TB_currentyes          0.334470861
## rifafour_treatmentno   .          
## rifafour_treatmentyes  .
```

```r
## alpha = 0.729
plot(model_a729,
     xvar = 'lambda',
     label = TRUE)
abline(v = log(a729))
title('alpha = 0.729', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/model_fit-4.png" style="display: block; margin: auto;" />

```r
coef(model_a729, s = a729)
```

```
## 12 x 1 sparse Matrix of class "dgCMatrix"
##                                   1
## (Intercept)           -8.411396e+00
## age_years              7.211446e-18
## sexF                   .           
## sexM                   .           
## height_m               4.556000e+00
## CD4_cell.ul            .           
## viral_load_copies.ml   .           
## alcohol_units.week    -1.690991e-17
## TB_currentno          -3.166942e-01
## TB_currentyes          3.116287e-01
## rifafour_treatmentno   .           
## rifafour_treatmentyes  .
```

```r
## alpha = 1.0
plot(model_a100,
     xvar = 'lambda',
     label = TRUE)
abline(v = log(a100))
title('alpha = 1.0', line = 2.5)
```

<img src="figures/suppl-04-incidence-analysis/model_fit-5.png" style="display: block; margin: auto;" />

```r
coef(model_a100, s = a100)
```

```
## 12 x 1 sparse Matrix of class "dgCMatrix"
##                                   1
## (Intercept)           -8.796416e+00
## age_years              .           
## sexF                   .           
## sexM                   .           
## height_m               4.945696e+00
## CD4_cell.ul            .           
## viral_load_copies.ml   .           
## alcohol_units.week     .           
## TB_currentno          -5.581879e-01
## TB_currentyes          5.350501e-15
## rifafour_treatmentno   .           
## rifafour_treatmentyes  .
```

Across all alphas, and best lambdas, the output shows the best model includes `height_m` and `TB_current`. 

Fit trimmed model using selected variables

```r
mod_final <- glm(sn ~ height_m + TB_current,
               data = data_model, 
               family = binomial(link = 'logit'))

## Generate model summaries
### (Type II SS ANOVA shows significant effect for height_m, TB_current)
### (NOTE: CI of odds ratios for height_m and TB_current are wide)
print(mod_final)
```

```
## 
## Call:  glm(formula = sn ~ height_m + TB_current, family = binomial(link = "logit"), 
##     data = data_model)
## 
## Coefficients:
##   (Intercept)       height_m  TB_currentyes  
##        -19.27          10.72           1.43  
## 
## Degrees of Freedom: 82 Total (i.e. Null);  80 Residual
## Null Deviance:	    91.66 
## Residual Deviance: 74 	AIC: 80
```

```r
car::Anova(mod_final)
```

```
## Analysis of Deviance Table (Type II tests)
## 
## Response: sn
##            LR Chisq Df Pr(>Chisq)   
## height_m     7.7696  1   0.005313 **
## TB_current   5.0998  1   0.023929 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod_final)
```

```
## 
## Call:
## glm(formula = sn ~ height_m + TB_current, family = binomial(link = "logit"), 
##     data = data_model)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.3935  -0.6913  -0.4096  -0.2304   2.4169  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)   
## (Intercept)   -19.2742     7.0573  -2.731  0.00631 **
## height_m       10.7248     4.2267   2.537  0.01117 * 
## TB_currentyes   1.4297     0.6357   2.249  0.02452 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 91.663  on 82  degrees of freedom
## Residual deviance: 73.996  on 80  degrees of freedom
## AIC: 79.996
## 
## Number of Fisher Scoring iterations: 5
```

```r
OR(mod_final, what = 'all')[1:3, c(1, 2, 6:8)] 
```

```
##            model      logit           OR        lowOR         upOR
## 1:   (Intercept) -19.274156 4.259322e-09 1.304270e-11 1.390956e-06
## 2:      height_m  10.724798 4.546955e+04 1.392348e+02 1.484887e+07
## 3: TB_currentyes   1.429731 4.177577e+00 1.279239e-02 1.364261e+03
```

## Model diagnostics

```r
# Diagnostic measures for a binary regression model by covariate pattern
data_dx <- dx(mod_final, byCov = TRUE)

# Outliers and leverage
## Plot 'dChisq' for each probability (P).
### 'DChisq' gives the change in the Pearson chi-square statistic with 
### observation i removed.
### Values should be < 4 if the observation has little influence on the model.
(gg_out <- ggplot(data = data_dx) +
    aes(x = P,
        y = dChisq) +
    geom_point(shape = 21,
               size = 2,
               fill = '#FFFFFF') +
    geom_hline(yintercept = 4,
               linetype =2))
```

<img src="figures/suppl-04-incidence-analysis/model_diagnostics-1.png" style="display: block; margin: auto;" />

```r
### Label dodgy point with height and TB info
gg_out + 
    geom_label(label = data_dx %>% 
                   filter(dChisq == max(dChisq)) %>% 
                   mutate(new = str_glue('height_m: {height_m}, TB(yes): {TB_currentyes}')) %>% 
                   .$new,
               x = data_dx %>% 
                   filter(dChisq == max(dChisq)) %>% 
                   .$P,
               y = data_dx %>% 
                   filter(dChisq == max(dChisq)) %>% 
                   .$dChisq,
              hjust = -0.1)
```

<img src="figures/suppl-04-incidence-analysis/model_diagnostics-2.png" style="display: block; margin: auto;" />

```r
### Influence plot
influenceIndexPlot(mod_final, 
                   id.n = 3, 
                   id.col = 'red')
```

<img src="figures/suppl-04-incidence-analysis/model_diagnostics-3.png" style="display: block; margin: auto;" />

```r
influencePlot(mod_final, 
              id.col = 'red')
```

<img src="figures/suppl-04-incidence-analysis/model_diagnostics-4.png" style="display: block; margin: auto;" />

```
##       StudRes        Hat      CookD
## 6   2.5024065 0.02341176 0.14362726
## 45 -1.0571673 0.08129135 0.02233145
## 47  2.5024065 0.02341176 0.14362726
## 61 -0.6759985 0.11187100 0.01125179
```

```r
### Remove influence points one at a time
mod_influence6 <- update(mod_final, 
                         subset = c(-6))
mod_influence46 <- update(mod_final, 
                          subset = c(-46))
mod_influence58 <- update(mod_final, 
                          subset = c(-58))

### Compare coefficients with and without influence points
compareCoefs(mod_final, mod_influence6)
```

```
## Calls:
## 1: glm(formula = sn ~ height_m + TB_current, family = binomial(link = 
##   "logit"), data = data_model)
## 2: glm(formula = sn ~ height_m + TB_current, family = binomial(link = 
##   "logit"), data = data_model, subset = c(-6))
## 
##               Model 1 Model 2
## (Intercept)    -19.27  -24.07
## SE               7.06    8.11
##                              
## height_m        10.72   13.52
## SE               4.23    4.82
##                              
## TB_currentyes   1.430   1.513
## SE              0.636   0.652
## 
```

```r
compareCoefs(mod_final, mod_influence46)
```

```
## Calls:
## 1: glm(formula = sn ~ height_m + TB_current, family = binomial(link = 
##   "logit"), data = data_model)
## 2: glm(formula = sn ~ height_m + TB_current, family = binomial(link = 
##   "logit"), data = data_model, subset = c(-46))
## 
##               Model 1 Model 2
## (Intercept)    -19.27  -19.05
## SE               7.06    7.03
##                              
## height_m        10.72   10.60
## SE               4.23    4.21
##                              
## TB_currentyes   1.430   1.418
## SE              0.636   0.636
## 
```

```r
compareCoefs(mod_final, mod_influence58)
```

```
## Calls:
## 1: glm(formula = sn ~ height_m + TB_current, family = binomial(link = 
##   "logit"), data = data_model)
## 2: glm(formula = sn ~ height_m + TB_current, family = binomial(link = 
##   "logit"), data = data_model, subset = c(-58))
## 
##               Model 1 Model 2
## (Intercept)    -19.27  -19.07
## SE               7.06    7.09
##                              
## height_m        10.72   10.60
## SE               4.23    4.25
##                              
## TB_currentyes   1.430   1.427
## SE              0.636   0.635
## 
```

```r
## Check '6' and '46' for anything strange 
data_model %>%
    select(sn, height_m, TB_current) %>%
    .[6, ]
```

```
## # A tibble: 1 x 3
##   sn    height_m TB_current
##   <fct>    <dbl> <fct>     
## 1 yes       1.53 no
```

```r
data_model %>%
    select(sn, height_m, TB_current) %>%
    .[46, ]
```

```
## # A tibble: 1 x 3
##   sn    height_m TB_current
##   <fct>    <dbl> <fct>     
## 1 no        1.61 no
```

```r
# Test for multicolinearity using variance inflation factor (vif)
## Rule of thumb: values > 4 are a problem
vif(mod_final) 
```

```
##   height_m TB_current 
##     1.0178     1.0178
```

```r
# Test for correlated residuals 
## p-vale > 0.05: no correlation between residuals
durbinWatsonTest(mod_final)
```

```
##  lag Autocorrelation D-W Statistic p-value
##    1       -0.056383      2.105132   0.646
##  Alternative hypothesis: rho != 0
```

```r
# Plot 'dBhat' for each probability (P).
## 'dBhat' is the change in beta coefficient with observation i excluded.
## 'dBhat' should be < 1 if the i has little influence on the coefficients.
ggplot(data = data_dx) +
    aes(x = P,
        y = dBhat) +
    geom_point(shape = 21,
               size = 2,
               fill = '#FFFFFF') +
    geom_hline(yintercept = 1,
               linetype =2)
```

<img src="figures/suppl-04-incidence-analysis/model_diagnostics-5.png" style="display: block; margin: auto;" />

```r
# Check goodness of fit (Hosmer-Lemeshow goodness of fit test)
## Significant p-values indicate a poor fit
## ROC 0.7 < auc < 0.8 indicates acceptable discrimination 
gof(mod_final, g = 8)
```

<img src="figures/suppl-04-incidence-analysis/model_diagnostics-6.png" style="display: block; margin: auto;" />

```
##       chiSq df    pVal
## PrI  90.992 80 0.18827
## drI  73.996 80 0.66785
## PrG  28.008 36 0.82691
## drG  29.809 36 0.75690
## PrCT 28.008 36 0.82691
## drCT 29.809 36 0.75690
##                      val df    pVal
## HL chiSq         4.66186  6 0.58785
## mHL F            0.63294  7 0.72505
## OsRo Z                NA NA 0.33399
## SstPgeq0.5 Z     1.21921 NA 0.22276
## SstPl0.5 Z       1.54241 NA 0.12297
## SstBoth chiSq    3.86551  2 0.14475
## SllPgeq0.5 chiSq 2.64901  1 0.10361
## SllPl0.5 chiSq   2.35301  1 0.12504
## SllBoth chiSq    3.08826  2 0.21350
```

----

# Session information

```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS  10.14.1
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
##  [1] bindrcpp_0.2.2  LogisticDx_0.2  survminer_0.4.3 ggpubr_0.2     
##  [5] survival_2.43-3 car_3.0-2       carData_3.0-2   boot_1.3-20    
##  [9] glmnetUtils_1.1 forcats_0.3.0   stringr_1.3.1   dplyr_0.7.8    
## [13] purrr_0.2.5     readr_1.2.1     tidyr_0.8.2     tibble_1.4.2   
## [17] ggplot2_3.1.0   tidyverse_1.2.1 magrittr_1.5   
## 
## loaded via a namespace (and not attached):
##  [1] TH.data_1.0-9       colorspace_1.3-2    rio_0.5.16         
##  [4] rprojroot_1.3-2     htmlTable_1.12      base64enc_0.1-3    
##  [7] rstudioapi_0.8      MatrixModels_0.4-1  fansi_0.4.0        
## [10] mvtnorm_1.0-8       lubridate_1.7.4     xml2_1.2.0         
## [13] codetools_0.2-15    splines_3.5.1       knitr_1.20         
## [16] Formula_1.2-3       jsonlite_1.5        speedglm_0.3-2     
## [19] pROC_1.13.0         broom_0.5.0         km.ci_0.5-2        
## [22] cluster_2.0.7-1     aod_1.3             compiler_3.5.1     
## [25] httr_1.3.1          backports_1.1.2     assertthat_0.2.0   
## [28] Matrix_1.2-15       lazyeval_0.2.1      cli_1.0.1          
## [31] acepack_1.4.1       htmltools_0.3.6     quantreg_5.36      
## [34] tools_3.5.1         gtable_0.2.0        glue_1.3.0         
## [37] Rcpp_1.0.0          cellranger_1.1.0    nlme_3.1-137       
## [40] iterators_1.0.10    openxlsx_4.1.0      rvest_0.3.2        
## [43] statmod_1.4.30      polspline_1.1.13    MASS_7.3-51.1      
## [46] zoo_1.8-4           scales_1.0.0        hms_0.4.2          
## [49] parallel_3.5.1      sandwich_2.5-0      SparseM_1.77       
## [52] RColorBrewer_1.1-2  yaml_2.2.0          curl_3.2           
## [55] gridExtra_2.3       KMsurv_0.1-5        rms_5.1-2          
## [58] rpart_4.1-13        latticeExtra_0.6-28 stringi_1.2.4      
## [61] foreach_1.4.4       checkmate_1.8.5     zip_1.0.0          
## [64] rlang_0.3.0.1       pkgconfig_2.0.2     evaluate_0.12      
## [67] lattice_0.20-38     bindr_0.1.1         labeling_0.3       
## [70] htmlwidgets_1.3     cmprsk_2.2-7        tidyselect_0.2.5   
## [73] plyr_1.8.4          R6_2.3.0            Hmisc_4.1-1        
## [76] multcomp_1.4-8      pillar_1.3.0        haven_2.0.0        
## [79] foreign_0.8-71      withr_2.1.2.9000    abind_1.4-5        
## [82] nnet_7.3-12         modelr_0.1.2        crayon_1.3.4       
## [85] survMisc_0.5.5      utf8_1.1.4          rmarkdown_1.10     
## [88] grid_3.5.1          readxl_1.1.0        data.table_1.11.8  
## [91] digest_0.6.18       xtable_1.8-3        munsell_0.5.0      
## [94] glmnet_2.0-16
```

