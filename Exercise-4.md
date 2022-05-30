Exercise 4
================
Oleg Kartavtsev
5/30/2022

### 1. Loading and pre-processing the data

First, we load the data

``` r
library(arrow)
```

    ## Warning: package 'arrow' was built under R version 4.1.3

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
library(readr)
applications <- read_parquet(paste0('C:\\Users\\oleg1\\Desktop\\McGill\\Classes\\Organizational Network Analysis\\app_data_sample.parquet'))
edges <- read_csv(paste0('C:\\Users\\oleg1\\Desktop\\McGill\\Classes\\Organizational Network Analysis\\edges_sample.csv'))
```

    ## Rows: 32906 Columns: 4

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (1): application_number
    ## dbl  (2): ego_examiner_id, alter_examiner_id
    ## date (1): advice_date

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
applications
```

    ## # A tibble: 2,018,477 x 16
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # ... with 2,018,467 more rows, and 12 more variables:
    ## #   examiner_name_middle <chr>, examiner_id <dbl>, examiner_art_unit <dbl>,
    ## #   uspc_class <chr>, uspc_subclass <chr>, patent_number <chr>,
    ## #   patent_issue_date <date>, abandon_date <date>, disposal_type <chr>,
    ## #   appl_status_code <dbl>, appl_status_date <chr>, tc <dbl>

``` r
edges
```

    ## # A tibble: 32,906 x 4
    ##    application_number advice_date ego_examiner_id alter_examiner_id
    ##    <chr>              <date>                <dbl>             <dbl>
    ##  1 09402488           2008-11-17            84356             66266
    ##  2 09402488           2008-11-17            84356             63519
    ##  3 09402488           2008-11-17            84356             98531
    ##  4 09445135           2008-08-21            92953             71313
    ##  5 09445135           2008-08-21            92953             93865
    ##  6 09445135           2008-08-21            92953             91818
    ##  7 09479304           2008-12-15            61767             69277
    ##  8 09479304           2008-12-15            61767             92446
    ##  9 09479304           2008-12-15            61767             66805
    ## 10 09479304           2008-12-15            61767             70919
    ## # ... with 32,896 more rows

As mentioned in the previous exercises, we use the “gender” package to
estimate the gender of the employees

``` r
library(gender)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v dplyr   1.0.7
    ## v tibble  3.1.6     v stringr 1.4.0
    ## v tidyr   1.1.4     v forcats 0.5.1
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x lubridate::as.difftime() masks base::as.difftime()
    ## x lubridate::date()        masks base::date()
    ## x lubridate::duration()    masks arrow::duration()
    ## x dplyr::filter()          masks stats::filter()
    ## x lubridate::intersect()   masks base::intersect()
    ## x dplyr::lag()             masks stats::lag()
    ## x lubridate::setdiff()     masks base::setdiff()
    ## x lubridate::union()       masks base::union()

``` r
library(dplyr)
# get examiner names
examiner_names <- applications %>% 
  distinct(examiner_name_first)

# get gender from their names
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

# remove extra columns from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells  4606881 246.1    7392907 394.9  6291556 336.1
    ## Vcells 49734786 379.5   92817436 708.2 80050347 610.8

Specify the date when the employee finished working on the application

``` r
applications <- applications %>% 
  mutate(patent_issue_date = coalesce(patent_issue_date, abandon_date))
names(applications)[11] <- "end_date"
```

At this point, we drop the null_values to help the quality of the
analysis

``` r
applications <- drop_na(applications, end_date)

applications <- drop_na(applications, filing_date)

applications <- drop_na(applications, gender)
```

Some necessary pre-processing (date manipulation)

``` r
#changing the format of the date column
applications$filing_date <- strptime(as.Date(applications$filing_date), "%Y-%m-%d")
applications$end_date <- strptime(as.Date(applications$end_date), "%Y-%m-%d")

applications$app_proc_time0 <- as.Date(applications$end_date) - as.Date(applications$filing_date)

#removing negative date values
remove <- c()
negative <- 0
zeroday1 <- as.difftime(negative, units = "days")

for (i in c(1: nrow(applications))) {
  if (applications$app_proc_time0[i] < zeroday1) {
    remove = c(remove, i)
  }
}

applications <- applications[-remove, ]
```

As a last step of dataset preparation, we add a app_proc_time column
that will be useful in the subsequent steps

``` r
applications$app_proc_time <- as.numeric(applications$app_proc_time0, units="days")
```

## 2. Picking workgroups to work with and calculating their centralities

we pick 2 workgroups to focus on: 163 and 172

``` r
w163 <- subset(applications, grepl("^163", applications$examiner_art_unit))
w163$gender <- factor(w163$gender)
w172 <- subset(applications, grepl("^172", applications$examiner_art_unit))
w172$gender <- factor(w172$gender)
```

Preparing the edges list

``` r
edges <- drop_na(edges, ego_examiner_id)
edges <-drop_na(edges, alter_examiner_id)

w163_edges <- inner_join(w163, edges, by = "application_number", copy = FALSE) 
w172_edges <- inner_join(w172, edges, by = "application_number", copy = FALSE)

remove1 <- c()
remove2 <- c()

for (i in c(1: nrow(w163_edges))) {
  if ((w163_edges$examiner_id[i] != w163_edges$ego_examiner_id[i])&(w163_edges$examiner_id[i] != w163_edges$alter_examiner_id[i])) {
    remove1 = c(remove1, i)
  }
}
for (i in c(1: nrow(w172_edges))) {
  if ((w172_edges$examiner_id[i] != w172_edges$ego_examiner_id[i])&(w172_edges$examiner_id[i] != w172_edges$alter_examiner_id[i])) {
    remove2 = c(remove2, i)
  }
}
w163_edges <- w163_edges[-remove1, ]
w172_edges <- w172_edges[-remove2, ]
```

Preparing the nodes list

``` r
w163_nodes_ego <- w163_edges %>% 
  distinct(ego_examiner_id) %>%
  rename(examiner_id = ego_examiner_id)

w163_nodes_alter <- w163_edges %>% 
  distinct(alter_examiner_id) %>%
  rename(examiner_id = alter_examiner_id)

w172_nodes_ego <- w172_edges %>% 
  distinct(ego_examiner_id) %>%
  rename(examiner_id = ego_examiner_id)

w172_nodes_alter <- w172_edges %>% 
  distinct(alter_examiner_id) %>%
  rename(examiner_id = alter_examiner_id)

w163_nodes <- union_all(w163_nodes_ego, w163_nodes_alter)
w172_nodes <- union_all(w172_nodes_ego, w172_nodes_alter)

w163_nodes <- unique(w163_nodes)
w172_nodes <- unique(w172_nodes)

#creating the edgelists
w163_edges_f <- w163_edges %>% 
  select(ego_examiner_id, alter_examiner_id)

w172_edges_f <- w172_edges %>% 
  select(ego_examiner_id, alter_examiner_id)
```

Calculating the centralities for each workgroup and merging them into a
single dataset

``` r
library(ggplot2)
library(igraph)
```

    ## Warning: package 'igraph' was built under R version 4.1.3

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     %--%, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(ggraph)
```

    ## Warning: package 'ggraph' was built under R version 4.1.3

``` r
library(tidygraph)
```

    ## Warning: package 'tidygraph' was built under R version 4.1.3

    ## 
    ## Attaching package: 'tidygraph'

    ## The following object is masked from 'package:igraph':
    ## 
    ##     groups

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(ggcorrplot)
```

    ## Warning: package 'ggcorrplot' was built under R version 4.1.3

``` r
g_w163 <- graph_from_data_frame(w163_edges_f, directed=TRUE)
g_w172 <- graph_from_data_frame(w172_edges_f, directed=TRUE)

# degree centrality
dg_w163 <- degree(g_w163)
dg_w172 <- degree(g_w172)

# betweenness centrality
bc_w163 <- betweenness(g_w163)
bc_w172 <- betweenness(g_w172)

# closeness centrality
cc_w163 <- closeness(g_w163)
cc_w172 <- closeness(g_w172)

# eigenvector centrality
ei_163 <- eigen_centrality(g_w163)$vector
ei_172 <- eigen_centrality(g_w172)$vector

centralities_163 <- cbind(bc_w163, dg_w163, cc_w163, ei_163)
centralities_172 <- cbind(bc_w172, dg_w172, cc_w172, ei_172)

centralities_df_163 <- cbind(w163_nodes, centralities_163)
centralities_df_172 <- cbind(w172_nodes, centralities_172)

head(centralities_df_163)
```

    ##       examiner_id bc_w163 dg_w163    cc_w163      ei_163
    ## 72253       72253      47      25 0.11111111 0.362241846
    ## 94257       94257       0      12 0.06666667 0.132603003
    ## 72848       72848       0       2 0.50000000 0.006338945
    ## 59407       59407       0       6 0.16666667 0.017391980
    ## 97242       97242       0       7 0.25000000 0.014202215
    ## 71385       71385       0       4 0.25000000 0.002867919

Join the centralities with app_proc_date and others on examiner ID

``` r
processed_163 <- inner_join(w163_edges, centralities_df_163, by = "examiner_id", copy = FALSE)
processed_172 <- inner_join(w172_edges, centralities_df_172, by = "examiner_id", copy = FALSE)
head(processed_163)
```

    ## # A tibble: 6 x 26
    ##   application_number filing_date         examiner_name_last examiner_name_first
    ##   <chr>              <dttm>              <chr>              <chr>              
    ## 1 09484331           2000-01-18 00:00:00 SINGH              ANOOP              
    ## 2 09709170           2000-11-10 00:00:00 GIBBS              TERRA              
    ## 3 09709170           2000-11-10 00:00:00 GIBBS              TERRA              
    ## 4 09709170           2000-11-10 00:00:00 GIBBS              TERRA              
    ## 5 09788268           2001-02-16 00:00:00 NEGIN              RUSSELL            
    ## 6 09788268           2001-02-16 00:00:00 NEGIN              RUSSELL            
    ## # ... with 22 more variables: examiner_name_middle <chr>, examiner_id <dbl>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, end_date <dttm>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <fct>, app_proc_time0 <drtn>, app_proc_time <dbl>,
    ## #   advice_date <date>, ego_examiner_id <dbl>, alter_examiner_id <dbl>,
    ## #   bc_w163 <dbl>, dg_w163 <dbl>, cc_w163 <dbl>, ei_163 <dbl>

## 3. Fit linear regression

Performing pre-processing for both groups

``` r
#163

to_drop2 <- c("application_number","examiner_name_first","examiner_name_last","examiner_name_middle","filing_date", "end_date", "abandon_date", "app_proc_time0", "appl_status_date", "advice_date","tc","ego_examiner_id","alter_examiner_id","examiner_id","patent_number")
processed_163_f <- processed_163[ , !(names(processed_163) %in% to_drop2)]

#172

to_drop3 <- c("application_number","examiner_name_first","examiner_name_last","examiner_name_middle","filing_date", "end_date", "abandon_date", "app_proc_time0", "appl_status_date", "advice_date","tc","ego_examiner_id","alter_examiner_id","examiner_id","patent_number")
processed_172_f <- processed_172[ , !(names(processed_172) %in% to_drop3)]

# as.factor for 163
processed_163_f$gender <- as.factor(processed_163_f$gender)
processed_163_f$disposal_type <- as.factor(processed_163_f$disposal_type)
processed_163_f$uspc_class <- as.factor(processed_163_f$uspc_class)
processed_163_f$uspc_subclass <- as.factor(processed_163_f$uspc_subclass)

# as.factor for 172
processed_172_f$gender <- as.factor(processed_172_f$gender)
processed_172_f$disposal_type <- as.factor(processed_172_f$disposal_type)
processed_172_f$uspc_class <- as.factor(processed_172_f$uspc_class)
processed_172_f$uspc_subclass <- as.factor(processed_172_f$uspc_subclass)

# rename for 163
names(processed_163_f)[8] <- "betweenness"
names(processed_163_f)[9] <- "degree"
names(processed_163_f)[10] <- "closeness"
names(processed_163_f)[11] <- "eigen"

# rename for 172
names(processed_172_f)[8] <- "betweenness"
names(processed_172_f)[9] <- "degree"
names(processed_172_f)[10] <- "closeness"
names(processed_172_f)[11] <- "eigen"
```

### Fitting the model and printing the summary for group 163

``` r
model_163 <- lm(app_proc_time ~ betweenness+degree+closeness+eigen, data=processed_163_f)
summary(model_163)
```

    ## 
    ## Call:
    ## lm(formula = app_proc_time ~ betweenness + degree + closeness + 
    ##     eigen, data = processed_163_f)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1334.07  -531.55   -95.07   566.52  1903.36 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2305.281    139.882  16.480  < 2e-16 ***
    ## betweenness   -2.306      5.838  -0.395  0.69329    
    ## degree       -11.065     15.649  -0.707  0.48038    
    ## closeness   -503.111    177.455  -2.835  0.00508 ** 
    ## eigen       -505.877    310.751  -1.628  0.10522    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 703.7 on 188 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.09011,    Adjusted R-squared:  0.07075 
    ## F-statistic: 4.655 on 4 and 188 DF,  p-value: 0.001322

With Adjusted R^2 of 0.07075, this model can explain only about 7% of
the variation in the dataset. In addition, all centralities are
negatively related to the target variable with betweenness being closer
to 0 (\~-2) and eigenvector being the furthest one (\~-506). Closeness
centrality seems to be the only significant variable in the regression.

### Fitting the model and printing the summary for group 172

``` r
model_172 <- lm(app_proc_time ~ betweenness+degree+closeness+eigen, data=processed_172_f)
summary(model_172)
```

    ## 
    ## Call:
    ## lm(formula = app_proc_time ~ betweenness + degree + closeness + 
    ##     eigen, data = processed_172_f)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -881.2 -330.6    0.0  219.8 1216.5 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2804.27     179.95  15.584  < 2e-16 ***
    ## betweenness    90.84     156.56   0.580    0.563    
    ## degree        -66.13      13.96  -4.735 6.74e-06 ***
    ## closeness    -359.15     285.66  -1.257    0.211    
    ## eigen        1128.26     224.01   5.037 1.93e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 431.6 on 107 degrees of freedom
    ## Multiple R-squared:  0.2346, Adjusted R-squared:  0.206 
    ## F-statistic:   8.2 on 4 and 107 DF,  p-value: 8.302e-06

With Adjusted R^2 of 0.206, this model can explain about 20-21% of the
variation in the dataset, which is a much better result than the
regression for group 163. In addition, beetweenness centrality and
eigenvector centrality are both positively related to the target
variables, with degree and eigenvector centralities being significant.

## 4. Combining the groups together

``` r
conc <- rbind(processed_163_f, processed_172_f)
nrow(conc)
```

    ## [1] 306

``` r
model_conc <- lm(app_proc_time ~ betweenness+degree+closeness+eigen, data=conc)
summary(model_conc)
```

    ## 
    ## Call:
    ## lm(formula = app_proc_time ~ betweenness + degree + closeness + 
    ##     eigen, data = conc)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1451.99  -490.19    61.82   498.26  1899.63 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2396.045    110.083  21.766   <2e-16 ***
    ## betweenness   -5.996      5.280  -1.136    0.257    
    ## degree       -25.246     10.883  -2.320    0.021 *  
    ## closeness   -458.554    153.238  -2.992    0.003 ** 
    ## eigen        164.106    204.151   0.804    0.422    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 652.6 on 300 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.04689,    Adjusted R-squared:  0.03418 
    ## F-statistic:  3.69 on 4 and 300 DF,  p-value: 0.005977

The Adjusted R^ is 0.03418, which means that the model explains about
%3-4 of the variation within the dataset, which is significantly lower
than the R^2 for group 172 only, and still lower than R^2 for group 163.
Eigenvector is the only centrality that has a positive relation with the
target variable, but it is insignificant with P of 0.422. The only
significant centralities are closeness (more significant) and degree
(less significant) and they are negatively related, meaning that higher
degree and closeness centrality make the application process shorter.

## 5. Including Gender

``` r
model_gender <- lm(app_proc_time ~ gender*betweenness+gender*degree+gender*closeness+gender*eigen, data=conc)
summary(model_gender)
```

    ## 
    ## Call:
    ## lm(formula = app_proc_time ~ gender * betweenness + gender * 
    ##     degree + gender * closeness + gender * eigen, data = conc)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1445.67  -335.97   -18.71   423.93  2012.90 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2098.62     159.43  13.163  < 2e-16 ***
    ## gendermale               293.81     212.73   1.381 0.168276    
    ## betweenness             -223.18     102.55  -2.176 0.030322 *  
    ## degree                    34.82      17.56   1.983 0.048277 *  
    ## closeness                109.97     211.54   0.520 0.603548    
    ## eigen                  -2284.56     547.20  -4.175 3.93e-05 ***
    ## gendermale:betweenness   219.12     102.66   2.134 0.033641 *  
    ## gendermale:degree        -79.50      21.65  -3.671 0.000286 ***
    ## gendermale:closeness    -381.11     310.25  -1.228 0.220267    
    ## gendermale:eigen        3104.30     588.26   5.277 2.55e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 588.2 on 295 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.2386, Adjusted R-squared:  0.2154 
    ## F-statistic: 10.27 on 9 and 295 DF,  p-value: 8.75e-14

Gender seems to really help improving the model, with the R^2 going up
to 0.2154. With gender in play, it seems that betweenness, eigenvector
and degree centralities become slightly significant. According to the
summary of the model, male employee with high degree and closeness
centralities is more likely to be quicker to process the applications,
while male employee with high beetweenness and (especially) eigenvector
centralities is more likely to take longer to process the applcation.

FINAL WORD: It seems that R^2 and variable significance can
significantly vary depending on the group. If we combine the groups
together, this doesn’t seem to improve. However, addition of gender to
the model helps explain much more. As an extension, it might be useful
to try an add more variables (tenure, race) to see whether it has a
positive effect on the predictive power of the regression.
