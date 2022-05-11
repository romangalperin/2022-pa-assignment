Exercise 1
================

## Setting up data and environment

We first need to do a few things before we can manipulate the data.

``` r
# set path for R to find our data
data_path <- "~/Dropbox/McGill/teaching/2021-2022/2022_summer/ORGB672/data/"
setwd(data_path)

# load the necessary packages ("extensions")
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.1.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate) # to work with dates
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(skimr) # for summaries of data
library(arrow) # to be able to load data in the .parquet format
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
# read application data
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
```

## Let’s find the first and the last observed date for each examiner

We’ll first get examiner IDs and application dates in a separate
dataframe (table), for ease of manipulation. We’ll keep examiner ID (the
field `examiner_id`), and earliest and latest dates for each application
(`filing_date` and `appl_status_date` respectively).

``` r
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 

examiner_dates
```

    ## # A tibble: 2,018,477 × 3
    ##    examiner_id filing_date appl_status_date  
    ##          <dbl> <date>      <chr>             
    ##  1       96082 2000-01-26  30jan2003 00:00:00
    ##  2       87678 2000-10-11  27sep2010 00:00:00
    ##  3       63213 2000-05-17  30mar2009 00:00:00
    ##  4       73788 2001-07-20  07sep2009 00:00:00
    ##  5       77294 2000-04-10  19apr2001 00:00:00
    ##  6       68606 2000-04-28  16jul2001 00:00:00
    ##  7       89557 2004-01-26  15may2017 00:00:00
    ##  8       97543 2000-06-23  03apr2002 00:00:00
    ##  9       98714 2000-02-04  27nov2002 00:00:00
    ## 10       65530 2002-02-20  23mar2009 00:00:00
    ## # … with 2,018,467 more rows

The dates look inconsistent in terms of formatting. Let’s make them
consistent. We’ll create new variables `start_date` and `end_date`.

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))
```

Let’s now identify the earliest and the latest date for each examiner
and calculate the difference in days, which is their tenure in the
organization.

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure = interval(earliest_date, latest_date) %/% days(1)
    )

examiner_dates
```

    ## # A tibble: 5,649 × 4
    ##    examiner_id earliest_date latest_date tenure
    ##          <dbl> <date>        <date>       <dbl>
    ##  1       59012 2004-07-28    2015-07-24    4013
    ##  2       59025 2009-10-26    2017-05-18    2761
    ##  3       59030 2005-12-12    2017-05-22    4179
    ##  4       59040 2007-09-11    2017-05-23    3542
    ##  5       59052 2001-08-21    2007-02-28    2017
    ##  6       59054 2000-11-10    2016-12-23    5887
    ##  7       59055 2004-11-02    2007-12-26    1149
    ##  8       59056 2000-03-24    2017-05-22    6268
    ##  9       59074 2000-01-31    2017-03-17    6255
    ## 10       59081 2011-04-21    2017-05-19    2220
    ## # … with 5,639 more rows

``` r
skim(examiner_dates)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | examiner_dates |
| Number of rows                                   | 5649           |
| Number of columns                                | 4              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| Date                                             | 2              |
| numeric                                          | 2              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| earliest_date |         0 |             1 | 2000-01-02 | 2016-03-03 | 2003-01-21 |     2325 |
| latest_date   |         0 |             1 | 2000-09-14 | 9468-10-16 | 2017-05-19 |      888 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |     mean |       sd |    p0 |      p25 |   p50 |      p75 |    p100 | hist  |
|:--------------|----------:|--------------:|---------:|---------:|------:|---------:|------:|---------:|--------:|:------|
| examiner_id   |         1 |             1 | 78752.86 | 13575.30 | 59012 | 66531.75 | 75346 | 93750.75 |   99990 | ▇▆▃▂▇ |
| tenure        |         0 |             1 |  5844.53 | 54449.66 |    27 |  3125.00 |  4918 |  6097.00 | 2727903 | ▇▁▁▁▁ |
