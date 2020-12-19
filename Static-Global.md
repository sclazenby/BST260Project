Static Global
================
December 7, 2020

``` r
library(tidyverse)
library(rvest)
library(lubridate)
library(knitr)
library(zoo)
library(maps)
library(ggplot2)
library(stringr)
library(dplyr)
library(ggrepel)
library(ggthemes)
```

\#\#Overview, Motivation, and Related Work

While the COVID-19 pandemic marches on, there continues to be a dearth
of evidence that illustrates **how SARS-CoV-2 impacts men and women
differently, and why these trends might vary over space.** When data
first emerged from China, the virus was dubbed “a man killer,” for men
were 2.4 times as likely to die from the virus. A similar trend was then
observed in Italy, where a disproportionate amount of men were dying
from the virus (men were representing over 70% of the deaths). Initial
hypotheses for the observed health outcomes ran the gamut from
differences in smoking and health-seeking behaviors to immunological
differences and variances in ACE 2 receptors for the coronavirus.

As COVID-19 continued to spread, researchers and journalists began to
investigate other gendered impacts of the pandemic. After studying the
rise in domestic abuse, caregiving and homeschooling responsibilities,
and exposure to the virus through over-representation in ‘essential
work,’ many have determined that the virus was indeed exacerbating
inequities and a “disaster for feminism.” The problem, at that time, was
that the paucity of sex-disaggregated data made this difficult to prove
and nearly impossible to act on.

The good news, according to Global Health 5050, is that in recent
months, more and more countries (now 79) have begun to **report
sex-disaggregated case and mortality data** – acknowledging that this is
integral to understanding the virus and informing a strong COVID-19
response. While the prevailing hypothesis today is that more men die
from COVID-19 even if and when more women are exposed, this trend is not
ubiquitous (in Vietnam, for example, just 37% of the deaths are male,
compared to Bangladesh where this number is 77%). **This project seeks
to illustrate the observed differences, and explore potential factors
that could explain what we are witnessing in the US and globally.**

This project was inspired by the [Global
Health 5050](https://globalhealth5050.org/) , as well as the work out of
[Harvard’s GenderSci
Lab](https://www.genderscilab.org/gender-and-sex-in-covid19).

*Note: sex-disaggregated data does not report or account for gender
identity, therefore data are absent on the impact of COVID-19 on
transgender and non-binary people. Some efforts are underway to redress
this gap, but for the scope of this project we will use and sex and
gender interchangeably.*

``` r
global_data = read_csv("global5050_clean.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   `Country code` = col_character(),
    ##   Country = col_character(),
    ##   `Case & death data by sex?` = col_character(),
    ##   `Cases date` = col_date(format = ""),
    ##   `Cases where sex-disaggregated data is available` = col_double(),
    ##   `Cases (% male)` = col_double(),
    ##   `Cases (% female)` = col_double(),
    ##   `Deaths date` = col_date(format = ""),
    ##   `Deaths where sex-disaggregated data is available` = col_double(),
    ##   `Deaths (% male)` = col_character(),
    ##   `Deaths (% female)` = col_character(),
    ##   `Deaths in confirmed cases date` = col_date(format = ""),
    ##   `Proportion of deaths in confirmed cases (male)` = col_character(),
    ##   `Proportion of deaths in confirmed cases (female)` = col_character(),
    ##   `Proportion of deaths in confirmed cases (Male:female ratio)` = col_double(),
    ##   Source = col_character()
    ## )

``` r
countries = read_csv("Country_Region_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Country = col_character(),
    ##   Region = col_character(),
    ##   `Sub-Region` = col_character()
    ## )

``` r
#Scatterplot for Global Case Data

global_cases <- select(global_data, "Country code", "Country", "Cases (% male)", "Cases (% female)") %>%
  drop_na() %>%
  rename(male = 'Cases (% male)') %>%
  rename(female = 'Cases (% female)') %>%
  rename(code = 'Country code')


global_cases <- left_join(global_cases, countries)
```

    ## Joining, by = "Country"

``` r
c <- global_cases %>% 
  ggplot(aes(male, female, label = code)) +
  geom_text_repel() +
  xlab("Cases % Male") +
  ylab("Cases % Female") +
  ggtitle("Global COVID-19 Case Data Disaggregated by Sex") + 
  theme_economist()

c + geom_point(aes(color = Region), size = 3)
```

![](Static-Global_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#Scatterplot for Global Death Data - BROKEN 

global_deaths <- select(global_data, "Country code", "Country", "Deaths (% male)", "Deaths (% female)") %>%
  drop_na() %>%
  rename(male = 'Deaths (% male)') %>%
  rename(female = 'Deaths (% female)') %>%
  rename(code = 'Country code')


global_deaths <- left_join(global_deaths, countries)
```

    ## Joining, by = "Country"

``` r
d <- global_deaths %>% 
  ggplot(aes(male, female, label = code)) +
  geom_text_repel() +
  xlab("Deaths % Male") +
  ylab("Deaths % Female") +
  ggtitle("Global COVID-19 Death Data Disaggregated by Sex") + 
  theme_economist()

d + geom_point(aes(color = Region), size = 3)
```

![](Static-Global_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#Stacked Bar Chart for Global Cases 

b <- ggplot(global_cases, aes(x = code))


scale_y_continuous(labels = scales::percent_format())
```

    ## <ScaleContinuousPosition>
    ##  Range:  
    ##  Limits:    0 --    1

## Including Plots

You can also embed plots, for example:

![](Static-Global_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
