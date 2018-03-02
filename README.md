
<!-- README.md is generated from README.Rmd. Please edit that file -->
relfeas
=======

The goal of relfeas is to allow researchers to use reliability reported in test-retest studies to make assessments of the feasibility of new study designs. This study accompanies the preprint: We need to talk about reliability: Making better use of test-retest studies for study design and interpretation.

The package is still a bit rough around the edges on some functions. It should be considered a work in progress.

Installation
------------

You can install relfeas from github with:

``` r
# install.packages("devtools")
devtools::install_github("mathesong/relfeas")
```

### Examples from the paper

Below, I detail the calculations performed in the manuscript, and show the code used from the package. Please refer to the manuscript for the context

``` r
library(relfeas)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(pwr)
library(knitr)
```

#### Example 1

``` r
sd2extrapRel(sd=0.32, icc_original = 0.32,
              sd_original = 0.10)
#> [1] 0.9335938

sd2extrapRel(sd = 0.32,icc_original =  0.32,
              sd_original =  0.10, rho = 2)
#> [1] 0.734375
```

#### Example 2

``` r
r_attenuation(0.8, 0.7)
#> [1] 0.7483315

pwr::pwr.r.test(r=sqrt(0.3), power=0.8)
#> 
#>      approximate correlation power calculation (arctangh transformation) 
#> 
#>               n = 23.00936
#>               r = 0.5477226
#>       sig.level = 0.05
#>           power = 0.8
#>     alternative = two.sided

pwr::pwr.r.test(r=sqrt(0.3)*r_attenuation(0.8, 0.7), power=0.8)
#> 
#>      approximate correlation power calculation (arctangh transformation) 
#> 
#>               n = 43.57966
#>               r = 0.409878
#>       sig.level = 0.05
#>           power = 0.8
#>     alternative = two.sided
```

#### Example 3

``` r
meanbp <- 1.91
delta_mean <- 0.12
delta_sd <- 0.1

sem <- icc2sem(icc = 0.8, sd =  0.22)
sd = delta_sd*(meanbp)

(delta_icc <- sem2icc(sem*2, sd))
#> [1] -0.06137441

samplesize <- 2

(sdd_indiv <- 100*((sem*1.96*sqrt(2))/meanbp))
#> [1] 14.27826

(sdd_group <- 100*(((sem/sqrt(samplesize))*1.96*sqrt(2))/meanbp))
#> [1] 10.09626

(power_n_within <- pwr::pwr.t.test(d=-12/7.3, sig.level = 0.05,
                                   type = "paired", alternative = "less",
                                   power=0.8))
#> 
#>      Paired t test power calculation 
#> 
#>               n = 4.014991
#>               d = -1.643836
#>       sig.level = 0.05
#>           power = 0.8
#>     alternative = less
#> 
#> NOTE: n is number of *pairs*


ss_total <- sumStat_total(n1 = 20, mean1 =  delta_mean*meanbp, sd1 = delta_sd*meanbp,
              n2 = 20, mean2 = 2.5*delta_mean*meanbp, sd2=delta_sd*meanbp)

(delta_icc_patcntrl <- sem2icc(sem*2, ss_total$sd_total))
#> [1] 0.4120227

(d_true <- ss_total$d)
#> [1] 1.8

(d_meas <- d_attenuation(rel_total = delta_icc_patcntrl, d = d_true))
#> [1] 0.9509363

(pwr_increase_unpaired <- pwr::pwr.t.test(d=d_meas, power = 0.8, alternative = "greater")$n /
  pwr::pwr.t.test(d=d_true, power = 0.8, alternative = "greater")$n)
#> [1] 3.081688

(pwr_increase_paired <- pwr::pwr.t.test(d=d_meas, power = 0.8, type = "paired", alternative = "greater")$n /
  pwr::pwr.t.test(d=d_true, type = "paired", power = 0.8)$n)
#> [1] 1.794431
```

#### Example 4

``` r
tspo_hab_10es <- 10/42
ser1b_10es <- 10/6

(tspo_n <- round(pwr::pwr.t.test(d = tspo_hab_10es, power = 0.8)$n))
#> [1] 278

(ser1b_n <- round(pwr::pwr.t.test(d = ser1b_10es, power = 0.8)$n))
#> [1] 7
```

#### Example 5

Note that the figure below is inspired by the work of Kristoffer Magnusson, a.k.a. R Psychologist, and his [Cohen's D interactive figure](http://rpsychologist.com/d3/cohend/) as well as his description of where [Cohen was wrong about overlap](http://rpsychologist.com/cohen-d-proportion-overlap).

``` r
(sd_basic <- extrapRel2sd(0.7, 0.5))
#> [1] 1.290994
(d_basic <- sdtot2mean2(sd_total = sd_basic, n1 = 20, n2=20, mean1 = 1))
#> $mean2pos
#> [1] 2.643168
#> 
#> $mean2neg
#> [1] -0.6431677
#> 
#> $meantotpos
#> [1] 1.821584
#> 
#> $meantotneg
#> [1] 0.1784162
#> 
#> $d
#> [1] 1.643168
#> 
#> $percdif
#> [1] 164.3168
(ol_basic <- d2overlap(d_basic$d))
#> [1] 0.4113138

(sd_acceptable <- extrapRel2sd(0.8, 0.5))
#> [1] 1.581139
(d_acceptable <- sdtot2mean2(sd_total = sd_acceptable, n1 = 20, n2=20, mean1 = 1))
#> $mean2pos
#> [1] 3.439262
#> 
#> $mean2neg
#> [1] -1.439262
#> 
#> $meantotpos
#> [1] 2.219631
#> 
#> $meantotneg
#> [1] -0.2196311
#> 
#> $d
#> [1] 2.439262
#> 
#> $percdif
#> [1] 243.9262
(ol_acceptable <- d2overlap(d_acceptable$d))
#> [1] 0.2226048

(sd_clin <- extrapRel2sd(0.9, 0.5))
#> [1] 2.236068
(d_clin <- sdtot2mean2(sd_total = sd_clin, n1 = 20, n2=20, mean1 = 1))
#> $mean2pos
#> [1] 4.962323
#> 
#> $mean2neg
#> [1] -2.962323
#> 
#> $meantotpos
#> [1] 2.981161
#> 
#> $meantotneg
#> [1] -0.9811613
#> 
#> $d
#> [1] 3.962323
#> 
#> $percdif
#> [1] 396.2323
(ol_clin <- d2overlap(d_clin$d))
#> [1] 0.04757319

graphtheme <- theme(axis.text.x=element_blank(),
                    axis.text.y=element_blank())

custcolours <- c('#85d4e3', '#e39f85')

d_fig <- grid.arrange(
  plot_difference(d = d_basic$d, colours=custcolours) + 
    labs(x='Lowest Acceptable Reliability (0.7)', title=NULL, y='Probability') + 
    graphtheme,
  plot_difference(d = d_acceptable$d, colours=custcolours) + 
    labs(x='Acceptable Reliability (0.8)', title=NULL, y='Probability') + 
    graphtheme,
  plot_difference(d = d_clin$d, colours=custcolours) + 
    labs(x='Clinical Reliability (0.9)', title=NULL, y='Probability') + 
    graphtheme,
  nrow=1)
```

<img src="man/figures/README-EffectSizes-1.png" width="100%" />

### Other Examples

Not all the functions are used in the paper. Here a demonstrate a couple of others.

#### Effect Size Comparisons

It is extremely important to consider effect sizes when performing study design. For this purpose, I have included plotting functions such as in example 5 above. I have also included a conversion function for Cohen's D alternatives for converting between them if one feels more intuitive than another.

``` r
## If Cohen's D feels more intuitive for some application
es_convert(d = 1)
#> $d
#> [1] 1
#> 
#> $u3
#> [1] 0.8413447
#> 
#> $overlap
#> [1] 0.6170751
#> 
#> $cles
#> [1] 0.7602499

## If the Common Language Effect Size feels more intuitive for another application
es_convert(cles = 0.8)
#> $d
#> [1] 1.190232
#> 
#> $u3
#> [1] 0.8830224
#> 
#> $overlap
#> [1] 0.5517659
#> 
#> $cles
#> [1] 0.8
```

#### Performing test-retest analysis

I have also included a test-retest analysis function which includes all the usual metrics which are reported in PET studies, as well as a few others which are useful. It also produces a tidy output, so it can be used with tidy data within R pipelines. I will use some data from the *agRee* package.

First we prepare the data into tidy format

``` r
data("petVT", package = 'agRee')

details <- map(petVT, ~nrow(.)) %>% 
  as.data.frame()

Region <- c(rep(names(details[1]), details[1]),
            rep(names(details[2]), details[2]),
            rep(names(details[3]), details[3]))

petVT <- do.call(rbind, petVT) %>% 
  as_tibble() %>% 
  rename(Measurement1=V1, Measurement2=V2) %>% 
  mutate(Region = Region) %>% 
  mutate(Participant = 1:n()) %>% 
  gather(Measurement, Outcome, -Participant, -Region)
```

Now the data looks as follows:

``` r
head(petVT)
#> # A tibble: 6 x 4
#>   Region   Participant Measurement  Outcome
#>   <chr>          <int> <chr>          <dbl>
#> 1 amygdala           1 Measurement1    38.1
#> 2 amygdala           2 Measurement1    24.7
#> 3 amygdala           3 Measurement1    20.4
#> 4 amygdala           4 Measurement1    29.9
#> 5 amygdala           5 Measurement1    18.3
#> 6 amygdala           6 Measurement1    23.3
```

Then we perform a test-retest analysis.

``` r
calc_trt <- petVT %>% 
  group_by(Region) %>% 
  nest() %>% 
  mutate(outcomes = map(data, ~ trt( as.data.frame(.x), values='Outcome', 
                                     cases = 'Participant', rater = 'Measurement' )))

trt_out <- map_df(calc_trt$outcomes, 'tidy') %>% 
  mutate(Region = calc_trt$Region)

kable(trt_out, digits = 2)
```

|   mean|    sd|   cov|  skew|  kurtosis|   icc|  icc\_l|  icc\_u|  wscv|   sdd|  absvar|  signvar|  signvar\_sd| Region     |
|------:|-----:|-----:|-----:|---------:|-----:|-------:|-------:|-----:|-----:|-------:|--------:|------------:|:-----------|
|  27.53|  6.18|  0.22|  0.10|     -1.43|  0.95|    0.83|    0.99|  0.05|  3.98|    0.06|    -0.02|         0.08| amygdala   |
|  12.26|  1.37|  0.11|  0.16|     -1.40|  0.83|    0.27|    0.97|  0.05|  1.65|    0.06|     0.03|         0.07| cerebellum |
|   0.79|  0.27|  0.35|  0.24|     -1.41|  0.36|   -0.28|    0.79|  0.28|  0.61|    0.30|    -0.01|         0.40| brainStem  |

The table lists the following for each sample:

##### Distribution

-   **mean**: Mean
-   **sd**: Standard deviation
-   **cov**: Coefficient of variation
-   **skew**: Skew
-   **kurtosis**: Kurtosis

##### Reliability

-   **icc**: ICC
-   **icc\_l**: Lower bound of the 95% confidence interval of ICC
-   **icc\_u**: Upper bound of the 95% confidence interval of ICC

##### Measurement Imprecision

-   **wscv**: Within-subject coefficient of variation: relative imprecision
-   **sdd**: Smallest detectable difference: absolute imprecision (95% confidence interval)

##### Variation

-   **absvar**: Absolute variability: the average absolute percentage change between measurements within individuals
-   **signvar**: Signed variability: same as above but signed. This is a check for bias between measurements
-   **signvar\_sd**: Standard deviation of the signed variance values. This is useful for power analysis for within-subjects designs.
