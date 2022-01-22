Revealing neural mechanisms of redundancy gain using a voxel-wise
background connectivity analysis
================
Korea Brain Research Institute, Cognitive Science Research Group, Deep
Memory Lab
2022-01-22

<br><br>

# Abstract

<br>

A previous study demonstrated that the retinotopic cortex shows greater
activity when identical stimuli are simultaneously presented on
different visual quadrants, than when different stimuli are presented,
which is termed ‘redundancy gain’. A theoretical account for the
phenomenon has been only suggestive: Responses in the early visual
cortex are enhanced by feedback from higher cortical areas with larger
receptive fields. Here we found the direct evidence supporting this
account in an fMRI study. We calculated the voxel-wise functional
connectivity between the early visual cortex and the higher-level
scene-dplyr::selective region (PPA) by using the background connectivity
analysis. We then examined how the strength of this functional
connectivity modulates the redundancy gain. In the first phase of an
experiment, four identical (same condition) or four different scene
images (different condition) were simultaneously presented in each
visual quadrant. We measured the redundancy gain in the early visual
cortex by comparing the same and different conditions. In the second
phase, participants were asked to attend to scene images while ignoring
superimposed face images. Using this task, we identified the voxel-wise
functional connectivity between the early visual cortex and PPA
independently of stimulus-evoked responses. In line with the previous
theoretical account that feedback from higher visual areas drives the
redundancy gain effect, the voxels in the early visual cortex (V1-V4)
showed greater redundancy gain when they had higher functional
connectivity with PPA. Our findings shed light on the interactive
mechanism explaining neural responses in the early retinotopic cortex.

<br><br>

------------------------------------------------------------------------

<br>

------------------------------------------------------------------------

<br>

# Results

<style type="text/css">
pre code, pre, code {
  white-space: pre !important;
  overflow-x: scroll !important;
  word-break: keep-all !important;
  word-wrap: initial !important;
}
</style>

``` r
set.seed(12345) # for reproducibility
options(knitr.kable.NA = '')

# install // load packages 
# Some packages need to be loaded. 
# We use `pacman` as a package manager, which takes care of the other packages. 
if (!require("distill", quietly = TRUE)) install.packages("distill")
if (!require("devtools", quietly = TRUE)) install.packages("devtools")
if (!require("papaja", quietly = TRUE)) devtools::install_github("crsh/papaja")
if (!require("patchwork", quietly = TRUE)) devtools::install_github("thomasp85/patchwork")
if (!require("klippy", quietly = TRUE)) devtools::install_github("RLesur/klippy")
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
if (!require("Rmisc", quietly = TRUE)) install.packages("Rmisc")
if (!require("ggbeeswarm", quietly = TRUE)) install.packages("ggbeeswarm") # Never load it directly.
pacman::p_load(tidyverse, papaja, knitr, dplyr, car, psych, afex, lme4, lmerTest, 
               emmeans, ggplot2, ggpubr, lattice, latticeExtra, parallel, effects, psycho, caret,
               effectsize, rstatix)
library("patchwork"); library("klippy")
klippy::klippy()
```

<script>
  addClassKlippyTo("pre.r, pre.markdown");
  addKlippy('left', 'top', 'auto', '1', 'Copy code', 'Copied!');
</script>

<br>

## Redundancy Gain Effect - Replication - Smoothed

<br>

### Behavioural Performance

<br>

Phase 1 Main Phase 의 central fixation task의 수행 요약치를 분석하였다.

<br>

``` r
p1 <- read.csv("data/main_behav.csv", header = T)

p1_l <- p1
p1_l$subj = factor(p1_l$subj)
p1_l <- p1_l %>% filter(subj!=99)
p1_l$run = factor(p1_l$run) 
p1_l$block = factor(p1_l$block) 
p1_l$cond = factor(p1_l$cond, levels=c(1,2,3), labels=c("single","same","diff")) 
# p1_l$catch = factor(p1_l$catch)
p1_l$rt = p1_l$rt*1000
p1_l$corr = 0
p1_l$corr[p1_l$catch==1 & p1_l$resp==1] = 1
p1_l$type = 0
p1_l$type[p1_l$catch==1 & p1_l$resp==1] = 1 #hit
p1_l$type[p1_l$catch==1 & p1_l$resp==0] = 2 #miss
p1_l$type[p1_l$catch==0 & p1_l$resp==1] = 3 #fa
p1_l$type[p1_l$catch==0 & p1_l$resp==0] = 4 #cr
p1_l$type <- factor(p1_l$type, levels=c(1,2,3,4), labels=c("hit","miss","fa","cr"))

headTail(p1_l)
##       subj  run block cond onset catch resp     rt corr type
## 1        1    1     1 diff     0     1    1 408.59    1  hit
## 2        1    1     1 diff   1.5     0    0      0    0   cr
## 3        1    1     1 diff     3     0    0      0    0   cr
## 4        1    1     1 diff   4.5     0    0      0    0   cr
## ...   <NA> <NA>  <NA> <NA>   ...   ...  ...    ...  ... <NA>
## 13821   25    4    12 diff   309     0    0      0    0   cr
## 13822   25    4    12 diff 310.5     1    0      0    0 miss
## 13823   25    4    12 diff   312     0    0      0    0   cr
## 13824   25    4    12 diff 313.5     0    0      0    0   cr
glimpse(p1_l, width = 70)
## Rows: 13,824
## Columns: 10
## $ subj  <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ run   <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ block <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, …
## $ cond  <fct> diff, diff, diff, diff, diff, diff, diff, diff, diff, …
## $ onset <dbl> 0.0, 1.5, 3.0, 4.5, 6.0, 7.5, 9.0, 10.5, 12.0, 13.5, 1…
## $ catch <int> 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, …
## $ resp  <int> 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, …
## $ rt    <dbl> 408.592260, 0.000000, 0.000000, 0.000000, 0.000000, 0.…
## $ corr  <dbl> 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ type  <fct> hit, cr, cr, cr, cr, cr, cr, hit, cr, cr, cr, cr, fa, …
```

<br>

제시 조건을 구분하지 않은 전반적인 요약치 d prime은 아래와 같다.

<br>

``` r
# subject-level, long format
dmy <- dummyVars(" ~ type", data = p1_l)
p1_2 <- data.frame(predict(dmy, newdata = p1_l))
p1_3 <- cbind(p1_l, p1_2)
p1_allL <- p1_3  %>% group_by(subj) %>%
  dplyr::summarise(hit=sum(`type.hit`), 
                   miss=sum(`type.miss`), 
                   fa=sum(`type.fa`),
                   cr=sum(`type.cr`),
                   targets=sum(`type.hit`)+sum(`type.miss`),
                   dists=sum(`type.fa`)+sum(`type.cr`),
                   hit.r=sum(`type.hit`)/(sum(`type.hit`)+sum(`type.miss`)), 
                   miss.r=sum(`type.miss`)/(sum(`type.hit`)+sum(`type.miss`)),
                   fa.r=sum(`type.fa`)/(sum(`type.fa`)+sum(`type.cr`)),
                   cr.r=sum(`type.cr`)/(sum(`type.fa`)+sum(`type.cr`))) %>%
  ungroup()

# subject-level, long format, calculating dprime
indices <- psycho::dprime(p1_allL$hit, p1_allL$fa, p1_allL$miss, p1_allL$cr, 
                          p1_allL$targets, p1_allL$dists)
indices <- psycho::dprime(p1_allL$hit.r, p1_allL$fa.r, p1_allL$miss.r, p1_allL$cr.r, 
                          1, 1)

p1_allL2 <- cbind(p1_allL, indices)
# p1_allL2 %>% kable(digits=2)

# check outlier
p1_allL3 <- p1_allL2 %>%
  mutate(lbound = mean(dprime)-3*sd(dprime),
         ubound = mean(dprime)+3*sd(dprime)) %>% 
  mutate(Outlier = (dprime < lbound)|(dprime > ubound)) %>% # set outlier
  ungroup()
p1_allL3$cond <- 1; p1_allL3$cond <- factor(p1_allL3$cond)
# p1_allL3 %>% dplyr::select(subj, hit, miss, fa, cr, dprime) %>% kable(digits=2)

# summary table: grand mean
p1_allG <- p1_allL3 %>% 
  summarise(dprime.m = mean(dprime), dprime.sd = sd(dprime)) %>%
  ungroup()
p1_allG$dprime.se <- Rmisc::summarySE(data = p1_allL3, measurevar = "dprime")$se
p1_allG$dprime.ci <- Rmisc::summarySE(data = p1_allL3, measurevar = "dprime")$ci
p1_allG <- p1_allG %>% 
  mutate(lower.ci = dprime.m-dprime.ci,
         upper.ci = dprime.m+dprime.ci)
p1_allG$cond <- 1; p1_allG$cond <- factor(p1_allG$cond)
p1_allG %>% dplyr::select(dprime.m, dprime.sd, dprime.se, lower.ci, upper.ci) %>% kable(digits=2)
```

| dprime.m | dprime.sd | dprime.se | lower.ci | upper.ci |
|---------:|----------:|----------:|---------:|---------:|
|     1.06 |      0.19 |      0.04 |     0.98 |     1.14 |

<br>

``` r
p_h1 <- p1_allL3 %>% 
  rstatix::t_test(dprime ~ 1, mu = 0, detailed= T, alternative = "two.sided") %>% 
  dplyr::select(estimate, conf.low, conf.high, df, statistic, p) 

p_h2 <- p1_allL3 %>% 
  rstatix::cohens_d(dprime ~ 1, mu = 0, ci = F) %>% 
  dplyr::select(effsize, magnitude) 

merge(p_h1, p_h2) %>% kable(digits=3)
```

| estimate | conf.low | conf.high |  df | statistic |   p | effsize | magnitude |
|---------:|---------:|----------:|----:|----------:|----:|--------:|:----------|
|    1.063 |    0.981 |     1.145 |  23 |    26.798 |   0 |    5.47 | large     |

<br>

제시 조건(single vs. 4-same vs. 4-different)에 따른 차이가 있는지
분석하였다. 요약치는 아래와 같다.

<br>

``` r
# subject-level, long format
dmy <- dummyVars(" ~ type", data = p1_l)
p1_2 <- data.frame(predict(dmy, newdata = p1_l))
p1_3 <- cbind(p1_l, p1_2)

p1_allL <- p1_3  %>% group_by(subj, cond) %>%
  dplyr::summarise(hit=sum(`type.hit`), 
                   miss=sum(`type.miss`), 
                   fa=sum(`type.fa`),
                   cr=sum(`type.cr`),
                   targets=sum(`type.hit`)+sum(`type.miss`),
                   dists=sum(`type.fa`)+sum(`type.cr`),
                   hit.r=sum(`type.hit`)/(sum(`type.hit`)+sum(`type.miss`)), 
                   miss.r=sum(`type.miss`)/(sum(`type.hit`)+sum(`type.miss`)),
                   fa.r=sum(`type.fa`)/(sum(`type.fa`)+sum(`type.cr`)),
                   cr.r=sum(`type.cr`)/(sum(`type.fa`)+sum(`type.cr`))) %>%
  ungroup()
## `summarise()` has grouped output by 'subj'. You can override using the `.groups` argument.
# p1_allL %>% kable(digits=2)

# subject-level, long format, calculating dprime
# indices <- psycho::dprime(p1_allL$hit, p1_allL$fa, p1_allL$miss, p1_allL$cr, 
#                           p1_allL$targets, p1_allL$dists)
indices <- psycho::dprime(p1_allL$hit.r, p1_allL$fa.r, p1_allL$miss.r, p1_allL$cr.r, 
                          1, 1)

p1_allL2 <- cbind(p1_allL, indices)
# p1_allL2 %>% kable(digits=2)

# check outlier
p1_allL3 <- p1_allL2 %>% group_by(cond) %>% 
  mutate(lbound = mean(dprime)-3*sd(dprime),
         ubound = mean(dprime)+3*sd(dprime)) %>% 
  mutate(Outlier = (dprime < lbound)|(dprime > ubound)) %>% # set outlier
  ungroup()
# p1_allL3$cond <- 1; p1_allL3$cond <- factor(p1_allL3$cond)
# p1_allL3 %>% dplyr::select(subj, hit, miss, fa, cr, dprime) %>% kable(digits=2)

# summary table: grand mean
p1_allG <- p1_allL3 %>% group_by(cond) %>% 
  summarise(dprime.m = mean(dprime), dprime.sd = sd(dprime)) %>%
  ungroup()

p1_allG$dprime.se <- Rmisc::summarySEwithin(data = p1_allL3, measurevar = "dprime", idvar = "subj", 
                                      withinvars = c("cond"))$se
p1_allG$dprime.ci <- Rmisc::summarySEwithin(data = p1_allL3, measurevar = "dprime", idvar = "subj", 
                                            withinvars = c("cond"))$ci

p1_allG <- p1_allG %>% group_by(cond) %>% 
  mutate(lower.ci = dprime.m-dprime.ci,
         upper.ci = dprime.m+dprime.ci)
# p1_allG$cond <- 1; p1_allG$cond <- factor(p1_allG$cond)
p1_allG %>% dplyr::select(dprime.m, dprime.sd, dprime.se, lower.ci, upper.ci) %>% kable(digits=2)
## Adding missing grouping variables: `cond`
```

| cond   | dprime.m | dprime.sd | dprime.se | lower.ci | upper.ci |
|:-------|---------:|----------:|----------:|---------:|---------:|
| single |     1.07 |      0.19 |      0.01 |     1.04 |     1.10 |
| same   |     1.05 |      0.21 |      0.02 |     1.02 |     1.09 |
| diff   |     1.07 |      0.20 |      0.01 |     1.04 |     1.09 |

<br>

RM anova 결과, 참가자들의 수행에서 제시 조건에 따른 차이는 유의하지
않았다.

<br>

``` r
p1.aov1 <- aov_ez(id="subj", dv="dprime", data = p1_allL3, within = c("cond"),
                  anova_table = list(correction = "none"))
# summary(t1.aov1)
nice(p1.aov1, es="pes") %>% kable(digits=3)
```

| Effect | df    | MSE  | F    | pes  | p.value |
|:-------|:------|:-----|:-----|:-----|:--------|
| cond   | 2, 46 | 0.00 | 0.35 | .015 | .705    |

<br>

조건 간 짝 비교에서도 유의한 차이는 관찰되지 않았다.

<br>

``` r
p_h1 <- p1_allL3 %>% 
  rstatix::pairwise_t_test(dprime ~ cond, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif)

p_h2 <- p1_allL3 %>% 
  rstatix::cohens_d(dprime ~ cond, paired=T, ci = F) %>% 
  dplyr::select(group1, group2, effsize, magnitude)

merge(p_h1, p_h2, by = c("group1", "group2")) %>% kable(digits=3)
```

| group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude  |
|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:-----------|
| same   | diff   |   -0.014 |   -0.055 |     0.028 |  23 |    -0.670 |     1 | ns           |  -0.137 | negligible |
| single | diff   |    0.002 |   -0.036 |     0.041 |  23 |     0.120 |     1 | ns           |   0.025 | negligible |
| single | same   |    0.016 |   -0.030 |     0.061 |  23 |     0.716 |     1 | ns           |   0.146 | negligible |

<br>

### fMRI results

<br>

전체 관심 영역 (PPA, V1\~V4) 에서 Redundancy Gain Effect 를 확인하였다.
자극 제시 조건은 Single, 4-Same, 4-Different으로 구분되었다. 이
분석에서는 전처리 과정에서 FWHM 5mm로 Spatial Smoothing이 수행된
데이터를 사용하였다.

<br>

``` r
# hRoi
t3 <- read.csv("data/p12_RG_main1_hroi_sm.csv", header = T)
# t3 <- read.csv("p12_RG_main1_hroi_nsm.csv", header = T)


t3_l <- t3
# change class of main factors: double to factor
t3_l$subj = factor(t3_l$subj, levels=c("rd_01","rd_02","rd_03","rd_04","rd_05",
                                       "rd_06","rd_07","rd_08","rd_09","rd_10",
                                       "rd_11","rd_12","rd_13","rd_14","rd_15",
                                       "rd_16", "rd_18", "rd_19", "rd_20",
                                       "rd_21", "rd_22", "rd_23", "rd_24", "rd_25"), 
                   labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25))

t3_l <- gather(t3_l, key = rgCon, value = z, sing1_l:diff, factor_key=T)
t3_l$rgCon = factor(t3_l$rgCon, 
                    levels=c("sing1_l","sing2_r","sing3_r","sing4_l","sing","same","diff"),
                    labels=c("sing1_l","sing2_r","sing3_r","sing4_l","single","same","diff")) 
targ_rgCon = c("single","same","diff")
t3_l <- t3_l %>% filter(rgCon %in% targ_rgCon)
# targ_hRoi = c("ffa","ppa")
t3_l$hRoi = factor(t3_l$hRoi, 
                   levels=c("rFFA","lFFA","FFA","rPPA","lPPA","PPA"), 
                   labels=c("rFFA","lFFA","FFA","rPPA","lPPA","PPA"))
t3_l$Roi <- t3_l$hRoi
t3_l <- t3_l %>% 
  filter(Roi != "FFA", Roi != "rFFA", Roi != "lFFA") %>% 
  dplyr::select(subj, Roi, rgCon, z)

# vRoi
t4 <- read.csv("data/p12_RGandBG_ms_main13_sm.csv", header = T)
# t4 <- read.csv("p12_RGandBG_ms_main13_nsm.csv", header = T)

t4_l <- gather(t4, vRoi, z, v1:v4, factor_key=TRUE)
# change class of main factors: double to factor
t4_l$subj = factor(t4_l$subj, levels=c("rd_01","rd_02","rd_03","rd_04","rd_05",
                                       "rd_06","rd_07","rd_08","rd_09","rd_10",
                                       "rd_11","rd_12","rd_13","rd_14","rd_15",
                                       "rd_16", "rd_18", "rd_19", "rd_20",
                                       "rd_21", "rd_22", "rd_23", "rd_24", "rd_25"), 
                   labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25))

t4_l$vRoi = factor(t4_l$vRoi) 
t4_l$bgCon = factor(t4_l$bgCon, 
                    levels=c("rPPA","lPPA","PPA","tPPA"), 
                   labels=c("rPPA","lPPA","PPA","tPPA"))
t4_l$fcCon =factor(t4_l$fcCon, levels=c("low","high","all"))
t4_l$rgCon =factor(t4_l$rgCon, levels=c("single","same","diff"))
t4_l$Roi <- t4_l$vRoi
t4_l <- t4_l %>% filter(fcCon == "all", bgCon == "PPA") %>% dplyr::select(subj, Roi, rgCon, z)
t34 <- rbind(t3_l, t4_l)

# t34 <- t34 %>% filter(subj != 4, subj != 5, subj != 6, subj != 8)

glimpse(t34, width = 70)
## Rows: 504
## Columns: 4
## $ subj  <fct> 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, …
## $ Roi   <fct> rPPA, lPPA, PPA, rPPA, lPPA, PPA, rPPA, lPPA, PPA, rPP…
## $ rgCon <fct> single, single, single, single, single, single, single…
## $ z     <dbl> 1.8258330, 1.4798900, 1.6528620, 0.5629505, 0.4340998,…
```

<br>

각 관심 영역에서 자극 제시 조건에 따른 활성화 값의 요약치는 아래와 같다.

<br>

``` r
t34_allL.1 <- t34 %>% 
  group_by(subj, Roi, rgCon) %>%
  dplyr::summarise(z=mean(z)) %>%
  ungroup()
## `summarise()` has grouped output by 'subj', 'Roi'. You can override using the `.groups` argument.
# t34_allL.1 %>% kable(digits=2)

# subject-level, wide format
t34_allW.1 <- t34_allL.1 %>% spread(key=rgCon, value = z)
# t34_allW.1 %>% filter(Roi != "ppa") %>% mutate(same_diff = same-diff) %>%
#   dplyr::select(subj, Roi, same_diff) %>% spread(key=Roi, value=same_diff) %>%
#   kable(digits=2)

# summary table: grand mean
t34_allG.1 <- t34_allL.1 %>% group_by(Roi, rgCon) %>%
  summarise(z.m = mean(z), z.sd = sd(z)) %>%
  ungroup()
## `summarise()` has grouped output by 'Roi'. You can override using the `.groups` argument.
t34_allG.1$z.se <- Rmisc::summarySEwithin(data = t34_allL.1, measurevar = "z", 
                                         idvar = "subj", withinvars = c("Roi", "rgCon"))$se
t34_allG.1$z.ci <- Rmisc::summarySEwithin(data = t34_allL.1, measurevar = "z", 
                                         idvar = "subj", withinvars = c("Roi", "rgCon"))$ci
t34_allG.1 <- t34_allG.1 %>% 
  mutate(lower.ci = z.m-z.ci,
         upper.ci = z.m+z.ci,
         lower.se = z.m-z.se,
         upper.se = z.m+z.se)
t34_allG.1 %>% dplyr::select(Roi, rgCon, z.m) %>% spread(key=rgCon, value=z.m) %>%  kable(digits=2)
```

| Roi  | single | same | diff |
|:-----|-------:|-----:|-----:|
| rPPA |   2.01 | 7.01 | 5.31 |
| lPPA |   1.76 | 6.47 | 5.20 |
| PPA  |   1.88 | 6.74 | 5.25 |
| v1   |   4.74 | 9.75 | 9.56 |
| v2   |   5.75 | 9.96 | 9.53 |
| v3   |   4.90 | 8.64 | 7.90 |
| v4   |   3.69 | 8.07 | 7.16 |

<br>

아래 그래프는 각 관심 영역에서 자극 제시 조건에 따른 활성화 값(z)을
나타낸다. 그래프에서 Same &gt; Diff &gt; Single 조건 순서로 활성화가
높게 나타나는 것을 Redundancy Gain Effect로 해석한다. 점은 평균, error
bar는 95% ci를 나타내었다.

<br>

``` r
targ_roi = c("PPA","v1","v2","v3","v4")
r1_hroi <- t34_allL.1 %>% filter(Roi %in% targ_roi)
# Roi != "v1", Roi != "v2", Roi != "v3", Roi != "v4")
r1_hroi_w <- t34_allW.1 %>% filter(Roi %in% targ_roi)
r1_hroi_g <- t34_allG.1 %>% filter(Roi %in% targ_roi)
r1_hroi_g$z <- r1_hroi_g$z.m
r1_main_allroi <- ggplot(data=r1_hroi, aes(x=Roi, y=z, fill=rgCon)) +
  # geom_hline(yintercept=0, linetype='solid', color='black', alpha =1, size=0.2) +
  stat_summary(fun = mean, geom = "bar", position="dodge", na.rm = TRUE,   
               alpha = .9, width = 0.8,  size = 0.5, color = "gray20") +
  geom_dotplot(data=r1_hroi, aes(x=Roi, y=z, fill=rgCon),
               binaxis = "y", stackdir = "center", stackratio = 1,
               color = "black", alpha = 0.5, #position = "nudge",
               position=position_dodge(0.8),
               inherit.aes = TRUE, binwidth = 0.15) +
  # geom_jitter(aes(x=Roi, y=z, fill=rgCon, color = rgCon),
  #             position=position_dodge(0.1), cex=2
  #             ) +

  # facet_grid(.~Btw, scales="free_x", space = "free",
  #            labeller = labeller(Btw = c("1" = "Experimental Group","2" = "Control Group"))) +
  # geom_point(data=r1_hroi, aes(x=Roi, y=z, fill=rgCon), position = position_dodge(width=0.8),
             # size=2, show.legend=F, color="gray90") +  
  # geom_segment(data=filter(r1_hroi_w, Roi == "ffa"), inherit.aes = FALSE,
  #              aes(x=0.73, y=filter(r1_hroi_w, Roi == "ffa")$single,
  #                  xend=1.0, yend=filter(r1_hroi_w, Roi == "ffa")$same),
  #              color="gray90", alpha = .7) +  
  # geom_segment(data=filter(r1_hroi_w, Roi == "ffa"), inherit.aes = FALSE,
  #              aes(x=1, y=filter(r1_hroi_w, Roi == "ffa")$same,
  #                  xend=1.27, yend=filter(r1_hroi_w, Roi == "ffa")$diff),
  #              color="gray90", alpha = .7) +  
  # geom_segment(data=filter(r1_hroi_w, Roi == "ppa"), inherit.aes = FALSE,
  #              aes(x=1.73, y=filter(r1_hroi_w, Roi == "ppa")$single,
  #                  xend=2.0, yend=filter(r1_hroi_w, Roi == "ppa")$same),
  #              color="gray90", alpha = .7) +  
  # geom_segment(data=filter(r1_hroi_w, Roi == "ppa"), inherit.aes = FALSE,
  #              aes(x=2, y=filter(r1_hroi_w, Roi == "ppa")$same,
  #                  xend=2.27, yend=filter(r1_hroi_w, Roi == "ppa")$diff),
  #              color="gray90", alpha = .7) +  
  # geom_pointrange(data=r1_hroi_g, aes(x = Roi, y=z.m, ymin = lower.ci, ymax = upper.ci),
                  # position = position_dodge(0.80), color = "darkred", size = 1, show.legend = FALSE) +
  geom_errorbar(data=r1_hroi_g, aes(ymin=lower.se, ymax=upper.se), width=.2,
                position=position_dodge(.8), color = "black") +
  scale_x_discrete(labels=c("PPA","V1","V2","V3","V4")) +
  scale_fill_manual(values = c("#D2E6BD", "#4AA7B4", "#235796"),  # , "#235796"
                    labels = c("single", "4-same", "4-diff")) +
  coord_cartesian(ylim = c(-2, 15), clip = "on") +
  labs(x = "", y = expression(paste("Parameter estimation (", italic("z"), ")"))) +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold", size = 16, color = "black"),
        axis.text = element_text(face = "plain", hjust = 0.5, size = 15, color = "black"),
        axis.line=element_line(),
        axis.text.x = element_text(face = "plain", vjust = -1, size = 15, color = "black"),
        strip.text.x = element_text(face = "plain", size = 15, color = "black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing=unit(1, "lines"),
        plot.margin = margin(1, 0.3, 1, 0.3, "cm"), 
        legend.title = element_blank(),
        legend.position="top")
r1_main_allroi
```

![](fBGRED_results_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# ggsave("fBGRED_main12_verSM.jpg", plot = r1_main_allroi, width=8, height=6, unit='in', dpi=600)
```

<br>

각 관심 영역(PPA, V1, V2, V3, V4)에서 자극 제시 조건(Single vs. 4-Same
vs. 4-Diff)을 요인으로 4 x 3 혼합 요인 분산분석을 수행하였다. 분석
결과에서 각 관심 영역 간 차이는 관심 효과가 아님에 유의할 필요가 있다.

<br>

``` r
targ_roi = c("PPA", "v1", "v2", "v3", "v4")
t34_allL.1.vroi <- t34_allL.1 %>% filter(Roi %in% targ_roi)
t34.aov1 <- aov_ez(id="subj", dv="z", data = t34_allL.1.vroi, within = c("Roi", "rgCon"),
                   anova_table = list(correction = "none"))
nice(t34.aov1, es="pes") %>% kable(digits=3)
```

| Effect    | df     | MSE  | F             | pes  | p.value  |
|:----------|:-------|:-----|:--------------|:-----|:---------|
| Roi       | 4, 92  | 3.94 | 41.69 \*\*\*  | .644 | &lt;.001 |
| rgCon     | 2, 46  | 2.50 | 271.35 \*\*\* | .922 | &lt;.001 |
| Roi:rgCon | 8, 184 | 0.43 | 9.05 \*\*\*   | .282 | &lt;.001 |

<br>

각 관심 영역에서 자극 제시 조건에 따른 차이를 사후 검정하였다.

<br>

``` r
p_h1 <- t34_allL.1.vroi %>% 
  group_by(Roi) %>% 
  rstatix::pairwise_t_test(z ~ rgCon, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(Roi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif)

p_h2 <- t34_allL.1.vroi %>% group_by(Roi) %>% 
  rstatix::cohens_d(z ~ rgCon, paired=T, ci = F) %>% 
  dplyr::select(Roi, group1, group2, effsize, magnitude)

merge(p_h1, p_h2, by = c("Roi", "group1", "group2")) %>% filter(group1=="single") %>%  kable(digits=3)
```

| Roi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| PPA | single | diff   |   -3.372 |   -4.139 |    -2.605 |  23 |    -9.095 |     0 | \*\*\*\*     |  -1.856 | large     |
| PPA | single | same   |   -4.853 |   -5.557 |    -4.150 |  23 |   -14.278 |     0 | \*\*\*\*     |  -2.915 | large     |
| v1  | single | diff   |   -4.818 |   -5.376 |    -4.261 |  23 |   -17.885 |     0 | \*\*\*\*     |  -3.651 | large     |
| v1  | single | same   |   -5.005 |   -5.594 |    -4.417 |  23 |   -17.596 |     0 | \*\*\*\*     |  -3.592 | large     |
| v2  | single | diff   |   -3.780 |   -4.307 |    -3.252 |  23 |   -14.825 |     0 | \*\*\*\*     |  -3.026 | large     |
| v2  | single | same   |   -4.209 |   -4.654 |    -3.764 |  23 |   -19.562 |     0 | \*\*\*\*     |  -3.993 | large     |
| v3  | single | diff   |   -3.002 |   -3.524 |    -2.479 |  23 |   -11.883 |     0 | \*\*\*\*     |  -2.426 | large     |
| v3  | single | same   |   -3.745 |   -4.186 |    -3.304 |  23 |   -17.576 |     0 | \*\*\*\*     |  -3.588 | large     |
| v4  | single | diff   |   -3.467 |   -4.234 |    -2.700 |  23 |    -9.350 |     0 | \*\*\*\*     |  -1.909 | large     |
| v4  | single | same   |   -4.377 |   -5.067 |    -3.687 |  23 |   -13.122 |     0 | \*\*\*\*     |  -2.678 | large     |

``` r
merge(p_h1, p_h2, by = c("Roi", "group1", "group2")) %>% filter(group1=="same") %>% kable(digits=3)
```

| Roi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude  |
|:----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:-----------|
| PPA | same   | diff   |    1.481 |    1.081 |     1.882 |  23 |     7.655 | 0.000 | \*\*\*\*     |   1.562 | large      |
| v1  | same   | diff   |    0.187 |   -0.211 |     0.585 |  23 |     0.973 | 1.000 | ns           |   0.199 | negligible |
| v2  | same   | diff   |    0.429 |    0.089 |     0.769 |  23 |     2.607 | 0.047 | \*           |   0.532 | moderate   |
| v3  | same   | diff   |    0.743 |    0.352 |     1.135 |  23 |     3.926 | 0.002 | \*\*         |   0.801 | large      |
| v4  | same   | diff   |    0.910 |    0.479 |     1.342 |  23 |     4.364 | 0.001 | \*\*\*       |   0.891 | large      |

**V1 결과**

``` r
t34_allL.1.tmp <- t34_allL.1 %>% filter(Roi=="v1")
t34.aov1 <- aov_ez(id="subj", dv="z", data = t34_allL.1.tmp, within = c("rgCon"),
                   anova_table = list(correction = "none"))
nice(t34.aov1, es="pes") %>% kable(digits=3)
```

| Effect | df    | MSE  | F             | pes  | p.value  |
|:-------|:------|:-----|:--------------|:-----|:---------|
| rgCon  | 2, 46 | 0.76 | 253.54 \*\*\* | .917 | &lt;.001 |

``` r
t34_allL.1.tmp %>% group_by(Roi) %>% rstatix::pairwise_t_test(z ~ rgCon, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(Roi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) %>% kable(digits=3)
```

| Roi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif |
|:----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|
| v1  | single | same   |   -5.005 |   -5.594 |    -4.417 |  23 |   -17.596 |     0 | \*\*\*\*     |
| v1  | single | diff   |   -4.818 |   -5.376 |    -4.261 |  23 |   -17.885 |     0 | \*\*\*\*     |
| v1  | same   | diff   |    0.187 |   -0.211 |     0.585 |  23 |     0.973 |     1 | ns           |

``` r
t34_allL.1.tmp %>% group_by(Roi) %>% 
  rstatix::cohens_d(z ~ rgCon, paired=T, ci = F) %>% kable(digits=2)
```

| .y. | group1 | group2 | effsize | Roi |  n1 |  n2 | magnitude  |
|:----|:-------|:-------|--------:|:----|----:|----:|:-----------|
| z   | single | same   |   -3.59 | v1  |  24 |  24 | large      |
| z   | single | diff   |   -3.65 | v1  |  24 |  24 | large      |
| z   | same   | diff   |    0.20 | v1  |  24 |  24 | negligible |

<br>

**V2 결과**

``` r
t34_allL.1.tmp <- t34_allL.1 %>% filter(Roi=="v2")
t34.aov1 <- aov_ez(id="subj", dv="z", data = t34_allL.1.tmp, within = c("rgCon"),
                   anova_table = list(correction = "none"))
nice(t34.aov1, es="pes") %>% kable(digits=3)
```

| Effect | df    | MSE  | F             | pes  | p.value  |
|:-------|:------|:-----|:--------------|:-----|:---------|
| rgCon  | 2, 46 | 0.55 | 232.59 \*\*\* | .910 | &lt;.001 |

``` r
t34_allL.1.tmp %>% group_by(Roi) %>% rstatix::pairwise_t_test(z ~ rgCon, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(Roi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) %>% kable(digits=3)
```

| Roi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif |
|:----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|
| v2  | single | same   |   -4.209 |   -4.654 |    -3.764 |  23 |   -19.562 | 0.000 | \*\*\*\*     |
| v2  | single | diff   |   -3.780 |   -4.307 |    -3.252 |  23 |   -14.825 | 0.000 | \*\*\*\*     |
| v2  | same   | diff   |    0.429 |    0.089 |     0.769 |  23 |     2.607 | 0.047 | \*           |

``` r
t34_allL.1.tmp %>% group_by(Roi) %>% 
  rstatix::cohens_d(z ~ rgCon, paired=T, ci = F) %>% kable(digits=2)
```

| .y. | group1 | group2 | effsize | Roi |  n1 |  n2 | magnitude |
|:----|:-------|:-------|--------:|:----|----:|----:|:----------|
| z   | single | same   |   -3.99 | v2  |  24 |  24 | large     |
| z   | single | diff   |   -3.03 | v2  |  24 |  24 | large     |
| z   | same   | diff   |    0.53 | v2  |  24 |  24 | moderate  |

<br>

**V3 결과**

``` r
t34_allL.1.tmp <- t34_allL.1 %>% filter(Roi=="v3")
t34.aov1 <- aov_ez(id="subj", dv="z", data = t34_allL.1.tmp, within = c("rgCon"),
                   anova_table = list(correction = "none"))

nice(t34.aov1, es="pes") %>% kable(digits=3)
```

| Effect | df    | MSE  | F             | pes  | p.value  |
|:-------|:------|:-----|:--------------|:-----|:---------|
| rgCon  | 2, 46 | 0.58 | 162.61 \*\*\* | .876 | &lt;.001 |

``` r
t34_allL.1.tmp %>% group_by(Roi) %>% rstatix::pairwise_t_test(z ~ rgCon, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(Roi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) %>% kable(digits=3)
```

| Roi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif |
|:----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|
| v3  | single | same   |   -3.745 |   -4.186 |    -3.304 |  23 |   -17.576 | 0.000 | \*\*\*\*     |
| v3  | single | diff   |   -3.002 |   -3.524 |    -2.479 |  23 |   -11.883 | 0.000 | \*\*\*\*     |
| v3  | same   | diff   |    0.743 |    0.352 |     1.135 |  23 |     3.926 | 0.002 | \*\*         |

``` r
t34_allL.1.tmp %>% group_by(Roi) %>% 
  rstatix::cohens_d(z ~ rgCon, paired=T, ci = F) %>% kable(digits=2)
```

| .y. | group1 | group2 | effsize | Roi |  n1 |  n2 | magnitude |
|:----|:-------|:-------|--------:|:----|----:|----:|:----------|
| z   | single | same   |   -3.59 | v3  |  24 |  24 | large     |
| z   | single | diff   |   -2.43 | v3  |  24 |  24 | large     |
| z   | same   | diff   |    0.80 | v3  |  24 |  24 | large     |

<br>

**V4 결과**

``` r
t34_allL.1.tmp <- t34_allL.1 %>% filter(Roi=="v4")
t34.aov1 <- aov_ez(id="subj", dv="z", data = t34_allL.1.tmp, within = c("rgCon"),
                   anova_table = list(correction = "none"))
nice(t34.aov1, es="pes") %>% kable(digits=3)
```

| Effect | df    | MSE  | F             | pes  | p.value  |
|:-------|:------|:-----|:--------------|:-----|:---------|
| rgCon  | 2, 46 | 1.17 | 109.51 \*\*\* | .826 | &lt;.001 |

``` r
t34_allL.1.tmp %>% group_by(Roi) %>% rstatix::pairwise_t_test(z ~ rgCon, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(Roi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) %>% kable(digits=3)
```

| Roi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif |
|:----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|
| v4  | single | same   |   -4.377 |   -5.067 |    -3.687 |  23 |   -13.122 | 0.000 | \*\*\*\*     |
| v4  | single | diff   |   -3.467 |   -4.234 |    -2.700 |  23 |    -9.350 | 0.000 | \*\*\*\*     |
| v4  | same   | diff   |    0.910 |    0.479 |     1.342 |  23 |     4.364 | 0.001 | \*\*\*       |

``` r
t34_allL.1.tmp %>% group_by(Roi) %>% 
  rstatix::cohens_d(z ~ rgCon, paired=T, ci = F) %>% kable(digits=2)
```

| .y. | group1 | group2 | effsize | Roi |  n1 |  n2 | magnitude |
|:----|:-------|:-------|--------:|:----|----:|----:|:----------|
| z   | single | same   |   -2.68 | v4  |  24 |  24 | large     |
| z   | single | diff   |   -1.91 | v4  |  24 |  24 | large     |
| z   | same   | diff   |    0.89 | v4  |  24 |  24 | large     |

<br>

------------------------------------------------------------------------

<br>

## Background Functional Connectivity - Phase 2 Replication

<br>

### Behavioral Performance

<br>

Phase 2 Attention Phsae, 1-back task 수행의 전반적인 요약치와 t-test
결과를 아래에 표시하였다.

<br>

``` r
p2 <- read.csv("data/bg_behav.csv", header = T)

p2_l <- p2
p2_l$subj = factor(p2_l$subj)
p2_l <- p2_l %>% filter(subj!=99)
p2_l$run = factor(p2_l$run) 
p2_l$block = factor(p2_l$block) 
p2_l$cond = factor(p2_l$cond, levels=c(1,2), labels=c("face","scene")) 
# p2_l$catch = factor(p2_l$catch)
p2_l$rt = p2_l$rt*1000
p2_l$corr = 0
p2_l$corr[p2_l$catch==1 & p2_l$resp==1] =1
p2_l$type = 0
p2_l$type[p2_l$catch==1 & p2_l$resp==1] = 1 #hit
p2_l$type[p2_l$catch==1 & p2_l$resp==0] = 2 #miss
p2_l$type[p2_l$catch==0 & p2_l$resp==1] = 3 #fa
p2_l$type[p2_l$catch==0 & p2_l$resp==0] = 4 #cr
p2_l$type <- factor(p2_l$type, levels=c(1,2,3,4), labels=c("hit","miss","fa","cr"))

headTail(p2_l)
##       subj  run block  cond onset catch resp  rt corr type
## 1        1    1     1 scene     0     0    0   0    0   cr
## 2        1    1     1 scene   1.5     0    0   0    0   cr
## 3        1    1     1 scene     3     0    0   0    0   cr
## 4        1    1     1 scene   4.5     0    0   0    0   cr
## ...   <NA> <NA>  <NA>  <NA>   ...   ...  ... ...  ... <NA>
## 13821   25    4    12  face   309     0    0   0    0   cr
## 13822   25    4    12  face 310.5     0    0   0    0   cr
## 13823   25    4    12  face   312     0    0   0    0   cr
## 13824   25    4    12  face 313.5     0    0   0    0   cr
glimpse(p2_l, width = 70)
## Rows: 13,824
## Columns: 10
## $ subj  <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ run   <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ block <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, …
## $ cond  <fct> scene, scene, scene, scene, scene, scene, scene, scene…
## $ onset <dbl> 0.0, 1.5, 3.0, 4.5, 6.0, 7.5, 9.0, 10.5, 12.0, 13.5, 1…
## $ catch <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, …
## $ resp  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, …
## $ rt    <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000…
## $ corr  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, …
## $ type  <fct> cr, cr, cr, cr, cr, cr, cr, miss, cr, cr, cr, hit, cr,…

# length(unique(p2_l$subj))
# check number of trials for each condition/subj
# table(p2_l$subj)
# table(p2_l$cond,  p2_l$catch, p2_l$subj) 
```

<br>

두 주의 조건을 구분하지 않은 전반적인 수행 요약치 d-prime은 다음과 같다.

<br>

``` r
# subject-level, long format
dmy <- dummyVars(" ~ type", data = p2_l)
p2_2 <- data.frame(predict(dmy, newdata = p2_l))
p2_3 <- cbind(p2_l, p2_2)
p2_allL <- p2_3  %>% group_by(subj) %>%
  dplyr::summarise(hit=sum(`type.hit`), 
                 miss=sum(`type.miss`), 
                 fa=sum(`type.fa`),
                 cr=sum(`type.cr`),
                 targets=sum(`type.hit`)+sum(`type.miss`),
                 dists=sum(`type.fa`)+sum(`type.cr`),
                 hit.r=sum(`type.hit`)/(sum(`type.hit`)+sum(`type.miss`)), 
                 miss.r=sum(`type.miss`)/(sum(`type.hit`)+sum(`type.miss`)),
                 fa.r=sum(`type.fa`)/(sum(`type.fa`)+sum(`type.cr`)),
                 cr.r=sum(`type.cr`)/(sum(`type.fa`)+sum(`type.cr`))) %>%
  ungroup()
# p2_allL %>% kable(digits=2)

# subject-level, long format, calculating dprime
# indices <- psycho::dprime(p2_allL$hit, p2_allL$fa, p2_allL$miss, p2_allL$cr,
                          # p2_allL$targets, p2_allL$dists)
indices <- psycho::dprime(p2_allL$hit.r, p2_allL$fa.r, p2_allL$miss.r, p2_allL$cr.r,
                          1, 1)
p2_allL2 <- cbind(p2_allL, indices)
# p2_allL2 %>% kable(digits=2)

# check outlier
p2_allL3 <- p2_allL2 %>%
  mutate(lbound = mean(dprime)-3*sd(dprime),
         ubound = mean(dprime)+3*sd(dprime)) %>% 
  mutate(Outlier = (dprime < lbound)|(dprime > ubound)) %>% # set outlier
  ungroup()

# summary table: grand mean
p2_allG.mean <- p2_allL3 %>%
  summarise(dprime.m = mean(dprime), dprime.sd = sd(dprime)) %>%
  ungroup()
p2_allG.mean %>% kable(digits=2)
```

| dprime.m | dprime.sd |
|---------:|----------:|
|      0.8 |       0.2 |

<br>

d prime 값이 0과 유의하게 다른지 one-sample t-test를 수행하였다.

<br>

``` r
p_h1 <- p2_allL3 %>% 
  rstatix::t_test(dprime ~ 1, mu = 0, detailed= T, alternative = "two.sided") %>% 
  dplyr::select(estimate, conf.low, conf.high, df, statistic, p) 

p_h2 <- p2_allL3 %>% 
  rstatix::cohens_d(dprime ~ 1, mu = 0, ci = F) %>% 
  dplyr::select(effsize, magnitude) 

merge(p_h1, p_h2) %>% kable(digits=3)
```

| estimate | conf.low | conf.high |  df | statistic |   p | effsize | magnitude |
|---------:|---------:|----------:|----:|----------:|----:|--------:|:----------|
|    0.797 |    0.711 |     0.884 |  23 |    19.094 |   0 |   3.898 | large     |

<br>

주의 조건별 요약치는 다음과 같다.

<br>

``` r
# subject-level, long format
dmy <- dummyVars(" ~ type", data = p2_l)
p2_2 <- data.frame(predict(dmy, newdata = p2_l))
p2_3 <- cbind(p2_l, p2_2)
p2_allL <- p2_3  %>% group_by(subj, cond) %>%
  dplyr::summarise(hit=sum(`type.hit`), 
                 miss=sum(`type.miss`), 
                 fa=sum(`type.fa`),
                 cr=sum(`type.cr`),
                 targets=sum(`type.hit`)+sum(`type.miss`),
                 dists=sum(`type.fa`)+sum(`type.cr`),
                 hit.r=sum(`type.hit`)/(sum(`type.hit`)+sum(`type.miss`)), 
                 miss.r=sum(`type.miss`)/(sum(`type.hit`)+sum(`type.miss`)),
                 fa.r=sum(`type.fa`)/(sum(`type.fa`)+sum(`type.cr`)),
                 cr.r=sum(`type.cr`)/(sum(`type.fa`)+sum(`type.cr`))) %>%
  ungroup()
## `summarise()` has grouped output by 'subj'. You can override using the `.groups` argument.
# p2_allL %>% kable(digits=2)

# subject-level, long format, calculating dprime
indices <- psycho::dprime(p2_allL$hit, p2_allL$fa, p2_allL$miss, p2_allL$cr,
                          p2_allL$targets, p2_allL$dists)
indices <- psycho::dprime(p2_allL$hit.r, p2_allL$fa.r, p2_allL$miss.r, p2_allL$cr.r,
                          1, 1)
p2_allL2 <- cbind(p2_allL, indices)
# p2_allL2 %>% kable(digits=2)

# check outlier
p2_allL3 <- p2_allL2 %>%
  mutate(lbound = mean(dprime)-3*sd(dprime),
         ubound = mean(dprime)+3*sd(dprime)) %>% 
  mutate(Outlier = (dprime < lbound)|(dprime > ubound)) %>% # set outlier
  ungroup()
# p2_allL3 %>% dplyr::select(subj, cond, hit, miss, fa, cr, dprime) %>%kable(digits=2)
p2_allG <- p2_allL3 %>% group_by(cond) %>% 
  summarise(dprime.m = mean(dprime), dprime.sd = sd(dprime)) %>%
  ungroup()

p2_allG$dprime.se <- Rmisc::summarySEwithin(data = p2_allL3, measurevar = "dprime", idvar = "subj", 
                                      withinvars = c("cond"))$se
p2_allG$dprime.ci <- Rmisc::summarySEwithin(data = p2_allL3, measurevar = "dprime", idvar = "subj", 
                                            withinvars = c("cond"))$ci
p2_allG <- p2_allG %>% 
  mutate(lower.ci = dprime.m-dprime.ci,
         upper.ci = dprime.m+dprime.ci)
p2_allG %>% dplyr::select(cond, dprime.m, dprime.sd, dprime.se, lower.ci, upper.ci) %>% kable(digits=2)
```

| cond  | dprime.m | dprime.sd | dprime.se | lower.ci | upper.ci |
|:------|---------:|----------:|----------:|---------:|---------:|
| face  |     0.83 |      0.24 |      0.03 |     0.76 |     0.90 |
| scene |     0.77 |      0.23 |      0.03 |     0.70 |     0.84 |

<br>

Face 조건과 Scene 조건 간의 paired t-test 결과, 두 조건 간의 수행 차이는
유의하지 않았다.

<br>

``` r
p_h1 <- p2_allL3 %>% 
  rstatix::pairwise_t_test(dprime ~ cond, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif)

p_h2 <- p2_allL3 %>% 
  rstatix::cohens_d(dprime ~ cond, paired=T, ci = F) %>% 
  dplyr::select(group1, group2, effsize, magnitude)

merge(p_h1, p_h2, by = c("group1", "group2")) %>% kable(digits=3)
```

| group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| face   | scene  |    0.064 |   -0.036 |     0.164 |  23 |     1.322 | 0.199 | ns           |    0.27 | small     |

<br>

### fMRI results

<br>

**Al-aidroos et al., 2013. PNAS**의 결과를 재현하기 위해, Phase 2
Attention Phase에서 주의 조건에 따라 조절되는 상위 영역과 시각 영역 간의
Background Connectivity를 분석하였다.

<br>

``` r
t2 <- read.csv("data/p2_BG_main2_nsm.csv", header = T)

t2_l <- gather(t2, bgCon, fc, rFFA:PPA, factor_key=TRUE)

# change class of main factors: double to factor
t2_l$subj = factor(t2_l$subj, levels=c("rd_01","rd_02","rd_03","rd_04","rd_05",
                                       "rd_06","rd_07","rd_08","rd_09","rd_10",
                                       "rd_11","rd_12","rd_13","rd_14","rd_15",
                                       "rd_16", "rd_18", "rd_19", "rd_20",
                                       "rd_21", "rd_22", "rd_23", "rd_24", "rd_25"), 
                   labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25))

t2_l <- t2_l %>% filter(subj!=99)

t2_l$vRoi = factor(t2_l$vROIs) 
t2_l$bgCon = factor(t2_l$bgCon, 
                    levels=c("rFFA","lFFA","FFA","rPPA","lPPA","PPA"), 
                   labels=c("rFFA","lFFA","FFA","rPPA","lPPA","PPA"))
targ_bg = c("rFFA","rPPA")
targ_bg = c("FFA","PPA")
t2_l <- t2_l %>% filter(bgCon %in% targ_bg)
t2_l$hRoi <- t2_l$bgCon
t2_l$fRun <- t2_l$fcCon
t2_l$fRun <- factor(t2_l$fRun, 
                    levels=c("faceRun", "sceRun"),
                    labels=c("face", "scene"))

t2_l <- t2_l %>% dplyr::select(subj,vRoi,fRun,hRoi,fc)

glimpse(t2_l, width = 70)
## Rows: 384
## Columns: 5
## $ subj <fct> 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3…
## $ vRoi <fct> v1, v2, v3, v4, v1, v2, v3, v4, v1, v2, v3, v4, v1, v2,…
## $ fRun <fct> face, face, face, face, scene, scene, scene, scene, fac…
## $ hRoi <fct> FFA, FFA, FFA, FFA, FFA, FFA, FFA, FFA, FFA, FFA, FFA, …
## $ fc   <dbl> 0.037988160, 0.008802742, 0.073941240, 0.173024800, -0.…
```

<br>

요약치는 아래와 같다.

<br>

``` r
# subject-level, long format
t2_allL <- t2_l %>% group_by(subj, vRoi, fRun, hRoi) %>%
  dplyr::summarise(fc=mean(fc)) %>%
  ungroup()
## `summarise()` has grouped output by 'subj', 'vRoi', 'fRun'. You can override using the `.groups` argument.

# subject-level, wide format
t2_allW <- t2_allL %>% group_by(subj, vRoi) %>% spread(key=fRun, value = fc)
t2_allW1 <- t2_allL %>% group_by(subj, vRoi) %>% spread(key=hRoi, value = fc)

# summary table: grand mean
t2_allG <- t2_allL %>% group_by(vRoi, fRun, hRoi) %>%
  summarise(fc.m = mean(fc), fc.sd = sd(fc)) %>%
  ungroup()
## `summarise()` has grouped output by 'vRoi', 'fRun'. You can override using the `.groups` argument.
t2_allG$fc.se <- Rmisc::summarySEwithin(data = t2_allL, measurevar = "fc", 
                                        idvar = "subj", withinvars = c("vRoi", "fRun", "hRoi"))$se
t2_allG$fc.ci <- Rmisc::summarySEwithin(data = t2_allL, measurevar = "fc", 
                                        idvar = "subj", withinvars = c("vRoi", "fRun", "hRoi"))$ci
t2_allG <- t2_allG %>% 
  mutate(lower.ci = fc.m-fc.ci,
         upper.ci = fc.m+fc.ci,
         lower.se = fc.m-fc.se,
         upper.se = fc.m+fc.se)
# for between-subject design. check help(summarySE)
t2_allG <- t2_allG %>% dplyr::select(vRoi, hRoi, fRun, fc.m, fc.sd, fc.se, lower.ci, upper.ci, lower.se, upper.se)
t2_allG %>% dplyr::select(fRun, hRoi, vRoi, fc.m) %>% 
  spread(key=vRoi, value=fc.m) %>% kable(digits=2)
```

| fRun  | hRoi |    v1 |   v2 |   v3 |   v4 |
|:------|:-----|------:|-----:|-----:|-----:|
| face  | FFA  |  0.05 | 0.13 | 0.29 | 0.44 |
| face  | PPA  | -0.02 | 0.00 | 0.08 | 0.25 |
| scene | FFA  |  0.01 | 0.08 | 0.23 | 0.38 |
| scene | PPA  |  0.07 | 0.12 | 0.30 | 0.51 |

<br>

아래 그래프에서 error bar는 95% ci를 나타내었다.

<br>

``` r
t2_allG$fc <- t2_allG$fc.m

r2_bgfc.1 <- ggplot(data=t2_allL, aes(x=hRoi, y=fc, fill=fRun)) +
  geom_hline(yintercept=0, linetype='solid', color='black', alpha =1, size=0.4) +
  stat_summary(fun = mean, geom = "bar", position="dodge", na.rm = TRUE, 
               alpha = .9, width = 0.8,  size = 0.4, color = "black") +
  facet_grid(.~vRoi, scales="free_x", space = "free",
             labeller = labeller(vRoi = c("v1" = "V1","v2" = "V2","v3" = "V3","v4" = "V4"))) +
  geom_dotplot(data=t2_allL, aes(x=hRoi, y=fc, fill=fRun),
               binaxis = "y", stackdir = "center", stackratio = 1,
               color = "black", alpha = 0.5, #position = "nudge",
               position=position_dodge(0.8),
               inherit.aes = TRUE, binwidth = 0.01) +
  # geom_point(data=t2_allL, aes(x=hRoi, y=fc, fill=fRun), position = position_dodge(width=0.8),
  #            size=2, show.legend=F, color="gray90") +    
  # geom_segment(data=filter(t2_allW, hRoi == "ffa"), inherit.aes = FALSE,
  #              aes(x=0.8, y=filter(t2_allW, hRoi == "ffa")$face,
  #                  xend=1.2, yend=filter(t2_allW, hRoi == "ffa")$scene),
  #              color="gray90", alpha = .7) +  
  # geom_segment(data=filter(t2_allW, hRoi == "ppa"), inherit.aes = FALSE,
  #              aes(x=1.8, y=filter(t2_allW, hRoi == "ppa")$face,
  #                  xend=2.2, yend=filter(t2_allW, hRoi == "ppa")$scene),
  #              color="gray90", alpha = .7) +  
  # geom_pointrange(data=t2_allG, aes(x = hRoi, y=fc.m, ymin = lower.ci, ymax = upper.ci),
                  # position = position_dodge(0.80), color = "darkred", size = 1, show.legend = FALSE) +
  geom_errorbar(data=t2_allG, aes(ymin=lower.se, ymax=upper.se), width=.2,
                position=position_dodge(.8), color = "black") +
  scale_x_discrete(labels=c("FFA","PPA")) +
  
  scale_fill_manual(values = c("#feb24c", "#91bfdb"), # c("#feb24c", "#91bfdb"),
                    labels = c("Face attention", "Scene attention")) +
  coord_cartesian(ylim = c(-0.3, 0.9), clip = "on") +
  scale_y_continuous(breaks=c(-0.2, 0, 0.2, 0.4, 0.6, 0.8)) +
  labs(x = "", y = "Background connectivity (Zr)") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"),
        axis.text = element_text(face = "plain", hjust = 0.5, size = 15, color = "black"),
        axis.line=element_line(),
        axis.title.y = element_text(face = "plain", size = 16, color = "black"),
        axis.title.x = element_text(face = "plain", size = 16, vjust = -2.5, color = "black"),
        axis.text.x = element_text(face = "plain", vjust = -1, size = 15, color = "black"),
        strip.text.x = element_text(face = "plain", size = 15, color = "black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing=unit(1, "lines"),
        plot.margin = margin(1, 0.3, 1, 0.3, "cm"), 
        legend.title = element_blank(),
        legend.position="top")
r2_bgfc.1
```

![](fBGRED_results_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
# ggsave("fBGRED_bgfc1_verSM.jpg", plot = r2_bgfc.1, width=8, height=6, unit='in', dpi=600)

r2_bgfc.2 <- ggplot(data=t2_allL, aes(x=fRun, y=fc, fill=hRoi)) +
  geom_hline(yintercept=0, linetype='solid', color='black', alpha =1, size=0.4) +
  stat_summary(fun = mean, geom = "bar", position="dodge", na.rm = TRUE, 
               alpha = .9, width = 0.8,  size = 0.4, color = "black") +
  # geom_point(data=t2_allL, aes(x=fRun, y=fc, fill=hRoi), position = position_dodge(width=0.8),
  #            size=2, show.legend=F, color="gray90") +  
  # geom_segment(data=filter(t2_allW1,fRun == "face"), inherit.aes = FALSE,
  #              aes(x=0.8, y=filter(t2_allW1, fRun == "face")$ffa,
  #                  xend=1.2, yend=filter(t2_allW1, fRun == "face")$ppa),
  #              color="gray90", alpha = .7) +  
  # geom_segment(data=filter(t2_allW1,fRun == "scene"), inherit.aes = FALSE,
  #              aes(x=1.8, y=filter(t2_allW1, fRun == "scene")$ffa,
  #                  xend=2.2, yend=filter(t2_allW1, fRun == "scene")$ppa),
  #              color="gray90", alpha = .7) +  
  # geom_pointrange(data=t2_allG, aes(x = fRun, y=fc.m, ymin = lower.ci, ymax = upper.ci),
  #                 position = position_dodge(0.80), color = "darkred", size = 1, show.legend = FALSE) +
  geom_errorbar(data=t2_allG, aes(ymin=lower.ci, ymax=upper.ci), width=.2,
                position=position_dodge(.8), color = "black") +
  facet_grid(.~vRoi, scales="free_x", space = "free",
             labeller = labeller(vRoi = c("v1" = "V1","v2" = "V2","v3" = "V3","v4" = "V4"))) +
  scale_x_discrete(labels=c("Face","Scene")) +
  # scale_x_discrete(labels=c("Face\nattention","Scene\nattention")) +
  scale_fill_manual(values = c("#B35750", "#628DCF"), # c("#feb24c", "#91bfdb"),
                    labels = c("FFA", "PPA")) +
  coord_cartesian(ylim = c(-0.1, 0.6), clip = "on") +
  labs(x = " ", y = "Background connectivity (Zr)") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"),
        axis.text = element_text(face = "plain", hjust = 0.5, size = 15, color = "black"),
        axis.line=element_line(),
        axis.title.y = element_text(face = "plain", size = 16, color = "black"),
        axis.title.x = element_text(face = "plain", size = 16, vjust = -2.5, color = "black"),
        axis.text.x = element_text(face = "plain", vjust = -1, size = 15, color = "black"),
        strip.text.x = element_text(face = "plain", size = 15, color = "black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing=unit(1, "lines"),
        plot.margin = margin(1, 0.3, 1, 0.3, "cm"), 
        legend.title = element_blank(),
        legend.position="top")
# r2_bgfc.2
# ggsave("fBGRED_bgfc2_verSM.jpg", plot = r2_bgfc.2, width=8, height=6, unit='in', dpi=600)

ggarrange(r2_bgfc.1, r2_bgfc.2, ncol=2, 
          labels=c("A","B"))
```

![](fBGRED_results_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

<br>

각 시각 영역(v1, v2, v3, v4), 주의 조건(face vs. scene)과 관심 상위
영역(FFA vs. PPA)을 요인으로 4 x 2 x 2 혼합 요인 분산분석을 수행하였다.
분석 결과에는 삼원 상호 작용이 포함되어 있으나 시각 영역에 따른 효과는
관심 효과가 아님에 주의할 필요가 있다.

<br>

``` r
t2.aov1 <- aov_ez(id="subj", dv="fc", data = t2_allL, within = c("vRoi", "fRun", "hRoi"), 
                  anova_table = list(correction = "none"))
nice(t2.aov1, es="pes") %>% kable(digits=3)
```

| Effect         | df    | MSE  | F            | pes  | p.value  |
|:---------------|:------|:-----|:-------------|:-----|:---------|
| vRoi           | 3, 69 | 0.03 | 91.56 \*\*\* | .799 | &lt;.001 |
| fRun           | 1, 23 | 0.01 | 22.31 \*\*\* | .492 | &lt;.001 |
| hRoi           | 1, 23 | 0.05 | 3.28 +       | .125 | .083     |
| vRoi:fRun      | 3, 69 | 0.00 | 9.71 \*\*\*  | .297 | &lt;.001 |
| vRoi:hRoi      | 3, 69 | 0.01 | 1.75         | .071 | .164     |
| fRun:hRoi      | 1, 23 | 0.02 | 59.41 \*\*\* | .721 | &lt;.001 |
| vRoi:fRun:hRoi | 3, 69 | 0.00 | 9.96 \*\*\*  | .302 | &lt;.001 |

<br>

사후 검정 결과는 아래와 같다.

<br>

``` r
ph1 <- t2_allL %>% 
  group_by(vRoi, hRoi) %>% 
  rstatix::pairwise_t_test(fc ~ fRun, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL %>% 
  group_by(vRoi, hRoi) %>%  
  rstatix::cohens_d(fc ~ fRun, paired=T, ci = F) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "hRoi", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | hRoi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:-----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v1   | FFA  | face   | scene  |    0.047 |   -0.003 |     0.097 |  23 |     1.936 | 0.065 | ns           |   0.395 | small     |
| v1   | PPA  | face   | scene  |   -0.090 |   -0.162 |    -0.019 |  23 |    -2.605 | 0.016 | \*           |  -0.532 | moderate  |
| v2   | FFA  | face   | scene  |    0.056 |    0.007 |     0.105 |  23 |     2.363 | 0.027 | \*           |   0.482 | small     |
| v2   | PPA  | face   | scene  |   -0.121 |   -0.169 |    -0.073 |  23 |    -5.185 | 0.000 | \*\*\*\*     |  -1.058 | large     |
| v3   | FFA  | face   | scene  |    0.065 |    0.021 |     0.109 |  23 |     3.074 | 0.005 | \*\*         |   0.627 | moderate  |
| v3   | PPA  | face   | scene  |   -0.224 |   -0.268 |    -0.180 |  23 |   -10.573 | 0.000 | \*\*\*\*     |  -2.158 | large     |
| v4   | FFA  | face   | scene  |    0.059 |    0.009 |     0.109 |  23 |     2.444 | 0.023 | \*           |   0.499 | small     |
| v4   | PPA  | face   | scene  |   -0.264 |   -0.321 |    -0.207 |  23 |    -9.526 | 0.000 | \*\*\*\*     |  -1.944 | large     |

``` r

ph1 <- t2_allL %>% 
  group_by(vRoi, fRun) %>% 
  rstatix::pairwise_t_test(fc ~ hRoi, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, fRun, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL %>% 
  group_by(vRoi, fRun) %>%  
  rstatix::cohens_d(fc ~ hRoi, paired=T, ci = F) %>% 
  dplyr::select(vRoi, fRun, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "fRun", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | fRun  | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:------|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v1   | face  | FFA    | PPA    |    0.080 |   -0.018 |     0.178 |  23 |     1.681 | 0.106 | ns           |   0.343 | small     |
| v1   | scene | FFA    | PPA    |   -0.057 |   -0.104 |    -0.010 |  23 |    -2.530 | 0.019 | \*           |  -0.517 | moderate  |
| v2   | face  | FFA    | PPA    |    0.135 |    0.061 |     0.210 |  23 |     3.741 | 0.001 | \*\*         |   0.764 | moderate  |
| v2   | scene | FFA    | PPA    |   -0.041 |   -0.079 |    -0.004 |  23 |    -2.264 | 0.033 | \*           |  -0.462 | small     |
| v3   | face  | FFA    | PPA    |    0.217 |    0.149 |     0.285 |  23 |     6.641 | 0.000 | \*\*\*\*     |   1.356 | large     |
| v3   | scene | FFA    | PPA    |   -0.072 |   -0.127 |    -0.018 |  23 |    -2.747 | 0.012 | \*           |  -0.561 | moderate  |
| v4   | face  | FFA    | PPA    |    0.194 |    0.094 |     0.294 |  23 |     4.022 | 0.001 | \*\*\*       |   0.821 | large     |
| v4   | scene | FFA    | PPA    |   -0.129 |   -0.187 |    -0.071 |  23 |    -4.602 | 0.000 | \*\*\*       |  -0.939 | large     |

<br>

구형성 가정 위배에 따른 보정을 고려하여 시각 영역별 분석을 수행하였다.

<br>

**V1 결과**

``` r
t2_allL.tmp <- t2_allL %>% filter(vRoi=="v1")

t2.aov1.1 <- aov_ez(id="subj", dv="fc", data = t2_allL.tmp, within = c("fRun", "hRoi"),
                    anova_table = list(correction = "none"))
nice(t2.aov1.1, es="pes") %>% kable(digits=3)
```

| Effect    | df    | MSE  | F       | pes  | p.value |
|:----------|:------|:-----|:--------|:-----|:--------|
| fRun      | 1, 23 | 0.01 | 1.68    | .068 | .207    |
| hRoi      | 1, 23 | 0.02 | 0.17    | .007 | .687    |
| fRun:hRoi | 1, 23 | 0.01 | 7.65 \* | .250 | .011    |

``` r
ph1 <- t2_allL.tmp %>% 
  group_by(vRoi, hRoi) %>% 
  rstatix::pairwise_t_test(fc ~ fRun, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL.tmp %>% 
  group_by(vRoi, hRoi) %>%  
  rstatix::cohens_d(fc ~ fRun, paired=T, ci = F) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "hRoi", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | hRoi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:-----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v1   | FFA  | face   | scene  |    0.047 |   -0.003 |     0.097 |  23 |     1.936 | 0.065 | ns           |   0.395 | small     |
| v1   | PPA  | face   | scene  |   -0.090 |   -0.162 |    -0.019 |  23 |    -2.605 | 0.016 | \*           |  -0.532 | moderate  |

``` r

ph1 <- t2_allL.tmp %>% 
  group_by(vRoi, fRun) %>% 
  rstatix::pairwise_t_test(fc ~ hRoi, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, fRun, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL.tmp %>% 
  group_by(vRoi, fRun) %>%  
  rstatix::cohens_d(fc ~ hRoi, paired=T, ci = F) %>% 
  dplyr::select(vRoi, fRun, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "fRun", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | fRun  | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:------|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v1   | face  | FFA    | PPA    |    0.080 |   -0.018 |     0.178 |  23 |     1.681 | 0.106 | ns           |   0.343 | small     |
| v1   | scene | FFA    | PPA    |   -0.057 |   -0.104 |    -0.010 |  23 |    -2.530 | 0.019 | \*           |  -0.517 | moderate  |

<br>

**V2 결과**

``` r
t2_allL.tmp <- t2_allL %>% filter(vRoi=="v2")

t2.aov1.1 <- aov_ez(id="subj", dv="fc", data = t2_allL.tmp, within = c("fRun", "hRoi"),
                    anova_table = list(correction = "none"))
nice(t2.aov1.1, es="pes") %>% kable(digits=3)
```

| Effect    | df    | MSE  | F            | pes  | p.value  |
|:----------|:------|:-----|:-------------|:-----|:---------|
| fRun      | 1, 23 | 0.01 | 3.85 +       | .143 | .062     |
| hRoi      | 1, 23 | 0.01 | 4.06 +       | .150 | .056     |
| fRun:hRoi | 1, 23 | 0.01 | 28.40 \*\*\* | .553 | &lt;.001 |

``` r
ph1 <- t2_allL.tmp %>% 
  group_by(vRoi, hRoi) %>% 
  rstatix::pairwise_t_test(fc ~ fRun, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL.tmp %>% 
  group_by(vRoi, hRoi) %>%  
  rstatix::cohens_d(fc ~ fRun, paired=T, ci = F) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "hRoi", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | hRoi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:-----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v2   | FFA  | face   | scene  |    0.056 |    0.007 |     0.105 |  23 |     2.363 | 0.027 | \*           |   0.482 | small     |
| v2   | PPA  | face   | scene  |   -0.121 |   -0.169 |    -0.073 |  23 |    -5.185 | 0.000 | \*\*\*\*     |  -1.058 | large     |

``` r

ph1 <- t2_allL.tmp %>% 
  group_by(vRoi, fRun) %>% 
  rstatix::pairwise_t_test(fc ~ hRoi, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, fRun, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL.tmp %>% 
  group_by(vRoi, fRun) %>%  
  rstatix::cohens_d(fc ~ hRoi, paired=T, ci = F) %>% 
  dplyr::select(vRoi, fRun, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "fRun", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | fRun  | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:------|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v2   | face  | FFA    | PPA    |    0.135 |    0.061 |     0.210 |  23 |     3.741 | 0.001 | \*\*         |   0.764 | moderate  |
| v2   | scene | FFA    | PPA    |   -0.041 |   -0.079 |    -0.004 |  23 |    -2.264 | 0.033 | \*           |  -0.462 | small     |

<br>

**V3 결과**

``` r
t2_allL.tmp <- t2_allL %>% filter(vRoi=="v3")

t2.aov1.1 <- aov_ez(id="subj", dv="fc", data = t2_allL.tmp, within = c("fRun", "hRoi"),
                    anova_table = list(correction = "none"))
nice(t2.aov1.1, es="pes") %>% kable(digits=3)
```

| Effect    | df    | MSE  | F             | pes  | p.value  |
|:----------|:------|:-----|:--------------|:-----|:---------|
| fRun      | 1, 23 | 0.01 | 25.32 \*\*\*  | .524 | &lt;.001 |
| hRoi      | 1, 23 | 0.02 | 7.69 \*       | .251 | .011     |
| fRun:hRoi | 1, 23 | 0.00 | 103.97 \*\*\* | .819 | &lt;.001 |

``` r
ph1 <- t2_allL.tmp %>% 
  group_by(vRoi, hRoi) %>% 
  rstatix::pairwise_t_test(fc ~ fRun, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL.tmp %>% 
  group_by(vRoi, hRoi) %>%  
  rstatix::cohens_d(fc ~ fRun, paired=T, ci = F) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "hRoi", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | hRoi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:-----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v3   | FFA  | face   | scene  |    0.065 |    0.021 |     0.109 |  23 |     3.074 | 0.005 | \*\*         |   0.627 | moderate  |
| v3   | PPA  | face   | scene  |   -0.224 |   -0.268 |    -0.180 |  23 |   -10.573 | 0.000 | \*\*\*\*     |  -2.158 | large     |

``` r

ph1 <- t2_allL.tmp %>% 
  group_by(vRoi, fRun) %>% 
  rstatix::pairwise_t_test(fc ~ hRoi, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, fRun, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL.tmp %>% 
  group_by(vRoi, fRun) %>%  
  rstatix::cohens_d(fc ~ hRoi, paired=T, ci = F) %>% 
  dplyr::select(vRoi, fRun, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "fRun", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | fRun  | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:------|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v3   | face  | FFA    | PPA    |    0.217 |    0.149 |     0.285 |  23 |     6.641 | 0.000 | \*\*\*\*     |   1.356 | large     |
| v3   | scene | FFA    | PPA    |   -0.072 |   -0.127 |    -0.018 |  23 |    -2.747 | 0.012 | \*           |  -0.561 | moderate  |

<br>

**V4 결과**

``` r
t2_allL.tmp <- t2_allL %>% filter(vRoi=="v4")

t2.aov1.1 <- aov_ez(id="subj", dv="fc", data = t2_allL.tmp, within = c("fRun", "hRoi"),
                    anova_table = list(correction = "none"))
nice(t2.aov1.1, es="pes") %>% kable(digits=3)
```

| Effect    | df    | MSE  | F            | pes  | p.value  |
|:----------|:------|:-----|:-------------|:-----|:---------|
| fRun      | 1, 23 | 0.01 | 38.47 \*\*\* | .626 | &lt;.001 |
| hRoi      | 1, 23 | 0.03 | 0.91         | .038 | .350     |
| fRun:hRoi | 1, 23 | 0.01 | 64.53 \*\*\* | .737 | &lt;.001 |

``` r
ph1 <- t2_allL.tmp %>% 
  group_by(vRoi, hRoi) %>% 
  rstatix::pairwise_t_test(fc ~ fRun, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL.tmp %>% 
  group_by(vRoi, hRoi) %>%  
  rstatix::cohens_d(fc ~ fRun, paired=T, ci = F) %>% 
  dplyr::select(vRoi, hRoi, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "hRoi", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | hRoi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:-----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v4   | FFA  | face   | scene  |    0.059 |    0.009 |     0.109 |  23 |     2.444 | 0.023 | \*           |   0.499 | small     |
| v4   | PPA  | face   | scene  |   -0.264 |   -0.321 |    -0.207 |  23 |    -9.526 | 0.000 | \*\*\*\*     |  -1.944 | large     |

``` r

ph1 <- t2_allL.tmp %>% 
  group_by(vRoi, fRun) %>% 
  rstatix::pairwise_t_test(fc ~ hRoi, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(vRoi, fRun, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- t2_allL.tmp %>% 
  group_by(vRoi, fRun) %>%  
  rstatix::cohens_d(fc ~ hRoi, paired=T, ci = F) %>% 
  dplyr::select(vRoi, fRun, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("vRoi", "fRun", "group1", "group2")) %>% kable(digits=3)
```

| vRoi | fRun  | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude |
|:-----|:------|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:----------|
| v4   | face  | FFA    | PPA    |    0.194 |    0.094 |     0.294 |  23 |     4.022 | 0.001 | \*\*\*       |   0.821 | large     |
| v4   | scene | FFA    | PPA    |   -0.129 |   -0.187 |    -0.071 |  23 |    -4.602 | 0.000 | \*\*\*       |  -0.939 | large     |

<br>

------------------------------------------------------------------------

<br>

## Linking voxel-wise background functional connectivity to the redundancy gain effect (voxel-level)

<br>

### 각 복셀별 RG Difference Score와 bgFC 간의 상관 관계 (P2 bgFC)

<br>

관심 시각 영역 (V1\~V4) 에서 Redundancy Gain Effect가 PPA-VC Voxel-wise
Background Connectivity(bgFC)의 강도에 따라 조절되는지 검증하기 위해
상관 분석을 수행하였다. 각 참가자 내의 관심 영역에서 voxel 별로 4-Same -
4-Diff의 Difference Score를 계산한 후 각 voxel 별 PPA-VC bgFC 값과
상관을 계산하였다. 이 분석에서 0 보다 유의하게 상관계수가 높다는 것은
bgFC 강도가 높을수록 Redundacy Gain Effect의 강도도 높다는 것을
나타낸다.

<br>

``` r
s1 <- read.csv("data/p12_RGandBG_corr_main3_nsm.csv", header = T)

s1_l <- s1
# change class of main factors: double to factor
s1_l$subj = factor(s1_l$subj, levels=c("rd_01","rd_02","rd_03","rd_04","rd_05",
                                       "rd_06","rd_07","rd_08","rd_09","rd_10",
                                       "rd_11","rd_12","rd_13","rd_14","rd_15",
                                       "rd_16", "rd_18", "rd_19", "rd_20",
                                       "rd_21", "rd_22", "rd_23", "rd_24", "rd_25"), 
                   labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25))

# s1_l <- s1_l %>% filter(subj!=4, subj!=5, subj!=6, subj!=8)

s1_l$vRoi = factor(s1_l$vRoi) 
s1_l$hRoi = factor(s1_l$hRoi, levels = c("rPPA", "lPPA", "PPA")) 
```

<br>

각 관심 영역에서 voxel 별 difference score와 bgFC의 pearson correlation
coefficient를 Fisher의 Z transform하여 요약하였다.

<br>

``` r
s1_allL <- s1_l %>% 
  dplyr::select(subj, hRoi, vRoi, Zr) %>% 
  group_by(subj, hRoi, vRoi) %>%
  dplyr::summarise(Zr=mean(Zr)) %>%
  ungroup()
## `summarise()` has grouped output by 'subj', 'hRoi'. You can override using the `.groups` argument.
# s1_allL %>% kable(digits=2)

# subject-level, wide format
s1_allW <- s1_allL %>% dplyr::select(subj, hRoi, vRoi, Zr) %>% spread(key=vRoi, value = Zr)
# s1_allW %>% kable(digits=2)

# summary table: grand mean
s1_allG <- s1_allL %>% group_by(hRoi, vRoi) %>%
  summarise(Zr.m = mean(Zr), Zr.sd = sd(Zr)) %>%
  ungroup()
## `summarise()` has grouped output by 'hRoi'. You can override using the `.groups` argument.
s1_allG$Zr.se <- Rmisc::summarySEwithin(data = s1_allL, measurevar = "Zr", 
                                        idvar = "subj", withinvars = c("hRoi","vRoi"))$se
s1_allG$Zr.ci <- Rmisc::summarySEwithin(data = s1_allL, measurevar = "Zr", 
                                        idvar = "subj", withinvars = c("hRoi","vRoi"))$ci
s1_allG <- s1_allG %>% 
  mutate(lower.ci = Zr.m-Zr.ci,
         upper.ci = Zr.m+Zr.ci,
         lower.se = Zr.m-Zr.se,
         upper.se = Zr.m+Zr.se)
s1_allG %>% dplyr::select(hRoi, vRoi, Zr.m) %>% 
  spread(key=vRoi, value = Zr.m) %>% kable(digits=2)
```

| hRoi |   v1 |   v2 |   v3 |   v4 |
|:-----|-----:|-----:|-----:|-----:|
| rPPA | 0.00 | 0.03 | 0.14 | 0.13 |
| lPPA | 0.02 | 0.02 | 0.13 | 0.13 |
| PPA  | 0.01 | 0.03 | 0.15 | 0.14 |

<br>

각 관심 영역에서 voxel 별 difference score와 bgFC의 Zr을 그래프로
나타내었다. 점은 평군, error bar는 95% CI를 나타낸다.

<br>

``` r
s1_allL.tmp <- s1_allL
r5_RG_with_bgfc.1 <- ggplot(data=s1_allL.tmp, aes(x=vRoi, y=Zr)) + #, fill=vRoi
  geom_hline(yintercept=0, linetype='solid', color='black', alpha =1, size=0.4) +
  geom_violin(width = 0.5, trim=TRUE) + 
  ggbeeswarm::geom_quasirandom(color = "blue", size = 3, alpha = 0.2, width = 0.2) +
  facet_grid(.~hRoi, scales="free_x", space = "free") +
  
  #stat_summary(fun = mean, geom = "bar", position="dodge", na.rm = TRUE, alpha = .9, width = 0.8,  size = 1.02, show.legend=F) +
  #geom_point(data=s1_allL, aes(x=vRoi, y=Zr, fill=vRoi), position = position_dodge(width=0.8),
  #           size=2, show.legend=F, color="gray90") +  
  geom_pointrange(data=s1_allG, aes(x = vRoi, y=Zr.m, ymin = lower.ci, ymax = upper.ci),
                  position = position_dodge(0.80), color = "darkred", size = 0.8, show.legend = FALSE) +
  
  # geom_errorbar(data=s1_allG, aes(ymin=lower.ci, ymax=upper.ci), width=.2,
  #               position=position_dodge(.8), color = "black") +
  
  scale_x_discrete(labels=c("V1","V2","V3","V4")) +
  scale_fill_manual(values = c("#B1D4E0", "#2E8BC0","#0C2D48", "#145DA0")) +
  coord_cartesian(ylim = c(-0.6, 0.9), clip = "on") +
  labs(x = "", y = "Correlation coefficient (Zr)") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"),
        axis.text = element_text(face = "plain", hjust = 0.5, size = 15, color = "black"),
        axis.line=element_line(),
        axis.title.y = element_text(face = "plain", size = 16, color = "black"),
        axis.title.x = element_text(face = "plain", size = 16, vjust = -2.5, color = "black"),
        axis.text.x = element_text(face = "plain", vjust = -1, size = 15, color = "black"),
        strip.text.x = element_text(face = "plain", size = 15, color = "black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing=unit(1, "lines"),
        plot.margin = margin(1, 0.3, 1, 0.3, "cm"), 
        legend.title = element_blank(),
        legend.position="top")

r5_RG_with_bgfc.1
```

![](fBGRED_results_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
# ggsave("fBGRED_main7_verNSM.jpg", plot = r5_RG_with_bgfc.1, width=10, height=6, unit='in', dpi=600)
s1_allL.tmp <- s1_allL %>% filter(hRoi == "PPA")
s1_allG.tmp <- s1_allG %>% filter(hRoi == "PPA")
s1_allG.tmp$Zr <- s1_allG.tmp$Zr.m
r5_RG_with_bgfc.1 <- ggplot(data=s1_allL.tmp, aes(x=vRoi, y=Zr, fill = vRoi)) + #, fill=vRoi
  # geom_violin(width = 0.5, trim=TRUE) + 
  # ggbeeswarm::geom_quasirandom(color = "blue", size = 3, alpha = 0.2, width = 0.2) +
  stat_summary(fun = mean, geom = "bar", position="dodge", na.rm = TRUE, alpha = .9, width = 0.8,  size = 1.02, show.legend=F) +
  geom_hline(yintercept=0, linetype='solid', alpha =1, size=0.75) + #, color='black'
  # geom_jitter(data=s1_allL.tmp, aes(x=vRoi, y=Zr, color=vRoi),
  #             position=position_jitter(0.2), cex=2) +
  geom_quasirandom(data=s1_allL.tmp, aes(x=vRoi, y=Zr),
                   color = "black", fill = "#adadad",
                   alpha = 0.3, #position = "nudge",
                   width = 0.3,
                   size = 1.3,
                   position=position_jitter(0.05),
                   # position="jitter",
                   inherit.aes = TRUE,
                   show.legend = F) +

  # geom_dotplot(data=s1_allL.tmp, aes(x=vRoi, y=Zr, fill=vRoi),
  #              binaxis = "y", stackdir = "center", stackratio = 1.2,
  #              color = "black", alpha = 0.5, #position = "nudge",
  #              position=position_jitter(0.2),
  #              # position="jitter",
  #              inherit.aes = TRUE, binwidth = 0.02) +
  # geom_point(data=s1_allL.tmp, aes(x=vRoi, y=Zr, fill=vRoi), position = position_jitter(0.2),
  #           size=2, show.legend=F, color="gray90") +
  # geom_pointrange(data=s1_allG.tmp, aes(x = vRoi, y=Zr.m, ymin = lower.se, ymax = upper.se),
  #                 position = position_dodge(0.80), color = "darkred", size = 0.8, show.legend = FALSE) +
  geom_errorbar(data=s1_allG.tmp, aes(ymin=lower.se, ymax=upper.se), width=.2,
                position=position_dodge(.8), color = "black") +
  
  scale_x_discrete(labels=c("V1","V2","V3","V4")) +
  scale_fill_manual(values = c("#adadad", "#adadad","#adadad", "#adadad")) +
  # scale_fill_manual(values = c("#B1D4E0", "#2E8BC0","#0C2D48", "#145DA0")) +
  coord_cartesian(ylim = c(-0.5, 0.7), clip = "on") +
  labs(x = "", y = "Correlation coefficient (Zr)") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"),
        axis.text = element_text(face = "plain", hjust = 0.5, size = 15, color = "black"),
        axis.line=element_line(),
        axis.title.y = element_text(face = "plain", size = 16, color = "black"),
        axis.title.x = element_text(face = "plain", size = 16, vjust = -2.5, color = "black"),
        axis.text.x = element_text(face = "plain", vjust = -1, size = 15, color = "black"),
        strip.text.x = element_text(face = "plain", size = 15, color = "black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing=unit(1, "lines"),
        plot.margin = margin(1, 0.3, 1, 0.3, "cm"), 
        legend.title = element_blank(),
        legend.position="top")

r5_RG_with_bgfc.1
```

![](fBGRED_results_files/figure-gfm/unnamed-chunk-34-2.png)<!-- -->

``` r
# ggsave("fBGRED_main8_verNSM.jpg", plot = r5_RG_with_bgfc.1, width=8, height=6, unit='in', dpi=600)
```

<br>

``` r
s1.aov1.1 <- aov_ez(id="subj", dv="Zr", data = s1_allL , within = c("vRoi", "hRoi"),
                    anova_table = list(correction = "none"))
#summary(t4.2.aov4.2)
nice(s1.aov1.1, es="pes") %>% kable(digits=3)
```

| Effect    | df     | MSE  | F         | pes  | p.value |
|:----------|:-------|:-----|:----------|:-----|:--------|
| vRoi      | 3, 69  | 0.06 | 5.60 \*\* | .196 | .002    |
| hRoi      | 2, 46  | 0.00 | 0.18      | .008 | .838    |
| vRoi:hRoi | 6, 138 | 0.00 | 0.73      | .031 | .624    |

``` r
ph1 <- s1_allL %>% 
  group_by(hRoi) %>% 
  rstatix::pairwise_t_test(Zr ~ vRoi, 
                           p.adjust.method="bonferroni", 
                           paired=T, detailed=T) %>% 
  dplyr::select(hRoi, group1, group2, estimate, conf.low, conf.high, df, statistic, p.adj, p.adj.signif) 

ph2 <- s1_allL %>% 
  group_by(hRoi) %>%  
  rstatix::cohens_d(Zr ~ vRoi, paired=T, ci = F) %>% 
  dplyr::select(hRoi, group1, group2, effsize, magnitude)

merge(ph1, ph2, by = c("hRoi",  "group1", "group2")) %>% kable(digits=3)
```

| hRoi | group1 | group2 | estimate | conf.low | conf.high |  df | statistic | p.adj | p.adj.signif | effsize | magnitude  |
|:-----|:-------|:-------|---------:|---------:|----------:|----:|----------:|------:|:-------------|--------:|:-----------|
| lPPA | v1     | v2     |    0.000 |   -0.047 |     0.048 |  23 |     0.020 | 1.000 | ns           |   0.004 | negligible |
| lPPA | v1     | v3     |   -0.112 |   -0.212 |    -0.012 |  23 |    -2.325 | 0.176 | ns           |  -0.475 | small      |
| lPPA | v1     | v4     |   -0.104 |   -0.212 |     0.003 |  23 |    -2.003 | 0.343 | ns           |  -0.409 | small      |
| lPPA | v2     | v3     |   -0.113 |   -0.190 |    -0.035 |  23 |    -3.018 | 0.037 | \*           |  -0.616 | moderate   |
| lPPA | v2     | v4     |   -0.105 |   -0.186 |    -0.023 |  23 |    -2.663 | 0.083 | ns           |  -0.544 | moderate   |
| lPPA | v3     | v4     |    0.008 |   -0.073 |     0.089 |  23 |     0.205 | 1.000 | ns           |   0.042 | negligible |
| PPA  | v1     | v2     |   -0.019 |   -0.066 |     0.029 |  23 |    -0.810 | 1.000 | ns           |  -0.165 | negligible |
| PPA  | v1     | v3     |   -0.138 |   -0.238 |    -0.038 |  23 |    -2.852 | 0.054 | ns           |  -0.582 | moderate   |
| PPA  | v1     | v4     |   -0.127 |   -0.239 |    -0.016 |  23 |    -2.370 | 0.160 | ns           |  -0.484 | small      |
| PPA  | v2     | v3     |   -0.119 |   -0.198 |    -0.040 |  23 |    -3.128 | 0.028 | \*           |  -0.639 | moderate   |
| PPA  | v2     | v4     |   -0.109 |   -0.199 |    -0.018 |  23 |    -2.482 | 0.125 | ns           |  -0.507 | moderate   |
| PPA  | v3     | v4     |    0.011 |   -0.078 |     0.099 |  23 |     0.250 | 1.000 | ns           |   0.051 | negligible |
| rPPA | v1     | v2     |   -0.034 |   -0.084 |     0.016 |  23 |    -1.388 | 1.000 | ns           |  -0.283 | small      |
| rPPA | v1     | v3     |   -0.145 |   -0.241 |    -0.049 |  23 |    -3.135 | 0.028 | \*           |  -0.640 | moderate   |
| rPPA | v1     | v4     |   -0.129 |   -0.241 |    -0.016 |  23 |    -2.362 | 0.162 | ns           |  -0.482 | small      |
| rPPA | v2     | v3     |   -0.112 |   -0.191 |    -0.032 |  23 |    -2.905 | 0.048 | \*           |  -0.593 | moderate   |
| rPPA | v2     | v4     |   -0.095 |   -0.193 |     0.003 |  23 |    -2.005 | 0.341 | ns           |  -0.409 | small      |
| rPPA | v3     | v4     |    0.017 |   -0.074 |     0.108 |  23 |     0.376 | 1.000 | ns           |   0.077 | negligible |

<br>

각 관심 영역에서 voxel 별 difference score와 bgFC의 Zr이 0보다 유의하게
큰 지 확인하기 위해 단일 표본 t-검정을 수행하였다.

<br>

``` r
p_h1 <- s1_allL %>% 
  group_by(hRoi, vRoi) %>% 
  rstatix::t_test(Zr ~ 1, mu = 0, detailed= T, alternative = "two.sided") %>% 
  dplyr::select(hRoi, vRoi, estimate, conf.low, conf.high, df, statistic, p) 


p_h2 <- s1_allL %>% 
  group_by(hRoi, vRoi) %>% 
  rstatix::cohens_d(Zr ~ 1, mu = 0, ci = F) %>% 
  dplyr::select(hRoi, vRoi, effsize, magnitude) 

merge(p_h1, p_h2, by = c("hRoi", "vRoi")) %>% kable(digits=3)
```

| hRoi | vRoi | estimate | conf.low | conf.high |  df | statistic |     p | effsize | magnitude  |
|:-----|:-----|---------:|---------:|----------:|----:|----------:|------:|--------:|:-----------|
| lPPA | v1   |    0.021 |   -0.045 |     0.088 |  23 |     0.668 | 0.511 |   0.136 | negligible |
| lPPA | v2   |    0.021 |   -0.028 |     0.070 |  23 |     0.881 | 0.387 |   0.180 | negligible |
| lPPA | v3   |    0.134 |    0.044 |     0.224 |  23 |     3.068 | 0.005 |   0.626 | moderate   |
| lPPA | v4   |    0.126 |    0.033 |     0.218 |  23 |     2.804 | 0.010 |   0.572 | moderate   |
| PPA  | v1   |    0.009 |   -0.059 |     0.076 |  23 |     0.262 | 0.796 |   0.054 | negligible |
| PPA  | v2   |    0.027 |   -0.028 |     0.082 |  23 |     1.021 | 0.318 |   0.208 | small      |
| PPA  | v3   |    0.147 |    0.057 |     0.237 |  23 |     3.367 | 0.003 |   0.687 | moderate   |
| PPA  | v4   |    0.136 |    0.034 |     0.238 |  23 |     2.765 | 0.011 |   0.564 | moderate   |
| rPPA | v1   |   -0.002 |   -0.072 |     0.067 |  23 |    -0.068 | 0.946 |  -0.014 | negligible |
| rPPA | v2   |    0.031 |   -0.029 |     0.092 |  23 |     1.073 | 0.294 |   0.219 | small      |
| rPPA | v3   |    0.143 |    0.057 |     0.229 |  23 |     3.436 | 0.002 |   0.701 | moderate   |
| rPPA | v4   |    0.126 |    0.024 |     0.228 |  23 |     2.560 | 0.018 |   0.523 | moderate   |

<br>

------------------------------------------------------------------------

<br>

<br>

<br>

------------------------------------------------------------------------

<br>

# Session Info

<Br>

``` r
sessionInfo()
## R version 4.1.1 (2021-08-10)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS Monterey 12.1
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] rstatix_0.7.0        effectsize_0.4.5     caret_6.0-88        
##  [4] psycho_0.6.1         effects_4.2-0        latticeExtra_0.6-29 
##  [7] ggpubr_0.4.0         emmeans_1.6.3        lmerTest_3.1-3      
## [10] afex_1.0-1           lme4_1.1-27.1        Matrix_1.3-4        
## [13] psych_2.1.9          car_3.0-11           carData_3.0-4       
## [16] knitr_1.37           forcats_0.5.1        stringr_1.4.0       
## [19] dplyr_1.0.7          purrr_0.3.4          readr_2.0.1         
## [22] tidyr_1.1.3          tibble_3.1.4         tidyverse_1.3.1     
## [25] ggbeeswarm_0.6.0     ggplot2_3.3.5        Rmisc_1.5           
## [28] plyr_1.8.6           lattice_0.20-44      pacman_0.5.1        
## [31] klippy_0.0.0.9500    patchwork_1.1.0.9000 papaja_0.1.0.9997   
## [34] devtools_2.4.2       usethis_2.0.1        distill_1.2         
## 
## loaded via a namespace (and not attached):
##   [1] utf8_1.2.2           tidyselect_1.1.1     grid_4.1.1          
##   [4] pROC_1.18.0          munsell_0.5.0        codetools_0.2-18    
##   [7] future_1.22.1        withr_2.4.3          colorspace_2.0-2    
##  [10] highr_0.9            rstudioapi_0.13      stats4_4.1.1        
##  [13] ggsignif_0.6.3       listenv_0.8.0        labeling_0.4.2      
##  [16] mnormt_2.0.2         farver_2.1.0         datawizard_0.2.0.1  
##  [19] rprojroot_2.0.2      coda_0.19-4          parallelly_1.28.1   
##  [22] vctrs_0.3.8          generics_0.1.0       TH.data_1.1-0       
##  [25] ipred_0.9-12         xfun_0.29            R6_2.5.1            
##  [28] cachem_1.0.6         assertthat_0.2.1     scales_1.1.1        
##  [31] multcomp_1.4-18      nnet_7.3-16          beeswarm_0.4.0      
##  [34] gtable_0.3.0         downlit_0.2.1        globals_0.14.0      
##  [37] processx_3.5.2       sandwich_3.0-1       timeDate_3043.102   
##  [40] rlang_0.4.12         splines_4.1.1        ModelMetrics_1.2.2.2
##  [43] broom_0.7.9.9000     yaml_2.2.1           reshape2_1.4.4      
##  [46] abind_1.4-5          modelr_0.1.8         backports_1.2.1     
##  [49] tools_4.1.1          lava_1.6.10          ellipsis_0.3.2      
##  [52] RColorBrewer_1.1-2   sessioninfo_1.1.1    Rcpp_1.0.7          
##  [55] ps_1.6.0             prettyunits_1.1.1    rpart_4.1-15        
##  [58] cowplot_1.1.1        zoo_1.8-9            haven_2.4.3         
##  [61] fs_1.5.0             survey_4.1-1         magrittr_2.0.1      
##  [64] data.table_1.14.0    openxlsx_4.2.4       reprex_2.0.1        
##  [67] tmvnsim_1.0-2        mvtnorm_1.1-2        pkgload_1.2.2       
##  [70] hms_1.1.0            evaluate_0.14        xtable_1.8-4        
##  [73] rio_0.5.27           jpeg_0.1-9           readxl_1.3.1        
##  [76] testthat_3.0.4       compiler_4.1.1       crayon_1.4.2        
##  [79] minqa_1.2.4          htmltools_0.5.2      tzdb_0.1.2          
##  [82] lubridate_1.7.10     DBI_1.1.1            dbplyr_2.1.1        
##  [85] MASS_7.3-54          boot_1.3-28          cli_3.1.0           
##  [88] mitools_2.4          insight_0.14.4       gower_0.2.2         
##  [91] pkgconfig_2.0.3      numDeriv_2016.8-1.1  foreign_0.8-81      
##  [94] recipes_0.1.16       xml2_1.3.3           foreach_1.5.1       
##  [97] vipor_0.4.5          estimability_1.3     prodlim_2019.11.13  
## [100] rvest_1.0.1          callr_3.7.0          digest_0.6.29       
## [103] parameters_0.14.0    rmarkdown_2.11       cellranger_1.1.0    
## [106] curl_4.3.2           nloptr_1.2.2.2       lifecycle_1.0.0     
## [109] nlme_3.1-152         jsonlite_1.7.2       desc_1.3.0          
## [112] fansi_0.5.0          pillar_1.6.2         fastmap_1.1.0       
## [115] httr_1.4.2           pkgbuild_1.2.0       survival_3.2-11     
## [118] glue_1.6.0           remotes_2.4.0        bayestestR_0.11.0   
## [121] zip_2.2.0            png_0.1-7            iterators_1.0.13    
## [124] class_7.3-19         stringi_1.7.6        memoise_2.0.0       
## [127] future.apply_1.8.1
```
