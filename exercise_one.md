Excercise One: Conditional models and population-averaged models
================

The purpose of this homework assignment is to examine differences between conditional unit-effect models and population averaged General estimation equation-models for longitudinal data with a binary outcome.

The general assignment is to replicate and assess (parts of) the analysis in Marinovs article "Do economic sanctions destabilize country leaders?" from 2005. This will be solved in three parts:

1.  replication of the model reported in column one of table 2 at page 571,
2.  assessing the robustness of how this model handles unit effects through the use of one other model, and
3.  assessing whether a GEE-model may be more justifiable, given data and research question, and through discussing various specifications of such models.

Data
====

According to Marinov, all countries with a population over 500 000 (160 countries) was included in his analysis, with data from 1947 to 1999 organised as country-years. Due to the availability of covariate data, the author records the total number of observations as 5 716.

This does not seem to be case for our data: The dataset made available through GitHub has 10 931 observations of 34 variables. We are left with a total of 5 766 observations after Dr. Zorns rowwise cleaning of missing values. 50 more than Marinov, for some reason. The data cover the 50 years from 1950 to 1999, for a total of 163 countries. The number of time points T for the different countries i varies from 4 for Eritrea to 50 for Venezuela and 44 other countries. The average number of observations per country is 35.

This is thus unbalanced panel data.

For informative purposes, here is a short breakdown of the variables used by Marinov in his analysis. All independent variables are lagged one year (notation: l1).

-   fail: Main dependent variable, an operationalization of leadership change. Whether a leadership change had taken place during that year (1) or not (0).
-   sanctionsl1: Main independent variable. Whether the country was subjected to economic sanctions (1) or not (0). The author notes that his source of data may have missed sanctions.
-   forcel1: Was the country involved in a militarized interstate dispute this year, where the hostility level says that force was actually applied? (1 if yes, 0 if no)
-   growthpc: Growth in income per capita
-   lngdppc: Income per capita (logged gdp per capita?)
-   democl1 and democlnt: Dummyvariable for wheteher the country was a democracy (1) or not (0). Reference cathegory (not included) is autocracy. Interactions between deomcracy and natural log of years in office is also included.
-   mixedl1 and mixedlnt: Dummyvariable for wheteher the country was a mixed regime (1) or not (0). Reference cathegory (not included) is autocracy. Interactions between mixed regime and natural log of years in office is also included.
-   age: only mentioned twice in the article, and not explained, but presumably age of the leader. The range goes from 14 (Swaziland) to 93 (China).
-   ot3: Years in office
-   X\_spline1, X\_spline2, X\_spline3: natural cubic splines, intended to capture increase or decrease in the risk of losing office over time, as suggested in the Beck, Katz and TUrner-article.

Replication.
============

Marinov uses a logistic regression with fixed country effects. All of his calculations were performed in STATA 8. I replicate these using the glmmML::glmmboot-function in R.

``` r
fit_rep = glmmboot(fail~sanctionsl1+forcel1+growthpc+lngdppc+democl1+democlnt+mixedl1+mixedlnt+age+ot3+X_spline1+X_spline2+X_spline3,
                   data=df,family=binomial,cluster=as.factor(country))

summary(fit_rep,digits=1)
```

    ## 
    ## Call:  glmmboot(formula = fail ~ sanctionsl1 + forcel1 + growthpc +      lngdppc + democl1 + democlnt + mixedl1 + mixedlnt + age +      ot3 + X_spline1 + X_spline2 + X_spline3, family = binomial,      data = df, cluster = as.factor(country)) 
    ## 
    ## 
    ##              coef se(coef)    z           Pr(>|z|)
    ## sanctionsl1  0.29    0.136  2.2 0.0300000000000000
    ## forcel1     -0.27    0.142 -1.9 0.0600000000000000
    ## growthpc    -1.54    0.688 -2.2 0.0200000000000000
    ## lngdppc     -0.10    0.116 -0.8 0.4000000000000000
    ## democl1      0.19    0.215  0.9 0.4000000000000000
    ## democlnt     0.77    0.122  6.4 0.0000000002000000
    ## mixedl1      0.42    0.191  2.2 0.0300000000000000
    ## mixedlnt     0.39    0.111  3.5 0.0005000000000000
    ## age          0.02    0.005  4.3 0.0000200000000000
    ## ot3         -0.99    0.122 -8.1 0.0000000000000006
    ## X_spline1   -0.18    0.024 -7.6 0.0000000000000400
    ## X_spline2    0.07    0.010  7.1 0.0000000000010000
    ## X_spline3   -0.01    0.002 -5.9 0.0000000050000000
    ## 
    ## Residual deviance: Inf on 5590 degrees of freedom    AIC: Inf

``` r
#stargazer nor broom::tidy doesn't support glmmboot yet - an attempt at making something slightly more readable
tabell_fit_rep = data.frame(coeff = names(fit_rep$coefficients),estimate=fit_rep$coefficients,`se_coeff` = fit_rep$sd, stringsAsFactors = FALSE,row.names = NULL)
kable(tabell_fit_rep,digits=2) #to be useful, should also include p-values.
```

| coeff       |  estimate|  se\_coeff|
|:------------|---------:|----------:|
| sanctionsl1 |      0.29|       0.14|
| forcel1     |     -0.27|       0.14|
| growthpc    |     -1.54|       0.69|
| lngdppc     |     -0.10|       0.12|
| democl1     |      0.19|       0.22|
| democlnt    |      0.77|       0.12|
| mixedl1     |      0.42|       0.19|
| mixedlnt    |      0.39|       0.11|
| age         |      0.02|       0.01|
| ot3         |     -0.99|       0.12|
| X\_spline1  |     -0.18|       0.02|
| X\_spline2  |      0.07|       0.01|
| X\_spline3  |     -0.01|       0.00|

``` r
#it would probably be useful to have Marinovs result as a table as well - but this can be added in word after-the-fact
```

This seems to be a pretty close replication.

There are some deviations from the coefficient estimates in Marinos article, the largest one beeing the coefficient estimate of economic growth, which is 0.07 lower in the results presented here.

Substantially, this means that a 1 unit increase in economic growth with a country, gives an increase in the log-odds of leadership change of 0.07 . A 1 unit increase in gdp growth per capita seems to imply a 100 % increase, and the median value of the variable is 0.02 (2 % yearly growth in gdp per capita). Thus, considering measurement errors and the like, an error at the second decimal place does not seem to imply any substantial differences here.

Robust handling of unit effects
===============================

Marinov uses a logistic regression with fixed country effects. He gives a couple of motivations for this choice, of which I will focus on the first one: assuming we have a large amount of unobserved country-specific time-invariant factors, fixed effects for countries will handle these unit effects.

The fixed effects approach chosen by the author does handle unobserved country-specific effects. However, it does not utilize the variation between the units. Given that this variation is of substantial interest, a random effects-model might be of interest.

As we are utilising a rather long time period for some countries (50 years at the most, 35 at average), it might be reasonable to think of the variation as more random than fixed. Furthermore, as the author himself specified, the dataset on sanctions might be incomplete. This might imply a random disturbance, that might not be uniform across countries.

We estimate a logistic random effects model using glmmML from the glmmML-package in R, using country as our grouping variable.

``` r
#for fixed effects GLM, I'm going with glmmML::glmmboot per the lectures (Gauss-Hermite)
fit_re = glmmML(fail~sanctionsl1+forcel1+growthpc+lngdppc+democl1+democlnt+mixedl1+mixedlnt+age+ot3+X_spline1+X_spline2+X_spline3,
                   data=df,family=binomial,cluster=as.factor(country))

summary(fit_re,digits=1)
```

    ## 
    ## Call:  glmmML(formula = fail ~ sanctionsl1 + forcel1 + growthpc + lngdppc +      democl1 + democlnt + mixedl1 + mixedlnt + age + ot3 + X_spline1 +      X_spline2 + X_spline3, family = binomial, data = df, cluster = as.factor(country)) 
    ## 
    ## 
    ##               coef se(coef)     z           Pr(>|z|)
    ## (Intercept) -2.850    0.431 -6.61 0.0000000000400000
    ## sanctionsl1  0.268    0.124  2.16 0.0300000000000000
    ## forcel1     -0.279    0.136 -2.06 0.0400000000000000
    ## growthpc    -1.232    0.635 -1.94 0.0500000000000000
    ## lngdppc      0.002    0.048  0.04 1.0000000000000000
    ## democl1      0.405    0.200  2.02 0.0400000000000000
    ## democlnt     0.630    0.115  5.47 0.0000000400000000
    ## mixedl1      0.517    0.182  2.84 0.0050000000000000
    ## mixedlnt     0.255    0.105  2.44 0.0100000000000000
    ## age          0.020    0.004  4.48 0.0000080000000000
    ## ot3         -0.952    0.118 -8.03 0.0000000000000009
    ## X_spline1   -0.171    0.023 -7.30 0.0000000000003000
    ## X_spline2    0.065    0.009  6.85 0.0000000000070000
    ## X_spline3   -0.013    0.002 -5.66 0.0000000200000000
    ## 
    ## Scale parameter in mixing distribution:  0.7 gaussian 
    ## Std. Error:                              0.08 
    ## 
    ##         LR p-value for H_0: sigma = 0:  0.00000000000000000000000000002 
    ## 
    ## Residual deviance: 4000 on 5751 degrees of freedom   AIC: 4000

``` r
#stargazer nor broom::tidy doesn't support glmmboot yet - an attempt at making something slightly more readable
tabell_fit_re = data.frame(coeff = names(fit_re$coefficients),estimate=fit_re$coefficients,`se_coeff` = fit_re$coef.sd, stringsAsFactors = FALSE,row.names = NULL)
kable(tabell_fit_re,digits=2) #to be useful, should also include p-values.
```

| coeff       |  estimate|  se\_coeff|
|:------------|---------:|----------:|
| (Intercept) |     -2.85|       0.43|
| sanctionsl1 |      0.27|       0.12|
| forcel1     |     -0.28|       0.14|
| growthpc    |     -1.23|       0.63|
| lngdppc     |      0.00|       0.05|
| democl1     |      0.41|       0.20|
| democlnt    |      0.63|       0.12|
| mixedl1     |      0.52|       0.18|
| mixedlnt    |      0.26|       0.10|
| age         |      0.02|       0.00|
| ot3         |     -0.95|       0.12|
| X\_spline1  |     -0.17|       0.02|
| X\_spline2  |      0.06|       0.01|
| X\_spline3  |     -0.01|       0.00|

| coeff       |  coeff\_random|  coeff\_replication|  se\_random|  se\_replication|
|:------------|--------------:|-------------------:|-----------:|----------------:|
| (Intercept) |          -2.85|                  NA|        0.43|               NA|
| age         |           0.02|                0.02|        0.00|             0.01|
| democl1     |           0.41|                0.19|        0.20|             0.22|
| democlnt    |           0.63|                0.77|        0.12|             0.12|
| forcel1     |          -0.28|               -0.27|        0.14|             0.14|
| growthpc    |          -1.23|               -1.54|        0.63|             0.69|
| lngdppc     |           0.00|               -0.10|        0.05|             0.12|
| mixedl1     |           0.52|                0.42|        0.18|             0.19|
| mixedlnt    |           0.26|                0.39|        0.10|             0.11|
| ot3         |          -0.95|               -0.99|        0.12|             0.12|
| sanctionsl1 |           0.27|                0.29|        0.12|             0.14|
| X\_spline1  |          -0.17|               -0.18|        0.02|             0.02|
| X\_spline2  |           0.06|                0.07|        0.01|             0.01|
| X\_spline3  |          -0.01|               -0.01|        0.00|             0.00|

In the table above, we compare the coefficient estimates of the two models - the replicated Marinov and the random effects.

A difference is of course that the random effects model gives us an intercept term. Since none of the variables are centered, this starts off at quite a low level.

The main variable of interest to Marinov, the effect of economic sanctions, is largely similar. Economic growth is no longer significant at the 0.05 level. None of the significant variables switches direction (the effect of wealth is estimated as 0 in the random effects-model, but indistinguisable from zero in the original article).

As for the effect sizes, a proper comparison of what constitutes a large and small change (as seen in the GDP-example) entails getting further into the variation of each variable, and assessing whether a unit change is substantially of interest or not. At some level, this requires subject matter knowledge. From a layman point of view, the dummy-variables for democratic and mixed institutions (contrary to the autocratic regimes) seems to exhibit significant positive change: with a random error term, there is now a larger effect of democratic or mixed institutions on the probability of leadership failure, everything else being equal.

### What model to choose?

In total, a random-effects approach gives us the same relationship for the main variable of interest, but with some slight alterations - economic growth is less important for leadership change, non-autocratic institutions more.

A staple statistical test for these circumstanses is the Hausman-test. As shown by f.i. Clark and Linzer (2015), the Hausman-test might be less informative that orginally intended. There also seems to be lacking an easily available R-function for objects from the glmmML-package. As such, we are left with other arguments. We will briefly consider one substantial argument, and one technical.

What does the substantive findings imply? Well, if we assume for a minute that both models are similarly unbiased and so on, we could possibly attribute the coefficients as differences in within- and between-variation, where economic growth matters more within a given country for leadership change, but less so for variation in leadership change between countries. Looking at the variation between countries, leadership change can better be attributed to whether the countries are autocratic or not.

Furthermore, it seems reasonable that data on the between-effects should be of interest. After all, the question Marinov asks is one of inter-national relations. Surely one is interested in assessing how different countries fare compared to each other?

Clark and Linzer (2015) argues that even though you with a random-effects-model assume the independence of the error term from the coefficient, not all types of correlation between the two are problematic. They show through simulation random effects can have a better (or at least not worse) RMSE for a wide range of N, T and correlations between the unit effects and independent variables. Especially for smaller samples and T, and a "non-sluggish dependent variable", random effects are similar in RMSE to fixed effects, trading bias for efficiency. A sluggish dependent variable is defined as a variable with more between than within variation. Intuitively, that might be the case here. A simple one-way ANOVA indicates the same.

``` r
kable(broom::tidy(aov(fail~as.factor(country),data=df)),digits=2)
```

| term               |    df|   sumsq|  meansq|  statistic|  p.value|
|:-------------------|-----:|-------:|-------:|----------:|--------:|
| as.factor(country) |   162|  105.81|    0.65|       5.52|        0|
| Residuals          |  5603|  662.62|    0.12|         NA|       NA|

As such, the choice between random and fixed effects is one that has to be taken on the grounds of whether we believe the unit effects to be randomly distributed, or systematicly distributed.

GEE, a more suitable model?
===========================

Concidering some of the wording in the article, it seems likely that the author also has some interest in the general, population-average effect of sanctions. At page 572, he spends valuable article-time investigating average effects.

Generalized Estimation Equations offers such a possibility, as it estimates an unconditional or marginal average effect of the independent variables.

When fitting a GEE, one has to specify a distribution, a link function and a correlation structure. We apply a binomial distribution and an l link. Where the unit effect models discussed above model the unit effects, a GEE approach uses a correlation structure to account for within-unit variation.

Both the estimatied coeffients and covariance (given that we employ the procedure for empirical correction) is consistent in N. This last property holds even when the correlation matrix is misspecified.

We will briefly consider two variations on the correlation structure: an exchangable correlation structure, and an ar1-correlation structure. - As we have an average of 35 time points per unit, and leadership change in one year might feasible effect either more leadership change next year, or less leadership change, an autoregressive strucutre seems interesting. - An exchangable correlation structure is similar to a random effects-modell, with a common component for each year.

``` r
fit_gee_x = geeglm(fail~sanctionsl1+forcel1+growthpc+lngdppc+democl1+democlnt+mixedl1+mixedlnt+age+ot3+X_spline1+X_spline2+X_spline3,
                  data=df,id=as.factor(country),family=binomial(logit),corstr="exchangeable")

summary(fit_gee_x)
```

    ## 
    ## Call:
    ## geeglm(formula = fail ~ sanctionsl1 + forcel1 + growthpc + lngdppc + 
    ##     democl1 + democlnt + mixedl1 + mixedlnt + age + ot3 + X_spline1 + 
    ##     X_spline2 + X_spline3, family = binomial(logit), data = df, 
    ##     id = as.factor(country), corstr = "exchangeable")
    ## 
    ##  Coefficients:
    ##              Estimate   Std.err   Wald      Pr(>|W|)    
    ## (Intercept) -2.806373  0.548891 26.141 0.00000031741 ***
    ## sanctionsl1  0.257002  0.123363  4.340      0.037224 *  
    ## forcel1     -0.279045  0.131593  4.497      0.033963 *  
    ## growthpc    -1.169807  0.590193  3.929      0.047471 *  
    ## lngdppc      0.015275  0.063772  0.057      0.810698    
    ## democl1      0.467304  0.276231  2.862      0.090700 .  
    ## democlnt     0.519703  0.138088 14.164      0.000168 ***
    ## mixedl1      0.556254  0.194161  8.208      0.004171 ** 
    ## mixedlnt     0.191248  0.108929  3.082      0.079139 .  
    ## age          0.019605  0.004953 15.670 0.00007541203 ***
    ## ot3         -0.904662  0.154907 34.106 0.00000000522 ***
    ## X_spline1   -0.161972  0.030597 28.023 0.00000011989 ***
    ## X_spline2    0.060692  0.012121 25.071 0.00000055269 ***
    ## X_spline3   -0.011863  0.002818 17.727 0.00002550119 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Estimated Scale Parameters:
    ##             Estimate Std.err
    ## (Intercept)    1.009  0.1699
    ## 
    ## Correlation: Structure = exchangeable  Link = identity 
    ## 
    ## Estimated Correlation Parameters:
    ##       Estimate Std.err
    ## alpha   0.0617 0.02665
    ## Number of clusters:   163   Maximum cluster size: 50

``` r
fit_gee_ar = geeglm(fail~sanctionsl1+forcel1+growthpc+lngdppc+democl1+democlnt+mixedl1+mixedlnt+age+ot3+X_spline1+X_spline2+X_spline3,
                  data=df,id=as.factor(country),family=binomial,corstr="ar1")

summary(fit_gee_ar)
```

    ## 
    ## Call:
    ## geeglm(formula = fail ~ sanctionsl1 + forcel1 + growthpc + lngdppc + 
    ##     democl1 + democlnt + mixedl1 + mixedlnt + age + ot3 + X_spline1 + 
    ##     X_spline2 + X_spline3, family = binomial, data = df, id = as.factor(country), 
    ##     corstr = "ar1")
    ## 
    ##  Coefficients:
    ##             Estimate  Std.err  Wald          Pr(>|W|)    
    ## (Intercept) -2.02252  0.41960 23.23 0.000001434623737 ***
    ## sanctionsl1  0.14822  0.11854  1.56            0.2112    
    ## forcel1     -0.30905  0.12078  6.55            0.0105 *  
    ## growthpc    -1.11716  0.55498  4.05            0.0441 *  
    ## lngdppc      0.00995  0.05240  0.04            0.8494    
    ## democl1      0.59641  0.23560  6.41            0.0114 *  
    ## democlnt     0.35466  0.15648  5.14            0.0234 *  
    ## mixedl1      0.52180  0.16683  9.78            0.0018 ** 
    ## mixedlnt     0.17342  0.10404  2.78            0.0956 .  
    ## age          0.01647  0.00400 16.93 0.000038874243577 ***
    ## ot3         -1.29091  0.16652 60.09 0.000000000000009 ***
    ## X_spline1   -0.21312  0.03331 40.94 0.000000000156660 ***
    ## X_spline2    0.07694  0.01298 35.16 0.000000003041790 ***
    ## X_spline3   -0.01392  0.00293 22.64 0.000001956554767 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Estimated Scale Parameters:
    ##             Estimate Std.err
    ## (Intercept)     1.06   0.215
    ## 
    ## Correlation: Structure = ar1  Link = identity 
    ## 
    ## Estimated Correlation Parameters:
    ##       Estimate Std.err
    ## alpha  -0.0705  0.0272
    ## Number of clusters:   163   Maximum cluster size: 50

An attempt was made to fit an unstructured correlation matrix as well, but it failed to converge within a reasonable timeframe. Furthermore, there seems to be a bug in the output of summary.geeglm: the link function is reported as identity, even though the object itself reports a logit-link when you choose the binomial family.

First, briefly on the estimated correlation parameters: - the exchangable correlation is estimated at 0.06 between any two timepoints. So controlled for the effect of the independent variable, there is still a postive assosiation between leadership change in any two years. - The ar(1)-correlation is estimated to be -0.07. This is slightly interesting one: the ar(1)-correlation-matrix is one where the correlation is -0.07 between t=1 and t=2, before it then goes to close-to-zero between t=2 and t=3. So controlled for the effect of the independent variables, a leadership change in one year negatively affects leadership change in the next year.

So the estimated correlations are pretty low (approx of the same size), but one is positive and the other negative. One could probably make reasonable story-time arguments for both: A leadership change in one year could be associated with a higher probability of leadership change in other years. But a leadership change in the preceding year might make a leadership change directly the next year marginally less likely.

Also, have in mind that the model already include quite a few interaction terms with time, cubic splines and some other variables that varies over time.

    ## Joining, by = "term"

| term        |  estimate\_ar|  estimate\_exchangable|  estimate\_original|  std.error\_ar|  std.error\_exchangable|  std.error\_original|
|:------------|-------------:|----------------------:|-------------------:|--------------:|-----------------------:|--------------------:|
| (Intercept) |         -2.02|                  -2.81|                  NA|           0.42|                    0.55|                   NA|
| age         |          0.02|                   0.02|                0.02|           0.00|                    0.00|                 0.01|
| democl1     |          0.60|                   0.47|                0.19|           0.24|                    0.28|                 0.22|
| democlnt    |          0.35|                   0.52|                0.77|           0.16|                    0.14|                 0.12|
| forcel1     |         -0.31|                  -0.28|               -0.27|           0.12|                    0.13|                 0.14|
| growthpc    |         -1.12|                  -1.17|               -1.54|           0.55|                    0.59|                 0.69|
| lngdppc     |          0.01|                   0.02|               -0.10|           0.05|                    0.06|                 0.12|
| mixedl1     |          0.52|                   0.56|                0.42|           0.17|                    0.19|                 0.19|
| mixedlnt    |          0.17|                   0.19|                0.39|           0.10|                    0.11|                 0.11|
| ot3         |         -1.29|                  -0.90|               -0.99|           0.17|                    0.15|                 0.12|
| sanctionsl1 |          0.15|                   0.26|                0.29|           0.12|                    0.12|                 0.14|
| X\_spline1  |         -0.21|                  -0.16|               -0.18|           0.03|                    0.03|                 0.02|
| X\_spline2  |          0.08|                   0.06|                0.07|           0.01|                    0.01|                 0.01|
| X\_spline3  |         -0.01|                  -0.01|               -0.01|           0.00|                    0.00|                 0.00|

The estimates from the replication is included for reference. Mind that they are not comparable - the GEE-estimates are unconditional population averages, while the FE-estimates are conditional on modelled unit effects.

As we can see from the model estimates, the choice of correlation structure seems to matter slightly for the sizes of the coefficient estimates. Sanctions are, on average, no longer significant if we assume an autoreggressive correlation structure. Years in office has a larger negative effect on leadership change in the AR(1)-case. Sanctions are still significant if we assume an exchangable correlation structure.

### What to choose?

GEE does not model unit effects, it more or less sees them as nuissance that has to be handled somehow. That imples that if your object of study is related to unit effects in some way, you are better off with a unit-effects approach.

The estimates are consistent - that is, asymptotically unbiased when N grows very large. Our N is 163, with an average of 35 observations per N. That is not very large. Furthermore, it is sort of hard to imagine the actual N we are interested in growing much larger: this is after all the countries of the world.
