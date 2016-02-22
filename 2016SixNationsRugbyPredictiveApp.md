<style>
.footer {
    color: black;
    background: #E8E8E8;
    position: fixed;
    top: 90%;
    text-align:center;
    width:100%;
}
.small-code pre code {
  font-size: 1em;
}
</style>

2016 Six Nations Rugby Championship Predictive Shiny Application
========================================================
author: Sébastien Durand
date: 22/02/2016

Problem definition
========================================================
<small>
I am a Rugby fan and enjoy applying analytics to various sports.  

The [2016 six nations rugby championship](https://en.wikipedia.org/wiki/2016_Six_Nations_Championship) has recently started and I wanted to create a predictive application to give some basic insights on match results based on the 2015 championship results.

__The problem to solve:__  
Can we predict the match outcomes of the 2016 Championship using a combination of the results of the past 2015 championship and the on-going 2016 championship results?
</small>


Training Data and Predictive Model
========================================================
class: small-code
<small>
For my shiny application, I made the 'naive' assumption that the Rugby scores were Poisson distributed and applied a simple Generalized Linear Model with Poisson-log link function similarly to the one used by  Martin Eastwood(see [Predicting Football Using R](http://pena.lt/y/2014/11/02/predicting-football-using-r/)).
</small>



<small>The training dataset contains the following column variables:</small>

```r
head(training,3)
```

```
     Team Opponent Score Home Result
1   Wales    Italy    23    1      W
2  France  England    26    1      W
3 Ireland Scotland    28    1      W
```

<small>And the prediction model is trained as follows:</small>

```r
model <- glm(Score ~ Home + Team + Opponent, data=training, family=poisson(link="log"))
```


Example of a Fixture Prediction: 
========================================================
class: small-code
<small>The code below illustrates how a fixture prediction is performed using the fitted glm model (e.g. 'England - Ireland' ):</small>

```r
## Home scoring rate prediction
lambda <- predict(model, data.frame(Home=1, Team="England", Opponent="Ireland"), type="response")

## Away Team scoring rate prediction
mu <- predict(model, data.frame(Home=0, Team="Ireland", Opponent="England"), type="response")

## Joint-Distribution table of Home scores Vs Away scores  - could also have used mean(rpois(1000,lambda)>rpois(1000,mu)) instead.
maxScore <- 65
prob_matrix <- dpois(0:maxScore, lambda) %*% t(dpois(0:maxScore, mu))

## W/D/L Probability calculation
WinPr <- sum(prob_matrix[lower.tri(prob_matrix)])
DrawPr <- sum(diag(prob_matrix))
LosePr <- sum(prob_matrix[upper.tri(prob_matrix)])

cat(sprintf("Lamdba=%.2f, Mu=%.2f\nEngland to Win=%.2f, to Draw=%.2f, to Lose=%.2f against Ireland",lambda,mu,WinPr,DrawPr,LosePr))
```

```
Lamdba=17.43, Mu=13.52
England to Win=0.73, to Draw=0.06, to Lose=0.21 against Ireland
```



Results and Conclusions
========================================================
<small>
<small>We are now two weeks into the 2016 championship, the [2016 Six Nations Rugby Predictive Shiny Application](https://sebastid.shinyapps.io/2016SixNationsRugbyPredApp/) correctly predicted 4 match outcomes out of 6 played matches. My initial conclusions are:  
- The estimated probabilities and decimal odd values seem too extreme and should not be trusted entirely (i.e. for value betting). 
- On the other hand, the application predictions look good enough to provide some insights for the remaining matches to play and to provide a quick way to compare the relative strengths of the playing Rugby teams (.i.e. Offensive/Defensive strength chart).</small>  

__To try next:__
- <small>Decompose the score variable into tries and penalty covariates (likely to be more like Poisson random variables : E(X) ~ Var(X)).</small>
- <small>To use more data and cross-validation to minimize overfitting.</small>
- <small>Try alternative model algorithms (e.g. hierarchical MCMC Bayesian Regression models to use priors).</small>
</small>



