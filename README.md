# gam
Group project exploring generalized additive models using the Wage dataset in R. GAMs are a non-parametric extension of generalized linear models that use smooth functions of the predictor variables to better model the relationship with a response variable.
In addition to learning about this extension of GLMs, I was able to learn a lot about ggplot while working on this project.

GAMs are a powerful tool, but can easily overfit the data if an improper smoothing parameter or number of basis functions is used:

<img src="https://github.com/ColeConte/gam/blob/master/SmoothingParameter.png"/>
<img src="https://github.com/ColeConte/gam/blob/master/BasisFunctions.png"/>

Here's a nice visual of how all of the basis functions come together to form one curve:

<img src="https://github.com/ColeConte/gam/blob/master/BasisFunctionBreakdown.png"/>
