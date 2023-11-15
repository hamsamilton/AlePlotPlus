# ALEPlotPlus

Accumulated Local Effect (ALE) Plots are a function decomposition method that allows researchers to interrogate complex machine learning functions by estimating non-linear functions that describe the effects of each predictor on the prediction. 

This package is designed to improve on and extend the ALEPlot package in the following ways:

## Improvements

1. **Improved Visualizations**: The visualizations produced by the original ALEPlot package have been improved. This pacakge now marks where the data used to generate each plot lie along the data manifold.This is important because estimations in sparse areas of the data manifold are less accurate and may be misleading or confusing. Additionally, the color scale of original ALEPlots has been replaced with a divergent color scale dramatically improves the readability of the plot.

2. **Variable Importance Score**: The ALEPlotPlus package can aggregate the estimates of the effect of each variable on prediction at each point into a variable importance score which represents the total absolute estimated effect of each variable on the predicted outcome. This can be used to assess the relative feature importance of each variable or variable interation on prediction.

## Contribution of variable importance scores derived from ALE plots

The significance of developing a variable importance score based on ALE plots is it provides a computationally scalable, model independent method to estimate variable interactions where there were previously very limited options.

In addition, this variable importance score is model independent, which means it provides a method to compare variable importance scores across models.

## Limitations

This approach has some notable and important limitations, stemming from the limitations of both ALEPlots and function decomposition in general:

1. Strongly correlated predictors will result in inaccurate effect size estimations.
2. The estimated importance of main effects and interactions are conflated to some extent. In other words, strong main effects will show stronger interactions.
3. As stated earlier, estimations in sparse areas of the data manifold may have inaccurate estimates, which will lead to misleading variable importance scores. 

## Usage Recommendations

**DO**:
- Try to fit models without strongly correlated predictors. Otherwise exercise caution while interpreting correlated variables.
- Use this tool a general idea of what interactions are most important within a model.
- Double-check important interactions using accompanying visualizations.
- Interpret the strength of interactions in the context of their main effects.
- Validate findings using alternative approaches when possible.

## Read About ALE Plots

Read additional information about ALE Plots using the following resources

https://christophm.github.io/interpretable-ml-book/ale.html 
https://arxiv.org/abs/1612.08468


