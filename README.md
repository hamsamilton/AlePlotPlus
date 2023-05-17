ALEPlotPlus

The Accumulated Local Effect (ALE) Plots are a function decomposition method that allow researchers to interrogate complex machine learning functions by estimating non-linear functions that describe the effects of each predictor on the prediction. To learn more about ALEPlots, please read the following resources.

This package is designed to improve on and extend the ALEPlot package, in the following ways.

The visualizations produced by the original ALEPlot package have been improved. A key weakness of both the original 1D and 2D visualizations is the lack of information on where the tra are. This is important because estimations in sparse areas of the model are less accurate, and thus may be misleading. In addition the color scale of the 2D ALEPlots is confusing. Replacing it with a divergent color scale dramatically improves the readability of the plot.

Second, using the effects estimated by the ALEPlot package, the ALEPlotPlus package aggregates this information into a variable importance score which represents the total absolute estimated effect of each variable on the predicted outcome. 

The key contribution of this approach is it enables estimation of the effect sizes of interactions within ML models. Currently, there are few computationally efficient way to estimate variable interactions of ML models. By leveraging the information produced by 2D ALE Plots, which provide estimates of interaction effects in a computationally scalable way we can generate a variable importance score that represents an interaction that fills an unfilled niche in the field. 

An additional advantage is this approach is model independent, meaning it provides a universal way to compare variable importance scores across models.

This approach has some notable and important limitations stemming from the limitations of both ALEPlots and function decomposition generally.

1) Strongly correlated predictors will result in inaccurate effect size estimations.

2) The estimated importance of main effects and interactions are conflated to some extent. IE strong main effects will show stronger interactions.

3) As stated earlier, estimations in sparse areas of the data manifold may have inaccurate estimates, which will lead to misleading variable importance. 

This suggests the following Do/Don't list when using this software.

DO:
	- Get a general idea of what interactions are most important within a model
	- Double check important variables using visualizations
	- Interpret the strength of interactions in the context of their main effects

DONT
	- Interpret the variable importance scores as absolute fact
	- Think the produced values are backed by statistical significance.

