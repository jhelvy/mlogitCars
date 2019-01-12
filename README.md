# mlogitCars

This set of files contains a comprehensive example of estimating discrete choice models in R using the `mlogit` package. Rather than use actual choice data for this example, I simulate choice data (see the `0-simulateData.R` file) so that the true model parameters are known. The simulated data are stored in the '/data' folder.

The `.R` files in this repository illustrate different examples. The following list provides a short description of each:

|    File    |    Description    |
|:-----------|:------------------|
|`0-simulateData.R`       | Code to simulate the choice data.|
|`1-loadTools.R`          | Various functions used throughout example.|
|`2.1-partworth_model.R`  | Estimating a full partworth model using `mlogit`.|
|`2.2-partworth_plots.R`  | Plotting the results of the partworth model using the `ggplot2` library.|
|`3.1-linear_model.R`     | Estimating a full model with linear parameters using the `mlogit` library.||
|`3.2-linear_plots.R`     | Plotting the results of the linear model using the `ggplot2` library.|
|`4.1-outsideGood_model.R`| Estimating a model with an outside good using the `mlogit` library.||
|`4.2-outsideGood_plots.R`| Plotting the results of the outside good model using the `ggplot2` library.|
|`5-uncertainty.R`        | Using multivariate normal draws of the linear model coefficients to generate a 95% confidence interval of the coefficients using the `MASS` library.|
|`6-wtp.R`                | Computing the willingness to pay from the linear model (both point estimates and 95% confidence intervals).|
|`7-market_simulation.R`  | Computing the expected market shares for a set of alternatives using coefficients from the linear model (both point estimates and 95% confidence intervals of the shares).|

# Author, Version, and License Information
- Author: *John Paul Helveston* (www.jhelvy.com)
- Date First Written: *Thursday, December 27, 2018*
- Most Recent Update: *Thursday, December 27, 2018*
- License: GPL-3
