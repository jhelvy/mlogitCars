# mlogitCars

This set of files contains a comprehensive example of estimating discrete choice models in `R` using the `mlogit` package. Simulated choice data (see the `0-simulateData.R` file) are used so that the true model parameters are known. The simulated data are stored in the `./data` folder.

The `.R` files in this repository illustrate different examples. The following list provides a short description of each:

|    File       |    Description    |
|:--------------|:------------------|
|`0-simulateData.R`       | Simulate the choice data.|
|`1-loadTools.R`          | Load functions & libraries.|
|`2.1-partworth_model.R`  | Estimate a logit model with partworth parameters using `mlogit`.|
|`2.2-partworth_plots.R`  | Plot the results of the partworth model using `ggplot2`.|
|`3.1-linear_model.R`     | Estimate a logit model with linear parameters using `mlogit`.||
|`3.2-linear_plots.R`     | Plot the results of the linear model using `ggplot2`.|
|`4.1-outsideGood_model.R`| Estimate a logit model with an outside good using `mlogit`.||
|`4.2-outsideGood_plots.R`| Plot the results of the outside good model using `ggplot2`.|
|`5-uncertainty.R`        | Use the `MASS` library to take multivariate normal draws of the linear model coefficients and generate a 95% confidence interval of the coefficients.|
|`6-wtp.R`                | Compute the willingness to pay from the linear model (both point estimates and a 95% confidence interval using simulation).|
|`7-market_simulation.R`  | Compute the expected market shares for a set of alternatives using coefficients from the linear model (both point estimates and 95% confidence intervals of the shares using simulation).|

# Author, Version, and License Information
- Author: *John Paul Helveston* (www.jhelvy.com)
- Date First Written: *Thursday, December 27, 2018*
- License: GPL-3
