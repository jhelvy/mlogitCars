# mlogitCars

This set of files contains a comprehensive example of estimating multinomial logit models in `R` using the `mlogit` package.

Simulated choice data are used so that the true model parameters are known. The simulated data are stored in the `./data` folder. The following list provides a short description of the data files:

|    File       |    Description    |
|:-----------------|:------------------|
|`data_mnl.csv` | Simulated choice data using a multinomial logit model.|
|`data_outsideGood.csv` | Similar data to the `data_mnl.csv` file, except this dataset include an "outside good" alternative (i.e. a "None" option in a conjoint survey).|
|`sensitivityCases.csv` | Cases used in the sensitivity analyses (see the 7.3 and 7.4 examples in the `./code` folder).|

The `.R` files in the `./code` folder illustrate different examples of models, plots, and analyses. The following list provides a short description of each file:

|    File       |    Description    |
|:-----------------|:------------------|
|`0-simulateData.R`       | Simulate the choice data.|
|`1.1-loadTools.R`        | Load functions & libraries.|
|`1.2-exploreData.R`      | Explore the data with summaries and plots.|
|`2.1-partworth_model.R`  | Estimate a logit model with partworth parameters using `mlogit`.|
|`2.2-partworth_plots.R`  | Plot the results of the partworth model using `ggplot2`.|
|`3.1-linear_model.R`     | Estimate a logit model with linear parameters using `mlogit`.||
|`3.2-linear_plots.R`     | Plot the results of the linear model using `ggplot2`.|
|`4.1-outsideGood_model.R`| Estimate a logit model with an outside good using `mlogit`.||
|`4.2-outsideGood_plots.R`| Plot the results of the outside good model using `ggplot2`.|
|`5-uncertainty.R`        | Use the `MASS` library to take multivariate normal draws of the linear model coefficients and generate a 95% confidence interval of the coefficients.|
|`5.2-uncertainty_plots.R`| Plot the coefficients (with uncertainty) using `ggplot2`.|
|`6.1-wtp.R`                | Compute the willingness to pay from the linear model (both point estimates and a 95% confidence interval using simulation).|
|`6.2-wtp_plots.R`| Plot the WTP results using `ggplot2`.|
|`6.3-wtp_logitr.R`| Directly estimate WTP using the [`logitr`](https://github.com/jhelvy/logitr) package.|
|`7.1-market_simulation.R`  | Compute the expected market shares for a set of alternatives using coefficients from the linear model (both point estimates and 95% confidence intervals of the shares using simulation).|
|`7.2-marketSimulation_plots.R`| Plot the market simulation results using `ggplot2`.|
|`8.1-marketSensitivity.R`| Conduct a sensitivity analysis of the market shares to changes in attribute values.|
|`8.2-marketSensitivity_plots.R`| Plot results of the sensitivity analysis, including a 'tornado plot', using `ggplot2`.|

# Author, Version, and License Information
- Author: *John Paul Helveston* (www.jhelvy.com)
- Date First Written: *Thursday, December 27, 2018*
- License: GPL-3
