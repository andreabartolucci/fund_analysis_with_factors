# fund_analysis_with_factors
Financial econometrics time-series analysis of a fund using equity factors

This R script is a detailed analysis of financial data using various statistical techniques and tests. The script begins by installing and loading necessary packages, such as `aod`, `corrplot`, `ggplot2`, `here`, `lmtest`, `olsrr`, `psych`, `stargazer`, `tidyverse`, `vtable`, and `jtools`. These packages provide the necessary tools for data manipulation, visualization, and statistical analysis.

The data is then loaded using `read.csv` function and is plotted using different visualization techniques, such as line plots, histograms, and scatterplots. Summary statistics and correlation matrices are also calculated, which provide insights into the data distribution and correlation between variables.

The script then moves on to linear regression models, starting with a FF5+Mom model where the analysis is performed by using the `lm` function. The script then performs various diagnostic tests such as multicollinearity, omitted variables, autocorrelation, and heteroscedasticity using `ols_vif_tol`, `resettest`, `acf`, `dwtest`, and `bptest` functions, respectively. These diagnostic tests help to identify potential issues with the model and provide insights into the model's performance.

The script then moves on to non-linear regression models with polynomial variables, and similar diagnostic tests are performed for these models as well. The script also performs hypothesis testing using a dummy variable for Covid-19 and a Wald test is used to test whether betas and alpha have changed during and after Covid-19. Finally, the script includes interactions between the factors and the market excess returns, and a Wald test is used to test whether the effect of the factors is mediated by the market excess returns.

Overall, this R script represents a comprehensive analysis of financial data using various statistical techniques and tests, providing valuable insights into the data distribution, correlation, and potential issues with the model.
