# The {PThelper} Package

This package is designed to walk users through the various steps required to clean and process purchase task data. Designed with non-R users in mind, the package focuses on familiarizing users on both the raw and processed data.

The {PThelper} package offers 8 different functions to walk users through 3 different stages of purchase task processing: 

a) The pre-processing stage of the raw data;

b) The calculation of elasticity and derived indices stage via curve-fitting; and

c) The index-level variable management

This package makes use of the wonderful package for processing Behavioral Economic Data by Brent Kaplan (2018) called {[beezdemand](https://github.com/brentkaplan/beezdemand)}. It also makes use of several other packages, including {ggplot2} for all visualization purposes.

## Introduction to Purchase Tasks

These demand instruments are used to measure the reinforcer pathology - the extent to which a value for a commodity is effected by increased cost. Greater demand (i.e. little sensitivity to price changes) is often associated with substance-related problems and use disorders (see Bickel et al., 2011). There 5 most commonly used indices generated from the purchase task are:

Breakpoint - The first increment of cost with zero consumption

Intensity - Consumption at the first price point

Omax - The maximum expenditure

Pmax - The price associated with the maximum total expenditure

Elasticity - Measures sensitivity of consumption to increases in cost

## a) Pre-Processing

Purchase task processing requires preparation, and using the `price_prep()`, `pt_prep()`, and `pt_qc()` functions will ensure the proper cleaning of the data prior calculating the purchase task indices.

The `plot_summary()` function can also be used at the end of the pre-processing stage to visualize the consumption and expenditure across the prices of the purchase task, prior to any outlier management.

The `winsor_price()` function is an optional final pre-processing step which offers different winsorization techniques for the price-level data prior to curve-fitting and other purchase task processing.

#### i) Prices as Variable Names:

To process purchase-task data, the variables need to be renamed as the prices they represent. The `price_prep()` function helps users do this within R easily by naming the variables and the equivalent prices.

#### ii) Zero-Values on Prices Once Zero-Consumption is Reached:

The `pt_prep()` function assigns zero values to items not administered. Since purchase task data is usually only administered until a consumption of zero is reached (often within an array of prices), these missing items need to be assigned zero values for proper processing. The assignment of zero-values is only done for individuals whose final item is a zero-response. 

In the case that individuals contradict themselves after giving a response of zero, and give a non-zero response for the final price of the array, these individuals do not have their responses re-assigned as zeroes. This is because it cannot be assumed that all subsequent responses would also be zeroes.

#### iii) Missing Data Identification:

The `pt_prep()` function will also identify those with missing data by their unique identifier (ID), by printing out the IDs in the console.

#### iv) Assign Maximum Consumption Values (Optional):

The `pt_prep()` function also gives the user the option to define a maximum value for the purchase task, which re-code values exceeding the `max_val` to the maximum value.

#### v) Price-Level Winsorization (Optional):

The `winsor_price()` function is used as outlier management at the price level. Outlying values are identified by z-scores higher than the `z_val` or lower than the negative `z_val` as chosen by the user. There are 3 different winsorization types:

i) Option 1 replaces outliers with the maximum non-outlying price rounded up;

ii) Option 2 replaces outliers with a price 1 higher than highest (or 1 lower than the lowest) non-outlying value; and

iii) Option 3 replaces outliers with 1 value above the next highest non-outlying value to maintain order

## b) Calculating Elasticity and Derived Values:

To calculate elasticity, a non-linear curve is fit to the price-level data using the  {PThelper} function `elasticity_curve()`. This function will also provide diagnostic overview of the sample curve for the data, as well as individual curves if requested by the user.

#### i) Empircal Purchase Task Indices:

In this step of calculating derived values by fitting a non-linear curve to the data using the `elasticity_curve()` function, the empirical values for Intensity, Breakpoint, Omax, and Pmax are also processed and retained automatically in this stage.

#### ii) Fitting the Curve:

The fit of the elasticity curve by the `elasticity_curve()` function is done using the {beezdemand} package. Either the Hursh & Silberbeg (2008) or the Koffarnus et al. (2015) equation can be used to fit a curve to the data. By fitting this curve, derived Q0, Omax, and Pmax values are also be calculated. 

#### iii) Curve Visualization:

The overall sample curve is visualized, with the option to visualize each individual curve on the same plot (known as a spaghetti plot), identifying those with extreme sensitivity to price (i.e. high elasticity values > a z-score of 3).

## c) Final Processing of Purchase Task Indices:

Additional optional processing are available by using the `winsor_index()` and `plot_transform()` functions, which aide outlier management and management of distributional shape of the index-level purchase task variables. 

#### i) Index-Level Winsorization (Optional):

The `winsor_index()` function offers an additional step used to manage outlying data at the index-level. It offers the same 3 winsorization types as the price-level winsorization function:

i) Option 1 replaces outliers with the maximum non-outlying value rounded up;

ii) Option 2 replaces outliers with a value 1 higher than highest (or 1 lower than the lowest) non-outlying value; and

iii) Option 3 replaces outliers with 1 value above the next highest non-outlying value to maintain order

#### ii) Transformation of Purchase Task Variables (Optional): 

Optional transformation of index-level variables using the `plot_transform()` function, aides in diagnosing the best transformation to achieve an approximately normal distribution. This function uses the two most common transformations for purchase task indices, specifically log (log of base 10) and square root.

## References

Bickel, Warren K, David P Jarmolowicz, E Terry Mueller, and Kirstin M Gatchalian. (2011). “The Behavioral Economics and Neuroeconomics of Reinforcer Pathologies: Implications for Etiology and Treatment of Addiction.” Current Psychiatry Reports 13 (5): 406–15.

Hursh, S. R., & Silberberg, A. (2008). Economic demand and essential value. Psychological Review, 115 (1), 186-198. http://dx.doi.org/10.1037/0033-295X.115.1.186

Kaplan, Brent. (2018). Beezdemand: Behavioral Economic Easy Demand.

Koffarnus, M. N., Franck, C. T., Stein, J. S., & Bickel, W. K. (2015). A modified exponential behavioral economic demand model to better describe consumption data. Experimental and Clinical Psychopharmacology, 23 (6), 504-512. http://dx.doi.org/10.1037/pha0000045

Stein, J. S., Koffarnus, M. N., Snider, S. E., Quisenberry, A. J., & Bickel, W. K. (2015).
Identification and management of nonsystematic purchase task data: Toward best practice.
Experimental and clinical psychopharmacology, 23(5), 377.
