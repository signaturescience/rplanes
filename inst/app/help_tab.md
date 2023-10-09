# How to use the app

## Introduction

This app helps to facilitate the use of the R package
[PLANES](https://signaturescience.github.io/rplanes/) (Plausibility
Analysis of Epidemiological Signals). This open-source package contains
tools/components to assess the uncertainty of near-term forecasts. Some
infectious disease forecasting signals may exhibit implausible values.
Our aim is to develop a set of tools to pick up such occurrences and
provide a scoring mechanism to flag any such values for further
evaluation.

The accepted forecasts can be daily, weekly or monthly forecasts. There
are two inputs that can be supplied, first is the observed data and
second a comparison. This comparison is either a forecast or another
observed data set containing signals whose dates do not overlap with the
first data set.

## Data

The format of these two uploads is outlined below. If both uploads are
observed data they must be formatted according to the observed data
format below.

The **observed data** uploaded must be in a .csv format.

The columns contain:

- date: Is the date of the reported value in date class format.

- The epiyear and epiweek is optional but can be obtained from the date
  column using lubridate::epiyear() for the epiyear and
  MMWRweek::MMWRweek() for the epiweek.

- location: Can be any type like geographic unit such as [FIPS
  code](https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm)
  as in the example below, but must match the location type in the
  forecast dataset.

- An outcome column, in this example is called flu.admits, representing
  hospital flu admissions.

| date       | epiyear | epiweek | location | flu.admits |
|:-----------|:-------:|:-------:|:--------:|:----------:|
| 2022-07-30 |  2022   |   30    |    06    |     46     |
| 2023-05-27 |  2023   |   21    |    17    |     24     |
| 2023-04-15 |  2023   |   15    |    32    |     4      |
| 2022-02-19 |  2022   |    7    |    22    |     40     |
| 2023-02-04 |  2023   |    5    |    45    |     42     |

The **forecast data** must also be a .csv file with the following
columns labeled explicitly as:

- forecast_date: The date the forecast was generated.

- target: The horizon for the forecast, in the example it is a weekly
  horizon with a number for 1, 2, 3 and 4 weeks ahead.

- target_end_date: The date of the expected forecast horizon in date
  class format.

- location: Can be any type like geographic unit such as [FIPS
  code](https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm),
  or any other but must match the type in the observed dataset.

- type: The type of predicted value as a point value or quantile value.
  Can contain quantile only but not point only.

- quantile: The quantile in numeric format.

- value: The predicted outcome value for the forecast in numeric format.

| forecast_date |         target          | target_end_date | location |   type   | quantile | value |
|:--------------|:-----------------------:|:---------------:|:--------:|:--------:|:--------:|:-----:|
| 2023-02-06    | 2 wk ahead inc flu hosp |   2023-02-18    |    45    | quantile |   0.40   |  31   |
| 2023-02-06    | 3 wk ahead inc flu hosp |   2023-02-25    |    08    | quantile |   0.15   |   0   |
| 2023-02-06    | 2 wk ahead inc flu hosp |   2023-02-18    |    40    | quantile |   0.75   |  126  |
| 2023-02-06    | 4 wk ahead inc flu hosp |   2023-03-04    |    44    | quantile |   0.45   |   0   |
| 2023-02-06    | 2 wk ahead inc flu hosp |   2023-02-18    |    48    | quantile |   0.60   |  311  |
| 2023-02-06    | 4 wk ahead inc flu hosp |   2023-03-04    |    36    | quantile |   0.80   |  177  |
| 2023-02-06    | 3 wk ahead inc flu hosp |   2023-02-25    |    35    | quantile |   0.95   |  86   |
| 2023-02-06    | 2 wk ahead inc flu hosp |   2023-02-18    |    06    | quantile |   0.95   |  444  |
| 2023-02-06    | 2 wk ahead inc flu hosp |   2023-02-18    |    25    | quantile |   0.75   |  82   |
| 2023-02-06    | 4 wk ahead inc flu hosp |   2023-03-04    |    21    |  point   |    NA    |   0   |

## Analysis Steps

After the two files have been uploaded, by default, the comparison data
set is marked as a forecast. Deselect “Is the Comparison a Forecast” if
it is not.

The resolution, depending on the signal, must be indicated as Daily,
Weekly or Monthly.

The type of outcome is the type of signal, in the example it is
flu.admits. This field must match the outcome column of the first upload
(Observed Data).

If the comparison is a forecast, the horizon drop down will appear and
this is to indicate how far the signal is forecasted. The example data
contains 4 week ahead forecasts so the horizon is 4.

The default components are run, which is all of them, with their
corresponding default parameters. Toggle “Modify Defaults” to select
individual components or modify any of the component parameters.

Press “Analyze” which will bring you to the Plots tab displaying the
scoring table and options to plot the individual components.

## Components

The scoring component `Coverage` assessed by rplanes::plane_cover
evaluates whether the signal covers the last observed value. Currently
set to work on forecast data sets.

`Difference` assessed by rplanes::plane_diff, evaluates the
point-to-point difference and whether the comparison exceeds the maximum
difference in observed data. Works on observed or forecast data sets.

`Repeat` assessed by rplanes::plane_repeat evaluates whether consecutive
values in observations or forecasts are repeated a k number of times.
Additional arguments tolerance is the number (k) of allowed repeats, and
prepend indicates the number of values from the observed data set to be
evaluated against. Both arguments default to NULL and the values are
determined from the observed data.

`Taper` assessed by rplanes::plane_taper evaluates whether or not the
forecast signal interval tapers (i.e., decreases in width) as horizons
progress. Currently set to work on forecast data sets.

`Trend` assessed by rplanes::plane_trend identifies any change points in
forecast data or in the final observed data point. Currently set to work
on forecast data sets. The additional argument significance, is the
significance level at which to identify change points (between zero and
one); default is 0.1.

<u>When</u> the comparison is <u>not a forecast</u> only the difference
and repeat components are run.

## Scoring Table

The table displays all the components run on each location and any flags
that may have occurred by each component. Currently, we have implemented
five components and the score column reflects the number of components
over five.

------------------------------------------------------------------------

### Developed by

Pete Nagraj, Desiree Williams & Amy Benefield

Signature Science, LLC
