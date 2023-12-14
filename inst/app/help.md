# How to use the `rplanes` Explorer

- [Introduction](#introduction)
- [Example Data](#example)
- [Analysis Steps](#analysis-steps)
- [Inputs](#inputs)
- [Outputs](#outputs)
- [About](#about)

----------------------------------------

<a name="introduction"></a>
## Introduction

The `rplanes` Explorer is written as a Shiny web application to translate the R package API to point-and-click features. The app includes functionality to intuitively run plausibility analysis and view output. The processing depends on a combination of observed data uploaded and used as a "seed" for baseline characteristics along with designated data to evaluate. As with the `rplanes` R package, the app can handle varying geographic and temporal resolutions (i.e., daily, weekly, or monthly reporting). 

<a name="example"></a>
## Example Data

To demonstrate usage, the app features an example data set. Users can select the "Example" option to load pre-populated forecast data for plausibility analysis. This data set contains 4 week-ahead forecasts for incident flu hospitalizations in select United States locations. The forecasts begin with the week ending 2022-11-05 and extend through the week of 2022-11-26. The baseline data used to generate the seed is loaded from [HHS Protect flu hospitalizations](https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh) that have been aggregated from daily to weekly reports at the state and national level. All of the data preparation is done internally. Users simply click "Analyze" to explore the kinds of outputs that `rplanes` generates.

<a name="analysis-steps"></a>
## Analysis Steps

The application allows users to run plausibility analysis with several steps:

1. Select the type of signal to be evaluated
2. Upload data to use for the plausibility analysis seed
3. Upload data containing the signal to be evaluated (or for an observed signal identify the number of points to evaluate)
4. Enter the resolution, outcome, and forecast horizon (if applicable)
5. Optionally modify default parameters used for analysis
6. Click "Analyze"

<a name="inputs"></a>
## Inputs

The steps above require that the user specify several inputs, each of which are described in detail below.

### Type of Signal Evaluated

The `rplanes` package implements a plausibility analysis algorithm that can work on either observed or forecasted data signals. Users begin by entering the type of signal as "Forecast" or "Observed".

It is important to note that depending on the type of signal to be evaluated, some components may not apply.

### Observed Data

The observed data uploaded is primarily used to seed the background characteristics used in plausibility analysis. The app internally finds the appropriate date for a cutoff to identify baseline features of the reported data. However, if a forecast is being evaluated then the uploaded data *cannot* have any gaps between the last report and the first horizon forecasted. 

Data must be uploaded to the app in `.csv` format. At minimum it must include columns for location (geographic unit such as FIPS code) and date (date of reported value in `yyyy-mm-dd` format). Note that these columns must be named as "location" and "date" respectively. The observed data must also include a column that contains the outcome (e.g., case count). The name of this column is arbitrary so long as it matches the outcome name provided in the app input for "Outcome" (see below). The uploaded `.csv` file may contain other columns, however these will not be used in plausibility analysis.

### Data to be Evaluated

The choice of the type of signal to evaluate will determine how the user specifies data to be evaluated.

If a forecast signal is selected, then the user must upload a `.csv` file containing forecast data. Forecasts must be a prepared in a "quantile" format with at minimum the following columns:

- *forecast_date*: The date on which the forecast was generated (`yyyy-mm-dd` format)
- *location*: Location code for the given forecast
- *target*: Name of the forecast structured as "N wk ahead {forecasted outcome}" (e.g., "4 wk ahead inc flu hospitalizations")
- *target_end_date*: The date corresponding to the forecasted target (`yyyy-mm-dd` format)
- *type*: The type of forecast (either "point" or "quantile")
- *quantile*: The quantile for the forecasted value; if the type is "point" then quantile is `NA`
- *value*: The forecasted value for the given quantile, location, and target

If an observed signal is selected, then the user can select the number of most recent observations to evaluate. The number of values will determine the cutoff date to identify the baseline characteristics in the original uploaded observed data. In other words, there is no need to upload separate observed data to be evaluated since the initial upload will contain all data for seed *and* evaluation.

### Resolution

The app can accommodate data reported or forecasted at daily, weekly, or monthly cadence. The user selects the appropriate resolution to match the observed data and the data to be evaluated.

### Outcome

The user must enter the name of the outcome. For observed data, the outcome entry should match the name of the column that contains the signal in the uploaded `.csv` file.

### Forecast Horizon

For forecast evaluations, the user will enter the horizon as a number. The app defaults to `4` for this input.

### Modify Defaults

Users can optionally modify the following parameters:

- **Prediction Interval**: The prediction interval defines the space between upper and lower bounds and internally maps to the appropriate quantiles (centered on the median) in the forecast evaluated.
- **PLANES Components**: By default the app will run all components available for the given signal. As noted elsewhere in the `rplanes` documentation, not all components are available for evaluating observed signals. The user can optionally select specific components to use in the analysis.
- **Significance (Trend)**: The significance level to identify change points via the trend component. Default is `0.1`.
- **Tolerance (Repeat)**: The number of tolerated repeats before flagging via the repeat component. Default is defined by the number of repeats observed for the given location in the seed.
- **Prepend Values (Repeat)**: The number of values to prepend to the evaluated signal from the seed during analysis with the repeat component. The default behavior is to use the maximum number of repeats observed for the given location in the seed.

<a name="outputs"></a>
## Outputs

The app includes output to view plausibility scoring results and the raw data used for analysis.

### Scoring

The plausibility scoring results are presented in "Overall" and "Individual Locations and Components" sections. The overall scores (i.e., all combinations of locations and components analyzed) are displayed in a tile plot and as a table with all scores. Users can download or copy the table contents. Additionally, for each location the user can view plots of individual components, each which shows the features that did or did not raise flags in scoring.

### Raw Data

The raw data is also displayed for the users in two tables. The first shows the observed data used to seed background characteristics, and the second is a table with data to evaluated.

<a name="about"></a>
## About

The app is developed and maintained as part of the `rplanes` package, which is licensed under MIT License and Copyright (c) 2023 Signature Science LLC.

Primary developers of the tool are VP Nagraj, Desiree Williams, and Amy Benefield.
