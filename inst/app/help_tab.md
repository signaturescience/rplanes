# How to use the app

## Data

The **observed data** uploaded must be in a .csv format.

The columns contain:

-   date: Is the date of the reported value in date class format.

-   The epiyear and epiweek is optional but can be obtained from the
    date column using `lubridate::epiyear()` for the epiyear and
    `MMWRweek::MMWRweek()` for the epiweek.

-   location: Can be any type like geographic unit such as [FIPS
    code](https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm)
    as in the example below, but must match the location type in the
    forecast dataset.

-   An outcome column, in this example is called flu.admits,
    representing hospital flu admissions.

| date       | epiyear | epiweek | location | flu.admits |
|:-----------|:--------|:--------|:---------|:-----------|
| 2023-02-04 | 2023    | 5       | 39       | 63         |
| 2022-08-27 | 2022    | 34      | 48       | 111        |
| 2022-04-09 | 2022    | 14      | 09       | 44         |
| 2022-06-25 | 2022    | 25      | 28       | 19         |
| 2022-03-26 | 2022    | 12      | 28       | 38         |

The **forecast data** must also be a .csv file with the following
columns labeled explicitly as:

-   forecast_date: The date the forecast was generated.

-   target: The horizon for the forecast, in the example it is a weekly
    horizon with a number for 1, 2, 3 and 4 weeks ahead.

-   target_end_date: The date of the expected forecast horizon in date
    class format.

-   location: Can be any type like geographic unit such as [FIPS
    code](https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm),
    or any other but must match the type in the observed dataset.

-   type: The type of predicted value as a point value or quantile
    value. Can contain quantile only but not point only.

-   quantile: The quantile in numeric format.

-   value: The predicted outcome value for the forecast in numeric
    format.

| forecast_date | target                  | target_end_date | location | type     | quantile | value |
|:-----------|:------------------|:------------|:-------|:-------|:-------|:-----|
| 2023-02-06    | 2 wk ahead inc flu hosp | 2023-02-18      | 45       | quantile | 0.40     | 31    |
| 2023-02-06    | 3 wk ahead inc flu hosp | 2023-02-25      | 08       | quantile | 0.15     | 0     |
| 2023-02-06    | 2 wk ahead inc flu hosp | 2023-02-18      | 40       | quantile | 0.75     | 126   |
| 2023-02-06    | 4 wk ahead inc flu hosp | 2023-03-04      | 44       | quantile | 0.45     | 0     |
| 2023-02-06    | 2 wk ahead inc flu hosp | 2023-02-18      | 48       | quantile | 0.60     | 311   |
| 2023-02-06    | 4 wk ahead inc flu hosp | 2023-03-04      | 36       | quantile | 0.80     | 177   |
| 2023-02-06    | 3 wk ahead inc flu hosp | 2023-02-25      | 35       | quantile | 0.95     | 86    |
| 2023-02-06    | 2 wk ahead inc flu hosp | 2023-02-18      | 06       | quantile | 0.95     | 444   |
| 2023-02-06    | 2 wk ahead inc flu hosp | 2023-02-18      | 25       | quantile | 0.75     | 82    |
| 2023-02-06    | 4 wk ahead inc flu hosp | 2023-03-04      | 21       | point    | NA       | 0     |
