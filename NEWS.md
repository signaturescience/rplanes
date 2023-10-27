# rplanes 0.0.1

Alpha release for the `rplanes` package!

Several of the key package features include:

- `read_forecast()`: Data prep function to read in forecasts in quantile format used by forecasting hubs
- `to_signal()`: Constructor for the "signal" S3 class used in the package
- `plane_seed()`: Function to create the baseline characteristics from observed data for a given range of dates
- `plane_score()`: Wrapper to run plausibility components independently across all locations

As of this release, there are five plausibility components included `rplanes`. We will continue to develop the package to add components, update functionality, and address any issues as needed.

For more information on features and use-cases, refer to the "Basic Usage" vignette on the [package website](https://signaturescience.github.io/rplanes/articles/basic-usage.html) or in the R console: `vignette("basic-usage", package="rplanes")`
