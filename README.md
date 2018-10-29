[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rte-antares-rpackage/unavailabilityProductionEntsoe?branch=master&svg=true)](https://ci.appveyor.com/project/rte-antares-rpackage/unavailabilityProductionEntsoe)
[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/unavailabilityProductionEntsoe.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/unavailabilityProductionEntsoe)

# The 'unavailabilityProductionEntsoe' R package

The `unavailabilityProductionEntsoe` package provides functions to download, analyze and aggregate indisponibility data fram ENTSOE platform.

## Installation

Todo!

```r
# Install dependencies

devtools::install_github("rte-antares-rpackage/unavailabilityProductionEntsoe")

```

## Prerequisites

To use this package you must create an account on ENTSOE platform, you will need an acces token. <br>
You can acces to ENTSOE API : https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html

## Data download : **getDataOutagesProduction**

#### Arguments :

  - **token** : ENTSOE token
  - **bz** : ENTSOE build zone use ENTSOE API
  - **docType** :  "A80" for generation unit and "A77" for produciton unit
  - **docStatus** : "A05" for active, "A09" for cancel
  - **start** : start date
  - **end** : end date
  - **fileToMerge** : CSV where data will be save
  - **by** :  load data by "day", "week", "month", "year"? Maximum of documents load by request is 200.


#### Exemple :

```r
library(unavailabilityProductionEntsoe)
token <- "My-token"

#NL
getDataOutagesProduction(token = token, bz = "10YNL----------L", docType = "A80",
   start = "2018-01-01", end = "2018-02-28", fileToMerge = "NLindispo.csv")
```
## Data visualization : **runIndispoApp**

After use of **getDataOutagesProduction**, you have save data in a CSV file. You can visualize data with :

```r
 runIndispoApp("NLindispo.csv")
```


## Make time series : **giveTS**

#### Arguments :

After use of **getDataOutagesProduction**, you have save data in a CSV file. You can create time series data with giveTS.

  - **path** : CSV file load with *getDataOutagesProduction**
  - **outFile** : new CSV file (output)
  - **minDate** :  Date for time series begin
  - **maxDate** :  Date for time series end
  - **overlappingvalues** : operation for overlap, can be "sum" or "min".

#### Exemple :

```r
  giveTS("NLindispo.csv", outFile = "NLindispoTS.csv")
```

## Aggregate time series : **aggregateIndispo**

#### Arguments :

  - **path** : CSV file product by giveTS
  - **aggregatedCol** :  column to aggregate, can be : "date", "hour", "month", "year", "productionType" can also be a vector (c("month", "year"))
  - **colDcast** :  render result by columns : see exemple

#### Exemple :

```r
 aggregateIndispo("NLindispoTS.csv", "ag1.csv", c("date", "hour", "productionType"))
 aggregateIndispo("NLindispoTS.csv", "ag2.csv", c("date", "hour", "productionType"), colDcast = "productionType")
```
