# CovidPackage

Project for the course of Coding for Data Science and Data Management, module R. 

## About the App

The aim of the project is to visualize the total number of first and second doses of COVID-19 Vaccinations for each Italian region according to the time period selected. 

Once the application is run and opened, it is possible to select a specific region and range of time, to check the number of first dose, second dose and totals as concerns COVID-19 Vaccinations given in Italy. 

## About data

Data have been taken from ```https://github.com/italia/covid19-opendata-vaccini```

## Installation

First install the following packages if not installed. 
```
install.packages("shiny")
install.packages("ggplot2")
install.packages("plotly")
```

## Usage

Load the packages and run it as follows:
```
shiny::runGitHub("VaccCov", "lauraciurca", ref ="main")
```

