<div align="justify">

# Philadelphia Household Travel Survey

## Overview

This repository contains the analysis and findings of the 2012 Philadelphia Household Travel Survey. The project aims to understand and work with travel survey data to analyze household travel behaviors, with a focus on mode choice, household income impact, and daily travel patterns.

## Projects Structure

- `Philadelphia_Household_Travel_Survey_Code.R`: R script for loading, examining, and analyzing the survey data.
- `Philadelphia_Household_Travel_Survey_Report.pdf`: A detailed report of the findings and analysis.
- `Philadelphia_Household_Travel_Survey_Purpose.docx`: Document detailing the purpose and objectives of the analysis.
- `DVRPC_HTS_Data_Dictionary.xlsx`: Data dictionary for understanding the survey data fields.
- `Data`:
  - `1_Household_Public.csv`: Household data from the survey.
  - `2_Person_Public.csv`: Individual person data from the surveyed households. (Not uploaded due to size constraints)
  - `3_Vehicle_Public.csv`: Vehicle data related to the surveyed households.
  - `4_Trip_Public.csv`: Trip data from the survey participants.

## Project Objectives

1. Analyze a random household from the dataset to understand their income, race composition, county of residence, and car ownership.
2. Describe the daily activities and travel patterns of the household’s members.
3. Estimate the total number of bicycle trips represented in the survey data using the Person Weight field.
4. Investigate the mode choice preferences among residents from the household's county.
5. Explore the relationship between household income and the age of household vehicles, as well as household income and mode choice for all households.

## Key Findings

- Mode choice for residents primarily includes private vehicles, significantly outweighing other transportation modes.
- A positive correlation exists between household income and the newer age of household vehicles, indicating higher-income households tend to own newer vehicles.
- The analysis reveals a predominant use of private vehicles across all income levels, with slight variations in mode choice based on income.

## Usage

Instructions on how to run the R script and an explanation of the data files can be found in the `Philadelphia_Household_Travel_Survey_Purpose.docx` and `DVRPC_HTS_Data_Dictionary.xlsx` files.

## Acknowledgements

Special thanks to the Department of City and Regional Planning at the University of Pennsylvania and Erick Guerra for overseeing this project.
