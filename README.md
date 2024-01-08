# Accessing Racial Mortality Rate Changes During and Before Pandemic: The Interplay of Racial Inequality with Income and The Essential Role of Health Insurance

## Overview

This paper used nine data sets from three difference sources. Specifically, we used five data sets regarding the socio-economic factors from American Community Survey (ACS) and death data from both before and during the pandemic from CDC WONDER. This paper compares the mortality rate for both White and Black before and during the pandemic, to see how the mortality inequality changed due to pandemic. This paper also investigated the relations between socio-economic factors such as income and health insurance coverage, to the change of inequality.

All the ACS data are extracted using API but the mortality data are downloaded directly from the website of CDC WONDER. All the inputs data are stored at `inputs/data`. The output paper is in `outputs/paper/paper.pdf`.

## File Structure

The repo is structured as:

-   `input/data` contains the data sources used in analysis including the raw data.
-   `outputs/data` contains the cleaned dataset that was constructed.
-   `outputs/paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.