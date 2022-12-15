# PhD thesis simulation code (Chapter 2)

In this repository, you can find the simulation code used to examine utility-weighted sampling in Chapter 2.

## Prerequisites

To run the simulation code you will need [R](https://cran.rstudio.com/) and [RStudio](https://posit.co/download/rstudio-desktop/) installed on your computer.

## Getting started

The easiest way to access the simulation code is downloading the `.zip` file. To do this: 

1. Click the "Code" button and then click the "Download ZIP" option
2. This will download a `.zip` file containing the code to your computer
3. Extract the contents of the `.zip` file to a directory of your choice

Alternatively, you can open the Terminal or Command Prompt, navigate to the folder where you want to clone this repository, and run the following command: `git clone https://github.com/joelholwerda/thesis-rational-extremes.git`. 

## Running the simulations

1. Open the `00_open_project.Rproj` file. This opens a new session in RStudio and sets the working directory to the correct location
2. Open the `01_simulate_uws.R` file
3. RStudio might prompt you to install missing packages. Alternatively, you can run the following code: `install.packages("tidyverse", "arrow", "furrr", "parallel", "cowplot", "ggtext", "rlang", "here", "sn")`
4. Click the "Source" button to run the entire script or highlight sections and press Cmd + Enter

## Options

- `n_simulations` specifies the number of simulations for each value of the free parameters in utility-weighted sampling. Leave this variable as `10000` to reproduce the reported values or choose a lower number to make the simulations run quicker
- The reported simulations were based on Experiment 1 of [Ludvig et al. (2014)](https://doi.org/10.1002/bdm.1792). We also simulated Experiments 2-4 to examine the generalisability of this analysis. Setting `simulate_all_experiments` to `FALSE` reproduces only the reported values
- The number of cores used for the parallel simulations, etc. will be the number of available cores minus the value of the `reserved_cores` variable (or one if the number of reserved cores is greater than or equal to the number of available cores)

## Other files

The "src" folder contains various functions used to run the simulations:

- "encode_outcomes.R" corresponds to Equation 16 in [Lieder et al. (2018)](https://doi.org/10.1037/rev0000074)
- "sample_utilities.R" corresponds to Equation 11 in [Lieder et al. (2018)](https://doi.org/10.1037/rev0000074)
- "aggregate_samples.R" corresponds to Equation 12 in [Lieder et al. (2018)](https://doi.org/10.1037/rev0000074)

## Get help

If you have trouble running the code in this repository or have questions, contact me at joeldavidholwerda@gmail.com.
