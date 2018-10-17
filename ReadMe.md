# Reproducibility in Hydrology

RStudio: [![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/jstagge/reproduc_hyd/r/master?urlpath=rstudio)


[![DOI](https://zenodo.org/badge/DOI/.svg)](https://doi.org/)

This repository contains code associated with paper, entitled, 'A novel replicability survey tool to measure and promote reproducibility in hydrology'. When run, it will replicate the results published in [Stagge et al. 2018](). It is provided here for transparency and so that other users may benefit from its underlying code. Please cite both the paper and this repository if you make use of any part of this.

## Getting Started

These instructions will allow you to process the reproducibility survey data on your local machine for testing purposes. All code is written in R. See Prerequisites and Running sections below for detailed instructions.

### Prerequisites

In order to run this code, you must install:
* [R for Statistical Computing](https://www.r-project.org/)

All necesary R packages will be installed automatically in the first file.

## Running the Code

### Running all scripts at once

Code is numbered based on the order of operations.  If you would like to simply recreate the results of [Stagge et al. (2018](http://), you may run the following from any command line after installing R. For more detailed information about each file, see below:

```
Rscript 00_prepare_file_system.R
Rscript 01_article_analysis.R
Rscript 02_reproduc_data_handling.R
Rscript 03_reproduc_figs.R
Rscript 04_pop_estimate.R
```

### Running scripts step-by-step
The following file prepares the file system, installing any necesary packages and creating folders for model output.

```
Rscript 00_prepare_file_system.R
```
The next script processes all articles articles from 2017, plots their keywords, separates the keyword or non-keyword papers, and randomly assigns papers to reviewers.

This code will randomly assign papers, so it will not exactly reproduce results from Stagge et al. (2018).
```
Rscript 01_article_analysis.R
```
The following script performs all calculations on the results of the  reproducibility survey. It prepares the data to be plotted using code file number 3. All results will be saved into a large .RDS file. This allows for the data to be plotted immediately or to be loaded later for additional analysis.

```
Rscript 02_reproduc_data_handling.R
```
The following file plots all figures from the analysis, incuding many that are not provided in the published paper. All files will be saved to a folder located at /output/figures.
```
Rscript 03_reproduc_figs.R
```
The final code file creates a estimate for all articles published in these journals during 2017 (i.e. the population).
```
Rscript 04_pop_estimate.R
```

## Reference and How to Cite

For any description of this methodology, please use the following citation (s):

* Stagge, J.H., Rosenberg, D.E., Abdallah, A., Akbar, A., Attallah, N., and James, R. (2018) "A novel replicability survey tool to measure and promote reproducibility in hydrology." Scientific Data.

* Stagge, J.H. (2018) "Reproducibility analysis repository accompanying Stagge et al. (2018)." doi: ????????.

## Authors

* **James H. Stagge** - *Owner* - [jstagge](https://github.com/jstagge)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments


