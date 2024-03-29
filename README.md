# JEADV Article Computations Repository
## AN INSIGHT INTO RACIAL BIAS IN DERMOSCOPY REPOSITORIES: A HAM10000 DATASET ANALYSIS



This repository contains the R scripts and data used for the analysis presented in the article published in the Journal of the European Academy of Dermatology and Venereology (JEADV). The scripts are organized into a clear structure to facilitate replication and further research.

## Repository Structure

- `/input`: This directory contains all the raw data files used for the analysis, as well some input data edited by the dermatologists. 
- `/output`: Here, you will find the processed data files and results generated by the scripts.
- `/src`: This directory contains the R scripts used for the analysis.

## Scripts Description

1. `Pixels_sampling.R`: This script is responsible for the pixel sampling method used in our study. It randomly selects 10% of the images from the whole dataset for validation purposes.

2. `Results.R`: After running the pixel sampling, this script is used to compile and analyze the results, forming the basis for our study's findings.

3. `Validation.R`: This script details the process for the validation of the pixel extraction method, preparing the sample for dermatological review.

4. `Validation_results.R`: Post dermatologist review, this script processes their feedback and compiles the final validation results.

## Usage

To replicate the study's findings or to conduct further research, follow the scripts in numerical order. Ensure that the raw data is placed in the `/input` directory before beginning.

## Contribution

If you would like to contribute to this repository or have any queries, please open an issue or a pull request.

## Acknowledgments

We thank all contributors to this research, especially those who have provided feedback on the methodology and helped to refine the algorithms.

## License

This project is licensed under the MIT License - see the LICENSE.md file for details.

## Contact

For any additional information or inquiries, please contact the corresponding author of the JEADV article.
