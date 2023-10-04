# Phenoplate data analysis

## Setup the files

1. You can download the files in this repo as a zip file here: [https://github.com/ChristineRoper/phenoplate/archive/refs/heads/main.zip](https://github.com/ChristineRoper/phenoplate/archive/refs/heads/main.zip)
1. Extract zip file and move the files to a folder you are going to be working from for this analysis
1. Set your working directory in R to the folder where you have just put these files. e.g. `setwd('/users/me/phenoplate')`

## `1. Fluorcam data` folder

Extracts data from Fluorcam files

1. Put your TXT files from Fluorcam in folders inside `Fluorcam Files`
   - One folder per Phenoplate run
   - Make sure the files are named `T1.TXT`, `T2.TXT`, `T3.TXT`
   - e.g.
     - `Fluorcam data/Fluorcam Files/Sample Set 1/T1.TXT`
     - `Fluorcam data/Fluorcam Files/Sample Set 1/T2.TXT`
     - `Fluorcam data/Fluorcam Files/Sample Set 1/T3.TXT`
1. Edit the sample list (`Sample list.csv`) to match you data
   - 1 row per sample
   - `Sample ID` can be whatever ID makes sense for you. They must be unique
   - `Folder` is the folder which contains your TXT files
     - `FirstArea` is the first phenoplate well for that sample (e.g. `1`)
   - `LastArea` is the last phenoplate well for that sample (e.g. `6`)
   - `Temp1-6` are the temperature set for each of the 6 wells
   - Any other columns (must come last) won’t be used in the analysis, but will be kept so you’ll still have them in the output files at the end
1. Run `extract_fluorcam_data.r`, this will output `outputs/fluorcam_data.csv` which contains your raw metrics from the TXT files in a format that can be used for analysis

## `2. Rapid Light Curves` folder

Fit the Rapid Light Curve data to `P/I` curves

1. Run `Extract_RLC_data.r` which extracts the light curve data from Fluorcam and output it to `outputs/fluorcam_RLC_data.csv`
1. Run `rlc_metrics_etr.r` and `rlc_metrics_qy.r` to fit the `P/I` curves to your `ETR` or `QY` data **Note: This code currently uses the Webb P/I model, you should explore which P/I model may fit your data best for most accurate results

1. This will output plots with your raw data and the `P/I` curves for you review (`outputs/QY/plots`), as well as a csv file (`outputs/rlc_qy_metrics_webb.csv`) with your extracted parameters (`QYmax`, `Ek`, `QYek`, `ETRmax`)
## `3. Thermal Curves` folder

Fit parameters to thermal curves

1. Run any `.r` file to fit your parameter(s) of interest to the thermal curve. Depending on the parameter, different models are used.
   - Plots and csv files will be output to the `outputs` folder.


## Citing 

If you use this code in your publication, please cite it by using the following BibTeX entry.

```bibtex
@Misc{phenoplate,
  title =        {FluorCam Rapid Light Curve and Thermal Performance Curve Analysis in R},
  author =       {Christine Roper, Jake Crosby},
  howpublished = {\url{https://github.com/ChristineRoper/phenoplate}},
  year =         {2023}
}
```
