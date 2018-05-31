# Airbnb Returns in Melbourne 

This project details research into the financial feasibility of short-term leasing (Airbnb) against more traditional long-term leasing strategies in Melbourne, Australia.   The initial work on this project was funded by a grant from the Real Estate Research Institute (RERI) of Chicago, Illinois. 

### Replicability

This repository contains the code necessary to replication the analysis found in the paper **To Airbnb: A Question of Revenue**.  All data preparation, analysis, visualization and final paper production were done in the R statistical language.  To replicate follow the general step below:

1. Clone the **airbnbMelbourne** repository found at: www.github.com/andykrause/airbnbMelbourne
2. Download the data from https://doi.org/10.7910/DVN/1XPDEU into the /data folder where you downloaded the repository in #1. 
    *  Into a subfolder called */raw* download the following:
        *  ltr_data.csv
        *  str_daily_1.csv
        *  str_daily_2.csv
        *  str_prop_1.csv
        *  str_prop_2.csv
    *   Into a subfolder called */geographic* download and unzip the following:
        *  melbSuburbs.zip
        *  portPhillipBeach.zip
        *  sa1s.zip
3. Run the scripts in order
    * /scripts/1_str_preprocessing.rmd
    * /scripts/2_ltr_preprocessing.rmd
    * /scripts/3_data_prep.rmd
    * /scripts/4_data_analysis.rmd
    * /scripts/5_data_viz.rmd
4. Knit the final_paper.rmd file (most easily done use the RStudio (www.rstudio.com) IDE)
    * /papers/final_paper.rmd

*Please note that the scripts above use relative path locations that are most easily used by first opening the RStudio project file (to_airbnb.Rproj) in RStudio and then running any scripts from there. If you intend to run from base R or another IDE you will have to alter the relative paths in order to access data and custom functions.* 

