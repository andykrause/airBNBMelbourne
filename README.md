# Airbnb Returns in Melbourne 
## (airbnbMelbourne)

Analysis of Airbnb Market in Melbourne, Australia

## Data

Data for this project is held in our shared dropbox account.  There are three 'levels' of data.

1. **Raw:**  Raw data is straight from the source.  In the case of the APM rental data I have trimmed off some unnecessary columns simply to make the file size smaller.  No observations have been removed. 
2. **Prepared:**  Here the raw data has been passed through two data preparation scripts (*apmDataPrep_Script.R* and *abbDataPrep_Script.rmd*).  A variety of fields have been removed as well as added/created.  Observations with missing or outlier data have also been removed.  The APM long term rental data has been split into two files, one with the information on the final rental transactions (*ltpropdata.csv*) and one with all listing information (*ltlistdata.csv*) -- one observation per change to the listing. The Airbnb data is divided into two files here, one for information on each property (*propdata.csv*) and one with information on the daily status and rates (*dailydata.csv*).  The Airbnb data has been prepared by removing outliers and fields as well as having the missing nightly observations imputed.
3. **Analyzed:** Data that has been analzed in some way and can be used for futuring plotting or analytical purposes.  At the moment this contains an R workspace of the analysis up to the point where the *abbReturns_Script.rmd* file ends and the *abbReturns_working.R* file picks up.  This divisions represents the accepted code in the first file and the working analysis in the latter. 

## Code

## Process


