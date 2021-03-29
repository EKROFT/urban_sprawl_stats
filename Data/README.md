This folder contains all the data used in my analyses.

All files titled "Compiled Data" are full datasets that include all data available for all study sites.

All files titled "Shortlist Data" are reduced datasets in which I set aside 10% of the study sites to avoid overfitting my models.

The last 4 numbers refer to the date in 2020-2021 in which the file was created (ie: "compiled_data_0727.csv" is a full dataset that was created on July 27th, 2020 and "compiled_data_0317" was created on March 17, 2021). Beginning in 2021, I started having the shortlist and longlist data in one document (separated by values in a column) rather than in 2 separate documents so the most recent data file should simply be labelled as "compiled" with no corresponding "shortlist" file.

Reduced_No2_Data: This is an experimental dataset in which I removed all study sites in NE Montreal to see if this changed any air quality trends (by
removing sites close to the large petrochemical facility). This was not used in the final project.

weighted_NO2_Data: This is the data with an added column in which I applied weights to the NO2 measurements according to distance from the petrochemical facility.This was not used in the final project.

study_site_coords: This is a file containing the geographic coordinates of each study site. These coordinates are now included in the full and compiled datasets
as the "Lat" and "Long" columns (as of August 1, 2020). 
