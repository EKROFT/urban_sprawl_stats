# urban_sprawl_stats
Emily's MSc project statistics

LICENSE: The license that applies to this repo.

.Rdata/.Rhistory: The automatically generated files associtaed with my R working process

Testing_Models.r: This is where I initially started testing different types of models for the project. Largely a brainstorming/scratchpad document and doesn't contain any of the models or analyses used in the final project.

GAM_tutorial: This is a series of R scripts where I followed along with tutorials about GAMs to learn how to code for them.

data_exploration.r: This is the file where I looked at my data for the first time to look for patterns. Mainly a brainstroming/scratchpad document.Nothing here was used in final analyses.

Testing_NO2_Possibilities: For this file I clipped the NO2 data to exclude all sites in the NE of Montreal to see what the NO2 patterns would look like without
the influence of the large petrochemical facility.This did not make it into the final project.

spatial_testing.r: This is the script where I began testing the data and models for spatial autocorrelation.The final version of this code can be found in the respective files for each indicator (LST_second_exploration, restart_NO2, and gs_exploration).

site_coords.gpkg / site_coords.shp: This is the GIS file containing the geographic coordinates for each study site (both gpkg and shp available).

restart_NO2.r: This is the file containing all NO2 based models and analyses that ended up in the final project. In this file I restarted my exploration of the NO2 data because I learned a better way of comparing different models. This is where to find the code for anything about NO2 that was used in the project.

LST_second_exploration.r: This file contains all LST based models and analyses that ended up in the final project. In this file I restarted my exploration of the LST data because I learned a better way of comparing different models. This is where to find the code for anything about LST that was used in the project.

pretty_graphs.r: The code for aesthetically polished versions of figures.

explaining_graphs.RMd/.html: The markdown document explaining my work from pretty_graphs.r

workflow.Rmd/.html: The markdown document explaining my working process over the course of creating statistical models. 

gs_exploration.r: File containing models I worked with for green space access data. This is where to find all green space related analyses that ended up in final project (private and public).

Backyard_Analyses: This is a scratchpad document where I started exploring private green space data. Nothing here was used in final project.

Backyard_Questions: R markdown document explaining rationale behind private green space related figures.
