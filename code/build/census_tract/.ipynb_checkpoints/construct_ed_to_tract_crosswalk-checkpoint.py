# Adapted from Eckert, Gvirtz, Liang, Peters, 2020
#"A Method to Construct Geographical Crosswalks with an Application to US Counties since 1790"
#www.fpeckert.me/eglp

## Create area weights for mapping 1930-1980 census tracts to 1990 census tracts

## A generic code to construct your own crosswalk, from two shapefiles

import pandas as pd
import geopandas as gpd
import os

## defining variables - change the things in ALL_CAPS
# reference: 2000 Census tracts 
reference_path = '/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_1990'
reference_fname = 'US_tract_1990.shp'
reference_geoid = 'GISJOIN_2'

# loop through 1930, 1940, 1950, 1960, 1970, 1980:
for y in ['1930', '1940', '1950', '1960', '1970', '1980']:

    reporting_path = '/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/data/raw/nhgis/gis/nhgis0027_shapefile_tl2000_us_tract_' + y
    reporting_fname = 'US_tract_' + y + '.shp'
    reporting_geoid = 'GISJOIN_1'
    
    
    output_path = '/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/data/derived/geographic_crosswalks'
    output_fname = 'tract_concordance_weights' + y + '_to_1990.csv'
    
    
    # A note: the reporting and reference geo_ids should be different.
    
    ## read in starting shapefile
    os.chdir(reporting_path)
    shp_reporting = gpd.GeoDataFrame.from_file(reporting_fname)
    shp_reporting['area_base'] = shp_reporting.area
    
    ## read in ending shapefile
    os.chdir(reference_path)
    shp_reference = gpd.GeoDataFrame.from_file(reference_fname)
    
    
    ## intersecting the file
    intersect = gpd.overlay(shp_reporting, shp_reference, how = 'intersection')
    intersect['area'] = intersect.area
    
    ## computing weights
    intersect['weight'] = intersect['area'] / intersect['area_base']
    
    ## renormalizing weights - this isn't necesary, but without it, if the shapefiles do not perfectly line up where they should, you may lose small fractions of area here and there
    reweight = intersect.groupby(reporting_geoid)['weight'].sum().reset_index()
    reweight['new_weight'] = reweight['weight']
    reweight = reweight.drop('weight', axis = 1)
    
    intersect = intersect.merge(reweight, left_on = reporting_geoid, right_on = reporting_geoid)
    intersect['weight'] = intersect['weight'] / intersect['new_weight']
    
    intersect = intersect.drop('new_weight', axis =1)
    
    
    ## keeping only relevant columns - again isn't necessary, but will help trim down the size of the crosswalk at the end
    output = intersect[[reporting_geoid, reference_geoid, 'weight']]
    
    # rename geoid columns so I know what they are 
    output = output.rename(columns={"GISJOIN_1": "GISJOIN_" + y, "GISJOIN_2": "GISJOIN_1990"})

    
    ## saving output
    os.chdir(output_path)
    output.to_csv(output_fname, index = False)


# A final note - if you have OSErrors, there is something wrong with your environment and shapely.
# A stated fix can be found here: https://github.com/conda-forge/shapely-feedstock/issues/64
# Thank you to Daniel Bischof for pointing out this potential problem and finding the solution.
