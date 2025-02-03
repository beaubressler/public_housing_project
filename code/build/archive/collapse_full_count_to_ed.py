#!/usr/bin/env python
# coding: utf-8


import pandas as pd
import dask.dataframe as dd
from ipumspy import readers
import os
import time

# Setup directories
home_dir = "/Users/beaubressler/Library/CloudStorage/Dropbox/Research/public_housing/"

full_count_dir = home_dir + "data/raw/ipums/full_count"
output_dir = home_dir + "data/derived/census/full_count/ed_by_city"
grf_dir = home_dir + "data/derived/geographic_reference_file"

# Cities of interest
cities = [
    "AkronOH", "AlbanyNY", "AtlantaGA", "BaltimoreMD", "BirminghamAL", "BostonMA",
    "BridgeportCT", "BronxNY", "BrooklynNY", "BuffaloNY", "ChattanoogaTN", "ChicagoIL",
    "CincinnatiOH", "ClevelandOH", "ColumbusOH", "DallasTX", "DaytonOH", "DenverCO",
    "DesMoinesIA", "DetroitMI", "FlintMI", "FortWorthTX", "GrandRapidsMI", "HartfordCT",
    "HoustonTX", "IndianapolisIN", "JacksonvilleFL", "JerseyCityNJ", "KansasCityKS",
    "KansasCityMO", "LongBeachCA", "LosAngelesCA", "LouisvilleKY", "ManhattanNY",
    "MemphisTN", "MiamiFL", "MilwaukeeWI", "MinneapolisMN", "NashvilleTN", "NewHavenCT",
    "NewOrleansLA", "NewarkNJ", "NorfolkVA", "OaklandCA", "OklahomaCityOK", "OmahaNE",
    "PatersonNJ", "PhiladelphiaPA", "PittsburghPA", "PortlandOR", "ProvidenceRI", "QueensNY",
    "RichmondVA", "RochesterNY", "SaltLakeCityUT", "SanAntonioTX", "SanDiegoCA", "SanFranciscoCA",
    "ScrantonPA", "SeattleWA", "SpokaneWA", "SpringfieldMA", "StLouisMO", "StPaulMN",
    "StatenIslandNY", "SyracuseNY", "ToledoOH", "TrentonNJ", "TulsaOK", "WashingtonDC",
    "WorcesterMA", "YonkersNY", "YoungstownOH"
]

# Path to IPUMS DDI XML file and microdata
ddi_file_path_1930 = os.path.join(full_count_dir, "usa_00032.xml")
microdata_file_path_1930 = os.path.join(full_count_dir, "usa_00032.dat.gz")


# Parse the DDI file into a DDI object
ddi = readers.read_ipums_ddi(ddi_file_path_1930)
#microdata = readers.read_microdata(ddi, microdata_file_path_1930)
# read in the microdata in chunks


# Load the Geographic Reference File for 1930
grf_1930_path = os.path.join(grf_dir, "grf_1930_histid_ed_city.csv")
grf_1930 = pd.read_csv(grf_1930_path, usecols=["histid", "b_ed"]).rename(columns={"histid": "HISTID"})



# Function to process each chunk
def process_chunk(chunk, grf_1930):
    """
    Process a chunk of microdata.
    Filters for RELATE == 1, merges with GRF data, and aggregates by CITY, b_ed, and RACE.
    """
    # Filter to heads of household
    chunk = chunk[chunk["RELATE"] == 1]

    # Merge with Geographic Reference File
    chunk = chunk.merge(grf_1930, on="HISTID", how="left")

    # Group by CITY, b_ed, and RACE, then count population
    grouped = (
        chunk.groupby(["CITY", "b_ed", "RACE"])
        .size()
        .reset_index(name="population")
    )
    return grouped

# Process IPUMS data in chunks
def process_large_ipums():
    # Initialize an empty list to store results
    results = []

    # Read microdata in chunks
    iter_microdata = readers.read_microdata_chunked(
        ddi=ddi, filename = microdata_file_path_1930, chunksize=100000
    )

    for i, chunk in enumerate(iter_microdata):
        # Process each chunk
        print(f"Processing chunk {i + 1}...")  # Log chunk start

        processed_chunk = process_chunk(chunk, grf_1930)
        results.append(processed_chunk)
        
        print(f"Chunk {i + 1} completed.")  # Log chunk completion


    # Combine all processed chunks into a single DataFrame
    final_df = pd.concat(results, ignore_index=True)

    # Drop missing CITY and b_ed
    final_df = final_df.dropna(subset=["CITY", "b_ed"])

    # Pivot the table to wide format
    final_df = final_df.pivot_table(
        values="population", index=["CITY", "b_ed"], columns="RACE", fill_value=0
    ).reset_index()

    # Add derived columns for specific racial groups
    final_df["white_pop"] = final_df.get("White", 0)
    final_df["black_pop"] = final_df.get("Black/African American", 0)
    final_df["other_pop"] = (
        final_df.get("Chinese", 0)
        + final_df.get("American Indian or Alaska Native", 0)
        + final_df.get("Japanese", 0)
        + final_df.get("Other Asian or Pacific Islander", 0)
    )

    # Select final columns
    final_df = final_df[["CITY", "b_ed", "white_pop", "black_pop", "other_pop"]]

    # Save the results to a CSV file
    output_file = os.path.join(output_dir, "population_by_race_city_ed_1930.csv")
    final_df.to_csv(output_file, index=False)
    print(f"Results saved to: {output_file}")



# Run the processing function
if __name__ == "__main__":
    process_large_ipums()

