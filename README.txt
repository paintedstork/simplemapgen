Steps to generate simple maps.

1. Download eBird data for all countries in the Indian subcontinent. 
2. Use locproc.R and create the file locality.csv. This is the full list of unique locations in the region.
3. Use Julia script distance.jl to create a LOCALITY x LOCALITY large datafram with the distances between them
4. Use the last two lines of locproc.R to convert into an RDS
5. Use prepareSpeciesData.R to split the large data from eBird into per species file.
6. Use localityMatrix.R to convert the loc.RDS and species data into per species LOCALITY x LOCALITY matrix with distances.
7. Use cluster.R to open localityMatrix and create clusters - with four configurations 50, 100, 150, 200 km as the inter-point distance.
8. Use polygonize.R to open these clusters and create a concave/convex hulls and cut the polygons by land boundaries.
9. Use plotMap.R to finally plot the polygons and points.
All uses the species list as configuration (apart from other parameters like convex/concave, interpoint distance to use for the plot, clipping polygon for range restricted species etc.

For extra species, 
1. Create an ExtraRecords.csv and ExtraSpecies.csv
2. Run ProcessExtraRecords - this should be done after step 5 of above
3. Use Julia script distanceExtr.jl to augument the new localities and then filter by locality pairs with distance < 200km
4. Use the last two lines of locproc.R to convert into an RDS. Continue with Step 6.

