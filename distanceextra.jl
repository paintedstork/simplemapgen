using CSV
using DataFrames

function distance(sinlat1, sinlat2, coslat1, coslat2, long1, long2)
    dot_product = sinlat1 * sinlat2 + coslat1 * coslat2 * cos(long2 - long1)
    dot_product = min(1, max(-1, dot_product))  # Ensure the value is within -1 to 1
    return round(6378 * acos(dot_product))
end


df1 = CSV.read("C:\\Transfer\\ShinyApps\\simplemapgen\\locality.csv", DataFrame)
n1 = nrow(df1)


df2 = CSV.read("C:\\Transfer\\ShinyApps\\simplemapgen\\Extralocality.csv", DataFrame)
n2 = nrow(df2)

df = vcat(df1, df2)
n = n1 + n2

sinlat = [sin(df.LATITUDE_R[i]) for i in 1:n]
coslat = [cos(df.LATITUDE_R[i]) for i in 1:n]
df[!, :SINLAT] = sinlat
df[!, :COSLAT] = coslat


#result = DataFrame(LOCALITY1 = Int64[], LOCALITY2 = Int64[], DISTANCE= Float64[]) 

result = CSV.read("C:\\Transfer\\ShinyApps\\simplemapgen\\loc.csv", DataFrame, types = [Int64, Int64, Float64])

# Calculate the distance of the new rows with the old rows and append to result
for i in n1 + 1:n
    println(i)
    for j in 1:n1
        d = distance(df.SINLAT[i],df.SINLAT[j],df.COSLAT[i],df.COSLAT[j],df.LONGITUDE_R[i],df.LONGITUDE_R[j])
        row = (df.LOCALITY_ID[i], df.LOCALITY_ID[j], d)
        push!(result, row)
    end 
end

# Calculate the distances between the new rows and append to the result
for i in n1 + 1 :(n-1)
    println(i)
    for j in i+1:n
        d = distance(df.SINLAT[i],df.SINLAT[j],df.COSLAT[i],df.COSLAT[j],df.LONGITUDE_R[i],df.LONGITUDE_R[j])
        row = (df.LOCALITY_ID[i], df.LOCALITY_ID[j], d)
        push!(result, row)
    end 
end


result = result[result.DISTANCE .< 200, :]

CSV.write("C:\\Transfer\\ShinyApps\\simplemapgen\\loc_extra.csv", result)


