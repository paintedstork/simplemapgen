using CSV
using DataFrames

distance(sinlat1, sinlat2, coslat1, coslat2, long1, long2) = round(6378 * acos(sinlat1 * sinlat2 + coslat1 * coslat2 * cos(long2-long1)))

function sub2ind(dims, i, j)
    ind = (j-1) * dims[1] + i
    return ind
end

df = CSV.read("C:\\Transfer\\ShinyApps\\simplemapgen\\locality.csv", DataFrame)
n = nrow(df)
println(n)

sinlat = [sin(df.LATITUDE_R[i]) for i in 1:n]
coslat = [cos(df.LATITUDE_R[i]) for i in 1:n]
df[!, :SINLAT] = sinlat
df[!, :COSLAT] = coslat

#distances = [distance(df.SINLAT[x], df.SINLAT[y], df.COSLAT[x], df.COSLAT[y], df.LONGITUDE_R[x], df.LONGITUDE_R[y]) for x in 1:(n-1);y in (x+1):n]
#distances = [distance(df.SINLAT[x], df.SINLAT[y], df.COSLAT[x], df.COSLAT[y], df.LONGITUDE_R[x], df.LONGITUDE_R[y]) for x in 1:(n-1), y in (x+1):n]
#distances = [distance(df.SINLAT[i], df.SINLAT[j], df.COSLAT[i], df.COSTLAT[j], df.LONGITUDE_R[i], df.LONGITUDE_R[j]) for i in 1:(n-1), j in (i+1):n]
#result = DataFrame(LOCALITY1 = repeat(df.LOCALITY_ID[1:(n-1)], inner = n-1), LOCALITY2 = repeat(df.LOCALITY_ID[2:n], outer = n-1), DISTANCE = 0)

#i = repeat(collect(1:(n-1)), inner = n - (n-1))
#j = repeat(collect(2:n), outer = n - (n-1))
#distances[sub2ind([n, n], i, j)] = i + convert(Vector{Int64}, j .+ 1)


#distances[i .+ (j .- 1) * (n-1)] = distance(df.SINLAT[i], df.SINLAT[j], df.COSLAT[i], df.COSLAT[j], df.LONGITUDE_R[i], df.LONGITUDE_R[j])

#for i in 1:(n-1)
#    for j in (i+1):n
#        distance[!, i, j] = i+j
#    end
#end
println(n)

result = DataFrame(LOCALITY1 = Int64[], LOCALITY2 = Int64[], DISTANCE= Float64[]) 

for i in 1:(n-1)
    println(i)
    for j in i+1:n
        d = distance(df.SINLAT[i],df.SINLAT[j],df.COSLAT[i],df.COSLAT[j],df.LONGITUDE_R[i],df.LONGITUDE_R[j])
        row = (df.LOCALITY_ID[i], df.LOCALITY_ID[j], d)
        push!(result, row)
    end 
end

#        CSV.write("C:\\Transfer\\ShinyApps\\simplemapgen\\loc.csv", DataFrame(LOCALITY1 = [df.LOCALITY_ID[i]], LOCALITY2 = [df.LOCALITY_ID[j]], DISTANCE = [d]); append=true)
#using DataFrames, CSV

#df = CSV.read("C:\\Transfer\\ShinyApps\\simplemapgen\\locality.csv", DataFrame)

#n = nrow(df)

# Compute the distances between all pairs of rows
#distances = [distance(df.LATITUDE_R[i], df.LONGITUDE_R[i], df.LATITUDE_R[j], df.LONGITUDE_R[j]) for i in 1:(n-1), j in (i+1):n]

# Combine the distances with the corresponding LOCALITY_ID values
#result = DataFrame(LOCALITY1 = repeat(df.LOCALITY_ID[1:(n-1)], inner = n-1), LOCALITY2 = reshape(df.LOCALITY_ID[2:n] .* ones(n-1), (n-1)^2), DISTANCE = distances)
#result = DataFrame(LOCALITY1 = repeat(df.LOCALITY_ID[1:(n-1)], inner = n-1), LOCALITY2 = repeat(df.LOCALITY_ID[2:n], outer = n-1), DISTANCE = distances)
# Write the result to a CSV file
CSV.write("C:\\Transfer\\ShinyApps\\simplemapgen\\loc.csv", result)



