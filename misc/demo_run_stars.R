

# plot various queries w/ stars and sum across time
x <- dc_stars |> with(N_DU > 0 & FlameLen > 0) |> apply(1, sum)
x <- dc_stars |> with(DISTURB == 51) |> apply(1, sum)
x <- dc_stars |> with(FlameLen > 0) |> apply(1, sum)
x <- dc_stars |> with(DISTURB == 51) |> apply(1, sum)
x <- dc_stars |> with(OUTREACH > 0) |> apply(1, sum)

apply_query(
  query = 'N_DU > 0 & FlameLen > 0',
  datacube = dc_stars
)

# pull data for specific year
yr = 2
x <- dc_stars |> with(OUTREACH > 0)
x <- ifelse(x[,yr], 1, 0)

n <- n_distinct(x)
idu_raster$NEW <- x[idu_raster$IDU_INDEX + 1]
plot(idu_raster[2])

plot(idu_raster[2], nbreaks = ifelse(n == 2, 3, n), col = sf.colors)
idu_raster$NEW <- factor(x[idu_raster$IDU_INDEX + 1])

# Convert the array into a stars object with properly defined dimensions
my_stars_object <- stars::st_as_stars(my_array, dimensions = dim_info)

# Check the created stars object
print(my_stars_object)
