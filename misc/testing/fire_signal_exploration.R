library(envisionr)
library(tidyverse)

# VARIANT:
# 1: NO DISTURB
# 2: SURFACE FIRE OR PRESCRIBED FIRE
# 3: MIXED SEVERITY FIRE
# 4: STAND REPLACING FIRE
# 5: MASTICATION
# 6: MECHANICAL



data('ref_data')
hc0 <- load_datacube('~/Dropbox/!!!datacubes/FTW-FF-RSK-HC_Run0.dc')
hc1 <- load_datacube('~/Dropbox/!!!datacubes/FTW-FF-RSK-HC_Run1.dc')
hd0 <- load_datacube('~/Dropbox/!!!datacubes/FTW-FF-RSK-HD_Run0.dc')
hd1 <- load_datacube('~/Dropbox/!!!datacubes/FTW-FF-RSK-HD_Run1.dc')
ld0 <- load_datacube('~/Dropbox/!!!datacubes/FTW-FF-RSK-LD_Run0.dc')
ld1 <- load_datacube('~/Dropbox/!!!datacubes/FTW-FF-RSK-LD_Run1.dc')

run_envisionr(hc1)

dc1$SN_REACT[,40] |> summary()

scale_to_0_1 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

private <- ref_data$idu_vect$Profile > 0
public <- ref_data$idu_vect$Profile < 0

dc1 <- load_datacube('~/Downloads/Delta_LandSig_HC_Run0/OHW-FF-RSK-HC_Run0.dc')
names(dc1)[35] <- 'SN_REACT'
run_envisionr(dc1)

library(animation)
ani.options(verbose = FALSE, autoplay = FALSE, ani.type = "png", ani.dev = "png", outdir = ".")

hc0['DISTURB'] |> st_apply(MARGIN = 'year', function(x) sum(x == 51))


make_plot <- function(dc, yr, var, ext){
  plot_raster(
    raster = update_raster(
      raster = ref_data$idu_rast,
      datacube = dc,
      field = var,
      year = yr,
      extent = ext),
    pal_lookup = var,
    pal_table = get_pal(var),
    extent = ext,
    options = c('Roads', 'Boundary','Places'))
}


animation::saveGIF({
ext = terra::ext(484389, 499066, 1109955, 1120962)
ext = NULL
  for(i in 1:41){
    print(i)
    par(mfrow=c(2,3), cex=1)
    {
      var = 'VARIANT'
      make_plot(hc0, i, var, NULL); title(main = 'Highest trust (more fire)')
      make_plot(hd0, i, var, NULL); title(main = 'More trust (more fire)')
      make_plot(ld0, i, var, NULL); title(main = 'Low trust (more fire)')
      make_plot(hc1, i, var, NULL); title(main = 'Highest trust')
      make_plot(hd1, i, var, NULL); title(main = 'More trust')
      make_plot(ld1, i, var, NULL); title(main = 'Low trust')
    }
  }
},
interval = 0.5,
ani.width = 1000,
ani.height = 650,
movie.name = 'variant.gif')



animation::saveVideo({
  ext = terra::ext(484389, 499066, 1109955, 1120962)
  ext = NULL
  for(i in 1:41){
    print(i)
    par(mfrow=c(2,3))
    {
      make_plot(hc0, i, 'VARIANT', NULL); title(main = 'High trust, high fire')
      make_plot(hd0, i, 'VARIANT', NULL); title(main = 'Mod trust, high fire')
      make_plot(ld0, i, 'VARIANT', NULL); title(main = 'Low trust, high fire')
      make_plot(hc1, i, 'VARIANT', NULL); title(main = 'High trust, low fire')
      make_plot(hd1, i, 'VARIANT', NULL); title(main = 'Mod trust, low fire')
      make_plot(ld1, i, 'VARIANT', NULL); title(main = 'Low trust, low fire')
    }
  }
},
interval = 0.5,
ani.width = 1000,
ani.height = 650)


tmp <- st_apply(hc0['FlameLen'], 1, function(x){
  # browser()
  sum(x > 0)
})

make_plot(tmp, 2, 'VARIANT', NULL); title(main = 'Low trust, low fire')

update_raster(
  raster = ref_data$idu_rast,
  datacube = tmp,
  field = 'FlameLen',
  year = 1,
  extent = NULL)


tmp['FlameLen',1]

 tmp <- 1:41 |> purrr::map(function(x){
  dc1$SN_REACT[private,x] |> cut(c(-Inf,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) |> as.numeric()
}) |> bind_cols() |> setNames(paste0('yr',1:41))

table(private) |> prop.table()
table(dc1$SN_REACT[private,2] == 0) |> prop.table()
table(dc1$SN_REACT[private,40] != 0) |> prop.table()

da <- data.table::fread('~/Downloads/Delta_LandSig_HC_Run0/DeltaArray/DeltaArray_OHW-FF-RSK-HC_Run0.csv')
unique(da$field)

rs <- read.csv('~/Downloads/Delta_LandSig_HC_Run0/Landscape_Evaluative_Statistics_(Raw_Scores)_OHW-FF-RSK-HC_Run0.csv') |> mutate(year = Time..years. - 2019, value = Fire.Risk)
plot(rs$Fire.Risk)

ls <- read.csv('~/Downloads/Delta_LandSig_HC_Run0/Fire_Risk_OHW-FF-RSK-HC_Run0.csv')
plot(ls$Risk.Total, type='l')
lines(scale_to_0_1(rs$Fire.Risk), col = 'red', pch=16, cex=0.5, type = 'l')

# identify idus w/ SN_REACT > 0
i <- which(dc1$SN_REACT[,40] > 0)
{
  # DN_REACT
  x <- dc1$SN_REACT[i,] |>
    as.data.frame() |>
    mutate(id = 1:n()) |>
    pivot_longer(-id) |>
    mutate(year = gsub('V','',name) |> as.numeric())

  # OUTREACH
  y <- dc1$OUTREACH[i,] |>
    as.data.frame() |>
    mutate(id = 1:n()) |>
    pivot_longer(-id) |>
    mutate(year = gsub('V','',name) |> as.numeric())

  z <- dc1$A2rxn[i,] |>
    as.data.frame() |>
    mutate(id = 1:n()) |>
    pivot_longer(-id) |>
    mutate(year = gsub('V','',name) |> as.numeric())

  xyz <- cbind(x, outreach = y$value, a2rxn = z$value)
}

create_linear_scale <- function(domain, range) {
  # Function to perform the scaling
  scale_function <- function(x) {
    scaled_value <- range[1] + (x - domain[1]) * (range[2] - range[1]) / (domain[2] - domain[1])
    return(scaled_value)
  }
  return(scale_function)
}

# Create the scaling function
scale_linear <- create_linear_scale(domain = c(0, 50), range = c(0, 2))

# Apply the scale to some values
values <- c(0, 25, 50, 75, 100)
scaled_values <- sapply(values, scale_linear)

pdat <- xyz |>
  filter(id %in% sample(unique(xyz$id), 1000)) |> filter(year > 1)

ggplot(pdat, aes(x = year, y = value)) +
  # geom_line() + facet_wrap(~id) +
  geom_line(aes(y = a2rxn, group = id), color = 'blue', alpha=0.1) +
  geom_line(data = rs, aes(x = year, y = value * 2 - 1), color='red')

ggplot(pdat, aes(x = year, y = value)) +
  geom_line() + facet_wrap(~id) +
  geom_line(aes(y = a2rxn), color = 'blue') + facet_wrap(~id) +
  geom_line(data = rs, aes(x = year, y = value), color='red') +
  # geom_line(data = pdat, aes(x = year, y = a2rxn), color='blue') +
  geom_vline(data = pdat |> filter(outreach == 1), aes(xintercept = year), color='green') +
  coord_cartesian(xlim=c(1,40)) +
  labs(y = '')

fire_df  <- dc1$FlameLen[i,] |>
  as.data.frame() |>
  mutate(id = 1:n()) |>
  pivot_longer(-id) |>
  mutate(year = gsub('V','',name) |> as.numeric()) |>
  mutate(fire = ifelse(value > 0, 1, 0)) |>
  group_by(year) |>
  summarize(value = sum(fire)/3000)

or_df  <- dc1$OUTREACH[i,] |>
  as.data.frame() |>
  mutate(id = 1:n()) |>
  pivot_longer(-id) |>
  mutate(year = gsub('V','',name) |> as.numeric()) |>
  mutate(fire = ifelse(value > 0, 1, 0)) |>
  group_by(year) |>
  summarize(value = sum(fire)/6000)

apply_decay(fire_df$value)[1:41]

xyz_segments <- xyz |>
  filter(id %in% sample(unique(xyz$id), 100)) |>
  # filter(year > 3) |>
  group_by(id) %>%
  mutate(
    slope = c(NA, diff(value) / diff(year)), # Calculate slope between consecutive years
    year_next = lead(year),   # Get the next year for each segment
    value_next = lead(value)  # Get the next value for each segment
  ) %>%
  filter(!is.na(slope))

# Plot using geom_segment with dynamic alpha based on slope
ggplot(xyz_segments |> filter(slope > 0)) +
  geom_segment(aes(
    x = year, y = value,
    xend = year_next, yend = value_next,
    group = id,
    alpha = abs(slope)
  )) +
  # scale_alpha_continuous(range = c(0.1, 1)) +  # Adjust range for transparency effect
  # scale_color_gradientn(colours = c("white","white", "red")) +  # Adjust colors as needed
  theme_minimal() +
  geom_col(data = rs, aes(x = year, y = value/1.5), fill='red', alpha=0.25) +
  geom_col(data = or_df, aes(x = year, y = value * 0.75), fill='green', alpha=0.2, width = 1)

ggplot() +
  geom_col(data = rs, aes(x = year, y = value/1.5), fill='red', alpha=0.25) +
  geom_col(data = or_df, aes(x = year, y = value * 0.75), fill='green', alpha=0.5, width = 1) +
  geom_line(data = xyz |> filter(id %in% sample(unique(xyz$id), 100)),
            aes(x = year, y = value, group = id), alpha = 0.5, lwd = 0.25, color ='black') +
  # geom_line(data = xyz |> filter(id %in% sample(unique(xyz$id), 10)),
  #           aes(x = year, y = value, group = id), alpha = 1, lwd = 1) +
  # geom_line(aes(x = year, y = I(a2rxn/2 + .5), group = id), alpha = 0.2) +
  coord_cartesian(ylim=c(0,0.65), xlim=c(5,40)) +
  theme_void()
  # geom_hline(yintercept = c(0,0.65), lwd=3)


plot(fire_df$value, type='h')
apply_decay(fire_df$value)[1:41] |> lines()
apply_decay(fire_df$value)[1:41]

envisionr::plot_raster()

tmp <- dc1$SN_REACT[,30]
plot(dc1$SN_REACT[958,])
tmp[tmp>0] |> hist()




# attuation factor at 1 = 5 year half-life
# attuation factor at 0.5 = 10 year half-life
# attuation factor at 0.25 = 20 year half-life

decay_function <- function(previous_value, half_life, attenuation_factor) {
  # Calculate the decay constant based on the half-life
  decay_constant <- log(2) / half_life

  # Adjust the decay rate using the attenuation factor
  adjusted_decay = exp(-decay_constant * attenuation_factor)

  # Apply the decay to the previous year's value
  current_value <- previous_value * adjusted_decay

  return(current_value)
}

rs
accumulate_decay <- function(){
  xyz$fire_risk <- rs$Fire.Risk
  tmp <- xyz |> filter(id == 1)
  tmp$value2 = 0
  for(i in 1:40){
    print(i)
    tmp$value2[i+1] <- decay_function(tmp$value2[i], 1, 1-tmp$fire_risk[i])
  }
  xyz |> filter(id == 1) |> pull(value) |> plot()
  plot(tmp$value, type='l')
  lines(tmp$fire_risk/3)
  lines(tmp$value2)
  lines(tmp$value * tmp$fire_risk)
  }


# Example usage:
previous_value <- 100  # Initial value
half_life <- 5         # Half-life in years
attenuation_factor <- .25  # Attenuation factor between 0 and 1

# Calculate the decayed value
current_value <- decay_function(previous_value, half_life, attenuation_factor)
(current_value <- decay_function(current_value, half_life, attenuation_factor))

pdat <- xyz |> filter(id %in% sample(unique(xyz$id), 20)) |> filter(year > 1)

pdat |> group_by(id) |>
  summarize(
    decayed_values = list(decay_function(value, 5, 0.5))
  ) |>
  unnest(cols = c(decayed_values))

