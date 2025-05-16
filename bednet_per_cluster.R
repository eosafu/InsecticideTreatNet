rm(list = ls())
library(haven)
library(sf)
library(dplyr)
library(ggplot2)
library(INLA)
library(raster)
library(ggplot2)
library(reshape2)

#2003
#Read the data
data1 <- read_dta("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2003_DHS_02032024_1924_197159/NGKR4BDT/NGKR4BFL.DTA")

# Extract coordinates
dhs_shapefile1 <- st_read("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2003_DHS_02032024_1924_197159/NGGE4BFL/NGGE4BFL.shp")
coordinates <- st_coordinates(dhs_shapefile1)
coordinates_df1 <- data.frame(
  v001 = dhs_shapefile1$DHSCLUST,
  latitude = coordinates[, "Y"],
  longitude = coordinates[, "X"]  
)

#Define state mapping for 2003
state_mapping <- data.frame(
  sstate = c(21, 36, 12, 25, 30, 6, 8, 34, 3, 11, 10, 26, 15, 37, 35, 19, 
             29, 5, 27, 13, 18, 28, 33, 17, 4, 2, 24, 32, 7, 1, 22, 9, 
             20, 31, 23, 14, 16),
  state = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 
            150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 
            270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370)
)

#Replace state codes for 2003
data1 <- data1 %>%
  left_join(state_mapping, by = "sstate") %>%
  mutate(state = ifelse(is.na(state), sstate, state)) %>%
  select(-sstate)

#Create the reg_data DataFrame
reg_data <- data.frame(
  REGCODE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370),
  REGNAME = c('sokoto', 'zamfara', 'katsina', 'jigawa', 'yobe', 'borno', 'adamawa', 'gombe', 'bauchi', 'kano', 'kaduna', 'kebbi', 'niger', 'fct-abuja', 'nasarawa', 'plateau', 'taraba', 'benue', 'kogi', 'kwara', 'oyo', 'osun', 'ekiti', 'ondo', 'edo', 'anambra', 'enugu', 'ebonyi', 'cross river', 'akwa ibom', 'abia', 'imo', 'rivers', 'bayelsa', 'delta', 'lagos', 'ogun')
)

#merge cooordinates with data by clusters
merged_data1<- merge(coordinates_df1, data1, by = "v001")

#select necessary columns
merged_data1_new<-select(merged_data1, "latitude", "longitude","v459","v460", "v461", "state", "v007", "v002", "v136", "v001")

#merge new dataframe with reg_data
combined_data1 <- left_join(merged_data1_new, reg_data, by = c("state" = "REGCODE"))

#rename column names
combined_data1 <- combined_data1 %>%
  select(latitude, longitude, v459, v460, v461, state, v007,v002,v136,v001, REGNAME) %>%
  rename(
    latitude = latitude,
    longitude = longitude,
    mosquito_bednet = v459,
    children_under5 = v460,
    respondent = v461,
    state_code = state,
    year = v007,
    house_no = v002,
    members = v136,
    cluster_no = v001,
    regname = REGNAME
  )

#2008
#Read the data
data2 <- read_dta("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2008_DHS_02032024_1923_197159/NGKR53DT/NGKR53FL.DTA")
# Extract coordinates
dhs_shapefile2 <- st_read("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2008_DHS_02032024_1923_197159/NGGE52FL/NGGE52FL.shp")
coordinates <- st_coordinates(dhs_shapefile2)
coordinates_df2 <- data.frame(
  v001 = dhs_shapefile2$DHSCLUST,
  latitude = coordinates[, "Y"],
  longitude = coordinates[, "X"]  
)

#merge coordinates with data by clusters
merged_data2<- merge(coordinates_df2, data2, by = "v001")

#select necessary columns
merged_data2_new<-select(merged_data2, "latitude", "longitude","v459","v460", "v461", "sstate", "v007","v002", "v136", "v001")

#merge new dataframe with reg_data
combined_data2 <- left_join(merged_data2_new, reg_data, by = c("sstate" = "REGCODE"))

#rename column names
combined_data2 <- combined_data2 %>%
  select(latitude, longitude, v459, v460, v461, sstate, v007,v002,v136,v001, REGNAME) %>%
  rename(
    latitude = latitude,
    longitude = longitude,
    mosquito_bednet = v459,
    children_under5 = v460,
    respondent = v461,
    state_code = sstate,
    year = v007,
    house_no = v002,
    members = v136,
    cluster_no = v001,
    regname = REGNAME
  )

#2010
#Read the data
data3 <- read_dta("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2010_MIS_09252024_2132_197159_new/NGHR61DT/NGHR61FL.DTA")

# Extract coordinates
dhs_shapefile3 <- st_read("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2010_MIS_02032024_1922_197159/NGGE61FL/NGGE61FL.shp")
coordinates <- st_coordinates(dhs_shapefile3)
coordinates_df3 <- data.frame(
  hv001 = dhs_shapefile3$DHSCLUST,
  latitude = coordinates[, "Y"],
  longitude = coordinates[, "X"]  
)

#merge coordinates with data by clusters
merged_data3<- merge(coordinates_df3, data3, by = "hv001")

#select necessary columns
merged_data3_new<-select(merged_data3, "latitude", "longitude", "hv001", "hv227", "hv228", "hv024", "hv009", "hv007")

#make state_code for 2010 merged with the rest of the years
merged_data3_new <- merged_data3_new %>%
  mutate(hv024 = hv024 * 10)

#merge new dataframe with reg_data
combined_data3 <- left_join(merged_data3_new, reg_data, by = c("hv024" = "REGCODE"))

#rename column names
combined_data3 <- combined_data3 %>%
  select(latitude, longitude, hv227, hv001, hv228, hv024, hv009, hv007, REGNAME) %>%
  rename(latitude = latitude,
         longitude = longitude,
         cluster_no = hv001,
         mosquito_bednet = hv227,
         children_under5 = hv228,
         state_code = hv024,
         members = hv009,
         year = hv007,
         regname = REGNAME
  )


#2013
#Read the data
data4 <- read_dta("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2013_DHS_02032024_1921_197159/NGKR6ADT/NGKR6AFL.DTA")

# Extract coordinates
dhs_shapefile4 <- st_read("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2013_DHS_02032024_1921_197159/NGGE6AFL/NGGE6AFL.shp")
coordinates <- st_coordinates(dhs_shapefile4)
coordinates_df4 <- data.frame(
  v001 = dhs_shapefile4$DHSCLUST,
  latitude = coordinates[, "Y"],
  longitude = coordinates[, "X"]  
)

#merge coordinates with data by clusters
merged_data4<- merge(coordinates_df4, data4, by = "v001")

#select necessary columns
merged_data4_new<-select(merged_data4, "latitude", "longitude","v459","v460", "v461", "sstate", "v007", "v002", "v136", "v001")

#merge new dataframe with reg_data
combined_data4 <- left_join(merged_data4_new, reg_data, by = c("sstate" = "REGCODE"))

#rename column names
combined_data4 <- combined_data4 %>%
  select(latitude, longitude, v459, v460, v461, sstate, v007,v002,v136,v001, REGNAME) %>%
  rename(
    latitude = latitude,
    longitude = longitude,
    mosquito_bednet = v459,
    children_under5 = v460,
    respondent = v461,
    state_code = sstate,
    year = v007,
    house_no = v002,
    members = v136,
    cluster_no = v001,
    regname = REGNAME
  )

#2015
#Read the data
data5 <- read_dta("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2015_MIS_02032024_1918_197159/NGKR71DT/NGKR71FL.DTA")

# Extract coordinates
dhs_shapefile5 <- st_read("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2015_MIS_02032024_1918_197159/NGGE71FL/NGGE71FL.shp")
coordinates <- st_coordinates(dhs_shapefile5)
coordinates_df5 <- data.frame(
  v001 = dhs_shapefile5$DHSCLUST,
  latitude = coordinates[, "Y"],
  longitude = coordinates[, "X"]  
)

#merge coordinates with data by clusters
merged_data5<- merge(coordinates_df5, data5, by = "v001")

#select necessary columns
merged_data5_new<-select(merged_data5, "latitude", "longitude","v459","v460", "v461", "sstate", "v007", "v002", "v136", "v001")

#merge new dataframe with reg_data
combined_data5 <- left_join(merged_data5_new, reg_data, by = c("sstate" = "REGCODE"))

#rename column names
combined_data5 <- combined_data5 %>%
  select(latitude, longitude, v459, v460, v461, sstate, v007,v002,v136, v001, REGNAME) %>%
  rename(
    latitude = latitude,
    longitude = longitude,
    mosquito_bednet = v459,
    children_under5 = v460,
    respondent = v461,
    state_code = sstate,
    year = v007,
    house_no = v002,
    members = v136,
    cluster_no = v001,
    regname = REGNAME
  )

#2018
#Read the data
data6 <- read_dta("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2018_DHS_02032024_1916_197159/NGKR7BDT/NGKR7BFL.DTA")

# Extract coordinates
dhs_shapefile6 <- st_read("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2018_DHS_02032024_1916_197159/NGGE7BFL/NGGE7BFL.shp")
coordinates <- st_coordinates(dhs_shapefile6)
coordinates_df6 <- data.frame(
  v001 = dhs_shapefile6$DHSCLUST,
  latitude = coordinates[, "Y"],
  longitude = coordinates[, "X"]  
)

#merge coordinates with data by clusters
merged_data6<- merge(coordinates_df6, data6, by = "v001")

#select necessary columns
merged_data6_new<-select(merged_data6, "latitude", "longitude","v459","v460", "v461", "sstate", "v007", "v002", "v136","v001")

#merge new dataframe with reg_data
combined_data6 <- left_join(merged_data6_new, reg_data, by = c("sstate" = "REGCODE"))

#rename column names
combined_data6 <- combined_data6 %>%
  select(latitude, longitude, v459, v460, v461, sstate, v007, v002, v136,v001, REGNAME) %>%
  rename(
    latitude = latitude,
    longitude = longitude,
    mosquito_bednet = v459,
    children_under5 = v460,
    respondent = v461,
    state_code = sstate,
    year = v007,
    house_no = v002,
    members = v136,
    cluster_no =v001,
    regname = REGNAME
  )

# 2021
# Read the data
data7 <- read_dta("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2021_MIS_01222024_2012_197159/NGKR81DT/NGKR81FL.DTA")

# Extract coordinates
dhs_shapefile7 <- st_read("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/NG_2021_MIS_01222024_2012_197159/NGGE81FL/NGGE81FL.shp")
coordinates <- st_coordinates(dhs_shapefile7)
coordinates_df7 <- data.frame(
  v001 = dhs_shapefile7$DHSCLUST,
  latitude = coordinates[, "Y"],
  longitude = coordinates[, "X"]  
)

# Merge coordinates with data by clusters
merged_data7 <- merge(coordinates_df7, data7, by = "v001")

# Select necessary columns and multiply v101 by 10
merged_data7_new <- merged_data7 %>%
  select(latitude, longitude, v459, v460, v461, v101, v007, v002, v136, v001) %>%
  mutate(v101 = v101 * 10)

# Merge new dataframe with reg_data
combined_data7 <- left_join(merged_data7_new, reg_data, by = c("v101" = "REGCODE"))

# Rename column names
combined_data7 <- combined_data7 %>%
  select(latitude, longitude, v459, v460, v461, v101, v007, v002, v136, v001, REGNAME) %>%
  rename(
    mosquito_bednet = v459,
    children_under5 = v460,
    respondent = v461,
    state_code = v101,
    year = v007,
    house_no = v002,
    members = v136,
    cluster_no = v001,
    regname = REGNAME
  )

#prepare datasets for binding

one <- select(combined_data1, latitude, longitude,cluster_no,members, mosquito_bednet, year, state_code,regname)
two <- select(combined_data2, latitude, longitude,cluster_no, members, mosquito_bednet,year, state_code,regname)
three <- select(combined_data3, latitude, longitude,cluster_no, members,mosquito_bednet, year, state_code,regname)
four <- select(combined_data4, latitude, longitude,cluster_no, members,mosquito_bednet, year, state_code,regname)
five <- select(combined_data5, latitude, longitude,cluster_no, members, mosquito_bednet, year, state_code,regname)
six <- select(combined_data6, latitude, longitude,cluster_no, members, mosquito_bednet, year, state_code,regname)
seven <- select(combined_data7, latitude, longitude,cluster_no, members,mosquito_bednet, year, state_code,regname)


one <- na.omit(one)
two <- na.omit(two)
three <- na.omit(three)
four <- na.omit(four)
five <- na.omit(five)
six <- na.omit(six)
seven <- na.omit(seven)


agg_data_one <- one %>%
  group_by(cluster_no) %>%
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    year = first(year),
    regname = first(regname),
    total_members = sum(members),
    total_bednet = sum(mosquito_bednet)
  )

agg_data_two <- two %>%
  group_by(cluster_no) %>%
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    year = first(year),
    regname = first(regname),
    total_members = sum(members),
    total_bednet = sum(mosquito_bednet)
  )

agg_data_three <- three %>%
  group_by(cluster_no) %>%
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    year = first(year),
    regname = first(regname),
    total_members = sum(members),
    total_bednet = sum(mosquito_bednet)
  )

agg_data_four <- four %>%
  group_by(cluster_no) %>%
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    year = first(year),
    regname = first(regname),
    total_members = sum(members),
    total_bednet = sum(mosquito_bednet)
  )

agg_data_five <- five %>%
  group_by(cluster_no) %>%
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    year = first(year),
    regname = first(regname),
    total_members = sum(members),
    total_bednet = sum(mosquito_bednet)
  )

agg_data_six <- six %>%
  group_by(cluster_no) %>%
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    year = first(year),
    regname = first(regname),
    total_members = sum(members),
    total_bednet = sum(mosquito_bednet)
  )


agg_data_seven <- seven %>%
  group_by(cluster_no) %>%
  summarise(
    latitude = first(latitude),
    longitude = first(longitude),
    year = first(year),
    regname = first(regname),
    total_members = sum(members),
    total_bednet = sum(mosquito_bednet)
  )


combined_data <- rbind(agg_data_one, agg_data_two, agg_data_three, agg_data_four, 
                       agg_data_five, agg_data_six, agg_data_seven)

#Calculate NPC
combined_data$npc <- combined_data$total_bednet / combined_data$total_members

#Modelling
combined_data_coord <- as.matrix(select(combined_data, longitude, latitude))

# Convert combined_data to an sf object
combined_data_sf <- st_as_sf(combined_data, coords = c("longitude", "latitude"), crs = st_crs(Nigeria_sf))

# Identify points that are within Nigeria's boundaries
inside_nigeria <- st_intersects(combined_data_sf, Nigeria_sf, sparse = FALSE)

# Filter out points outside Nigeria's boundaries
combined_data_filtered <- combined_data_sf[which(rowSums(inside_nigeria) > 0), ]

# Convert back to data frame
combined_data_filtered <- as.data.frame(combined_data_filtered)

# Extract coordinates from the geometry column
combined_data_filtered <- combined_data_filtered %>%
  mutate(longitude = st_coordinates(geometry)[,1],
         latitude = st_coordinates(geometry)[,2])

# Create the matrix
combined_data_coord <- as.matrix(select(combined_data_filtered, longitude, latitude))
Nigeria_sf <- st_read("C:/Users/faith/OneDrive/Desktop/FAITH'S THESIS/DHS MALARIA/Nigeria 2018 DHS - sdr_subnational_boundaries_2024-10-03/shps/sdr_subnational_boundaries2.shp")

# Plot the mesh
Nigeria_border <- st_union(Nigeria_sf)
Nigeria_boundary <- inla.sp2segment(Nigeria_border)
ng.mesh <- inla.mesh.2d(
  boundary = Nigeria_boundary, 
  loc = combined_data_coord, 
  max.edge = c(0.5, 1.25), 
  offset = c(0.1, 1.5), 
  cutoff = 0.1
)
plot(ng.mesh, asp=1, main='')
points(combined_data_coord, col = "black", pch = 16, cex = 0.4)

spde <- inla.spde2.pcmatern(
  mesh = ng.mesh, 
  prior.range = c(0.5, 0.01), 
  prior.sigma = c(1, 0.01)
)

timesn <- length(unique(combined_data_filtered$year))
indexs <- inla.spde.make.index("s",
                               n.spde = spde$n.spde,
                               n.group = timesn
)
group <- as.numeric(factor(combined_data_filtered$year)) -  min(as.numeric(factor(combined_data_filtered$year))) + 1
time_g = group

A <- inla.spde.make.A(mesh = ng.mesh, loc = combined_data_coord, group = group)
raster_file <- "C:/Users/faith/OneDrive/Desktop/PROJECT 1/SSA/Alt/NGA_msk_alt/NGA_msk_alt.grd"

r <- raster(raster_file)
ag <- aggregate(r, fact = 5, fun = mean)
agpreddata <- rasterToPoints(ag)
coordpred <- agpreddata[, c("x", "y")]
coordpred <- rbind(cbind(coordpred, 1), cbind(coordpred, 2), cbind(coordpred, 3), 
                   cbind(coordpred, 4), cbind(coordpred, 5), cbind(coordpred, 6), 
                   cbind(coordpred, 7))
coop <- coordpred[, 1:2]
groupp <- coordpred[, 3]
Ap <- inla.spde.make.A(mesh = ng.mesh, loc = coop, group = groupp)

# Add a small constant 
combined_data_filtered$npc <- combined_data_filtered$npc + 0.000001

stk.e <- inla.stack(
  tag = "est",
  data = list(y = combined_data_filtered$npc),
  A = list(1, A, 1),
  effects = list(data.frame(b0 = rep(1, nrow(combined_data_filtered))), s = indexs, t = time_g)
)

stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = rep(1, nrow(coordpred))), s = indexs)
)
stk.full <- inla.stack(stk.e, stk.p)

rprior <- list(theta = list(prior = "pccor1", param = c(0, 0.9)))
summary(combined_data_filtered$npc)
formula <- y ~ 0 + b0 + 
  (f(s, model = spde, group = s.group, control.group = list(model = "ar1", hyper = rprior)) +
     f(t, model = "ar1"))

res <- inla(formula, family="beta", data = inla.stack.data(stk.full), control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full)))


# Time plot
time_effects <- res$summary.random$t
time_effects
time_df <- data.frame(
  year = unique(combined_data$year),
  mean = time_effects$mean,
  sd = time_effects$sd,
  lower_0.025 = time_effects$`0.025quant`,
  upper_0.975 = time_effects$`0.975quant`
)

ggplot(time_df, aes(x = year, y = mean, group = 1)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = lower_0.025, ymax = upper_0.975), alpha = 0.2) +
  labs(title = NULL,
       x = "Year",
       y = "Effect") +
  theme_minimal()


#Nets per cluster plot
index <- inla.stack.index(stack = stk.full, tag = "pred")$data
coordpred <- data.frame(coordpred)
names(coordpred) <- c("x", "y", "time")
coordpred$pred_mean <- res$summary.fitted.values[index, "mean"]
coordpred$pred_mean <- exp(coordpred$pred_mean) / (1 + exp(coordpred$pred_mean))
dpm <- melt(coordpred,
            id.vars = c("x", "y", "time"),
            measure.vars = c("pred_mean")
)

colnames(dpm)[colnames(dpm) == "value"] <- "pred_mean"

Nigeria_sp <- as(Nigeria_sf, "Spatial")
Nigeria_sp_fortified <- fortify(Nigeria_sp)

# Create a mapping of time to actual years
year_labels <- c("1" = "2003", "2" = "2008", "3" = "2010", 
                 "4" = "2013", "5" = "2015", "6" = "2018", 
                 "7" = "2021")


Nigeria_lga_sf <- st_read("C:/Users/faith/OneDrive/Desktop/Practice_sessions_on anemia_prevalence/Practice_sessions_on anemia_prevalence/gadm41_NGA_shp/gadm41_NGA_2.shp")


ggplot() +
  geom_sf(data = Nigeria_sf, fill = NA, color = "black") +  
  coord_sf(datum = NA) +
  
  geom_tile(data = dpm, aes(x = x, y = y, fill = pred_mean)) +
  
  geom_sf(data = Nigeria_lga_sf, fill = NA, color = "light grey", size = 0.1) +
  
  geom_polygon(data = Nigeria_sp_fortified, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", lwd=0.5) +
  
  labs(x = "", y = "", fill = "Predicted Value") +
  
  facet_wrap(~time, labeller = labeller(time = year_labels)) +
  
  scale_fill_gradientn(colors = brewer.pal(11, "RdYlBu"), name = "") +
  
  
  theme_bw() +
  theme(panel.grid = element_blank())


# Deviation plot
coordpred$lower_0.025 <- res$summary.fitted.values[index, "0.025quant"]
coordpred$upper_0.975 <- res$summary.fitted.values[index, "0.975quant"]
coordpred$deviation <- coordpred$upper_0.975 - coordpred$lower_0.025
dpm$deviation <- coordpred$deviation


ggplot() +
  geom_sf(data = Nigeria_sf, fill = NA, color = "black") +  
  coord_sf(datum = NA) +
  geom_tile(data = dpm, aes(x = x, y = y, fill = deviation)) +
  geom_sf(data = Nigeria_lga_sf, fill = NA, color = "light grey", size = 0.1) +
  geom_polygon(data = Nigeria_sp_fortified, aes(x = long, y = lat, group = group), fill = "transparent", color = "black") +
  labs(x = "", y = "", fill = "Deviation") + 
  facet_wrap(~time, labeller = labeller(time = year_labels)) +
  scale_fill_gradientn(colors =  rev(brewer.pal(11, "RdYlBu")), name = "") +
  theme_bw() +
  theme(panel.grid = element_blank())

