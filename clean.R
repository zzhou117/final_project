df <- haven::read_dta(file.path(sourcedir, "BPCIA_Data_Raw.dta")) %>% as.data.table()

# Calculate mean_target_price_dif
df <- df %>%
  mutate(mean_target_price_dif = (prelim_target_price - mean_target_price) / mean_target_price)

# Create group indicators
df <- df %>%
mutate(
    service_line_g = as.integer(factor(service_line)),
    episode_description_g = as.integer(factor(episode_description)),
    setting_g = as.integer(factor(ClinicalEpisodeTypeIPOP)),
    prelim_target_price_ln = log(prelim_target_price),
    hospid = as.integer(factor(ccn))
)

impact <- haven::read_dta(file.path(sourcedir, "Impact2018_2021_Raw.dta")) %>% as.data.table()
# Fix the medicare percentage
impact[medicarepercentage > 1, medicarepercentage := NA_real_]
impact[, mean_medicarepercentage := mean(medicarepercentage, na.rm = TRUE), by = ccn]
impact[is.na(medicarepercentage), medicarepercentage := mean_medicarepercentage]

# Merge the data frames
simulation_data <- merge(df, impact, by = c("ccn", "model_year"), all.x = TRUE)

# Make IPOP numerical
simulation_data[, ClinicalEpisodeTypeIPOP_g := as.integer(factor(ClinicalEpisodeTypeIPOP))]

# Save the dataset
saveRDS(simulation_data, file = file.path(sourcedir, "mandatory_simulation_data.rds"))

# Load the hospital characteristics data
hosp_char <- haven::read_dta(file.path(sourcedir, "hospital_characteristics_my3.dta")) %>%
  as.data.table()

final_data <- merge(simulation_data, hosp_char, by = "ccn")

# Drop unnecessary variables
final_data[, c(
  "EpisodeInitiatorBPID", "CQS", "hospid", "region", "urgeo", "urspa", "rday",
  "beds", "averagedailycensus", "dshpct", "operatingccr", "bills", "cmi", 
  "proxyvaluebasedpurchasingadj", "readmissionadjustmentfactor", "proxyqualityreduction",
  "proxyehrreduction", "proxyreadmissionadjustmentfac", "medicarepercentage", 
  "urgeo_g", "region_g", "mean_medicarepercentage"
) := NULL]

# Rename variables
setnames(final_data, old = "PositiveNegativeReconciliation_A", new = "PositiveNegativeReconciliationA")
setnames(final_data, old = "bpci_a", new = "bpcia")

# Reshape to wide format
final_data <- dcast(
  final_data,
  ccn + ClinicalEpisodeTypeIPOP + episode_description + service_line + teaching + urban + safety_net + bed_size + census_division ~ model_year,
  value.var = c(
    "PositiveNegativeReconciliationA", "PositiveNegativeReconciliation", 
    "episode_count", "episodes", "prelim_target_price", "mean_target_price", 
    "mean_target_price_dif", "prelim_target_price_ln", "bpcia"
  )
)
setorder(final_data, ccn, bpcia_1)

# Convert character columns to numeric
final_data$teaching <- as.numeric(as.character(final_data$teaching))
final_data$urban <- as.numeric(as.character(final_data$urban))
final_data$safety_net <- as.numeric(as.character(final_data$safety_net))
final_data$bed_size <- as.numeric(as.character(final_data$bed_size))
final_data$census_division <- as.numeric(as.character(final_data$census_division))

# Recode variables
final_data[census_division == -9, census_division := NA]
if ("hospsize" %in% names(final_data)) {
  final_data[, hospsize := NULL]
}
final_data[bed_size == 1, hospsize := 0]
final_data[bed_size %in% c(2, 3, 4), hospsize := 1]
final_data[teaching == 2, teaching := 0]
final_data[urban == 2, urban := 0]
final_data[safety_net == 2, safety_net := 0]

saveRDS(final_data, file = file.path(sourcedir, "final_data.rds"))