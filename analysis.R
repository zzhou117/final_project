final_data <- readRDS(file.path(sourcedir, "final_data.rds"))

setorder(final_data, ccn, bpcia_1)
final_data <- final_data[!is.na(bpcia_1)]

final_data <- final_data[
  , .(
    PositiveNegativeReconciliationA1 = max(PositiveNegativeReconciliationA_1, na.rm = TRUE),
    PositiveNegativeReconciliationA3 = max(PositiveNegativeReconciliationA_3, na.rm = TRUE),
    PositiveNegativeReconciliationA4 = max(PositiveNegativeReconciliationA_4, na.rm = TRUE),
    PositiveNegativeReconciliationA5 = max(PositiveNegativeReconciliationA_5, na.rm = TRUE),
    bpcia1 = tail(na.omit(bpcia_1), 1),  # last non-NA
    teaching = teaching[1],
    urban = urban[1],
    safety_net = safety_net[1],
    bed_size = bed_size[1],
    census_division = census_division[1],
    hospsize = hospsize[1]
  ),
  by = ccn
]

# Characteristic differences between BPCI-A and non-BPCI-A hospitals
final_data[, bed_size := factor(bed_size)]
final_data[, census_division := factor(census_division)]
model <- glm(
  bpcia1 ~ teaching + urban + safety_net + bed_size + census_division,
  data = final_data,
  family = binomial(link = "logit")
)
summary(model)

final_data <- final_data[PositiveNegativeReconciliationA1 != -Inf]
if ("leave" %in% names(final_data)) {
  final_data[, leave := NULL]
}
final_data[PositiveNegativeReconciliationA1 != -Inf & PositiveNegativeReconciliationA3 == -Inf, leave := 1]
final_data[PositiveNegativeReconciliationA1 != -Inf & PositiveNegativeReconciliationA3 != -Inf, leave := 0]
final_data <- final_data[order(PositiveNegativeReconciliationA1)]

# # Run RD estimation
# rd_result <- rdrobust(
#   y = final_data$leave,
#   x = final_data$PositiveNegativeReconciliationA1,
#   c = 0,
#   masspoints = "check",
#   bwselect = "msetwo"
# )
# summary(rd_result)

# # Plot the RD results
# plot_data <- final_data[
#   !is.na(leave) &
#   is.finite(PositiveNegativeReconciliationA1)
# ]
# rdplot(
#   y = plot_data$leave,
#   x = plot_data$PositiveNegativeReconciliationA1,
#   c = 0,               
#   p = 1,                  
#   nbins = c(6, 10),       
#   masspoints = "check"     
# )


# MY 1&2 to MY3 (Hospital Clinical Episode Category Withdraw)
final_data <- readRDS(file.path(sourcedir, "final_data.rds"))
setorder(final_data, ccn, bpcia_1)
final_data <- final_data[!(is.infinite(bpcia_1) | bpcia_1 == 0)]
if ("leave" %in% names(final_data)) {
  final_data[, leave := NULL]
}

# Define leave variable
final_data[
  !is.na(PositiveNegativeReconciliation_1) & is.na(PositiveNegativeReconciliation_3),
  leave := 1
]
final_data[
  !is.na(PositiveNegativeReconciliation_1) & !is.na(PositiveNegativeReconciliation_3),
  leave := 0
]

setorder(final_data, PositiveNegativeReconciliation_1)


rd_data <- final_data[
  !is.na(leave) & !is.na(PositiveNegativeReconciliation_1)
]
rd_result <- rdrobust(
  y = rd_data$leave,
  x = rd_data$PositiveNegativeReconciliation_1,
  c = 0,
  masspoints = "check",
  bwselect = "msetwo"
)
summary(rd_result)

# Plot the RD results
rd_data <- final_data[
  !is.na(leave) &
  !is.na(PositiveNegativeReconciliation_1) &
  PositiveNegativeReconciliation_1 >= -15000 &
  PositiveNegativeReconciliation_1 <= 15000
]
rdplot(
  y = rd_data$leave,
  x = rd_data$PositiveNegativeReconciliation_1,
  c = 0,                      # cutoff
  p = 1,                      # linear polynomial
  nbins = c(7, 7),            # 7 bins left, 7 bins right
  masspoints = "check",
  binselect = "es",
  y.label = "Probability of Leaving",
  x.label = "Average Reconciliation Payment from Previous Year",
  title = "Hospital Clinical Episode Category Level Plot"     
)
