# Bring the FedInvest history data up to date
# ===========================================

# =============================================================
loaded_file_names <- load(file="../R/sysdata.rda")

print("LIST OF LOADED FILES")
print(loaded_file_names)
# =============================================================

backup <- FedInvest_historical_data

FedInvest_historical_data <- FedInvestData()

# NOTE: make sure you include all saved internal files
# ====================================================
devtools::use_data(FRB_H15_1962_2015_mod, FedInvest_historical_data,
                   internal = TRUE, overwrite = TRUE, compress = "bzip2")

# determine the best compression scheme
# use in 'compress' parameter of
tools::checkRdaFiles("../R")
