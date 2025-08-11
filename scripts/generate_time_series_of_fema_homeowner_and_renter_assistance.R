# Packages ----

# Set the package names to read in
packages <- c("tidyverse", "openxlsx", "xts")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

# File paths ----

# Download the latest data here: https://www.fema.gov/about/openfema/data-sets#individual

disaster_declarations_summaries_file_path <- "inputs/DisasterDeclarationsSummaries.csv"
disaster_inidividual_assistance_owners_file_path <- "inputs/HousingAssistanceOwners.csv"
disaster_inidividual_assistance_renters_file_path <- "inputs/HousingAssistanceRenters.csv"

date_last_updated <- Sys.Date()

output_file_path <- paste0("outputs/fema_homeowner_and_renter_assistance_time_series_", date_last_updated, ".xlsx")

# Reading in data ----

disaster_summaries <- read.csv(disaster_declarations_summaries_file_path) 
disaster_inidividual_assistance_owners <- read.csv(disaster_inidividual_assistance_owners_file_path) 
disaster_inidividual_assistance_renters <- read.csv(disaster_inidividual_assistance_renters_file_path) 

# Clean the data ----

disaster_summaries <- disaster_summaries %>%
  select(femaDeclarationString:incidentType, incidentBeginDate:incidentEndDate, hash:id) %>%
  mutate(declarationDate = as.Date(declarationDate),
         incidentBeginDate = as.Date(incidentBeginDate),
         incidentEndDate = as.Date(incidentEndDate)) %>%
  distinct(disasterNumber, .keep_all = TRUE)

disaster_inidividual_assistance_owners <- disaster_inidividual_assistance_owners %>%
  left_join(disaster_summaries, by = 'disasterNumber') %>%
  mutate(declarationDate = as.yearmon(declarationDate)) %>%
  filter(!state.x %in% c('MP', 'GU', 'AS', 'PR', 'FM', 'VI'))

disaster_inidividual_assistance_renters <- disaster_inidividual_assistance_renters %>%
  left_join(disaster_summaries, by = 'disasterNumber') %>%
  mutate(declarationDate = as.yearmon(declarationDate)) %>%
  filter(!state.x %in% c('MP', 'GU', 'AS', 'PR', 'FM', 'VI'))

# Create time series for owners ----

monthly_time_series_owners <- disaster_inidividual_assistance_owners %>%
  group_by(declarationDate, county, state.x) %>%
  summarize(total_damage = sum(totalDamage, na.rm = T),
            total_rr_amt = sum(repairReplaceAmount, na.rm = T),
            total_rental_amt = sum(rentalAmount, na.rm = T),
            total_other_needs_amt = sum(otherNeedsAmount, na.rm = T),
            total_inspections = sum(totalInspected, na.rm = T),
            total_approvals = sum(approvedForFemaAssistance, na.rm = T),
            total_approved_ihp_amount = sum(totalApprovedIhpAmount, na.rm = T),
  ) %>%
  ungroup() %>%
  rename(state = state.x)

quarterly_time_series_owners <- disaster_inidividual_assistance_owners %>%
  mutate(declarationDate = as.yearqtr(declarationDate)) %>%
  group_by(declarationDate, county, state.x) %>%
  summarize(total_damage = sum(totalDamage, na.rm = T),
            total_rr_amt = sum(repairReplaceAmount, na.rm = T),
            total_rental_amt = sum(rentalAmount, na.rm = T),
            total_other_needs_amt = sum(otherNeedsAmount, na.rm = T),
            total_inspections = sum(totalInspected, na.rm = T),
            total_approvals = sum(approvedForFemaAssistance, na.rm = T),
            total_approved_ihp_amount = sum(totalApprovedIhpAmount, na.rm = T),
  ) %>%
  ungroup() %>%
  rename(state = state.x)

annual_time_series_owners <- disaster_inidividual_assistance_owners %>%
  mutate(declarationDate = year(declarationDate)) %>%
  group_by(declarationDate, county, state.x) %>%
  summarize(total_damage = sum(totalDamage, na.rm = T),
            total_rr_amt = sum(repairReplaceAmount, na.rm = T),
            total_rental_amt = sum(rentalAmount, na.rm = T),
            total_other_needs_amt = sum(otherNeedsAmount, na.rm = T),
            total_inspections = sum(totalInspected, na.rm = T),
            total_approvals = sum(approvedForFemaAssistance, na.rm = T),
            total_approved_ihp_amount = sum(totalApprovedIhpAmount, na.rm = T),
  ) %>%
  ungroup() %>%
  rename(state = state.x)

# Create time series for renters ----

monthly_time_series_renters <- disaster_inidividual_assistance_renters %>%
  group_by(declarationDate, county, state.x) %>%
  summarize(total_rr_amt = sum(repairReplaceAmount, na.rm = T),
            total_rental_amt = sum(rentalAmount, na.rm = T),
            total_other_needs_amt = sum(otherNeedsAmount, na.rm = T),
            total_inspections = sum(totalInspected, na.rm = T),
            total_approvals = sum(approvedForFemaAssistance, na.rm = T),
            total_approved_ihp_amount = sum(totalApprovedIhpAmount, na.rm = T),
  ) %>%
  ungroup() %>%
  rename(state = state.x)

quarterly_time_series_renters <- disaster_inidividual_assistance_renters %>%
  mutate(declarationDate = as.yearqtr(declarationDate)) %>%
  group_by(declarationDate, county, state.x) %>%
  summarize(total_rr_amt = sum(repairReplaceAmount, na.rm = T),
            total_rental_amt = sum(rentalAmount, na.rm = T),
            total_other_needs_amt = sum(otherNeedsAmount, na.rm = T),
            total_inspections = sum(totalInspected, na.rm = T),
            total_approvals = sum(approvedForFemaAssistance, na.rm = T),
            total_approved_ihp_amount = sum(totalApprovedIhpAmount, na.rm = T),
  ) %>%
  ungroup() %>%
  rename(state = state.x)

annual_time_series_renters <- disaster_inidividual_assistance_renters %>%
  mutate(declarationDate = year(declarationDate)) %>%
  group_by(declarationDate, county, state.x) %>%
  summarize(total_rr_amt = sum(repairReplaceAmount, na.rm = T),
            total_rental_amt = sum(rentalAmount, na.rm = T),
            total_other_needs_amt = sum(otherNeedsAmount, na.rm = T),
            total_inspections = sum(totalInspected, na.rm = T),
            total_approvals = sum(approvedForFemaAssistance, na.rm = T),
            total_approved_ihp_amount = sum(totalApprovedIhpAmount, na.rm = T),
  ) %>%
  ungroup() %>%
  rename(state = state.x)

# Join the data ----

names(annual_time_series_owners)[4:10] <- paste0(names(annual_time_series_owners)[4:10], "_owner")
names(annual_time_series_renters)[4:9] <- paste0(names(annual_time_series_renters)[4:9], "_renter")
annual_time_series <- annual_time_series_owners %>%
  left_join(annual_time_series_renters, by = c('declarationDate', 'county', 'state'))

annual_time_series <- annual_time_series %>%
  mutate(total_approved_ihp_amount = total_approved_ihp_amount_owner + total_approved_ihp_amount_renter,
         total_rr_amt = total_rr_amt_owner + total_rr_amt_renter,
         total_rental_amt = total_rental_amt_owner + total_rental_amt_renter,
         total_other_needs_amt = total_other_needs_amt_owner + total_other_needs_amt_renter,
         total_approvals = total_approvals_owner + total_approvals_renter,
         total_inspections = total_inspections_owner + total_inspections_renter) %>%
  select(declarationDate:state, total_damage_owner, total_approved_ihp_amount:total_inspections)

names(quarterly_time_series_owners)[4:10] <- paste0(names(quarterly_time_series_owners)[4:10], "_owner")
names(quarterly_time_series_renters)[4:9] <- paste0(names(quarterly_time_series_renters)[4:9], "_renter")
quarterly_time_series <- quarterly_time_series_owners %>%
  left_join(quarterly_time_series_renters, by = c('declarationDate', 'county', 'state'))

quarterly_time_series <- quarterly_time_series %>%
  mutate(declarationDate = as.Date(declarationDate),
         total_approved_ihp_amount = total_approved_ihp_amount_owner + total_approved_ihp_amount_renter,
         total_rr_amt = total_rr_amt_owner + total_rr_amt_renter,
         total_rental_amt = total_rental_amt_owner + total_rental_amt_renter,
         total_other_needs_amt = total_other_needs_amt_owner + total_other_needs_amt_renter,
         total_approvals = total_approvals_owner + total_approvals_renter,
         total_inspections = total_inspections_owner + total_inspections_renter) %>%
  select(declarationDate:state, total_damage_owner, total_approved_ihp_amount:total_inspections)

names(monthly_time_series_owners)[4:10] <- paste0(names(monthly_time_series_owners)[4:10], "_owner")
names(monthly_time_series_renters)[4:9] <- paste0(names(monthly_time_series_renters)[4:9], "_renter")
monthly_time_series <- monthly_time_series_owners %>%
  left_join(monthly_time_series_renters, by = c('declarationDate', 'county', 'state'))

monthly_time_series <- monthly_time_series %>%
  mutate(declarationDate = as.Date(declarationDate),
         total_approved_ihp_amount = total_approved_ihp_amount_owner + total_approved_ihp_amount_renter,
         total_rr_amt = total_rr_amt_owner + total_rr_amt_renter,
         total_rental_amt = total_rental_amt_owner + total_rental_amt_renter,
         total_other_needs_amt = total_other_needs_amt_owner + total_other_needs_amt_renter,
         total_approvals = total_approvals_owner + total_approvals_renter,
         total_inspections = total_inspections_owner + total_inspections_renter) %>%
  select(declarationDate:state, total_damage_owner, total_approved_ihp_amount:total_inspections)


# Output data ----

dataset_list <- list('Monthly' = monthly_time_series,
                     'Quarterly' = quarterly_time_series,
                     'Annual' =  annual_time_series)

write.xlsx(dataset_list, output_file_path)
