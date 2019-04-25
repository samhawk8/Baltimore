############################################
# Baltimore EMS - Temperature Analysis #####
############################################
rm(list=ls())
gc()

#######################
#### Load Packages ####
#######################

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)

#######################
#### Load EMS Data ####
#######################

# Load EMS data, which is contained in five separate Excel files.
# Note that we have complete years for 2014, 2015, 2016, 2017 and partial (1.1 - 11.30) for 2018.

EMS_2014 <- read.csv("2014.csv")
EMS_2015 <- read.csv("2015.csv")
EMS_2016 <- read.csv("2016.csv")
EMS_2017 <- read.csv("2017.csv")
EMS_2018_partial <- read.csv("2018-1.1-11.30.csv")

# Bind together into one dataframe

EMS_all <- bind_rows(EMS_2014, EMS_2015, EMS_2016, EMS_2017, EMS_2018_partial)

# Remove all dataframes but EMS_all

rm(list=setdiff(ls(), "EMS_all"))

#######################
### Clean EMS Data ####
#######################

# Rename columns to get rid of funky codes

EMS_all <- EMS_all %>%
  rename(incident_date = Incident.Date,
         incident_number = Incident.Number,
         primary_impression =  Primary.Impression,
         arrived_on_scene_time = Times...Arrived.on.Scene.Time,
         zipcode = Incident.Postal.Code..E8.15.,
         destination_patient_disposition = Destination.Patient.Disposition..E20.10.,
         destination_code = Destination.Code..E20.2.
  )

# Filter EMS all to remove the dispositions we don't want.

EMS_all <- EMS_all %>%
  filter(destination_patient_disposition != "" &
           destination_patient_disposition != "No Patient Found" &
           destination_patient_disposition != "Cancelled en Route/On Arrival" &
           destination_patient_disposition != "Cancelled Prior to Response" &
           destination_patient_disposition != "Provided ALS personnel to scene, no transport" &
           destination_patient_disposition != "Standby Only - No Patient Contacts" &
           destination_patient_disposition != "Cancelled On Arrival" &
           destination_patient_disposition != "Operational Support Provided Only")

# Remove N/A and blank values
# Later redo this the tidyverse way
EMS_all <- EMS_all[-which(EMS_all$arrived_on_scene_time=="" | is.na( EMS_all$incident_date) | EMS_all$incident_number=="" | EMS_all$primary_impression=="" | EMS_all$zipcode=="" | EMS_all$destination_patient_disposition==""), ]

# remove all characters from ZIP code field
# Later redo this the tidyverse way
v1 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
EMS_all$zipcode <- gsub(paste0("[^", paste(v1, collapse=""), "]+"), "", EMS_all$zipcode)

# Filter for only Baltimore ZIP codes used in our analysis
# Later redo this the tidyverse way
balt_zips <- c("21201","21202","21205","21206","21207","21208","21209","21210","21211","21212","21213","21214", "21215","21216","21217","21218","21222","21223","21224","21225","21226","21227","21228","21229", "21230","21231","21234","21236","21237","21239","21251")

EMS_all <- subset(EMS_all, zipcode %in% balt_zips )

# create full date/time object of arrival time
EMS_all <- EMS_all %>%
  mutate(arrive_time = as.POSIXct(paste(incident_date, arrived_on_scene_time), format="%m/%d/%y %H:%M:%S"))

# add temperature time object by forcing time to closest temp time (every hour at 54 min and 0 sec)
EMS_all <- EMS_all  %>%
  mutate(temp_time = case_when((minute(EMS_all$arrive_time) < 24) ~ update((EMS_all$arrive_time - as.difftime(1, unit="days")), minutes = 54, seconds = 0),
                               (minute(EMS_all$arrive_time) >= 24)  ~ update(EMS_all$arrive_time, minutes = 54, seconds = 0)))
update((EMS_all$arrive_time - as.difftime(1, unit="days")), minutes = 54, seconds = 0)

# remove seconds from temperature time
EMS_all$temp_time <- format(as.POSIXct(EMS_all$temp_time), "%d-%m-%Y %H:%M")

# Create categories for conditions we care about
EMS_all <- EMS_all %>%
  mutate(primary_impression_group = if_else(primary_impression %in% c("Pain", "Other", "Abdominal Pain/Problems", "Weakness", "Other Illness/Injury", "Nausea/Vomiting (Unknown Etiology)", "General Malaise/Sick", "Back Pain (Non-Traumatic)", "Headache", "OB/Gyn - Vaginal Hemorrhage", "Fever", "Unknown Problem", "Poisoning", "Drowning/Near Drowning", "Electrocution", "Inhalation Injury (Toxic Gas)", "Lightening Strike", "Depressurization/Scuba", "SIDS (Sudden Infant Death Syndrome)", "Traumatic Injury", "OB/Gyn - Other", "OB/Gyn - OB/Delivery", "Sepsis",  "Other CNS Problem", "Migraine", "Allergic Reaction", "Abuse / Neglect", "Stings/Venomous Bites", "Airway Obstruction", "Anaphylaxis", "Hypovolemia/Shock", "Overpressurization", "Hazmat Exposure - Chem, Bio, Rad", "Inhalation - Smoke", "Sexual Assault/Rape", "Other Endocrine/Metabolic Problem", "Bowel Obstruction", "Apparent Life-Threatening Event", "Diarrhea", "Burn", "Pregnancy/OB Delivery", "G.I. Bleed", "Other GU Problems", "Patient Assist Only", "Not Applicable", "Other Abdominal/GI Problem", "No Apparent Illness/Injury"), "Other", primary_impression),
         primary_impression_category = case_when(
           primary_impression_group %in% c("ETOH Abuse", "Withdrawal/Overdose ETOH", "Withdrawal/Overdose Drugs", "Altered Level of Consciousness", "Substance/Drug Abuse") ~ "Substance abuse",
           primary_impression_group %in% c("Cardiac Arrest", "Cardiac Rhythm Disturbance", "Hypotension", "Hypertension", "CHF (Congestive Heart Failure)", "Chest Pain/Discomfort", "Chest Pain - STEMI", "Other Cardiovascular Problem", "Abdominal Aortic Aneurysm" ) ~ "Heart/circulatory",
           primary_impression_group %in% c("Heat Exhaustion/Heat Stroke", "Hyperthermia" ) ~ "Acute heat conditions",
           primary_impression_group %in% c("Diabetic Hypoglycemia", "Diabetic Hyperglycemia") ~ "Diabetes complication",
           primary_impression_group %in% c("COPD (Emphysema/Chronic Bronchitis)", "Asthma", "Respiratory Distress", "Respiratory Arrest", "Croup") ~ "Respiratory",
           primary_impression_group %in% c("Stroke/CVA", "TIA (Transient Ischemic Attack)") ~ "Stroke",
           TRUE ~ primary_impression_group
         ))

# Save as an RDS file and as a csv for later loading
saveRDS(EMS_all, file = "ems_clean.rds")
write_csv(EMS_all, path = "ems_clean.csv")

#########################
# Load Temperature Data #
#########################

# Load Baltimore Inner Harbor Temperature Data
temp_data <- read_excel("Baltimore-Harbor-Temp-Data.xlsx")

##########################
# Clean Temperature Data #
##########################

# Build a datetime object from DATE and TIME
temp_data  <- temp_data  %>%
  mutate(temp_time = as.POSIXct(paste(DATE, TIME), format="%Y-%m-%d %H:%M"))

# remove seconds from datettime object
temp_data$temp_time <- format(as.POSIXct(temp_data$temp_time), "%d-%m-%Y %H:%M")

# Round temperature
temp_data <- temp_data %>%
  mutate(TEMPERATURE = round(as.numeric(TEMPERATURE),0))

# Take out inconsistent values
temp_data <- temp_data %>%
  filter(str_detect(TIME, ":54")) %>%
  filter(TEMPERATURE != 1) %>%
  distinct()

# Create a temperature bucket in main data set, and a column so we can sort by temp_bucket nicely.
temp_data <- temp_data %>%
  mutate(temp_bucket = case_when(
    TEMPERATURE >= 0 & TEMPERATURE <10 ~ "0s",
    TEMPERATURE >= 10 & TEMPERATURE <20 ~ "10s",
    TEMPERATURE >= 20 & TEMPERATURE <30 ~ "20s",
    TEMPERATURE >= 30 & TEMPERATURE <40 ~ "30s",
    TEMPERATURE >= 40 & TEMPERATURE <50 ~ "40s",
    TEMPERATURE >= 50 & TEMPERATURE <60 ~ "50s",
    TEMPERATURE >= 60 & TEMPERATURE <70 ~ "60s",
    TEMPERATURE >= 70 & TEMPERATURE <80 ~ "70s",
    TEMPERATURE >= 80 & TEMPERATURE <90 ~ "80s",
    TEMPERATURE >= 90 & TEMPERATURE <100 ~ "90s",
    TEMPERATURE >= 100 & TEMPERATURE <110 ~ "100s",
    TEMPERATURE >= 110 & TEMPERATURE <120 ~ "110s"
  ), temp_bucket_order = case_when(
    temp_bucket == "0s" ~ "A",
    temp_bucket == "10s" ~ "B",
    temp_bucket == "20s" ~ "C",
    temp_bucket == "30s" ~ "D",
    temp_bucket == "40s" ~ "E",
    temp_bucket == "50s" ~ "F",
    temp_bucket == "60s" ~ "G",
    temp_bucket == "70s" ~ "H",
    temp_bucket == "80s" ~ "I",
    temp_bucket == "90s" ~ "J",
    temp_bucket == "100s" ~ "K",
    temp_bucket == "110s" ~ "L"
  )
  )

######
# TBD - need to correct for fact that 9190 obs out of 581530 obs in EMS don't have a temperature time.  Need to figure out how to fill in the gps there by cleaning temp data first.  Maybe take average of daily temp.
# full_data_anti <- anti_join(EMS_all, temp_data, by=c("temp_time"))
# full_data_anti_2 <- anti_join(temp_data, EMS_all, by=c("temp_time"))
# full_data_anti_group <- full_data_anti %>%
#  group_by(incident_date) %>%
#  summarise(count = n())
# temp_data_count <- temp_data %>%
#  group_by(year(as_datetime(temp_time))) %>%
#  summarise(count = n())
# glimpse(temp_data)
######

####################################################
# Create dataframe of hourly reading temp counts  ##
####################################################

# Create a dataframe with a count of hourly temperature readings for each degree
temp_count_per_degree <- temp_data %>%
  select(DATE, TIME, TEMPERATURE) %>%
  group_by(TEMPERATURE) %>%
  summarise(temp_count_per_degree=n()) %>%
  arrange(TEMPERATURE)

# Create a dataframe with a count of hourly temperature readings for each bucket
temp_count_per_bucket <- temp_data %>%
  select(DATE, TIME, temp_bucket) %>%
  group_by(temp_bucket) %>%
  summarise(temp_count_per_bucket=n()) %>%
  arrange(temp_bucket)

##########################
# Merge Temp & EMS Data ##
##########################

# Merge data

full_data <- inner_join(EMS_all, temp_data, by=c("temp_time"))

# Save data
saveRDS(full_data, file = "data/modified_data/full_data.rds")
write_csv(full_data, path = "data/modified_data/full_data.csv")

##########################
####### Analysis #########
##########################

# If starting here, read in data
# full_data <- readRDS(file = "full_data.rds")

# Create a data frame that lists total calls by temperature bucket

# Create a dataframe with a count of calls for each degree
all_calls_count_per_degree <- full_data %>%
  select(DATE, TIME, TEMPERATURE) %>%
  group_by(TEMPERATURE) %>%
  summarise(all_calls_count_per_degree=n()) %>%
  arrange(TEMPERATURE)

# Create a dataframe with a count of calls for each degree
all_calls_count_per_bucket<- full_data %>%
  select(DATE, TIME, temp_bucket) %>%
  group_by(temp_bucket) %>%
  summarise(all_calls_count_per_bucket=n()) %>%
  arrange(temp_bucket)

###########################
### Create Matrixes #######
###########################

############################################
###### Method 1: Temp v Call Ratios ########
############################################

# Table 1: Ratio of number of calls for each condition type in each bucket to total number of hours in a given temperature bucket.
# A lower number indicates a higher number of calls for each condition relative to adjusted for the fact that some temperatures -- the 70s -- are simply more common than other temperature buckets.
# Table 2: Standard deviation above and below mean for each ratio (a way of normalizing across conditions).  This helps compare more easily across each category, and compare across categories.

#### Primary Impression Group Table 1 Ratios #####

call_temp_ratio_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(primary_impression_group, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`)
View(call_temp_ratio_primary_impression_group)


#### Primary Impression Group Table 2 Ratio SD #####

sd_call_temp_ratio_primary_impression_group <- full_data %>%
 #  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(primary_impression_group, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`) %>%
  rowwise() %>%
  mutate(call_to_temp_ratio_mean = mean(na.rm=TRUE, c(`0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`))) %>%
  mutate(call_to_temp_ratio_sd = sd(na.rm=TRUE, c(`0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`))) %>%
  mutate_at(vars(contains("0s")), ~((.-call_to_temp_ratio_mean)/call_to_temp_ratio_sd)*-1) %>%
  rename_at(vars(contains("0s")), funs(paste0(.,"_-sd")))

#### Primary Impression Category Table 1 Ratios #####

call_temp_ratio_primary_impression_category <- full_data %>%
  filter(primary_impression_category != "Other") %>%
  group_by(primary_impression_category, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_category, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(primary_impression_category, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`)

#### Primary Impression Category Table 2 Ratio SD #####

sd_call_temp_ratio_primary_impression_category <- full_data %>%
  filter(primary_impression_category != "Other") %>%
  group_by(primary_impression_category, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_category, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(primary_impression_category, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`) %>%
  rowwise() %>%
  mutate(call_to_temp_ratio_mean = mean(na.rm=TRUE, c(`0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`))) %>%
  mutate(call_to_temp_ratio_sd = sd(na.rm=TRUE, c(`0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`))) %>%
  mutate_at(vars(contains("0s")), ~((.-call_to_temp_ratio_mean)/call_to_temp_ratio_sd)*-1) %>%
  rename_at(vars(contains("0s")), funs(paste0(.,"_-sd")))

#### Primary Impression Table 1 Ratios #####

call_temp_ratio_primary_impression <- full_data %>%
  filter(primary_impression != "Other") %>%
  group_by(primary_impression, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(primary_impression, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`)

#### Primary Impression Table 2 Ratio SD #####

sd_call_temp_ratio_primary_impression <- full_data %>%
  filter(primary_impression != "Other") %>%
  group_by(primary_impression, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(primary_impression, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`) %>%
  rowwise() %>%
  mutate(call_to_temp_ratio_mean = mean(na.rm=TRUE, c(`0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`))) %>%
  mutate(call_to_temp_ratio_sd = sd(na.rm=TRUE, c(`0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`))) %>%
  mutate_at(vars(contains("0s")), ~((.-call_to_temp_ratio_mean)/call_to_temp_ratio_sd)*-1) %>%
  rename_at(vars(contains("0s")), funs(paste0(.,"_-sd")))

###############################################################################
###### Method 2: Percent of Calls in Given Bucket with Given Conditinon NEED TO EDIT THIS FUNCTION ########
###############################################################################

# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When it's 20-29 degrees, 6 percent of all calls at that temperature are for Asthma.

##### OUTPUT THIS FOR ADAM #########
#### Primary Impression Group #####
percent_calls_condition_primary_impression_category <- full_data %>%
  # filter(primary_impression_category != "Other") %>%
  group_by(primary_impression_category, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_bucket)*100) %>%
  select(primary_impression_category, temp_bucket, percent_calls_w_condition) %>%
  spread(temp_bucket, percent_calls_w_condition) %>%
  select(primary_impression_category, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`)

a_percent_calls_condition_primary_impression_group <- full_data %>%
  filter(primary_impression_group == "Asthma") %>%
  group_by(primary_impression_group, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n())
write_csv(a_percent_calls_condition_primary_impression_group, "asthma.csv")

all_percent_calls_condition_primary_impression_group <- full_data %>%
  group_by(temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n())
write_csv(all_percent_calls_condition_primary_impression_group, "all.csv")


%>%
  summarise
inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_bucket)*100) %>%
  select(primary_impression_group, temp_bucket, percent_calls_w_condition) %>%
  spread(temp_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`)

all_percent_calls_condition_primary_impression_group <- full_data %>%
  group_by(temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_bucket)*100) %>%
  mutate(primary_impression_group = "all") %>%
  select(primary_impression_group, temp_bucket, percent_calls_w_condition) %>%
  spread(temp_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`)



#### Primary Impression #####
call_temp_ratio_primary_impression <- full_data %>%
  filter(primary_impression != "Other") %>%
  group_by(primary_impression, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_bucket)*100) %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(primary_impression, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`,`100s`)

###############################################################################
###### Method 3: Percent of all calls for a given condition that appear in a given bucket -- differs from above because the numerator is condition calls per bucket and denominatnor is total number of calls in ALL buckets for a given condition. Need to calculate that. ########
###############################################################################

###############################################################################
###### Method 4: Some more complicated percentage that adjusts for temperature variations that I haven't figured out yet ########
###############################################################################


summarize(temp = mean(as.double(TEMPERATURE), na.rm = TRUE),
          illness_total = sum(primary_impression == condition),
          total_cases = n(),
          illness_perc = sum(primary_impression == condition)/n())
assign(condition, as_tibble(summary))
# group by zipcode
by_destination <- full_data %>%
  group_by(destination_patient_disposition) %>%
  summarize(temp = mean(as.double(TEMPERATURE), na.rm = TRUE),
            count = n())


# group by zipcode
by_month <- full_data %>%
  group_by(month(arrive_time)) %>%
  summarize(
    total_count = n(),
    temp = mean(as.double(TEMPERATURE), na.rm = TRUE),
  )

###############################################################################
###### Method 4: Some more complicated percentage that adjusts for temperature variations that I haven't figured out yet ########
###############################################################################

###############################################################################
###### Method 5: Some more complicated percentage that adjusts for temperature variations that I haven't figured out yet ########
###############################################################################
# Zip Code Percentage of calls in that zip code for each condition.

print(list(unique(full_data$primary_impression)))
#sapply(full_data, typeof)
#warnings()

# create a data frame with a count of cases by condition

asthma_cases_by_temperature <- full_data %>%
  filter(primary_impression == "Asthma") %>%
  group_by(temp_round) %>%
  summarise(condition_count = n()) %>%
  arrange(temp_round) %>%
  filter(temp_round != 1)

# bind asthma to temperature data frame, calculate percentage
joining_tables <- asthma_cases_by_temperature %>%
  right_join(all_cases_by_temperature, by = "temp_round") %>%
  mutate(bins = cut_interval(temp_round, length=10)) %>%
  group_by(bins) %>%
  summarise(condition_cases = sum(condition_count), total_cases = sum(total_count)) %>%
  mutate(percent_condition = round((condition_cases / total_cases)*100, 2))


chest_pain <- full_data %>%
  select(primary_impression, zipcode, incident_number) %>%
  filter(primary_impression == "Chest Pain - STEMI") %>%
  group_by(zipcode) %>%
  summarise(count=n()) %>%
view(chest_pain)

#COPD

call_temp_ratio_COPD <- full_data %>%
  filter(primary_impression_group == "COPD (Emphysema/Chronic Bronchitis)") %>%
  group_by(zipcode, primary_impression_group, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(zipcode, primary_impression_group, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(zipcode, primary_impression_group, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`, `100s`)

#Heart Conditions

call_temp_ratio_heart <- full_data %>%
  filter(primary_impression_category == "Heart/circulatory") %>%
  group_by(zipcode, primary_impression_category, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(zipcode, primary_impression_category, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(zipcode, primary_impression_category, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`, `100s`)

#Change asthma, COPD, Heart conditions from a mix of primary_impression_group and primary_impression_category to primary_impression_care_category so it's easier to look at
full_data <- full_data %>%
  mutate(primary_impression_group = if_else(primary_impression %in% c("Pain", "Other", "Abdominal Pain/Problems", "Weakness", "Other Illness/Injury", "Nausea/Vomiting (Unknown Etiology)", "General Malaise/Sick", "Back Pain (Non-Traumatic)", "Headache", "OB/Gyn - Vaginal Hemorrhage", "Fever", "Unknown Problem", "Poisoning", "Drowning/Near Drowning", "Electrocution", "Inhalation Injury (Toxic Gas)", "Lightening Strike", "Depressurization/Scuba", "SIDS (Sudden Infant Death Syndrome)", "Traumatic Injury", "OB/Gyn - Other", "OB/Gyn - OB/Delivery", "Sepsis",  "Other CNS Problem", "Migraine", "Allergic Reaction", "Abuse / Neglect", "Stings/Venomous Bites", "Airway Obstruction", "Anaphylaxis", "Hypovolemia/Shock", "Overpressurization", "Hazmat Exposure - Chem, Bio, Rad", "Inhalation - Smoke", "Sexual Assault/Rape", "Other Endocrine/Metabolic Problem", "Bowel Obstruction", "Apparent Life-Threatening Event", "Diarrhea", "Burn", "Pregnancy/OB Delivery", "G.I. Bleed", "Other GU Problems", "Patient Assist Only", "Not Applicable", "Other Abdominal/GI Problem", "No Apparent Illness/Injury"), "Other", primary_impression),
         primary_impression_care_category = case_when(
           primary_impression_group %in% c("Cardiac Arrest", "Cardiac Rhythm Disturbance", "Hypotension", "Hypertension", "CHF (Congestive Heart Failure)", "Chest Pain/Discomfort", "Chest Pain - STEMI", "Other Cardiovascular Problem", "Abdominal Aortic Aneurysm" ) ~ "Heart/circulatory",
           primary_impression_group %in% c("Asthma") ~ "Asthma",
           primary_impression_group %in% c("COPD (Emphysema/Chronic Bronchitis)") ~ "COPD (Emphysema/Chronic Bronchitis)",
           TRUE ~ primary_impression_group
         ))


#Asthma, COPD, Heart Conditions, by zipcode

call_temp_ratio_zip <- full_data %>%
  filter(zipcode == "21201", primary_impression_care_category == "Asthma" | primary_impression_care_category == "COPD (Emphysema/Chronic Bronchitis)" | primary_impression_care_category == "Heart/circulatory") %>%
  group_by(zipcode, primary_impression_care_category, temp_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket, by = "temp_bucket") %>%
  inner_join(temp_count_per_bucket, by = "temp_bucket") %>%
  mutate(calls_to_temp_ratio = temp_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(zipcode, primary_impression_care_category, temp_bucket, calls_to_temp_ratio) %>%
  spread(temp_bucket, calls_to_temp_ratio) %>%
  select(zipcode, primary_impression_care_category, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`, `100s`)

#TO DO: write function that calculates this for each zipcode AND THEN use RowBind to bind together all of the columns 

total <- rbind(zipcode)




