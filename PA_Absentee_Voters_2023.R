install.packages("plotly")
install.packages("readr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")

library(dplyr)
library(plotly)
library(readr)

NO_PII_EXPORT_FRIENDLY <- data.frame(PA_Mail_November$CountyName,PA_Mail_November$Party,PA_Mail_November$PrecinctDesc,PA_Mail_November$BallotReturnedDate,PA_Mail_November$Senate,PA_Mail_November$Congressional,PA_Mail_November$`Ballot.status.reason`,PA_Mail_November$Addr1,PA_Mail_November$Perm.Indicator)


PA_Mail_November <- read.csv("PA_Mail_November.csv")
Collected_PA_Mail_November <- subset(PA_Mail_November, PA_Mail_November$`Ballot.status.reason` == "RECORD - BALLOT RETURNED")
    ## 79.52% of ballots were counted

PA_Mail_R_2023 <- subset(PA_Mail_November, PA_Mail_November$Party == "R")
Collected_R_2023 <- subset(PA_Mail_R_2023, PA_Mail_R_2023$Ballot.status == "Vote Recorded")

PA_Mail_D_2023 <- subset(PA_Mail_November, PA_Mail_November$Party == "D")
Collected_D_2023 <- subset(PA_Mail_D_2023, PA_Mail_D_2023$Ballot.status == "Vote Recorded")


table(Collected_PA_Mail_November$BallotReturnedDate)

head(PA_Mail_November)
summary(PA_Mail_November)

statewide_address_counts <- table(Collected_PA_Mail_November$Addr1)
statewide_shared_addresses <- names(statewide_address_counts[statewide_address_counts > 10])

statewide_shared_address_data <- Collected_PA_Mail_November[Collected_PA_Mail_November$Addr1 %in% statewide_shared_addresses, ]

    # Create a frequency table of the full addresses for statewide data
statewide_full_address_counts <- table(Collected_PA_Mail_November$FullAddress)
statewide_shared_full_addresses <- names(statewide_full_address_counts[statewide_full_address_counts > 6])

    # Find entries in the statewide data that have these shared full addresses
statewide_shared_full_address_data <- Collected_PA_Mail_November[Collected_PA_Mail_November$FullAddress %in% statewide_shared_full_addresses, ]

    # Create a frequency table of the Address_CSZ
address_CSZ_counts <- table(Collected_PA_Mail_November$Address_CSZ)

    # Find Address_CSZ that occur seven or more times
ballot_hotspots <- names(address_CSZ_counts[address_CSZ_counts > 7])


shared_address_CSZ_data <- Collected_PA_Mail_November[Collected_PA_Mail_November$Address_CSZ %in% shared_address_CSZ, ]
    ## 3128 PERM Absentee voters at shared addresses
table(statewide_shared_full_addresses)


Cancelled_Votes_2023 <- subset(PA_Mail_November, PA_Mail_November$Ballot.status == "Cancelled")

CD8_23 <- subset(PA_Mail_November, PA_Mail_November$Congressional == "8TH CONGRESSIONAL DISTRICT")

  CD8_23_GOP_Absentee <- subset(PA_Mail_November, PA_Mail_November$Congressional == "8TH CONGRESSIONAL DISTRICT" & PA_Mail_November$Party == "R")
  CD8_23_GOP_SUCCESS <- subset(CD8_23_GOP_Absentee, CD8_23_GOP_Absentee$Ballot.status == "Vote Recorded")
    ## 13399 GOP voters in the 2023 universe which is more than the 12019 voters in the 2022 universe
    ## 10190 Recorded GOP votes in PA-8 during 2023 Municipal Elections
    ## 553 Cancelled GOP votes
    ## 2624 Pending GOP votes
    ## 0.7605045 GOP Success Rate

  CD8_23_DEM_Absentee <- subset(PA_Mail_November, PA_Mail_November$Congressional == "8TH CONGRESSIONAL DISTRICT" & PA_Mail_November$Party == "D")
  CD8_23_DEM_SUCCESS <- subset(CD8_23_DEM_Absentee, CD8_23_DEM_Absentee$`Ballot status` == "Vote Recorded")
    ## 45636 DEM voters in the 2023 universe
    ## 36410 Recorded DEM votes in PA-8 during 2023 Municipal Elections
    ## 2178 Cancelled DEM votes
    ## 6981 Pending DEM votes
    ## 0.797835 DEM Success Rate

CD_8_Wayne <- subset(CD8_23_TOTAL, CD8_23_TOTAL$CountyName == "WAYNE")
    ## 3672 voters in 2023

CD_8_Lackawanna <- subset(CD8_23_TOTAL, CD8_23_TOTAL$CountyName == "LACKAWANNA")
    ## 17873 voters in 2023

CD_8_Monroe <- subset(CD8_23_TOTAL, CD8_23_TOTAL$CountyName == "MONROE")
    ## 12518 voters in 2023

CD_8_Pike <- subset(CD8_23_TOTAL, CD8_23_TOTAL$CountyName == "PIKE")
    ## 5744 voters in 2023

CD_8_Luzerne <- subset(CD8_23_TOTAL, CD8_23_TOTAL$CountyName == "LUZERNE")
    ## 24114 voters in 2023





## what percentage of 8th Congressional District Absentee Voters are permanently registered? (93.09%)
table(CD8_23$Perm.Indicator)

## what % of perm registered voters had their votes cancelled? (4.61%)
PERM_ABS_8TH <- subset(PA_Mail_November, PA_Mail_November$Congressional == "8TH CONGRESSIONAL DISTRICT" & PA_Mail_November$Perm.Indicator == "TRUE")
COUNTED_PERM_ABS_8TH <- subset(PERM_ABS_8TH, PERM_ABS_8TH$Ballot.status == "Vote Recorded")

## what % of non-perm voters had their votes cancelled? (4.69%)
NONPERM_ABS_8TH <- subset(PA_Mail_November, PA_Mail_November$Congressional == "8TH CONGRESSIONAL DISTRICT" & PA_Mail_November$Perm.Indicator != "TRUE")
NONPERM_CANC <- subset(NONPERM_ABS_8TH, NONPERM_ABS_8TH$Ballot.Application.Disposition == "Declined")







CD_8_PERM_COUNTED <- subset(PA_Mail_November, PA_Mail_November$Perm.Indicator == "TRUE" & PA_Mail_November$Congressional == "8TH CONGRESSIONAL DISTRICT" & PA_Mail_November$Ballot.status == "Vote Recorded")
LACKAWANNA_PERM_COUNTED <- subset(CD_8_PERM_COUNTED, CD_8_PERM_COUNTED$CountyName == "LACKAWANNA")



table(CD_8_PERM_COUNTED$PrecinctDesc)

LACKAWANNA_PERM <- subset(PA_Mail_November, PA_Mail_November$Perm.Indicator == "TRUE" & PA_Mail_November$CountyName == "LACKAWANNA")

LACKAWANNA_COLLECTED <- subset(Collected_PA_Mail_November, Collected_PA_Mail_November$CountyName == "LACKAWANNA")
table(LACKAWANNA_PERM$PrecinctDesc)
table(LACKAWANNA_PERM_COUNTED$PrecinctDesc)
table(LACKAWANNA_COLLECTED$PrecinctDesc)




Dauphin_County_Prison <- subset(Collected_PA_Mail_November, Collected_PA_Mail_November$Addr1 == "501 MALL ROAD")
Clarks_Summit_State_Hospital <- Dauphin_County_Prison <- subset(Collected_PA_Mail_November, Collected_PA_Mail_November$Addr1 == "1451  HILLSIDE DR ")
So1_Anomoly <- subset(Collected_PA_Mail_November, Collected_PA_Mail_November$Addr1 == "SO1")


install.packages("plotly")
install.packages("readr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")

library(dplyr)
library(plotly)
library(readr)

PA_Mail_November <- read.csv("PA_Mail_November.csv")

# Subset data by party and ballot status
PA_Mail_R_2023 <- subset(PA_Mail_November, Party == "R")
Collected_R_2023 <- subset(PA_Mail_R_2023, Ballot.status == "Vote Recorded")

PA_Mail_D_2023 <- subset(PA_Mail_November, Party == "D")
Collected_D_2023 <- subset(PA_Mail_D_2023, Ballot.status == "Vote Recorded")

# Subset data for collected ballots
Collected_PA_Mail_November <- subset(PA_Mail_November, Ballot.status.reason == "RECORD - BALLOT RETURNED")

# Create a frequency table of addresses for statewide data
statewide_address_counts <- table(Collected_PA_Mail_November$Addr1)
statewide_shared_addresses <- names(statewide_address_counts[statewide_address_counts > 10])

statewide_shared_address_data <- Collected_PA_Mail_November[Collected_PA_Mail_November$Addr1 %in% statewide_shared_addresses, ]

# Output a few rows for each shared address in the statewide data to check
for (address in statewide_shared_addresses) {
  print(statewide_shared_address_data[statewide_shared_address_data$Addr1 == address, ][1:5, ])
}

# Create a new column concatenating address and city-state-zip
Collected_PA_Mail_November$FullAddress <- paste(Collected_PA_Mail_November$Addr1, Collected_PA_Mail_November$CSZ)

# Create a frequency table of full addresses for statewide data
statewide_full_address_counts <- table(Collected_PA_Mail_November$FullAddress)
statewide_shared_full_addresses <- names(statewide_full_address_counts[statewide_full_address_counts > 6])

# Find entries in the statewide data that have shared full addresses
statewide_shared_full_address_data <- Collected_PA_Mail_November[Collected_PA_Mail_November$FullAddress %in% statewide_shared_full_addresses, ]

# Create a new column concatenating address and city-state-zip
Collected_PA_Mail_November$Address_CSZ <- paste(Collected_PA_Mail_November$Addr1, Collected_PA_Mail_November$CSZ)

# Create a frequency table of Address_CSZ
address_CSZ_counts <- table(Collected_PA_Mail_November$Address_CSZ)

# Find Address_CSZ that occur five or more times
focus_addresses <- names(address_CSZ_counts[address_CSZ_counts > 7])

# Find the entries in the original data that have these shared Address_CSZ
shared_address_CSZ_data <- Collected_PA_Mail_November[Collected_PA_Mail_November$Address_CSZ %in% focus_addresses, ]

## 3128 PERM Absentee voters at shared addresses



# Subset data for cancelled votes
Cancelled_Votes_2023 <- subset(PA_Mail_November, Ballot.status == "Cancelled")

# Subset data for 8th Congressional District
CD8_23 <- subset(PA_Mail_November, Congressional == "8TH CONGRESSIONAL DISTRICT")

# Subset data for GOP Absentee voters in CD8
CD8_23_GOP_Absentee <- subset(PA_Mail_November, Congressional == "8TH CONGRESSIONAL DISTRICT" & Party == "R")
CD8_23_GOP_SUCCESS <- subset(CD8_23_GOP_Absentee, Ballot.status == "Vote Recorded")

# Subset data for DEM Absentee voters in CD8
CD8_23_DEM_Absentee <- subset(PA_Mail_November, Congressional == "8TH CONGRESSIONAL DISTRICT" & Party == "D")
CD8_23_DEM_SUCCESS <- subset(CD8_23_DEM_Absentee, `Ballot.status` == "Vote Recorded")

# Subset data for specific counties in CD8
CD_8_Wayne <- subset(CD8_23, CountyName == "WAYNE")
CD_8_Lackawanna <- subset(CD8_23, CountyName == "LACKAWANNA")
CD_8_Monroe <- subset(CD8_23, CountyName == "MONROE")
CD_8_Pike <- subset(CD8_23, CountyName == "PIKE")
CD_8_Luzerne <- subset(CD8_23, CountyName == "LUZERNE")

# Export data without PII
NO_PII_EXPORT_FRIENDLY <- data.frame(PA_Mail_November$CountyName, PA_Mail_November$Party, PA_Mail_November$PrecinctDesc, PA_Mail_November$BallotReturnedDate, PA_Mail_November$Senate, PA_Mail_November$Congressional, PA_Mail_November$`Ballot.status.reason`, PA_Mail_November$Addr1, PA_Mail_November$Perm.Indicator)
write.csv(NO_PII_EXPORT_FRIENDLY, file = "PA_Mail_November_NO_PII.csv")

Eighth_CD_Overview <- subset(NO_PII_EXPORT_FRIENDLY, PA_Mail_November.Congressional == "8TH CONGRESSIONAL DISTRICT")
Forty_SD_Overview <- subset(NO_PII_EXPORT_FRIENDLY, PA_Mail_November.Senate == "40TH SENATORIAL DISTRICT")

Archbald_Ward1_Precinct2 <- subset(CD_8_Lackawanna, PrecinctDesc == "ARCHBALD W-01 P-2")

# ballot hotspots: what percentage of hotspot voters are permanent absentees? 3630/4628 (78.435%)
table(shared_address_CSZ_data$Perm.Indicator)

non_perm_hotspots <- subset(shared_address_CSZ_data, shared_address_CSZ_data$Perm.Indicator == "FALSE")


Not_Collected <- subset(PA_Mail_November, Ballot.status != "Vote Recorded")

Philadelphia_Department_of_Prisons <- subset(PA_Mail_November, Addr1 == "7901 STATE RD")
Clarks_Summit_State_Hospital <- subset(PA_Mail_November, PA_Mail_November$Addr1 == "1451  HILLSIDE DR")