# Setup
library(tidyverse)
library(readxl)

# Alex's favourite function
`%notin%` <- Negate(`%in%`)

# Set the filepath for data
wd <- "C:\\Users\\alaurencetraynor\\Documents\\2019\\UI\\Chapter 1\\Data\\"


############################### Organizing stocking rate data #############################################

# ocular estimates
ocular <-  read.csv(paste0(wd,"ocular_final_all.csv"))
#frequency of grazed plants (derived from ocular estimates)
freq <-  read.csv(paste0(wd,"freq_final.csv"))
# paired plot
pp_raw <- readxl::read_xlsx(paste0(wd,"tidyweight.xlsx"))
#height weight and landscape appearance
hw_la <- read.csv(paste0(wd,"hw_la_final.csv"))

## stocking rate data
stocking_rate <- readxl::read_xlsx(paste0(wd,"tbl_StockingRateData.xlsx"))

### Tidy up the data
#### Stocking rate 
# check which pastures we have data for
observed_pastures_oc <- unique(ocular$PastureID_YearOfSurvey) # 39 pastures
observed_pastures_hwla <- unique(hw_la$PastureID_YearOfSurvey) # 47 pastures

# need to join paired plots to pasture first
# for some reason the ocular data's point id also contain year (despite there being a separate column for year)
# create new variable with only pointid no year
# remove all characters after and including "_" 
ocular$PointID_noyear <- gsub("_.*","",ocular$PointID)

# well use the ocular data to look up which plotids are in which pasture
pp_raw <- merge(x = pp_raw,
                              y = ocular[,c("PointID_noyear", "PastureID_YearOfSurvey")],
                              by.x = "PointID",
                              by.y = "PointID_noyear",
                              all.x = TRUE,
                              all.y = FALSE)

observed_pastures_pp <- unique(pp_raw$PastureID_YearOfSurvey)

# just curious on the overlap of data collection here
hw_only_pastures <- observed_pastures_hwla[observed_pastures_hwla %notin% observed_pastures_oc] # 16 unique pastures with only height-weight/landscape appearance data
oc_only_pastures <- observed_pastures_oc[observed_pastures_oc %notin% observed_pastures_hwla] #7 unique pastures with only ocular/frequency data
pp_only_pastures <- observed_pastures_pp[observed_pastures_pp %notin% observed_pastures_hwla & observed_pastures_pp %notin% observed_pastures_oc] # none

# pull them all together so we can use them to subset stocking rate data
observed_pastures <- unique(c(observed_pastures_hwla, observed_pastures_oc, observed_pastures_pp))
# remove the NA
observed_pastures <- observed_pastures[!is.na(observed_pastures)] #   total of 54 unique pasture ids

# Lets filter the stocking rate data to those pastures we have data for
stocking_rate_observed <- stocking_rate[stocking_rate$PastureID %in% observed_pastures,]

# we are also not interested in stocking rate from years prior to data collection (although this is interesting)
# we can pull this out once the utilization data is summarised to pasture

# Calculating AUMs
stocking_rate_observed$MonthsGrazed <- difftime(units="days",stocking_rate_observed$DateOut,stocking_rate_observed$DateIn)/30 # Changed this to 30 based formula from VJ 

# every where theres a monthsgrazed of 0 we are rounding up to 1 day (1/31) to capture that day of grazing
# need to format date/time cols appropriately
stocking_rate_observed$MonthsGrazed[stocking_rate_observed$MonthsGrazed == 0] <- 1/30

stocking_rate_observed$DateIn <- as.Date(stocking_rate_observed$DateIn)
stocking_rate_observed$DateOut <- as.Date(stocking_rate_observed$DateOut)

# assuming 1000lb cows
stocking_rate_observed$AUM <- stocking_rate_observed$NumberOfAnimals*stocking_rate_observed$MonthsGrazed

# check comments to amend any individual records at this point
# theres an ton of missing data an erroneous records however, 
# leaving in NA where we dont have compelte info
stocking_rate_observed <- stocking_rate_observed %>%
  mutate(AUM = ifelse(Rested == "Yes", 0, AUM))

####################################################################################################################
# # these plots give a visual of when the spring/fall grazing cut off was - there doesnt really seem to be any..
# # create better palette
# library(RColorBrewer)
# # Define the number of colors you want
# nb.cols <- length(unique(stocking_rate_observed$PastureID))
# mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
# 
# # fix structure a little
# stocking_rate_observed$ID <- as.factor(stocking_rate_observed$ID)
# stocking_rate_observed$Year <- as.factor(stocking_rate_observed$Year)
# stocking_rate_observed$Site <- as.factor(stocking_rate_observed$Site)
# stocking_rate_observed$PastureID <- as.factor(stocking_rate_observed$PastureID)
# 
# # lets do a quick plot here to look at whats going on
# # size of rectangle is basically AUMs
# for(i in c("BIBU", "BRBE", "JISA", "PAVA", "SHCR")){
#   data <- stocking_rate_observed[stocking_rate_observed$Site == i & !is.na(stocking_rate_observed$DateIn),]
#   
#   ggplot(data = data, aes(fill = PastureID))+
#     geom_rect(aes(xmin = DateIn, xmax = DateOut, ymin = 0, ymax = NumberOfAnimals))+
#     facet_wrap(.~Year)+
#     scale_x_date(date_labels = "%d-%m", date_breaks = "2 week")+
#     theme(axis.text.x  = element_text(angle = 60, hjust =1), legend.position = "")+
#     scale_fill_manual(name = "Pasture ID", values = mycolors)+
#     labs(y = "Number of Animals", x = "Day-Month")
#   
#   ggsave(filename = paste0("stocking rate data ",i, ".jpg"), dpi = 500, device = "jpg", path = wd, width = 20, height = 16, units = "in")
#   
# }
########################################################################################################################

# Import pasture information
#tbl_pastures <- readxl::read_xlsx(path =paste0(wd, "tbl_PastureNames.xlsx"))
tbl_pastures_new <- readxl::read_xlsx(path = paste0(wd, "Temp_PasturesNew.xlsx"))

# compare with this grazing data
#grazing_events <- readxl::read_xlsx(path = paste0(wd, "qry_GrazeDataByGrazeEvent_GrazData.xlsx"))

#grazing_events$AUMs <- as.character(grazing_events$AUMs)

# check_aums <-  merge(x = grazing_events,
#                      y = stocking_rate_pasture,
#                      by = c("PastureID", "Year"))

# Summarising utilization to pasture and year to sum individual grazing events
# But we may also want to capture spring vs fall grazing here so splitting up to spring AUMs and total AUMs
# the spring cut off date is June 15th asccording to teh G and G report--

stocking_rate_pasture <-  stocking_rate_observed %>%
  group_by(Year, PastureID, Site) %>%
  summarise(AUMs = sum(AUM))

stocking_rate_pasture <- merge(x = stocking_rate_pasture,
                               y = tbl_pastures_new,
                               by = "PastureID",
                               all.x = TRUE,
                               all.y = FALSE)


# specify treatment type based on treatment start year
stocking_rate_pasture$Treatment[stocking_rate_pasture$Treatment != "Non-Treatment" & as.numeric(as.character(stocking_rate_pasture$Year)) < stocking_rate_pasture$YearStudyStart] <- "Pre-Treatment"
     
# calc AUMs/hectare
stocking_rate_pasture$AUMha <- stocking_rate_pasture$AUMs / stocking_rate_pasture$Area_ha

# select only useful columns:

stocking_rate_pasture <- stocking_rate_pasture %>%
  select(PastureID,
         Year,
         Treatment,
         Site.x,
         AUMs,
         AUMha) %>%
  rename(Site = Site.x)

# export to csv
write.csv(x = stocking_rate_pasture, file = paste0(wd, "final_stocking_rate_observed_pastures.csv"))


#####################################################################################################################

### Ocular Estimates

ocular$SiteMove <- ifelse(as.character(ocular$Site) == as.character(ocular$Original_Site_Assignment), ocular$SiteMove <- "No",ocular$SiteMove <-  "Yes")

ocular<- dplyr::select(ocular, -c(Original_Site_Assignment,
                                  X,
                                  SdMaxHt,
                                  SdMaxLeaf,
                                  SdEff_Ht,
                                  SdRemoved,
                                  n,
                                  PastureID_YearOfSurvey,
                                  StartTime,
                                  EndTime,
                                  Comment))

# convert all charcter variables to factors for the modelling
ocular[sapply(ocular, is.character)] <- lapply(ocular[sapply(ocular, is.character)], 
                                       as.factor)

# same with pasture ids and year
ocular$ObsID1 <- as.factor(ocular$ObsID1)
ocular$ObsID2 <- as.factor(ocular$ObsID2)
ocular$Year <- as.factor(ocular$Year)
ocular$SiteMove <- factor(ocular$SiteMove, levels=c("Yes","No"))
ocular$New_YearOfSurveyID <- as.factor(ocular$New_YearOfSurveyID)

# Joining ocular and frequency data (since they were collected at the same locations and by the same people)
util_join <- merge(x = ocular,
                   y = freq[,c("PointID", "percent_grazed")], 
                   by = "PointID",
                   all.x = TRUE,
                   all.y = FALSE)

write.csv(x = util_join, file = paste0(wd, "final_ocular_frequency_plot.csv"))

## Summarise to pasture
ocular_freq_pasture <- util_join %>%
  rename(ocular = AvgRemoved, frequency = percent_grazed) %>%
  select(TheorID, Year, New_YearOfSurveyID, Site, ocular, frequency) %>%
  pivot_longer(cols = c(ocular, frequency), names_to = "method", values_to = "Utilization") %>%
  group_by(Site, Year, New_YearOfSurveyID, method) %>%
  summarise(mean_utilization = mean(Utilization),
            sd_utilization = sd(Utilization),
            var_utilization = var(Utilization),
            n = n())

# these are for sure not normally distributed - use beta distribution?

# joining to stocking rate and pasture data 
ocular_freq_pasture <- merge(x=ocular_freq_pasture,
                      y=stocking_rate_pasture,
                      all.x = TRUE,
                      all.y =FALSE,
                      by.x = c("New_YearOfSurveyID","Year","Site"),
                      by.y = c("PastureID", "Year", 'Site'))

# Filter to pastures that were grazed
ocular_freq_pasture_grazed <- filter(ocular_freq_pasture, AUMs > 0)

write.csv(x = ocular_freq_pasture_grazed, file = paste0(wd, "final_ocular_freq_pasture.csv"))

### Height weight / landscape appearance
hw_la$SiteMove <- ifelse(as.character(hw_la$Site) == as.character(hw_la$Original_Site_Assignment), hw_la$SiteMove <- "No",hw_la$SiteMove <-  "Yes")

# convert all charcter variables to factors for the modelling
hw_la[sapply(hw_la, is.character)] <- lapply(hw_la[sapply(hw_la, is.character)], 
                                               as.factor)

# same with pasture ids and year
hw_la$Year <- as.factor(hw_la$Year)
hw_la$SiteMove <- factor(hw_la$SiteMove, levels=c("Yes","No"))
hw_la$PastureID_YearOfSurvey <- as.factor(hw_la$PastureID_YearOfSurvey)

# Summarise to pasture 

