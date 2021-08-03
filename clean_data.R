# Setup
library(tidyverse)
library(readxl)

# Alex's favourite function
`%notin%` <- Negate(`%in%`)

# Set the filepath for data
wd <- "C:\\Users\\alaurencetraynor\\Documents\\2019\\UI\\Chapter 1\\Data\\"

## Utilization data
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

# just curiopus on the overlap of data collection here
hw_only_pastures <- observed_pastures_hwla[observed_pastures_hwla %notin% observed_pastures_oc]
oc_only_pastures <- observed_pastures_oc[observed_pastures_oc %notin% observed_pastures_hwla]
pp_only_pastures <- observed_pastures_pp[observed_pastures_pp %notin% observed_pastures_hwla & observed_pastures_pp %notin% observed_pastures_oc]

# pull them all together so we can use them to subset stocking rate data
observed_pastures <- unique(c(observed_pastures_hwla, observed_pastures_oc, observed_pastures_pp))
# remove the NA
observed_pastures <- observed_pastures[!is.na(observed_pastures)] #   total of 54 unique pasture ids

# Lets filter the stocking rate data to those pastures we have data for
stocking_rate_observed <- stocking_rate[stocking_rate$PastureID %in% observed_pastures,]

# we are also not interested in stocking rate from years prior to data collection (although this is interesting)
# we can pull this out once the utilization data is summarised to pasture

# Calculating AUMs
stocking_rate_observed$MonthsGrazed <- difftime(units="days",stocking_rate_observed$DateOut,stocking_rate_observed$DateIn)/31

# every where theres a monthsgrazed of 0 we are rounding up to 1 day (1/31) to capture that day of grazing
# need to format date/time cols appropriately
stocking_rate_observed$MonthsGrazed[stocking_rate_observed$MonthsGrazed == 0] <- 1/31

stocking_rate_observed$DateIn <- as.Date(stocking_rate_observed$DateIn)
stocking_rate_observed$DateOut <- as.Date(stocking_rate_observed$DateOut)

# assuming 1000lb cows
stocking_rate_observed$AUM <- stocking_rate_observed$NumberOfAnimals*stocking_rate_observed$MonthsGrazed

# check comments to amend any individual records at this point
# theres an ton of missing data an erroneous records however, 
# leaving in NA where we dont have compelte info
stocking_rate_observed <- stocking_rate_observed %>%
  mutate(AUM = ifelse(Rested == "Yes", 0, AUM))

# create better palette
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- length(unique(stocking_rate_observed$PastureID))
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# fix structure a little
stocking_rate_observed$ID <- as.factor(stocking_rate_observed$ID)
stocking_rate_observed$Year <- as.factor(stocking_rate_observed$Year)
stocking_rate_observed$Site <- as.factor(stocking_rate_observed$Site)
stocking_rate_observed$PastureID <- as.factor(stocking_rate_observed$PastureID)

# lets do a quick plot here to look at whats going on
# size of rectangle is basically AUMs
for(i in c("BIBU", "BRBE", "JISA", "PAVA", "SHCR")){
  data <- stocking_rate_observed[stocking_rate_observed$Site == i & !is.na(stocking_rate_observed$DateIn),]
  
  ggplot(data = data, aes(fill = PastureID))+
    geom_rect(aes(xmin = DateIn, xmax = DateOut, ymin = 0, ymax = NumberOfAnimals))+
    facet_wrap(.~Year)+
    scale_x_date(date_labels = "%d-%m", date_breaks = "2 week")+
    theme(axis.text.x  = element_text(angle = 60, hjust =1), legend.position = "")+
    scale_fill_manual(name = "Pasture ID", values = mycolors)+
    labs(y = "Number of Animals", x = "Day-Month")
  
  ggsave(filename = paste0("stocking rate data ",i, ".jpg"), dpi = 500, device = "jpg", path = wd, width = 20, height = 16, units = "in")
  
}
# these plots also give a visual of when the spring/fall grazing cut off was - there doesnt really seem to be any..


# Summarising to pasture and year to sum individual grazing events
# But we may also want to capture spring vs fall grazing here so splitting up to spring AUMs and total AUMs
# the spring cut off date is June 15th asccording to teh G and G report

stocking_rate_pasture <-  stocking_rate_observed %>%
  group_by(Year, PastureID, Site) %>%
  summarise(AUMs = sum(AUM))

### Ocular Estimates
# joining to stocking rate data and filtering based on grazing events
ocular <- base::merge(x=ocular, y=stocking_rate, all.x = TRUE, all.y =FALSE, by.x = c("New_YearOfSurveyID","Year"), by.y = c("PastureID", "Year"))

ocular <- ocular %>%
  dplyr::select(-c(StudyPasture_Yes_No,YearStudyStart,YearStudyEnd, HomeState))

# ascertaining actual grazing treatment
#ocular$grazed <- ifelse(ocular$Treatment == "Spring Odd Years" & ocular$Year == "2015" | ocular$Treatment == "Spring Odd Years" & ocular$Year == "2017", ocular$grazed <- "Yes", ifelse(ocular$Treatment == "Spring Even Years" & ocular$Year == "2016" | ocular$Treatment == "Spring Even Years" & ocular$Year == "2018", ocular$grazed <- "Yes", ifelse(ocular$Treatment == "Spring and Fall", ocular$grazed <- "Yes","No")))

ocular <- filter(ocular, AUMs != 0)

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
                                  ALLOT_NAME,
                                  PAST_NAME,
                                  ADMIN_ORG_,
                                  Shape_Length,
                                  Shape_Area,
                                  Grazed2018,
                                  Pst_2015Earlier,
                                  Pst_2016,
                                  Pst_2018,
                                  Pst_2019,
                                  Comment,
                                  Area_ha))

ocular$ObsID1 <- as.factor(ocular$ObsID1)

ocular$ObsID2 <- as.factor(ocular$ObsID2)

ocular$Year <- as.factor(ocular$Year)

ocular$SiteMove <- factor(ocular$SiteMove, levels=c("Yes","No"))

# need to create AUM/area with correct pasture area - join new oasture id to pasture table
pastures <- sqlFetch(con1, "Temp_PasturesNew")[,c(1,8)]

ocular <- merge(x = ocular, y = pastures, by.x = "New_YearOfSurveyID", by.y = "PastureID", all.x = TRUE, all.y = FALSE)

ocular$AUMha <-  ocular$AUMs/ocular$Area_ha

stocking_rate <- dplyr::distinct(ocular[,c(1,2,31)])

util_join <- left_join(ocular,freq, by = "PointID")
