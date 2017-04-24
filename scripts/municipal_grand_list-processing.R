library(dplyr)
library(RCurl)
library(devtools)
library(datapkg)


##################################################################
#
# Processing Script for Municipal-Grand-List
# Created by Jenna Daly
# On 02/22/2017
#
##################################################################

#source componentGL to create legacy component GL list
source('scripts/componentGL-processing.R')

sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
year_folders <- dir(path, pattern = "2")
#grabs all csvs (even not FISCIN data)
all_csvs <- dir(path, recursive=T, pattern = ".csv") 
#isolates FISCIN csvs
only_FISCIN <- all_csvs[grep("FISCIN", all_csvs)] 

#create empty data frame with set columns (defines final working columns that are generated from raw columns in FISCIN)
municipal_grand_list_data <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), 
                     c("Town", 
                       "Year", 
                       "Current Year Adjusted Taxes Collectible",
                       "Equalized Net Grand List",
                       "Actual Mill Rate",
                       "Net Grand List",
                       "Population"))

#read in each raw file and get ready for master combine
for (j in 1:length(year_folders)) {
  year_path <- (paste0(path, "/", year_folders[j]))
  all_csvs <- dir(year_path, recursive=T, pattern = ".csv") 
  only_FISCIN <- all_csvs[grep("FISCIN", all_csvs)] 
  for (i in 1:length(only_FISCIN)) {
    current_file <- read.csv(paste0(year_path, "/", only_FISCIN[i]), stringsAsFactors=F, header=T)
    remove_folder <- sub(".*/", "", only_FISCIN[i]) #filename
    get_year <- unique(as.numeric(unlist(gsub("[^0-9]", "", unlist(remove_folder)), "")))
    get_year <- get_year + 2000
    SFY <- paste(get_year - 1, get_year, sep = "-")
    SFY2 <- paste("SFY", SFY)
    current_file$Year <- SFY2
    col_names <- colnames(current_file)
    pop_col <- grep("population", col_names, ignore.case=T, value=T)
    NGL_col <- grep("ACGLFY", col_names, ignore.case=T, value=T)
    #defines raw columns that we want to grab to populate final working columns
    final_columns <- current_file[, c("Municipality", 
                                      "Year", 
                                      "Curr_Year_Adjusted_Taxes_Collectible",
                                      "EGL",         
                                      "ACMR",
                                      NGL_col, 
                                      pop_col)]  
    #relabels raw columns to final working column names
    names(final_columns) <- c("Town", 
                              "Year", 
                              "Current Year Adjusted Taxes Collectible",
                              "Equalized Net Grand List",
                              "Actual Mill Rate",
                              "Net Grand List",
                              "Population")
    
    # take out "Groton (City of)" because it is a political subdivision of groton and not the town.
    final_columns <- final_columns[final_columns$Town != "GROTON (City of)",]
    
    # Add this iteration's data to main container, first removing duplicated year data
    municipal_grand_list_data <- municipal_grand_list_data[municipal_grand_list_data$Year!=SFY2,]
    municipal_grand_list_data <- rbind(municipal_grand_list_data, final_columns)
  }
}

#Clean up 
rm(current_file, final_columns)

# Town names to title case
municipal_grand_list_data$"Town" <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\E\\2", tolower(municipal_grand_list_data$Town), perl=TRUE)

# Round actual mill rate column to 2 digits 
#use this function to round multiple columns to different digits
round_df <- function(x, digits) {
  columns_to_round <- c("Actual Mill Rate")
  columns_to_round <- sapply(x, mode) == 'numeric'
  municipal_grand_list_data[columns_to_round] <-  round(municipal_grand_list_data[columns_to_round], digits)
  municipal_grand_list_data
}

municipal_grand_list_data <- round_df(municipal_grand_list_data, 2)

municipal_grand_list_data <- arrange(municipal_grand_list_data, Year, Town)

#############################################################################################################################

destfile <- paste0(path, "/", "components", "/", "componentGL_2015.csv")

if (!file.exists(destfile)) {  
  #process and create destfile (componentGL_processing script)
} else {
  #set componentGL to destfile
  combined_componentGL <- read.csv(destfile, stringsAsFactors=F, header=T)
}

#only reporting data from the following years (excluding historical data before 2000)
final_years <- c("SFY 2000-2001", "SFY 2001-2002", "SFY 2002-2003", "SFY 2003-2004", 
                 "SFY 2004-2005", "SFY 2005-2006", "SFY 2006-2007", "SFY 2007-2008", 
                 "SFY 2008-2009", "SFY 2009-2010", "SFY 2010-2011", "SFY 2011-2012", 
                 "SFY 2012-2013", "SFY 2013-2014", "SFY 2014-2015")

componentGL_used <- combined_componentGL[combined_componentGL$Year %in% final_years,]
rm(combined_componentGL)

#merge GL data and all data
municipal_grand_list_data_merged <- merge(componentGL_used, municipal_grand_list_data, by = c("Town", "Year"))
rm(componentGL_used, municipal_grand_list_data)


#Create calculated columns
municipal_grand_list_data_merged$"Equalized Net Grand List per Capita" <- NA
municipal_grand_list_data_merged$"Equalized Net Grand List per Capita" <- round((municipal_grand_list_data_merged$"Equalized Net Grand List" / municipal_grand_list_data_merged$"Population"), 0)

municipal_grand_list_data_merged$"Commercial and Industrial Share of Total Net Grand List" <- NA
municipal_grand_list_data_merged$"Commercial and Industrial Share of Total Net Grand List" <- round(100 *((municipal_grand_list_data_merged$"Gross.Commercial.Grand.List" + municipal_grand_list_data_merged$"Gross.Industrial.Grand.List") / (municipal_grand_list_data_merged$"Net Grand List")), 2)

municipal_grand_list_data_merged$"Equalized Mill Rate" <- NA
municipal_grand_list_data_merged$"Equalized Mill Rate" <- round(1000 * (municipal_grand_list_data_merged$"Current Year Adjusted Taxes Collectible" / municipal_grand_list_data_merged$"Equalized Net Grand List"), 2)

#Equalized Net Grand List per Capita as Percent of State Average
#Calculated as: 

#[                     Equalized Net Grand List per Capita of a given town for a given year                           ]
#[--------------------------------------------------------------------------------------------------------------------] X 100
#[(sum of all towns Equalized Net Grand List for that given year) / (sum of all states population for that given year)]

sum_engl <- aggregate(`Equalized Net Grand List` ~ Year, municipal_grand_list_data_merged, sum)
sum_pop <- aggregate(Population ~ Year, municipal_grand_list_data_merged, sum)

municipal_grand_list <- merge(municipal_grand_list_data_merged, sum_engl, by="Year")
municipal_grand_list <- merge(municipal_grand_list, sum_pop, by="Year")

municipal_grand_list$"Equalized Net Grand List per Capita as Percent of State Average" <- NA
municipal_grand_list$"Equalized Net Grand List per Capita as Percent of State Average" <- round(((municipal_grand_list$"Equalized Net Grand List per Capita")/((municipal_grand_list$`Equalized Net Grand List.y`)/(municipal_grand_list$Population.y)))*100, 2)  

municipal_grand_list_arranged <- select(municipal_grand_list, `Year`, `Town`, `Gross.Residential.Grand.List`, `Gross.Commercial.Grand.List`, 
                                                              `Gross.Industrial.Grand.List`, `Total.Gross.Grand.List`, `Equalized Net Grand List.x`, 
                                                              `Actual Mill Rate`, `Net Grand List`, `Equalized Net Grand List per Capita`, 
                                                              `Commercial and Industrial Share of Total Net Grand List`, `Equalized Mill Rate`, 
                                                              `Equalized Net Grand List per Capita as Percent of State Average`) %>% 
  arrange(Town, Year)

colnames(municipal_grand_list_arranged) <- c("Year", "Town", "Gross Residential Grand List", "Gross Commercial Grand List", 
                                             "Gross Industrial Grand List", "Total Gross Grand List", "Equalized Net Grand List", 
                                             "Actual Mill Rate", "Net Grand List", "Equalized Net Grand List per Capita", 
                                             "Commercial and Industrial Share of Total Net Grand List", "Equalized Mill Rate", 
                                             "Equalized Net Grand List per Capita as Percent of State Average")

rm(sum_engl, sum_pop, municipal_grand_list, municipal_grand_list_data_merged)

###############################################################################################################################################
#convert to long format
cols_to_stack <- c("Actual Mill Rate",
                   "Equalized Mill Rate",
                   "Equalized Net Grand List",
                   "Equalized Net Grand List per Capita",
                   "Gross Commercial Grand List",
                   "Gross Industrial Grand List",
                   "Gross Residential Grand List",
                   "Net Grand List",
                   "Total Gross Grand List",
                   "Commercial and Industrial Share of Total Net Grand List",
                   "Equalized Net Grand List per Capita as Percent of State Average")

long_row_count = nrow(municipal_grand_list_arranged) * length(cols_to_stack)

combined_final_long <- reshape(municipal_grand_list_arranged, 
                               varying = cols_to_stack, 
                               v.names = "Value", 
                               timevar = "Variable", 
                               times = cols_to_stack, 
                               new.row.names = 1:long_row_count,
                               direction = "long"
)

combined_final_long$Variable <- factor(combined_final_long$Variable, levels = cols_to_stack)
combined_final_long <- combined_final_long[order(combined_final_long$Town, combined_final_long$Year, combined_final_long$Variable),]
combined_final_long$id <- NULL

#check to see if any duplicates made it into the data frame (this should be empty)
duplicates <- combined_final_long[duplicated(combined_final_long[,1:3]),]

#add Measure Type
combined_final_long$"Measure Type" <- NA
##fill in 'Measure Type' column based on criteria listed below
combined_final_long$"Measure Type"[which(combined_final_long$Variable %in% c("Equalized Net Grand List",                                           
                                                                             "Equalized Net Grand List per Capita",	                               
                                                                             "Gross Commercial Grand List",		                                     
                                                                             "Gross Industrial Grand List",	                                       
                                                                             "Gross Residential Grand List",	                                     
                                                                             "Net Grand List",		                                                 
                                                                             "Total Gross Grand List"))] <- "Number"	                                           

combined_final_long$"Measure Type"[which(combined_final_long$Variable %in% c("Commercial and Industrial Share of Total Net Grand List",        
                                                                             "Equalized Net Grand List per Capita as Percent of State Average"))] <- "Percent"  

combined_final_long$"Measure Type"[which(combined_final_long$Variable %in% c("Actual Mill Rate",                                                   
                                                                             "Equalized Mill Rate"))] <- "Mill Rate"                                        

#add FIPS (using raw URL from GitHub)
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

merge_long_fips <- merge(combined_final_long, fips, all=T)

#remove "Connecticut"
municipal_grand_list_data <- merge_long_fips[!merge_long_fips$Town == "Connecticut",]

#Reorder columns
municipal_grand_list_data <- municipal_grand_list_data[c("Town", "FIPS", "Year", "Measure Type", "Variable", "Value")]

#Sort data
municipal_grand_list_data <- arrange(municipal_grand_list_data, Year, Variable, `Measure Type`, Town)

# Write to File
write.table(
  municipal_grand_list_data,
  file.path(getwd(), "data", "municipal_grand_list_data_2001-2015.csv"),
  sep = ",",
  na = "",
  row.names = F
)


