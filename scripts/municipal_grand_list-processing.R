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
#source('scripts/componentGL-processing.R')

sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
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
for (i in 1:length(only_FISCIN)) {
  current_file <- read.csv(paste0(path, "/", only_FISCIN[i]), stringsAsFactors=F, header=T)
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
  #process and create destfile
} else {
  #set componentGL to destfile
  combined_componentGL <- read.csv(destfile, stringsAsFactors=F, header=T)
}
##Merge in GL Totals from component files
#read in latest GL data
# componentGL_file_path <-file.path(getwd(), "raw", "components")
# legacy_GL_csvs <- dir(componentGL_file_path, pattern = "componentGL")
# 
# sort(legacy_GL_csvs)
# 
# #selects latest version of componentGL file 
# last_GL_file <- tail(legacy_GL_csvs, 1)
# 
# legacy_GL_file_path <-file.path(getwd(), "raw", "components", last_GL_file)
# legacy_GL_file <- read.csv(legacy_GL_file_path, header=T, stringsAsFactors=F)
# 
# #use updated components csv as a resource in this script 
# #(each time data gets processed, update the folder it is coming from)
# current_GL_file_path <-file.path(getwd(), "raw", "2011-2015")
# 
# current_GL_csv <- dir(current_GL_file_path, pattern = "^GL") 
# 
# for (i in 1:length(current_GL_csv)) {
#   current_file <- read.csv(paste0(current_GL_file_path, "/", current_GL_csv[i]), stringsAsFactors=F, header=T)
#   get_year <- unique(as.numeric(unlist(gsub("[^0-9]", "", unlist(current_GL_csv[i])), "")))
#   SFY <- paste(get_year , get_year + 1, sep = "-")
#   SFY2 <- paste("SFY", SFY)
#   current_file$Year <- SFY2
#   col_names <- colnames(current_file)
# 
#   #assign town (brings in town or town code)
#   town_col <- col_names[1]
#   
#   #search for "Gross Commercial Grand List" 
#   commercial_col <- c("Commercial$", "100$") #OR
#   commercial_col_select <- grep(paste(commercial_col, collapse = "|"), col_names, value=T, ignore.case=T)
#   
#   #search for "Gross Industrial Grand List"	 
#   industrial_col <- c("Industrial$", "200$") #OR
#   industrial_col_select <- grep(paste(industrial_col, collapse = "|"), col_names, value=T, ignore.case=T)
#   
#   #search for "Gross Residential Grand List"	    
#   residential_col <- c("Residential$", "300$") #OR
#   residential_col_select <- grep(paste(residential_col, collapse = "|"), col_names, value=T, ignore.case=T)
#   
#   #search for "Gross Real"
#   #####Manually updated 1995_list.xlsx to have "Total Real" as column header, 2008: 'Real Exemptions' header
#   gross_real_cols <- c("TotalReal", "Total_Real", "2005 Real", "Total Real", "Total.Real")
#   #takes all columns that include gross_real_cols but excludes those that contain "Tax Property Exemptions"
#   gross_real_cols_select <- intersect(grep(paste(gross_real_cols, collapse = "|"), col_names, value=T, ignore.case=T), grep("Tax Property Exemptions", col_names, invert=TRUE, value=T, ignore.case=T))
#   
#   #search for "Gross Motor Vehicle"
#   gross_mv_cols <- c("Motor$", "^Total MV$", "Motor Vehicle$", "2005 MV", "Total Gross MV", "Total.MV") #OR
#   gross_mv_cols_select <- grep(paste(gross_mv_cols, collapse = "|"), col_names, value=T, ignore.case=T)
#   
#   # #search for "Gross Personal Property" 
#   gross_pp_cols <- c("Total Personal Property$", "Personal$", "Pers Prop", "^Perp$", "PP$", "Personal Prop", "Total.Pers") #OR
#   gross_pp_cols_select <- intersect(grep(paste(gross_pp_cols, collapse = "|"), col_names, value=T, ignore.case=T), grep("Net|Exemptions", col_names, invert=TRUE, value=T, ignore.case=T))
#   
#   #assign correct columns to final list from each file
#   final_columns <- current_file[, c(town_col,
#                                     commercial_col_select,
#                                     industrial_col_select,
#                                     residential_col_select,
#                                     gross_real_cols_select,
#                                     gross_mv_cols_select,
#                                     gross_pp_cols_select,
#                                     "Year")]
#   
#   #rename the columns
#   names(final_columns) <- c("Town",
#                             "Gross.Commercial.Grand.List", 
#                             "Gross.Industrial.Grand.List", 
#                             "Gross.Residential.Grand.List", 
#                             "Gross.Real",
#                             "Gross.Motor.Vehicle",
#                             "Gross.Personal.Property",
#                             "Year")
#   
#   assign(paste0("current_data_", i), final_columns)
# }
# 
# #update this line (shouldn't hard code it)
# current_GL_file <- current_data_1
# 
# rm(current_data_1, current_data_2)
# 
# # take out "Groton (City of)" because it is a political subdivision of groton and not the town.
# current_GL_file <- current_GL_file[current_GL_file$Town != "GROTON (City of)",]
# 
# # Town names to title case
# current_GL_file$"Town" <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\E\\2", tolower(current_GL_file$Town), perl=TRUE)
# 
# #Calculated columns
# current_GL_file$"Gross.Real" <- as.numeric(current_GL_file$"Gross.Real") 
# current_GL_file$"Gross.Motor.Vehicle" <- as.numeric(current_GL_file$"Gross.Motor.Vehicle") 
# current_GL_file$"Gross.Personal.Property" <- as.numeric(current_GL_file$"Gross.Personal.Property") 
# 
# current_GL_file$"Total.Gross.Grand.List" <- NA
# current_GL_file$"Total.Gross.Grand.List" <- current_GL_file$"Gross.Real" + current_GL_file$"Gross.Motor.Vehicle" + current_GL_file$"Gross.Personal.Property" 
# 
# #Select and reorder columns
# current_GL_file <- current_GL_file[,c(
#   "Town",                       
#   "Gross.Residential.Grand.List",
#   "Gross.Commercial.Grand.List",
#   "Gross.Industrial.Grand.List",
#   "Total.Gross.Grand.List",
#   "Year"            
# )]
# 
# current_GL_file<- arrange(current_GL_file, Year, Town)
# 
# #append new data
# combined_componentGL <- rbind(legacy_GL_file, current_GL_file)
# 
# existing_version <- as.numeric(substr(last_GL_file, 13, 16))
# new_version <- existing_version + 1
# 
# #save updated components csv back out to raw repo
# write.table(
#   combined_componentGL,
#   paste0(getwd(), "/", "raw", "/", "components", "/", "componentGL_", new_version, ".csv"),
#   sep = ",",
#   row.names = F
# )
###########################################################################################################################
#Clean up
#rm(list=ls(pattern="^data"))
#rm(current_file, final_columns, legacy_GL_file, current_GL_file)

#componentGL list = combined_componentGL
#all other data = municipal_grand_list_data

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
  file.path(getwd(), "data", "municipal_grand_list_data.csv"),
  sep = ",",
  na = "",
  row.names = F
)


