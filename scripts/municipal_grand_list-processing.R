library(dplyr)
library(RCurl)

##################################################################
#
# Processing Script for Municipal-Grand-List
# Created by Jenna Daly
# On 02/22/2017
#
##################################################################

sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
#grabs all csvs (even not FISCIN data)
all_csvs <- dir(path, recursive=T, pattern = ".csv") 
#isolates FISCIN csvs
only_FISCIN <- all_csvs[grep("FISCIN", all_csvs)] 

#create empty data frame with set columns (defines final working columns that are generated from raw columns in FISCIN)
all_data <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), 
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
  all_data <- all_data[all_data$Year!=SFY2,]
  all_data <- rbind(all_data, final_columns)
}

# Town names to title case
all_data$"Town" <- gsub("\\b([a-z])([a-z]+)", "\\U\\1\\E\\2", tolower(all_data$Town), perl=TRUE)

## Round numeric columns to whole numbers
round_df <- function(x, digits) {
  columns_to_round <- c("Current Year Adjusted Taxes Collectible",
                        "Equalized Net Grand List",   
                        "Actual Mill Rate",                                                   
                        "Net Grand List",		                                                 
                        "Population")
  columns_to_round <- sapply(x, mode) == 'numeric'
  all_data[columns_to_round] <-  round(all_data[columns_to_round], digits)
  all_data
}

all_data <- round_df(all_data, 0)

#Merge in GL Totals from component files

#Create calculated columns

"Equalized Net Grand List per Capita"
"Commercial and Industrial Share of Total Net Grand List"	
"Equalized Net Grand List per Capita as Percent of State Average"
"Equalized Mill Rate" 




##Municipal Grand List data on ctdata.org
#######################################################################################################################
#11 Variables
-Numbers:
"Equalized Net Grand List"                                           
"Equalized Net Grand List per Capita"	                               
"Gross Commercial Grand List"		                                     
"Gross Industrial Grand List"	                                       
"Gross Residential Grand List"	                                     
"Net Grand List"		                                                 
"Total Gross Grand List"	                                           

-Percents:
"Commercial and Industrial Share of Total Net Grand List"	           
"Equalized Net Grand List per Capita as Percent of State Average"	   

-Mill Rates:
"Actual Mill Rate"                                                   
"Equalized Mill Rate"   

Supporting columns:
"Population"
"Current Year Adjusted Taxes Collectible"





#Final Column Name                                                            Raw Column Name(s)
#=======================================================================================================================================================================================================================
"Equalized Net Grand List"                                                    FISCIN(EGL)
"Equalized Net Grand List per Capita"	                                        FISCIN(EGL/Population)
"Gross Commercial Grand List"                                                 collatedGLcomponents(Gross Commercial Grand List)
"Gross Industrial Grand List"                                                 collatedGLcomponents(Gross Industrial Grand List)
"Gross Residential Grand List"                                                collatedGLcomponents(Gross Residential Grand List)
"Net Grand List"                                                              FISIN(ACGLFY)
"Total Gross Grand List"                                                      collatedGLcomponents(Total Gross Grand List)
"Commercial and Industrial Share of Total Net Grand List"	                    collatedGLcomponents(Gross Commercial Grand List + Gross Industrial Grand List) / FISCIN(Net Grand List)      
"Equalized Net Grand List per Capita as Percent of State Average"             `Equalized Net Grand List per Capita` / [(sum(`Equalized Net Grand List`)/ sum(`Population`)]  over year
"Actual Mill Rate"                                                            FISCIN(ACMR)
"Equalized Mill Rate"                                                         `Current Year Adjusted Taxes Collectible` / `Equalized Net Grand List`
"Population"                                                                  FISCIN(Population)
"Current Year Adjusted Taxes Collectible"                                     FISCIN(Curr_Year_Adjusted_Taxes_Collectible)


