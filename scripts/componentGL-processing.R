library("readxl")
library(stringr)
library(dplyr)

##################################################################
#
# Processing Script for componentGL
# Created by Jenna Daly
# On 02/23/2017
#
##################################################################

#reads in legacy excel files, creates 'componentGL.csv'
#this file will be the master file for all historical GL data
#In the future, we will be able to append new years to this existing file

#The following code is used to generate initial master file

sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location, "/", "components"))
#grabs all csvs (even not FISCIN data)
all_xls <- dir(path, pattern = ".xls")

#Step 1: Bring in all years, correct sheet from xls
#bring all files into working environment

#all the years where we need to read in sheet 3
sheet_match <- c("2002")
for(i in 1:length(all_xls)) {
  if (grepl(paste(sheet_match, collapse="|"), all_xls[i]))
    current_file <- (read_excel(paste0(path, "/", all_xls[i]), sheet=3, skip=0))
  else
    current_file <- (read_excel(paste0(path, "/", all_xls[i]), sheet=1, skip=0))
  assign(paste0("GLdata_", substr(all_xls[i], 1, 4)), current_file)
}

#Step 2: With all years in, now remove duplicate header
#2a: all years where we need to remove 2nd row
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
get_GLdata_only <- grep("^GLdata", dfs, value=T)
row_match1 <- c("1995","2004","2005","2006","2007","2008")

for(i in 1:length(get_GLdata_only)) {
  if (grepl(paste(row_match1, collapse="|"), get_GLdata_only[i]))
    current_file <- get(get_GLdata_only[i])[-c(1), ]
  else
    current_file <- get(get_GLdata_only[i])
  assign(paste0("updated_GLdata_", substr(get_GLdata_only[i], 8, 12)), current_file)
}

#2b: all years where we need to set second row to header
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
get_updated_only <- grep("^updated", dfs, value=T)
row_match2 <- c("2009", "2010", "2011")

for(i in 1:length(get_updated_only)) {
  if (grepl(paste(row_match2, collapse="|"), get_updated_only[i])) {
    current_file <- get(get_updated_only[i])
    colnames(current_file) = current_file[1, ] # the first row will be the header
    current_file = current_file[-1, ]
    colnames(current_file)[2] <- "Town"
    assign(paste0("updated_GLdata_", substr(get_updated_only[i], 16, 20)), current_file)    
  } else {
    current_file <- get(get_updated_only[i])
    assign(paste0("updated_GLdata_", substr(get_updated_only[i], 16, 20)), current_file)    
  }
}    

#Step 3: Add year column and only take first 169 rows
for (i in 1:length(get_updated_only)) {
  current_file <- get(get_updated_only[i])
  current_file <- current_file[1:169,]
  get_year <- substr(get_updated_only[i], 16, 20)
  get_year <- as.numeric(get_year)
  SFY <- paste(get_year + 1, get_year + 2, sep = "-")
  SFY2 <- paste("SFY", SFY)
  current_file$Year <- SFY2  
  assign(paste0("updated_GLdata_", substr(get_updated_only[i], 16, 20)), current_file)  
}

#Step 4: read in select columns and bind together
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
get_updated_only <- grep("^updated", dfs, value=T)

#create empty data frame
all_GL_data <- data.frame(stringsAsFactors=F)

for (i in 1:length(get_updated_only)) {
  col_names <- colnames(get(get_updated_only[i]))
  
  #assign town (brings in town or town code)
  town_col <- col_names[1]
  
  #search for "Gross Commercial Grand List" 
  commercial_col <- c("Commercial$", "100$") #OR
  commercial_col_select <- grep(paste(commercial_col, collapse = "|"), col_names, value=T, ignore.case=T)

  #search for "Gross Industrial Grand List"	 
  industrial_col <- c("Industrial$", "200$") #OR
  industrial_col_select <- grep(paste(industrial_col, collapse = "|"), col_names, value=T, ignore.case=T)

  #search for "Gross Residential Grand List"	    
  residential_col <- c("Residential$", "300$") #OR
  residential_col_select <- grep(paste(residential_col, collapse = "|"), col_names, value=T, ignore.case=T)

  #search for "Gross Real"
  #####Manually updated 1995_list.xlsx to have "Total Real" as column header, 2008: 'Real Exemptions' header
  gross_real_cols <- c("TotalReal", "Total_Real", "2005 Real", "Total Real")
  #takes all columns that include gross_real_cols but excludes those that contain "Tax Property Exemptions"
  gross_real_cols_select <- intersect(grep(paste(gross_real_cols, collapse = "|"), col_names, value=T, ignore.case=T), grep("Property Tax Exemptions", col_names, invert=TRUE, value=T, ignore.case=T))
  
  #search for "Gross Motor Vehicle"
  gross_mv_cols <- c("Motor$", "Total MV$", "Motor Vehicle$", "2005 MV", "Total Gross MV") #OR
  gross_mv_cols_select <- grep(paste(gross_mv_cols, collapse = "|"), col_names, value=T, ignore.case=T)

  # #search for "Gross Personal Property" 
  gross_pp_cols <- c("Total Personal Property$", "Personal$", "Pers Prop", "^Perp$", "PP$", "Personal Prop") #OR
  gross_pp_cols_select <- intersect(grep(paste(gross_pp_cols, collapse = "|"), col_names, value=T, ignore.case=T), grep("Net|Exemptions", col_names, invert=TRUE, value=T, ignore.case=T))
  
  #assign correct columns to final list from each file
  final_columns <- get(get_updated_only[i])[, c(town_col,
                                                commercial_col_select,
                                                industrial_col_select,
                                                residential_col_select,
                                                gross_real_cols_select,
                                                gross_mv_cols_select,
                                                gross_pp_cols_select,
                                                "Year")]
  
  #rename the columns
  names(final_columns) <- c("Town Code",
                            "Gross Commercial Grand List", 
                            "Gross Industrial Grand List", 
                            "Gross Residential Grand List", 
                            "Gross Real",
                            "Gross Motor Vehicle",
                            "Gross Personal Property",
                            "Year")
  
  assign(paste0("data_", i), final_columns)
  
}

#Clean up
rm(list=ls(pattern="^updated"))
rm(list=ls(pattern="^GLdata"))

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
final <- grep("^data", dfs, value=T)

final <- final[order(nchar(final), final)]

#create empty data frame
GL_data <- data.frame(stringsAsFactors=F)

#bind all years together
for (i in 1:length(final)) {
  GL_data <- rbind(GL_data, get(final[i]))
}

#Clean up 
rm(list=ls(pattern="^data"))

#Step 5: Merge in town-code xwalk file to assign Town names
colnames(GL_data)[1] <- "TownCode"

town_code_xwalk_file <-file.path(getwd(), "raw", "components", "town_town-code_crosswalk.csv")
town_code_xwalk <- read.csv(town_code_xwalk_file, header=T, stringsAsFactors=F)

coded_GL_data <- merge(town_code_xwalk, GL_data, by = "TownCode")

coded_GL_data$`TownCode` <- NULL

all_GL_data <- arrange(coded_GL_data, Year, Town)

#Calculated columns
all_GL_data$"Gross Real" <- as.numeric(all_GL_data$"Gross Real") 
all_GL_data$"Gross Motor Vehicle" <- as.numeric(all_GL_data$"Gross Motor Vehicle") 
all_GL_data$"Gross Personal Property" <- as.numeric(all_GL_data$"Gross Personal Property") 

all_GL_data$"Total Gross Grand List" <- NA
all_GL_data$"Total Gross Grand List" <- all_GL_data$"Gross Real" + all_GL_data$"Gross Motor Vehicle" + all_GL_data$"Gross Personal Property" 

#Create Subset of corrected year
#all_GL_data_2014 <- all_GL_data[all_GL_data$Year == "SFY 2014-2015",]

#Rename columns
all_GL_data <- all_GL_data[,c(
                              "Town",                       
                              "Gross Residential Grand List",
                              "Gross Commercial Grand List",
                              "Gross Industrial Grand List",
                              "Total Gross Grand List",
                              "Year"            
                              )]

all_GL_data<- arrange(all_GL_data, Year, Town)

#Clean up
rm(current_file, coded_GL_data, final_columns, GL_data, town_code_xwalk)

# Write to File 
write.table(
  all_GL_data,
  file.path(getwd(), "raw", "components", "componentGL_2015.csv"),
  sep = ",",
  row.names = F
)

rm(all_GL_data)

#In the future, componentGL.csv, componentGL_latest.csv, and town_town-code_crosswalk.csv should be the only files in raw/components
#municipal_grand_list-processing.R script should now source in 'componentGL.csv'


