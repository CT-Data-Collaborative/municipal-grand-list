library("readxl")
#library("xlsx")
library(stringr)

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
toMatch1 <- c("2002")
for(i in 1:length(all_xls)) {
  if (grepl(paste(toMatch1, collapse="|"), all_xls[i]))
    current_file <- (read_excel(paste0(path, "/", all_xls[i]), sheet=3, skip=0))
  else
    current_file <- (read_excel(paste0(path, "/", all_xls[i]), sheet=1, skip=0))
    assign(paste0("GLdata_", substr(all_xls[i], 1, 4)), current_file)
}


#Step 2: With all years in, now remove duplicate header
#cycle through all df, remove duplicate headers

#2a: all years where we need to remove 2nd row
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
get_GLdata_only <- grep("^GLdata", dfs, value=T)
toMatch <- c("1995","2004","2005","2006","2007","2008")

for(i in 1:length(get_GLdata_only)) {
  if (grepl(paste(toMatch, collapse="|"), get_GLdata_only[i]))
    current_file <- get(get_GLdata_only[i])[-c(1), ]
  else
    current_file <- get(get_GLdata_only[i])
    assign(paste0("updated_GLdata_", substr(get_GLdata_only[i], 8, 12)), current_file)
}

#2b: all years where we need to set second row to header
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
get_updated_only <- grep("^updated", dfs, value=T)
toMatch2 <- c("2009", "2010", "2011")

for(i in 1:length(get_updated_only)) {
  if (grepl(paste(toMatch2, collapse="|"), get_updated_only[i])) {
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

#Step 3: Add year column
for (i in 1:length(get_updated_only)) {
  current_file <- get(get_updated_only[i])
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
  
  #search for "Town Code" 
  # town_code_col <- c("Town Code", "Code") #OR
  # town_code_col_select_grep <- grep(paste(town_code_col, collapse = "|"), col_names, value=T)
  # 
  # if (identical(town_code_col_select_grep, "Town Code")) { #if Town Code is found, populate it
  #   town_code_col_select <- "Town Code"
  # } else if (identical(town_code_col_select_grep, "Code")) { #if Code is found, populate it
  #   town_code_col_select <- "Code"
  # } else if (identical(character(0), town_code_col_select_grep)) { #if neither is found, set it to NA, will be imputed later
  #   NA_data <- get(get_updated_only[i])
  #   NA_data$"Town Code" <- NA
  #   town_code_col_select <- "Town Code"
  # }
  
  #search for "Town" 
  town_col <- c("Town Name", "Town") #OR
  town_col_select_grep <- grep(paste(town_col, collapse = "|"), col_names, value=T)
  
  if (identical(town_col_select_grep, "Town Name")) { #if Town Name is found, populate it
    town_col_select <- "Town Name"
  } else if (identical(town_col_select_grep, "Town")) { #if Town is found, populate it
    town_col_select <- "Town"
  } else if (identical(character(0), town_col_select_grep)) { #if neither is found, set it to NA, will be imputed later
    NA_data <- get(get_updated_only[i])
    NA_data$"Town Name" <- NA
    town_col_select <- "Town Name"
  } 
  
  #search for "Gross Commercial Grand List" only picks columns that are exactly "Commercial" or exactly "100"
  commercial_col <- c("Commercial$", "100$") #OR
  commercial_col_select <- grep(paste(commercial_col, collapse = "|"), col_names, value=T)
  # if ( identical(commercial_col, "Commercial")) {
  #   commercial_col_select <- "Commercial"
  # } else if ( identical(commercial_col, "100")) {
  #   commercial_col_select <- "100"
  # } 

  #search for "Gross Industrial Grand List"	 
  industrial_col <- c("Industrial$", "200$") #OR
  industrial_col_select <- grep(paste(industrial_col, collapse = "|"), col_names, value=T)
  # if ( identical(industrial_col, "Industrial")) {
  #   industrial_col_select <- "Industrial"
  # } else if ( identical(industrial_col, "200")) {
  #   industrial_col_select <- "200"
  # } 
  
  #search for "Gross Residential Grand List"	    
  residential_col <- c("Residential$", "300$") #OR
  residential_col_select <- grep(paste(residential_col, collapse = "|"), col_names, value=T)
  # if ( identical(residential_col, "Residential")) {
  #   residential_col_select <- "Residential"
  # } else if ( identical(residential_col, "300")) {
  #   residential_col_select <- "300"
  # } 
  
  #search for "Gross Real"
#####Manually updated 195_list.xlsx to have "Total Real" as column header
  gross_real_cols <- c("Total Real$")
  gross_real_cols_select <- grep(paste(gross_real_cols, collapse = "|"), col_names, value=T)
  # if (identical(gross_real_cols, "Total Real")) { #if Town Name is found, populate it
  #   gross_real_cols_select <- "Total Real"
  # } else if (identical(character(0), gross_real_cols)) { #if neither is found, set it to NA, will be imputed later
  #   #NA_data <- get(get_updated_only[i])
  #   NA_data$"Total Real" <- NA
  #   gross_real_cols_select <- "Total Real"
  # } 
  
  #search for "Gross Motor Vehicle"
  gross_mv_cols <- c("^Motor$", "^MV$", "Total MV$") #OR
  gross_mv_cols_select <- grep(paste(gross_mv_cols, collapse = "|"), col_names, value=T)
  # if ( identical(gross_mv_cols, "Motor")) {
  #   gross_mv_cols_select <- "Motor"
  # } else if ( identical(gross_mv_cols, "MV")) {
  #   gross_mv_cols_select <- "MV"
  # } 

  #search for "Gross Personal Property" 
  gross_pp_cols <- c("Total Personal Property$", "^Personal$", "^Pers$", "^Perp$",  "Total[Real]") #OR
  gross_pp_cols_select <- grep(paste(gross_pp_cols, collapse = "|"), col_names, value=T)


    if (     identical(character(0), town_col_select_grep)     ) {
      final_columns <- NA_data[, c(
                               #town_code_col_select_grep, 
                               town_col_select_grep, 
                               commercial_col_select, 
                               industrial_col_select,
                               residential_col_select,
                               #gross_total_col_select,
                               gross_real_cols_select,
                               gross_mv_cols_select,
                               gross_pp_cols_select,
                               "Year"
                               )]
    } else {
      final_columns <- get(get_updated_only[i])[, c(
                                                #town_code_col_select_grep, 
                                                town_col_select_grep, 
                                                commercial_col_select, 
                                                industrial_col_select,
                                                residential_col_select,
                                                #gross_total_col_select,
                                                gross_real_cols_select,
                                                gross_mv_cols_select,
                                                gross_pp_cols_select,
                                                "Year"
                                                )]
    }
 
    names(final_columns) <- c(
                            #"Town Code", 
                            "Town", 
                            "Gross Commerical Grand List", 
                            "Gross Industrial Grand List", 
                            "Gross Residental Grand List", 
                            #"Total Gross Grand List",
                            "Gross Real",
                            "Gross Motor Vehicle",
                            "Gross Personal Property",
                            "Year")
    assign(paste0("data_", i), final_columns)
  
}


for (i in 1:length(final_columns)) {
  all_GL_data <- rbind(all_GL_data, get(final_columns[i]))
}


#rm(list=ls(pattern="GLdata_"))


#municipal_grand_list-processing.R script should now source in 'collatedGLComponents.csv'


