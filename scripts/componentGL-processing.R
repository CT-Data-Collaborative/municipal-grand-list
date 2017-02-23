library("readxl")
library("xlsx")

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
  col_names <- colnames(get(get_updated_only[1]))
  
  #search for "Town Code" 
  town_code_col <- c("Town Code", "Code") #OR
  town_code_col_select <- grep(paste(town_code_col, collapse = "|"), col_names, value=T)
  
  if (identical(town_code_col, "Town Code")) { #if Town Code is found, populate it
    town_code_col_select <- "Town Code"
  } else if (identical(town_code_col, "Code")) { #if Code is found, populate it
    town_code_col_select <- "Code"
  } else if (identical(character(0), town_code_col)) { #if neither is found, set it to NA, will be imputed later
    final_columns$"Town Code" <- NA
    town_code_col_select <- "Town Code"
  }
  
  #search for "Town" 
  town_col <- c("Town Name", "Town") #OR
  town_col_select <- grep(paste(town_col, collapse = "|"), col_names, value=T)
  
  if (identical(town_col, "Town Name")) { #if Town Name is found, populate it
    town_col_select <- "Town Name"
  } else if (identical(town_col, "Town")) { #if Town is found, populate it
    town_col_select <- "Town"
  } else if (identical(character(0), town_col)) { #if neither is found, set it to NA, will be imputed later
    final_columns$"Town Name" <- NA
    town_col_select <- "Town Name"
  } 
  
  #search for "Gross Commercial Grand List"
  commercial_col <- c("Commercial", "100") #OR
  commercial_col_select <- grep(paste(commercial_col, collapse = "|"), col_names, value=T)

  #search for "Gross Industrial Grand List"	 
  industrial_col <- c("Industrial", "200") #OR
  industrial_col_select <- grep(paste(industrial_col, collapse = "|"), col_names, value=T)
  
  #search for "Gross Residential Grand List"	    
  residential_col <- c("Residential", "300") #OR
  residential_col_select <- grep(paste(residential_col, collapse = "|"), col_names, value=T)
  
  #search for "Gross Total"
  #gross_total_col <- "Gross" #only (without anything else)
  
  gross_total_col <- grep("Gross[^MV]", col_names, ignore.case=T, value=T)
  
  if (identical(gross_total_col, "GROSS")) { #if GROSS is found, populate it
    gross_total_col_select <- "GROSS"
  } else if (identical(character(0), gross_total_col)) { #if GROSS is not found, set it to NA, will be calculated later
    final_columns$"GROSS" <- NA
    gross_total_col_select <- "GROSS"
  }

  #search for "Gross Real"
  gross_real_cols <- c("Total", "Real") #AND
  gross_real_cols_select <- grep(paste(gross_real_cols, collapse = ","), col_names) ###need to fix this
  
  #search for "Gross Motor Vehicle"
  gross_mv_cols <- c("Motor", "MV") #OR
  gross_mv_cols_select <- grep(paste(gross_mv_cols, collapse = "|"), col_names, value=T)
  #gross_mv_cols <- agrep("Motor;MV", col_names, ignore.case=T, value=T) #Motor or MV
  
  #search for "Gross Personal Property" 
  gross_pp_cols <- c("Personal Property", "Personal", "Pers", "Perp",  "Total[^Real]") #OR
  gross_pp_cols_select <- grep(paste(gross_pp_cols, collapse = "|"), col_names, value=T)
  
  final_columns <- get(get_updated_only[1])[, c(town_code_col_select, 
                                                town_col_select, 
                                                commercial_col_select, 
                                                industrial_col_select,
                                                residential_col_select,
                                                #gross_total_col_select,
                                                gross_real_cols_select,
                                                gross_mv_cols_select,
                                                gross_pp_cols_select,
                                                "Year"
                                                )]
 
  names(final_columns) <- c("Town Code", "Town", 
                            "Gross Commerical Grand List", 
                            "Gross Industrial Grand List", 
                            "Gross Residental Grand List", 
                            "Total Gross Grand List",
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


