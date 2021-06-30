# API is returning too many matches for specific search terms
# so I'm going to download the whole dataset and search it myself
# so avoid overloading we're going to only download each register once
# so check if any new registers are available before downloading
library(httr)
library(jsonlite)
library(tidyverse)
library(readr)
library(data.table)
library(fst)

# save static working directory here
# so that cron can get the file paths correct
static_wd <- "/Users/sophiehill/Google-Drive/Harvard/Tory-networks/crony-connect/"

# to automate the script
# library(cronR)
# cron_rstudioaddin()
# cron_ls()
# library(shinyFiles)
library(git2r)

base_url <- "https://www.membersinterests.org.uk/api/v1/commons/registers"

temp <- httr::GET(base_url)

# save list of available registers
current_reg_list <- temp %>%
  httr::content("text") %>%
  fromJSON(flatten=TRUE) %>%
  as.data.frame() %>%
  select(registerDate) %>%
  mutate(registerDate = substr(registerDate, 1, 10))

sort(current_reg_list$registerDate)

reg_list <- current_reg_list$registerDate

reg_list <- paste(substr(reg_list, 3,4),
                  substr(reg_list, 6, 7),
                  substr(reg_list, 9, 10), sep="")

# save list of registers already downloaded
current_data <- fst::read_fst(paste0(static_wd, "data/mp_register_combined.fst"))
names(current_data)
my_reg_list <- as.character(substr(current_data$URL_end, 1,  6))
my_reg_list <- unique(my_reg_list)
# test code by removing a few registers
# my_reg_list <-  my_reg_list[4:length(my_reg_list)]

# check to see if there are any new registers to download
reg_to_download <- reg_list[!(reg_list %in% my_reg_list)]

if (length(reg_to_download)==0){
  print(paste0("No new registers available. Updated as of: ", Sys.time()))
} else {

# if there are no new registers available, do nothing
# if there are new registers avaiilable, download them and
# create a new "combo" file, push to git

# create column names
mp_colnames <- c("MP", "Constituency", "Party", "URL", "Type", "Description")

# create list of URLs
reg_url <- paste("https://membersinterests.blob.core.windows.net/register/", reg_to_download, ".zip", sep="")

new_list <- list()

for (i in 1:length(reg_to_download)){
print(i)
download.file(reg_url[i], destfile="temp.zip", mode="wb")
tab_temp <- read.table(unz("temp.zip", paste0(reg_to_download[i], ".csv")), header=F, sep=",", col.names=mp_colnames)
tab_temp$Register_date <- reg_to_download[i]
# save each new dataset as an object in a list
new_list[[i]] <- as.data.frame(tab_temp)
# delete these objects so we can be sure they are not
# assign(paste0("tab_", reg_to_download[i]), tab_temp)
tab_temp  <- NULL
file.remove("temp.zip")
Sys.sleep(20)
}

new_data <- do.call("rbind", new_list)
new_data <- new_data[unique(new_data$Description),]
new_data <- new_data %>% mutate(URL_end = substr(URL, 55,nchar(URL)))
new_data <- new_data %>% select(-c(MP, URL, Type, Register_date, Constituency))

rownames(new_data) <- NULL
# remove any HTML tags in the description
# (some blocks of text are highlighted/bold for some reason)
# need to clear this so we can highlight the search results consistently
new_data$Description <- gsub("<.*?>", "", new_data$Description)

# bind current and new data
file_combo <- rbind(current_data, new_data)

# remove duplicates
# since the registers are published several times a year
# there are lots of dupes in here
file_combo <- unique(file_combo, by="Description")

# write.csv(file_combo, "data/mp_register_combined.csv")
# use fst format with maximum compression
# smaller and faster than csv/fread
fst::write_fst(file_combo, paste0(static_wd, "data/mp_register_combined.fst"), 100)


# first select file to push
git2r::add(repo = getwd(), path = "data/mp_register_combined.fst")
# then commit changes
git2r::commit(repo = getwd(),
              message = paste0("Updated on : ", Sys.time())
)
# then pull any changes (for safety)
git2r::pull(repo = getwd(), credentials = NULL)
# then push this change
git2r::push(object = getwd())

}


