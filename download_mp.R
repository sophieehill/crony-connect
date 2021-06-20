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

# to automate the script
library(cronR)
library(shinyFiles)
library(git2r)
# cron_rstudioaddin()

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
my_reg_list <- list.files("data/mp_register/") %>%
  substr(1, 6)

# check to see if there are any new registers to download
reg_to_download <- reg_list[!(reg_list %in% my_reg_list)]

# if there are no new registers available, do nothing
# if there are new registers avaiilable, download them and
# create a new "combo" file, push to git
if (length(reg_to_download) > 0){

# create column names
mp_colnames <- c("MP", "Constituency", "Party", "URL", "Type", "Description")

# create list of URLs
reg_url <- paste("https://membersinterests.blob.core.windows.net/register/", reg_to_download, ".zip", sep="")

for (i in 1:length(reg_to_download)){
print(i)
download.file(reg_url[i], destfile="temp.zip", mode="wb")
tab_temp <- read.table(unz("temp.zip", paste0(reg_to_download[i], ".csv")), header=F, sep=",", col.names=mp_colnames)
tab_temp$Register_date <- reg_to_download[i]
fst::write.fst(tab_temp, file = paste0("data/mp_register/", reg_to_download[i], ".fst"))
# delete these objects so we can be sure they are not
tab_temp  <- NULL
file.remove("temp.zip")
Sys.sleep(20)

}

file_names <- paste0("data/mp_register/", list.files("data/mp_register/"))
file_combo <- rbindlist(lapply(file_names, read.fst))
file_combo <- file_combo %>%
  mutate(URL_end = substr(URL, 55,nchar(URL)))

file_combo <- file_combo %>% select(-c(V1, MP, URL, Type, Register_date, Constituency))

rownames(file_combo) <- NULL
# remove any HTML tags in the description
# (some blocks of text are highlighted/bold for some reason)
# need to clear this so we can highlight the search results consistently
file_combo$Description <- gsub("<.*?>", "", file_combo$Description)

# remove duplicates
# since the registers are published several times a year
# there are lots of dupes in here
file_combo <- unique(file_combo, by="Description")

# write.csv(file_combo, "data/mp_register_combined.csv")
# use fst format with maximum compression
# smaller and faster than csv/fread
fst::write.fst(file_combo, "data/mp_register_combined.fst", 100)


# first select file to push
git2r::add(repo = getwd(), path = "mp_register_combined.fst")
# then commit changes
git2r::commit(repo = getwd(),
              message = paste0("Updated on : ", Sys.time())
)
# then pull any changes (for safety)
git2r::pull(repo = getwd(), credentials = NULL)
# then push this change
git2r::push(object = getwd())

}
