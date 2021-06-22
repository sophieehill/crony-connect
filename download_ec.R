# use cronR package to automate this script
# install.packages("cronR")
# install.packages("shinyFiles")
# open the addin
library(cronR)
library(shinyFiles)
# cron_rstudioaddin()

library(fst)
library(tidyverse)

# find the most recent record in the data I've already downloaded
current_data <- fst::read.fst("data/donations_data.fst")
# the table is sorted by "AcceptedDate"
# so let's grab the top value
most_recent <- current_data[1,"AcceptedDate"]
most_recent_ID <- current_data[1, "ECRef"]
# now let's search the database for any more recent entries
year <- substr(most_recent, 7, 10)
month <- substr(most_recent, 4, 5)
day <- substr(most_recent, 1, 2)
date_urlformat <- paste(year, month, day, sep="-")

test_link1 <- "http://search.electoralcommission.org.uk/api/csv/Donations?start={start}&rows={pageSize}&query=&sort=AcceptedDate&order=desc&et=pp&et=ppm&et=tp&et=perpar&et=rd&date=Reported&from="

test_link2 <- "&to=&rptPd=&prePoll=false&postPoll=true&register=gb&register=ni&register=none&isIrishSourceYes=true&isIrishSourceNo=true&includeOutsideSection75=true"

test_link3 <- paste0(test_link1, date_urlformat, test_link2)

download.file(test_link3, destfile="/Users/sophiehill/Google-Drive/Harvard/Tory-networks/crony-connect/scrap/test.csv", mode="wb")

test <- read.csv("scrap/test.csv")

  if (test$ECRef[1] == most_recent_ID){

  print(paste0("No new results. Most recent record on: ",
               as.Date(most_recent, format="%d/%m/%Y"),
               ". Last checked: ", Sys.time()))
  } else {

# this is the URl for a blank search on the
# electoral commission website
# which returns all results
    new_data <- rbind(test, current_data) %>%
      select(ECRef, RegulatedEntityName, Value, AcceptedDate,
             DonorName, DonorStatus, RegulatedDoneeType,
             DonationType, NatureOfDonation, ReportedDate)
    # remove duplicates
    # (since the "new data" starts on the most recent day of the
    # old data, so records from that day will be duplicated)
        new_data <- new_data[!duplicated(new_data$ECRef),]



# use fst instead of csv
write.fst(new_data, "data/donations_data.fst", 100)

# save zipped file

# use git2r to push to git automatically
# also need to install libssh2 and libgit2:
# in Terminal:
# brew install libssh2
# brew install libgit2
# install.packages("git2r", type="source", configure.vars="autobrew=yes")
install.packages("git2r",
                 type = "source",
                 configure.vars='LIBS=-L/usr/local/Cellar/libssh2/1.9.0_1/lib'
)
library(git2r)
# check that SSH is enabled:
git2r::libgit2_features()


# first select file to push
git2r::add(repo = getwd(), path = "donations_data.fst")
# then commit changes
git2r::commit(repo = getwd(),
              message = paste0("Updated on : ", Sys.time())
)
# then pull any changes (for safety)
git2r::pull(repo = getwd(), credentials = NULL)
# then push this change
git2r::push(object = getwd())

}
