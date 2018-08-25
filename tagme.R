install.packages("devtools")
library(devtools)

# install motus for data download, data
# manipulation, visualization and analysis
install_github("MotusWTS/motus")

# install motusClient for data download
install_github("MotusWTS/motusClient")

library(motus)

# set the system environment to GMT
Sys.setenv(TZ = "GMT")

# specify the project number or receiver ID that you wish to download
proj.num <- "SG-1234BBBK4321" # receiver data
proj.num <- 176 # project data

# Download a .motus sql file to your working directory
sql.motus <- tagme(projRecv = proj.num, new = TRUE, 
                   update = TRUE)
# OR you can specify a different location to save
# the data by entering your preferred filepath
sql.motus <- tagme(projRecv = proj.num, new = TRUE, 
                   update = TRUE, dir = "C:/Users/zcrysler/Documents/data/")

# For our purposes we're most interested in the "alltags" view
# the following code retrieves the "alltags" view from the sql.motus file we created 
tbl.alltags <- tbl(sql.motus, "alltags")  # virtual table

# Let's convert the virtual table into a flat database for easy viewing
df.alltags <- tbl.alltags %>% collect() %>% as.data.frame()

# convert ts into date/time format
df.alltags <- mutate(df.alltags, ts = as_datetime(ts, tz = "UTC"))




## Other ways to load data:

# select certain variables, in this case a unique list of Motus tag IDs at 
# each receiver and antenna.
df.alltags.sub <- select(tbl.alltags, recv, port, motusTagID) %>% 
  distinct() %>% collect() %>% as.data.frame()

# filter to only Red Knot (using English name)
df.redKnot <- filter(tbl.alltags, speciesEN == "Red Knot") %>% 
  collect() %>% as.data.frame() %>% mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

# you can also summarize data prior to converting it to a flat file using dplyr()
# for example to determine the number of detections of each tag at each receiver:
df.detectSum <- tbl.alltags %>% 
                group_by(motusTagID, recv) %>% 
                tally() %>% 
                collect() %>% 
                as.data.frame()
  

  
## additional features:
# to determine if there is any new data available:
tellme(projRecv = proj.num)

# To force an update/re-import of tag/receiver metadata:
sql.motus <- tagme(projRecv = proj.num, forceMeta = TRUE)

# to import full NETWORK tag and receiver metadata:
metadata(sql.motus)
  