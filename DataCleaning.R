library(tidyverse)

sql.motus <- tagme(proj.num, update = TRUE)
tbl.alltags <- tbl(sql.motus, "alltags")

df.alltags <- tbl.alltags %>% 
  mutate(recvLat = if_else((is.na(gpsLat)|gpsLat == 0), # replacing NA/0 values in gpsLat/gpsLon variables
                           recvDeployLat, gpsLat), 
         recvLon = if_else((is.na(gpsLon)|gpsLon == 0), 
                           recvDeployLon, gpsLon), 
         recvAlt = if_else(is.na(gpsAlt), recvDeployAlt, gpsAlt)) %>%
  select(-noise, -slop, -burstSlop, -done, -bootnum, -mfgID, # removing unwanted variables
         -codeSet, -mfg, -nomFreq, -markerNumber, -markerType, 
         -tagDeployComments, -fullID, -deviceID, -recvDeployLat, 
         -recvDeployLon, -recvDeployAlt, -speciesGroup, -gpsLat, 
         -gpsLon, - recvAlt, - recvSiteName) %>%
  collect() %>% 
  as.data.frame() %>%
  mutate(ts = as_datetime(ts), # work with dates AFTER transforming to flat file
         tagDeployStart = as_datetime(tagDeployStart),
         tagDeployEnd = as_datetime(tagDeployEnd), 
         recvLat = plyr::round_any(recvLat, 0.05), # rounding latitude/longitude
         recvLon = plyr::round_any(recvLon, 0.05),
         recvDeployName = if_else(is.na(recvDeployName), # replacing instances of no deployment name with lat/lon values
                                  paste(recvLat, recvLon, sep=":"), 
                                  recvDeployName))


# We may revisit those detections with a runLen = 2, but for now we just remove them:
df.alltags.sub <- filter(df.alltags, runLen > 2)
# we'll also create a filter to remove anything with a runLen == 2
df.block.0 <- filter(df.alltags, runLen == 2) %>% select(motusTagID, 
                                                         runID) %>% distinct()

# we can quickly plot summarized detections by lat/lon to see any obvious outliers
fun.getpath <- function(df) 
{
  df %>%
    filter(tagProjID == proj.num, # keep only tags registered to the sample project
           !is.na(recvLat) | !(recvLat == 0)) %>% # drops data without lon/lat
    group_by(motusTagID, runID, recvDeployName, ambigID, 
             tagDeployLon, tagDeployLat, recvLat, recvLon) %>%
    #summarizing by runID to get max run length and mean time stamp:
    summarize(max.runLen = max(runLen), 
              ts.h = mean(ts)) %>% 
    arrange(motusTagID, ts.h)
} # end of function call

df.alltags.path <- fun.getpath(df.alltags.sub)

p <- ggplot(data = filter(df.alltags.path, motusTagID %in% c(16011, 16035, 16036, 16037, 16038, 16039)), 
            aes(ts.h, recvLat))
p + geom_point() + geom_path() + theme_bw() + 
  facet_wrap(~motusTagID, scales = "free", ncol = 2) + theme(axis.text.x = element_text(angle = 45, 
                                                                                                                       vjust = 1, hjust = 1))
# a further examination of tags detected around 44 degrees in the winter:
filter(df.alltags.sub, month(ts) %in% c(12, 1), motusTagID %in% 
         c(16036, 16038, 16039)) %>% group_by(recvDeployName, month(ts), runLen) %>% 
        summarize(n = length(ts), n.tags = length(unique(motusTagID)))

# now we'll get a list of these motusTagIDs and runIDs that we have identified as false positives above
# create the filter
df.block.1 <- filter(df.alltags.sub, month(ts) %in% 
                       c(12, 1), motusTagID %in% c(16036, 16038, 16039)) %>% 
  select(motusTagID, runID) %>% distinct()

# use the function we created earlier to make a new
# 'path' data frame for plotting
df.alltags.path <- fun.getpath(filter(df.alltags.sub, 
                                      motusTagID %in% c(16011, 16035, 16036, 16037, 16038, 
                                                        16039), !(runID %in% df.block.1$runID)))

p <- ggplot(data = df.alltags.path, aes(ts.h, recvLat))
p + geom_point() + geom_path() + theme_bw() + facet_wrap(~motusTagID, 
                                                         scales = "free", ncol = 2) + theme(axis.text.x = element_text(angle = 45, 
                                                                                                                       vjust = 1, hjust = 1))

## Ambiguous tags
clarify(sql.motus)

# lets get a databse of only our ambiguous detections:
df.ambigTags <- select(df.alltags.sub, ambigID, motusTagID) %>% 
  filter(!is.na(ambigID)) %>% distinct()

# we'll use the getpath function to create paths and plot the detections
df.alltags.path <- fun.getpath(filter(df.alltags.sub, 
                                      motusTagID %in% df.ambigTags$motusTagID, 
                                      tagProjID == proj.num)) %>%
  mutate(Ambiguous = !(is.na(ambigID)))

# to put all ambiguous tags from the same project
# on the same plot together, we need to create a
# new 'ambig tag' variable we call 'newID'.

ambigTags.2 <-  df.alltags.sub %>% select(ambigID, motusTagID) %>% 
                filter(!is.na(ambigID)) %>% distinct() %>% 
                group_by(ambigID) %>% 
                summarize(newID = paste(unique(ambigID), toString(motusTagID), sep = ": ")) %>% 
                left_join(df.ambigTags, by = "ambigID")

# and merge that with df.alltags.path
df.alltags.path <- left_join(df.alltags.path, ambigTags.2, 
                             by = "motusTagID") %>% arrange(ts.h)

p <- ggplot(data = df.alltags.path, aes(ts.h, recvLat, group = Ambiguous, colour = Ambiguous))
p + geom_point() + geom_path() + theme_bw() + 
  facet_wrap(~newID,scales = "free", ncol = 2) + theme(axis.text.x = element_text(angle = 45, 
                                                                                                                       vjust = 1, hjust = 1))
# Let's start with a very simple ambig tag: -337, and look at deployment locations
filter(df.alltags.sub, ambigID == -337) %>% group_by(motusTagID, tagDeployStart, tagDeployEnd, 
                                                     tagDeployLat, tagDeployLon) %>% tally()
# we remove the ambiguous detections assigned to the other tag:
df.block.2 <- filter(df.alltags.sub, ambigID == -337, 
                     motusTagID == 10811) %>% select(motusTagID, runID) %>% 
  distinct()

# a more complex example: -171
# lets look at the deployment dates and locations:
filter(df.alltags, ambigID == -171) %>% filter(!is.na(tagDeployStart)) %>% 
  select(motusTagID, tagProjID, start = tagDeployStart, 
         end = tagDeployEnd, lat = tagDeployLat, lon = tagDeployLon, 
         species = speciesEN) %>% distinct() %>% arrange(start)
# then plot the detections by signal strength
df.ambig.171 <- filter(df.alltags.sub, ambigID == -171)

p <- ggplot(data = df.ambig.171, aes(ts, sig, colour = as.factor(port)))
p + geom_point() + geom_smooth(method = "loess", se = FALSE) + 
  theme_bw() + facet_wrap(as_date(ts) ~ recvDeployName, 
                          scales = "free_x") + theme(axis.text.x = element_text(angle = 45, 
                                                                                vjust = 1, hjust = 1))
# so we remove all ambiguous detections for this tag:
df.block.4 <- filter(df.alltags.sub, ambigID == -171) %>% 
  select(motusTagID, runID) %>% distinct()


## Filtering
# Once you have all your filters, you can apply them to your data

# first combine all the df blocks into one, and assign a probability of 0 to all records
df.block.all <- bind_rows(df.block.0, df.block.1, df.block.2, 
                          df.block.4) %>% mutate(probability = 0)
# assign a probability of 1 to all the records we want to keep
df.alltags.sub <- left_join(df.alltags, df.block.all, 
                            by = c("runID", "motusTagID")) %>%
  mutate(probability = ifelse(is.na(probability), 1, 
                              probability)) %>% filter(probability > 0)

# you can now save this as an RDS for future use
saveRDS(df.alltags.sub, file = "./dfAlltagsSub.rds")



# Alternatively, you can save filters directly within your .motus file
# create a new filter with the name filtAmbigFalsePos and populate it with your df.block.all
tbl.filter = writeRunsFilter(sql.motus, "filtAmbigFalsePos", 
                             df = df.block.all, delete = TRUE)

# obtain a table object of the filter
tbl.filter = getRunsFilters(sql.motus, "filtAmbigFalsePos")

# filter and convert the table into a dataframe, with a few modications
df.alltags.sub <- left_join(tbl.alltags, tbl.filter, by = c("runID", "motusTagID")) %>%
  mutate(probability = ifelse(is.na(probability), 1, probability),
         recvLat = if_else((is.na(gpsLat)|gpsLat == 0), 
                           recvDeployLat, 
                           gpsLat),
         recvLon = if_else((is.na(gpsLon)|gpsLon == 0), 
                           recvDeployLon, 
                           gpsLon),
         recvAlt = if_else(is.na(gpsAlt), 
                           recvDeployAlt, 
                           gpsAlt)) %>%
  filter(probability > 0) %>%
  select(-noise, -slop, -burstSlop, -done, -bootnum, -codeSet, 
         -mfg, -nomFreq,-markerNumber, -markerType, -tagDeployComments, 
         -fullID, -deviceID,-recvDeployLat, -recvDeployLon, -recvDeployAlt, 
         -speciesGroup, -gpsLat,-gpsLon, - recvAlt, - recvSiteName) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(ts = as_datetime(ts),  # work with dates AFTER transforming to flat file
         tagDeployStart = as_datetime(tagDeployStart),
         tagDeployEnd = as_datetime(tagDeployEnd))







