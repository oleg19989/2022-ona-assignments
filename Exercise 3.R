library(arrow)
library(readr)
applications <- read_parquet(paste0('C:\\Users\\oleg1\\Desktop\\McGill\\Classes\\Organizational Network Analysis\\app_data_sample.parquet'))
edges <- read_csv(paste0('C:\\Users\\oleg1\\Desktop\\McGill\\Classes\\Organizational Network Analysis\\edges_sample.csv'))

applications


edges

library(gender)
library(tidyverse)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it

# get a list of first names without repetitions
examiner_names <- applications %>% 
  distinct(examiner_name_first)

examiner_names

# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

examiner_names_gender

# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()

library(wru)

examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_surnames


examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()

examiner_race

examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

examiner_race


# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)

applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()

library(lubridate) # to work with dates

examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 

examiner_dates

examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))

examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
  ) %>% 
  filter(year(latest_date)<2018)

examiner_dates


applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc()


#Q2
#we pick 2 workgroups to focus on: 163 and 172

#creating a subset and summaries of the workgroup 163
wg163 <- subset(applications, grepl("^163", applications$examiner_art_unit))
wg163$gender <- factor(wg163$gender)
wg163$race <- factor(wg163$race)
summary(wg163$gender)
summary(wg163$race)
summary(wg163$tenure_days)

#creating a subset and summaries of the workgroup 172
wg172 <- subset(applications, grepl("^172", applications$examiner_art_unit))
wg172$gender <- factor(wg172$gender)
wg172$race <- factor(wg172$race)
summary(wg172$gender)
summary(wg172$race)
summary(wg172$tenure_days)

#combining 2 datasets together
wg163$workgroup <- c('wg_163')
wg172$workgroup <- c('wg_172')

combined = union(x=wg163,y=wg172)


#plot gender and race 
library(ggplot2)
#individually
ggplot(wg163, aes(gender, ..count..)) + geom_bar(aes(fill = race), position = "dodge")
ggplot(wg172, aes(gender, ..count..)) + geom_bar(aes(fill = race), position = "dodge")
#on combined workgroups
ggplot(combined, aes(workgroup, ..count..)) + geom_bar(aes(fill = race), position = "dodge")
ggplot(combined, aes(workgroup, ..count..)) + geom_bar(aes(fill = gender), position = "dodge")

#another visual
#individual
toPlot<-wg163%>%
  group_by(gender, race)%>%
  summarise(n = n())%>%
  group_by(race)%>%
  mutate(prop = n/sum(n))


ggplot(data = toPlot, aes(gender, prop, fill = race)) + 
  geom_col() + 
  facet_grid(~race)+
  scale_fill_manual(values = c("white","green", "red","blue"))

toPlot<-wg172%>%
  group_by(gender, race)%>%
  summarise(n = n())%>%
  group_by(race)%>%
  mutate(prop = n/sum(n))


ggplot(data = toPlot, aes(gender, prop, fill = race)) + 
  geom_col() + 
  facet_grid(~race)+
  scale_fill_manual(values = c("white","green", "red", "blue"))
#combined group
#for race
toPlot<-combined%>%
  group_by(workgroup, gender, race)%>%
  summarise(n = n())%>%
  group_by(race)%>%
  mutate(prop = n/sum(n))


ggplot(data = toPlot, aes(workgroup, prop, fill = race)) + 
  geom_col() + 
  facet_grid(~race)+
  scale_fill_manual(values = c("white","green", "red", "blue"))

#for gender (need help)
toPlot<-combined%>%
  group_by(workgroup, gender, race)%>%
  summarise(n = n())%>%
  group_by(race)%>%
  mutate(prop = n/sum(n))


ggplot(data = toPlot, aes(workgroup, prop, fill = gender)) + 
  geom_col() + 
  facet_grid(~race)+
  scale_fill_manual(values = c("white","green", "red", "blue"))

#Q3
#combining the datasets together
test = merge(x=combined,y=edges,by.x = 'examiner_id', by.y = 'ego_examiner_id', all.x = TRUE)
test = merge(x=combined,y=edges,by = 'application_number')

#plotting the directed graph
library(igraph)

graphdata <- subset(test, select = c('ego_examiner_id','alter_examiner_id','workgroup'))

graph1 <- graph_from_data_frame(graphdata, directed = TRUE)

library(tidygraph)
graph <- tbl_graph(edges = edges, nodes=combined$examiner_id, directed = TRUE)
#degree centrality
degree(graph1)

library(ggraph)

ggraph(graph, layout = "graphopt") + 
  geom_edge_link() + 
  geom_node_point(aes(color = workgroup), show.legend = FALSE)+
  theme_graph()


V(graph1)$degree <- degree(graph)
plot(graph1, layout=layout.fruchterman.reingold,
     vertex.size = 6,
     vertex.label = NA,
     vertex.label.cex = 0.8,
     vertex.label.dist = 1.5,
     vertex.label.color = "black",
     vertex.color = V(graph)$workgroup)

# add to have different colors for different workgroups: vertex.color = V(graph1)$workgroup)
#testing eigenvector centrality colored graph

V(graph)$ec <- eigen_centrality(graph, directed=T, weights=NA)$vector
hist(V(graph)$ec)

# You could use the scales package, or define this normalisation function:
normalize <- function(x){(x-min(x))/(max(x)-min(x))}
(V(graph)$ec_index <- round(normalize(V(graph)$ec) * 9) + 1)
#ec_index should now be a category between 1 and 10 for your centralities

V(graph)$color <- colorRampPalette(c("turquoise", "yellow","red"))(10)[V(graph)$ec_index]

# Look at what we did
table(V(graph)$color)
plot(graph, vertex.label=NA, vertex.size=5)