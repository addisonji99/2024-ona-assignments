linkedin_data=read.csv("Connections.csv", sep=",")
attach(linkedin_data)
View(linkedin_data)
library(dplyr)
library(stringr)
library(tidyr)
library(igraph)
library(ggplot2)

### Get the count of your contacts by their current employer + total count
# Get count by current employer
count_by_employer <- linkedin_data %>%
  group_by(Company) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
# View the counts by employer
print(count_by_employer)
# Calculate total count
total_count <- sum(count_by_employer$Count)
# Print total count
print(paste("Total count of contacts:", total_count))

###Create nodes and edges dataframes to use with igraph
# Adjusting the unique identifier for each individual
linkedin_data$Label <- with(linkedin_data, paste(First.Name, substr(Last.Name, 1, 1)))

linkedin_data <- linkedin_data %>%
  mutate(ID = row_number())

# Nodes dataframe now uses ID, Label and Company
nodes <- linkedin_data %>%
  distinct(ID, Label, Company)

linkedin_data_with_ids <- linkedin_data %>%
  left_join(nodes, by = c("Label", "Company"))

# Create edges based on these IDs within the same company
edges <- nodes %>%
  group_by(Company) %>%
  filter(n() > 1) %>%
  summarise(Combo = list(combn(ID, 2, simplify = FALSE))) %>%
  unnest(Combo) %>%
  ungroup() %>%
  mutate(From = sapply(Combo, `[`, 1),
         To = sapply(Combo, `[`, 2)) %>%
  select(From, To)

# View the edges dataframe
print(edges)

# Create graph from edges dataframe, using the updated nodes and labels
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
# Plot the graph
plot(g, vertex.label = V(g)$Label)

##############Visualize Networks of McGill Affliation############

# Create a new column 'McGillAffiliation' based on company name containing "McGill"
nodes <- nodes %>%
  mutate(McGillAffiliation = ifelse(str_detect(Company, "McGill"), "McGill", "Other"))
nodes$ID <- as.character(nodes$ID)

# Generate layout
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
layout <- as.data.frame(layout_with_fr(g))
names(layout) <- c("x", "y")
layout$ID <- V(g)$name

# Add McGillAffiliation information to the layout
layout <- layout %>%
  left_join(nodes %>% select(ID, McGillAffiliation), by = "ID")

edges$From <- as.character(edges$From)
edges$To <- as.character(edges$To)
# Join edge start positions
edges_coords <- edges %>%
  left_join(layout %>% select(ID, x_start = x, y_start = y), by = c("From" = "ID"))
# Join edge end positions
edges_coords <- edges_coords %>%
  left_join(layout %>% select(ID, x_end = x, y_end = y), by = c("To" = "ID"))

# Plotting
ggplot() +
  geom_segment(data = edges_coords, aes(x = x_start, y = y_start, xend = x_end, yend = y_end), color = "gray50") +
  geom_point(data = layout, aes(x = x, y = y, color = McGillAffiliation), size = 4) +
  scale_color_manual(values = c("McGill" = "red", "Other" = "blue")) +
  theme_void() +
  theme(legend.position = "right") +
  labs(color = "McGill Affiliation")
