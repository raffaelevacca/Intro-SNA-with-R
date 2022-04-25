# Author: Raffaele Vacca <raffaele.vacca@unimi.it>
#
# 
# License: Creative Commons Attribution-NonCommercial-ShareAlike CC BY-NC-SA 
# http://creativecommons.org/licenses/by-nc-sa/3.0/


############################################################################## #
###                 IMPORTING AND DISPLAYING NETWORK DATA                   ====
############################################################################## #
## ---- import

# Packages
library(tidyverse)
library(igraph)
library(skimr)
library(janitor)
library(ggraph)

# Read the edgelist data into R
(elist <- read_csv("./Data/class_edgelist_clean.csv"))

# Using a function from igraph, convert the data frame above into a network.
graph <- graph_from_data_frame(elist)

# In igraph, networks are objects of class "igraph"
class(graph)

# Summary information about this network
graph
# The graph is Directed, Named, NOT Weighted, NOT Bipartite.
# The graph has 12 nodes and 144 edges. 
# It has a vertex attribute called "name" and an edge attribute called
# "tie_weight".

# Plot the network
# Set the seed for reproducibility (more on this below)
set.seed(221)
# plot
plot(graph)

# The plot() function is extremely flexible. We can set vertex 
# parameters (size, color etc.), edge parameters (width, color, line type etc.), 
# label parameters (font, color, size), vertex layout, and more. See 
# http://igraph.org/r/doc/plot.common.html for details.

# For example, let's plot with smaller arrows and labels for more clarity.
# Set the seed for reproducibility (more on this below).
set.seed(221)
# Plot
plot(graph, edge.arrow.size=0.5, vertex.label.cex=0.5)

# Let's now also import vertex attributes
(vert.attr <- read_csv("./Data/class_attributes.csv"))

# The same function we used above can import vertex attributes together with edge data.
graph <- graph_from_data_frame(d= elist, vertices= vert.attr)

# The igraph object now includes vertex attributes
graph

# Two ways to fix the network layout for visualization.

# 1) Set the same seed before each plot
set.seed(215)
plot(graph, edge.arrow.size=0.5)
# Plot again
set.seed(215)
plot(graph, edge.arrow.size=0.5)

# 2) Calculate network layout matrix separately, and always use that matrix for plot
set.seed(215)
layout.mat.fr <- layout_(graph=graph, layout=with_fr())

# Plot using that layout matrix
plot(graph, layout=layout.mat.fr, edge.arrow.size=0.5)

# Plot again
plot(graph, layout=layout.mat.fr, edge.arrow.size=0.5)

# Plot using different layout algorithms
# As star
set.seed(215)
layout.mat.st <- layout_(graph=graph, layout=as_star())
plot(graph, layout=layout.mat.st, edge.arrow.size=0.5)

# Kamada-kawai
set.seed(215)
layout.mat.kk <- layout_(graph=graph, layout=with_kk())
plot(graph, layout=layout.mat.kk, edge.arrow.size=0.5)

# You can use any matrix (with 2 columns and N rows, N being the number of
# vertices) for the graph layout, e.g. a matrix with spatial coordinates.

# More information on layout functions: http://igraph.org/r/doc/layout_.html

# A layout matrix can be set as the "default" layout matrix for a 
# graph, by setting it as a graph attribute called "layout". If we do that, 
# plot() will always (silently) use that matrix as graph's layout.
graph$layout <- layout.mat.kk 

# Now we don't need to set the "layout" argument any more.
plot(graph, edge.arrow.size=0.5)

# To export the plot to an external file, use png() or pdf()
pdf("graph_kk.pdf")
plot(graph)
dev.off()

# Plot with ggraph and ggplot2 grammar
ggraph(graph) + 
  # Draw edges
  geom_edge_link(
    # Specify arrow size
    arrow = arrow(length = unit(2, 'mm')),
    # Distance between edge end and node
    end_cap = circle(3, 'mm'),
    # Distance between edge start and node
    start_cap = circle(3, 'mm')) + 
  # Draw nodes
  geom_node_point(color= "blue", fill = "lightblue", shape = 21, size=5) + 
  # Draw node labels (names)
  geom_node_text(aes(label = name), color= "black", size=3) + 
  # Theme details
  theme_graph(base_family = 'Helvetica')


## ---- end-import
############################################################################## #
###                       ATTRIBUTES AND INDEXING                           ====
############################################################################## #
## ---- attr-indexing

# Vertex sequence of the graph
V(graph)

# Edge sequence of the graph
E(graph)

# Indexing based on vertex and edge attributes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Extract vertex attribute
V(graph)$age

# A vertex attribute is just a vector that we can re-use for any operation. For
# example: What's the average age in the network?
V(graph)$age %>% 
  mean
# What's its standard deviation?
V(graph)$age %>% 
  sd
# Battery of descriptive stats using skimr functions
V(graph)$age %>% 
  skimr::skim()

# What's the distribution of gender in the network?
V(graph)$sex %>% 
  tabyl

# Vertex names are a vertex attribute created by default by graph_from_data_frame()
V(graph)$name

# Extract edge attribute
E(graph)$tie_weight

# Average tie weight (i.e., strength) in the network
E(graph)$tie_weight %>% 
  mean

# View female actors
V(graph)[sex=="F"]

# View strong ties
E(graph)[tie_weight > 2]

# View age of female actors
V(graph)[sex=="F"]$age

# Mean age of female actors in the network
V(graph)[sex=="F"]$age %>% 
  mean

# Display actor gender in graph visualization via ggraph
ggraph(graph) + 
  # Draw edges
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')),
    end_cap = circle(3, 'mm'),
    start_cap = circle(3, 'mm')) + 
  # Draw nodes
  geom_node_point(aes(fill= sex), color = "blue", shape = 21, size=5) + 
  # Draw node labels (names)
  geom_node_text(aes(label = name), color= "darkblue", size=3) + 
  # Theme details
  theme_graph(base_family = 'Helvetica')

# Alternative code to plot via igraph

# First plot with uniform blue color
plot(graph, vertex.color= "blue", edge.arrow.size=0.5, 
     vertex.label.cex=0.5, vertex.label.color= "white")
# The color can also be set as a vertex attribute in the graph itself, and the
# plot function will recognize it.
V(graph)$color <- "blue"

# Now the plot function recognizes the vertex attribute "color".
plot(graph, edge.arrow.size=0.5, vertex.label.cex=0.5, vertex.label.color= "white")

# Using indexing, set a different color for female actors
V(graph)[sex=="F"]$color
V(graph)[sex=="F"]$color <- "red"

# Plot again
plot(graph, edge.arrow.size=0.5, vertex.label.cex=0.5, vertex.label.color= "white")


# Indexing based on network structure
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# View all actors who know Mark
V(graph)[nei("Mark")]

# View the age of all actors who know Mark.
V(graph)[nei("Mark")]$age

# Average age in Mark's first-order neighborhood.
V(graph)[nei("Mark")]$age %>% 
  mean

# View all edges that are incident on David
E(graph)[inc("David")]
# All edges "to" David
E(graph)[to("David")]
# View the strength of these edges
E(graph)[to("David")]$tie_weight

# Average strength of all incoming edges to David
E(graph)[to("David")]$tie_weight %>% 
  mean

# View all edges to David whose tie_weight is ==2
E(graph)[to("David") & tie_weight==2]

# View all edges between women in the network.
# First get the vertex sequence of all women
women <- V(graph)[sex=="F"]
women
# Then get the edges among them.
E(graph)[women %--% women]

# Are there more edges among women or among men?
# Number of edges among women
E(graph)[women %--% women] %>% length
# Number of edges among men
men <- V(graph)[sex=="M"]
E(graph)[men %--% men] %>% length

# What is the strength of ties among women?
E(graph)[women %--% women]$tie_weight

# Compare the distribution of tie strength among women vs among men
E(graph)[women %--% women]$tie_weight %>% 
  tabyl
E(graph)[men %--% men]$tie_weight %>% 
  tabyl

# Example of plot combining igraph and ggraph to highlight structural features
# of the network

# Create vertex attribute to flag nodes that are adjacent to Mark
V(graph)$nei.mark <- "No"
V(graph)[nei("Mark")]$nei.mark <- "Yes"

# Create edge attribute to flag edges that are incident on Mark
E(graph)$inc.mark <- "No"
E(graph)[inc("Mark")]$inc.mark <- "Yes"

# Create vertex attribute with vertex degree
V(graph)$degree <- degree(graph)

# Plot
ggraph(graph) + 
  # Draw edges
  geom_edge_link(end_cap = circle(3, 'mm'),
                 start_cap = circle(3, 'mm'),
                 # For edge transparency
                 alpha = 0.7,
                 # Edge width as tie weight
                 aes(width = tie_weight, 
                     # Edge color indicates if edge is incident on Mark
                     color = inc.mark)) + 
  # Draw nodes
  geom_node_point(
    # Node fill indicates whether node is adjacent to Mark
    aes(fill= nei.mark, 
        # Node size is degree
        size = degree), 
                  shape= 21, 
                  color = "blue") + 
  # Draw node labels (names)
  geom_node_text(aes(label = name), color= "darkblue", size=3) + 
  # Theme details
  theme_graph(base_family = 'Helvetica')


## ---- end-attr-indexing
############################################################################## #
###                       NETWORK CENTRALITY                                ----
############################################################################## #
## ---- centrality

# Read in the campnet network adjacency matrix
campnet.adj <- read_csv(file="./Data/campnet_adj.csv") %>%
        # Remove first column (node names, they're already stored as column names)
        dplyr::select(-1) %>%
        # Convert to matrix
        as.matrix

# Import into directed graph 
camp <- graph_from_adjacency_matrix(campnet.adj, 
                                    mode="directed",
                                    # To use matrix column names as "name" vertex attribute in igraph
                                    add.colnames = NULL)

# Read in the vertex attributes
(campnet.attr <- read_csv(file="./Data/campattr.csv"))

# Note that actors in the graph are in the same order as actors in the attribute
# data frame.
V(camp)
campnet.attr$id

# So we can simply set the columns from the attribute data frame as vertex 
# attributes (because the order is the same, no merge is needed)
V(camp)$Gender <- campnet.attr$Gender
V(camp)$Role <- campnet.attr$Role

# Let's print the graph
camp

# Plot it.
# Calculate layout and set as layout attribute of graph.
set.seed(219)
camp$layout <- layout_(graph=camp, layout=with_kk())
# Plot the graph.
plot(camp, edge.arrow.size=0.1, vertex.label.cex=0.5)


# Calculate indegree
(camp.deg <- degree(camp, mode= "in"))

# Note that the result is simply a numeric vector
# It's a *named* numeric vector, where igraph vertex names are stored as 
# vector names
names(camp.deg)

# With tidyverse (enframe), we can convert this indegree vector to data frame
# and set the indegree variable name
camp.deg <- degree(camp, mode= "in") %>%
        enframe(value = "indegree")

# View result
camp.deg

# Betweenness, directed
camp.bet.dir <- betweenness(camp, directed = TRUE) %>%
        enframe(value = "betw.dir")

# Betweenness, undirected
camp.bet.undir <- betweenness(camp, directed = FALSE) %>%
        enframe(value = "betw.undir")

# Closeness, undirected
camp.clos.undir <- closeness(camp, mode="all") %>%
        enframe(value = "clos.undir")

# Let's create a data frame with all centrality values.
# Start with degree data frame
camp.centr <- camp.deg %>%
  # Join with directed betweenness data frame
  left_join(camp.bet.dir, by = "name") %>%
  # Join with undirected betweenness data frame
  left_join(camp.bet.undir, by = "name") %>%
  # Join with closeness data frame
  left_join(camp.clos.undir, by = "name") 

camp.centr

# Create histogram of degree centrality using ggplot2
ggplot(data= camp.centr, aes(x=indegree)) + 
        geom_histogram(binwidth = 1, color= "black")

# Plot network with nodes sized by centrality

# Create indegree attribute
V(camp)$indegree <- degree(camp, mode= "in")
  
# Plot
ggraph(camp) + 
  # Draw edges
  geom_edge_link(end_cap = circle(3, 'mm'),
                 start_cap = circle(3, 'mm')) + 
  # Draw nodes
  geom_node_point(
    # Node fill indicates whether node is adjacent to Mark
    aes(size = indegree), 
    shape= 21, 
    fill = "lightblue",
    color = "blue") + 
  # Draw node labels (names)
  geom_node_text(aes(label = name), color= "darkblue", size=3) + 
  # Theme details
  theme_graph(base_family = 'Helvetica')


# Same for betweenness: but now use both node size and color to visualize 
# centrality
V(camp)$betw <- betweenness(camp, directed = FALSE)

# Plot
ggraph(camp) + 
  # Draw edges
  geom_edge_link(end_cap = circle(3, 'mm'),
                 start_cap = circle(3, 'mm')) + 
  # Draw nodes
  geom_node_point(
    aes(size = betw, color = betw)) + 
  # Draw node labels (names)
  geom_node_text(aes(label = name), color= "darkblue", size=3) + 
  # Theme details
  theme_graph(base_family = 'Helvetica')

# Plot with size = betweenness, color = role, shape = gender
ggraph(camp) + 
  # Draw edges
  geom_edge_link(end_cap = circle(3, 'mm'),
                 start_cap = circle(3, 'mm')) + 
  # Draw nodes
  geom_node_point(
    # Node size, color, shape
    aes(size = betw, color = Role, shape = Gender)) + 
  # Draw node labels (names)
  geom_node_text(aes(label = name), color= "darkblue", size=3) + 
  # Theme details
  theme_graph(base_family = 'Helvetica')

## ---- end-centrality


