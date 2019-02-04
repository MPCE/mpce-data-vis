###########################################################################
#
# MAPPING PRINT, CHARTING ENLIGHTENMENT
#
# DATA VISUALISATION EXPERIMENTS
#
# Script: Mapping Parisian Book Sales Data
#
# Author: Michael Falk
#
# Date: 30/01/2019
#
# As of late 2018, all the data from the Parisian book sales
# has been entered. Can we display this data in an attractive fashion
# to enable new kinds of analysis?
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

source("init.R")

fch_ms_tbl <- partial(fetch_table, con = manuscripts) # Partial function for convenience

french_price <- function(livres, sous, deniers) {
  # Calculates the total price in livres given the component parts
  # Returns NA if all components are missing.
  
  livres <- replace_na(livres, 0)
  sous <- replace_na(sous, 0)
  deniers <- replace_na(deniers, 0)
  
  total_d <- 240 * livres + 12 * sous + deniers
  
  total_l <- total_d / 240
  
  total_l[total_l == 0] <- NA
  
  return(total_l)
  
}

gen_edg_ls <- function(.data, vertex, edge, weight) {
  ##
  # Generates an edgelist from a tbl of data.
  # 
  # The produced network is an association network and therefore has neither loops
  # nor directed edges.
  #
  # Arguments:
  #   data (tbl): A tibble of data
  #   vertex (expr): The name of the column where the vertices of the graph are found
  #   edge (expr): The name of the column that will define the edges of the graph
  #   weight_name (expr): The name of the edge weight variable
  #
  # Returns:
  #   edge_list (tbl): A tibble with three columns, 'source', 'target' and 'weight', describing
  #     a directed network between the entities in 'vertex'.
  ##
  
  require(dplyr)
  
  # Enquote the user input.
  vertex <- rlang::enquo(vertex)
  edge <- rlang::enquo(edge)
  weight <- rlang::enquo(weight)
  
  # Maniuplate the data
  three_col <- .data %>%
    select(
      vertex = !!vertex,
      edge = !!edge,
      weight = !!weight
    ) %>%
    group_by(edge, vertex) %>%
    summarise(
      weight = sum(weight, na.rm = T)
    ) %>%
    ungroup()
  
  # The data is now a three-column tibble with columns 'vertex', 'edge' and 'weight'
  
  out <- three_col %>%
    inner_join(three_col, by = "edge") %>%
    transmute(
      source = vertex.x,
      target = vertex.y,
      weight = weight.x + weight.y
    ) %>%
    # Sum the edge weights
    ungroup() %>%
    group_by(source, target) %>%
    summarise(weight = sum(weight)) %>%
    ungroup() %>%
    # Now remove loops and duplicate edges
    mutate(
      temp_src = as.factor(source) %>% as.numeric(),
      temp_tar = as.factor(target) %>% as.numeric()
    ) %>%
    arrange(temp_src) %>%
    filter(temp_src < temp_tar) %>%
    select(-temp_src, -temp_tar)
  
  return(out)
    
}

##### SECTION 1: IMPORT DATA #####

estampillage <- fch_ms_tbl("manuscript_events")

sale <- fch_ms_tbl("manuscript_events_sales")

super_book <- fch_ms_tbl("manuscript_books")

edition <- fch_ms_tbl("manuscript_books_editions")

place <- fch_ms_tbl("manuscript_places") %>%
  distinct(Place_Code, .keep_all = T) # There are three entries for Paris

dealer <- fch_ms_tbl("manuscript_dealers")

##### SECTION 2: EXPLORE IT #####

# So many character columns... need to split stock sales from privilege sales
# in order to convert sales data into numeric.

# Complete data cleaning script:
stock_sale <- sale %>%
  filter(EventType == "Stock Sale") %>%
  # Clean up sales data
  mutate(
    EventCopies = str_replace(EventCopies, "(?<=\\d), {0,1}(?=\\d)", "."), # Replace commas with decimal points
    EventCopies = str_extract(EventCopies, "\\d+\\.?\\d*"), # Grab first complete number in column (some contain snippets of text)
    EventCopies = as.numeric(EventCopies) # Convert to numeric
  ) %>%
  # The lot price has been entered in the format L-S-D
  separate(EventLotPrice, into = c("livres","sous","deniers"), sep = "-", remove = F, extra = "warn", fill = "right") %>%
  mutate(
    # There are a couple of pieces of rogue punctuation
    livres = livres %>% str_remove("\\D") %>% as.numeric(),
    sous = sous %>% str_remove("\\D") %>% as.numeric(),
    deniers = deniers %>% str_remove("\\D") %>% as.numeric(),
    total_lot_price = french_price(livres, sous, deniers)
  ) %>%
  # We know the places that the dealers were from...
  select(-ID_PlaceName) %>% # Drop the superfluous information that every sale took place in Paris
  left_join(select(dealer, - ID), by = c("ID_DealerName" = "Client_Code")) %>%
  left_join(select(place, -ID), by = "Place_Code") %>%
  # Now let's split up the dates too...
  mutate(
    year = EventDate %>% ymd() %>% year(),
    month = EventDate %>% ymd() %>% month()
  )


##### SECTION 2 NOTES #####

# That transformation has introduced some nas... what's the deal?
stock_sale %>%
  select(ID, EventCopies) %>%
  filter(is.na(EventCopies)) %>%
  left_join(select(sale, ID, EventCopies, EventOther), by = "ID") %>%
  print(n = Inf)
# It seems that all the NAs were empty strings in the original column

# Also, the price data has apparently been entered systematically in the format L-S-D.
# Can we verify this?

stock_sale %>%
  # How many books were sold on average for each number of hyphens in the lot price?
  mutate(num_hyphens = str_count(EventLotPrice, "-")) %>%
  group_by(num_hyphens) %>%
  summarise(copies_sold = mean(EventCopies, na.rm = T))
# Since 65 books were sold on average in sales where the lot price is a single digit, it is unlikely to represent deniers.

# Lets look at the relationship between copies and price for the zero-hyphen prices
# Fiddling around with this, it seems there is not a strong relationship between the lot price and the number
# of copies sold when there is no hyphen in the price...
stock_sale %>%
  mutate(num_hyphens = str_count(EventLotPrice, "-")) %>%
  filter(num_hyphens == 0, nchar(EventLotPrice) > 0) %>%
  mutate(
    price_numeric = str_trim(EventLotPrice) %>% as.numeric()
    ) %>%
  ggplot(aes(EventCopies, price_numeric)) +
  facet_wrap(~EventCopiesType) + # It doesn't seem to matter which units - nearly all are in 'copies' anyhow
  geom_rug() +
  geom_point() +
  ggthemes::theme_tufte()

# Let's see how it is if we assume that prices without hyphens are expressed in livres,
# and then try to divide the sales by book format.
stock_sale %>%
  separate(EventLotPrice, into = c("livres","sous","deniers"), sep = "-", remove = F, extra = "warn", fill = "right") %>%
  # If this split is correct, there should never be more than 19 sous or 11 deniers...
  filter(str_detect(deniers, "\\D")) %>%
  select(deniers) %>%
  mutate(d_num = as.numeric(deniers))

##### SECTION 3: GRAPH IT #####

# Finding links between the dealers...

# There are 26 distinct sales in the data.
distinct(stock_sale, ID_Sale) %>% nrow()

# Draw an edge between two dealers if they attend at least one sale together
library(igraph)
sale_edgelist <- stock_sale %>%
  gen_edg_ls(ID_DealerName, ID_Sale, EventCopies) %>%
  mutate(distance = 1 / weight) %>%
  igraph::graph_from_data_frame(directed = FALSE)

# Basic graph stats:
edge_density(dealer_graph)
transitivity(dealer_graph)

# Node-level stats:
graph_stats <- tibble(
  client_code = igraph::V(dealer_graph) %>% names(),
  degree = igraph::degree(dealer_graph),
  betweenness = igraph::betweenness(dealer_graph, weights = E(dealer_graph)$distance)
) %>%
  left_join(
    select(dealer, Client_Code, Dealer_Name),
    by = c("client_code" = "Client_Code")
  ) %>%
  arrange(desc(betweenness))

# Generate the network by books shared in common
edition_net <- stock_sale %>%
  gen_edg_ls(ID_DealerName, ID_EditionName, EventCopies) %>%
  mutate(distance = 1 / weight) %>%
  graph_from_data_frame(directed = FALSE)

edge_density(edition_net)
transitivity(edition_net)

# Try superbooks too
super_book_net <- stock_sale %>%
  gen_edg_ls(ID_DealerName, ID_SuperBookTitle, EventCopies) %>%
  mutate(distance = 1 / weight) %>%
  graph_from_data_frame(directed = FALSE)

edge_density(super_book_net)
transitivity(super_book_net)

##### SECTION 4: ANOTHER APPROACH TO BUILDING THE GRAPHS #####

# We can also try building bipartite graphs, where people are connected to sales/books
# rather than directly to each other.

vertex_data <- stock_sale %>%
  distinct(ID_DealerName) %>%
  transmute(
    vertex = ID_DealerName,
    type = "person"
    ) %>%
  bind_rows(
    stock_sale %>% distinct(ID_Sale) %>%
      transmute(vertex = ID_Sale, type = "sale")
  )
  

sale_bipartite <- stock_sale %>%
  transmute(
    source = ID_DealerName,
    target = ID_Sale,
    copies_sold = EventCopies,
    total_lot_price = total_lot_price
  ) %>%
  group_by(source, target) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = T
  ) %>%
  graph_from_data_frame(
    directed = F,
    vertices = vertex_data
    )


betweenness(sale_bipartite, directed = F) %>%
  enframe(name = "vertex", value = "betweenness") %>%
  arrange(desc(betweenness)) %>%
  print(n = 20)
