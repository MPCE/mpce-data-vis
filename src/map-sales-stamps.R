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
  #   weight_name (str): Optional. The name of the weight variable
  #
  # Returns:
  #   edge_list (tbl): A tibble with three columns, 'source', 'target' and 'weight', describing
  #     a directed network between the entities in 'vertex'.
  ##
  
  require(dplyr)
  
  # Enquote the user input.
  vertex <- rlang::enquo(vertex)
  edge <- rlang::enquo(edge)
  if (rlang::maybe_missing(weight)) {
    weight <- quote(expr = weight)
  }
  weight <- rlang::enquo(weight)
  
  # Maniuplate the data
  two_col <- .data %>%
    select(
      vertex = !!vertex,
      edge = !!edge
    )
  # The data is now a two-column tibble with columns 'vertex' and 'edge'
  
  out <- two_col %>%
    group_by(edge, vertex) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    inner_join(two_col, by = "edge") %>%
    select(
      source = vertex.x,
      target = vertex.y,
      !!weight := n
    ) %>%
    # Sum the edge weights
    ungroup() %>%
    group_by(source, target) %>%
    summarise(!!weight := sum(!!weight)) %>%
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
  theme_tufte()

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
sale_edgelist <- stock_sale %>%
  group_by(ID_Sale, ID_DealerName) %>%
  summarise(num_purchases = n()) %>%
  inner_join(., select(., ID_Sale, ID_DealerName), by = "ID_Sale") %>%
  rename(
    source = ID_DealerName.x,
    target = ID_DealerName.y,
    num_purchases = num_purchases,
    sale = ID_Sale
  ) %>%
  ungroup() %>%
  filter(source != target) %>%
  group_by(source, target) %>%
  summarise(num_purchases = sum(num_purchases)) %>%

sale_edgelist %>%
  ggplot(aes(from_id = source, to_id = target, linewidth = sqrt(num_purchases) / 10)) +
  geom_net() +
  theme_net()

dealer_graph <- sale_edgelist %>%
  select(source, target) %>%
  as.matrix() %>%
  graph_from_edgelist()

E(dealer_graph)$weight <- sale_edgelist$num_purchases %>%
  (function(x) 1/x)()

# Basic graph stats:
edge_density(dealer_graph)
transitivity(dealer_graph)

# Node-level stats:
graph_stats <- tibble(
  client_code = igraph::V(dealer_graph) %>% names(),
  degree = igraph::degree(dealer_graph),
  betweenness = igraph::betweenness(dealer_graph)
) %>%
  left_join(
    select(dealer, Client_Code, Dealer_Name),
    by = c("client_code" = "Client_Code")
  ) %>%
  arrange(desc(betweenness))

# Generate the network by books shared in common
book_edge <- stock_sale %>%
  group_by(ID_EditionName, ID_DealerName) %>%
  summarise(num_purchases = n()) %>%
  inner_join(., select(., ID_EditionName, ID_DealerName), by = "ID_EditionName") %>%
  rename(
    source = ID_DealerName.x,
    target = ID_DealerName.y,
    num_purchases = num_purchases,
    book = ID_EditionName
  ) %>%
  ungroup() %>%
  filter(source != target) %>%
  group_by(source, target) %>%
  summarise(num_purchases = sum(num_purchases))

book_edge %>%
  ggplot(aes(from_id = source, to_id = target, linewidth = sqrt(num_purchases) / 10)) +
  geom_net() +
  theme_net()

book_net <- book_edge %>%
  select(source, target) %>%
  as.matrix() %>%
  graph_from_edgelist()

E(book_net)$weight <- book_edge$num_purchases %>%
  (function(x) 1/x)()

edge_density(book_net)
transitivity(book_net)

book_net_stats <- tibble(
  client_code = igraph::V(book_net) %>% names(),
  degree = igraph::degree(book_net),
  betweenness = igraph::betweenness(book_net)
) %>%
  left_join(
    select(dealer, Client_Code, Dealer_Name),
    by = c("client_code" = "Client_Code")
  ) %>%
  arrange(desc(betweenness))

# Write the graph stats to disk
write_excel_csv(graph_stats, "dealers_joined_by_sales_in_common.csv")
write_excel_csv(book_net_stats, "dealers_joined_by_books_in_common.csv")
