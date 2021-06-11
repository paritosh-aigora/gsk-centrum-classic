# data cleaning, conditioning, and exploration ----------------------------

# set printing options
print_panel_plots <- TRUE
print_cons_plots <- FALSE

# prepare output folder
output_dir <- file.path("output", "data_exploration", source_stem)

if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}

# load prepped data -----
load(file.path(
  "input",
  "prepped_data",
  str_c(source_stem, "_prepped_data.Rdata")
))

# extract general panel data (only look at subset of variables for testing purposes) -----

panel_data <- prepped_data$x

x_header_vars <- c("ID_X", "Session", "Stim")

scale_data <- panel_data %>%
  select(-one_of(x_header_vars))

scale_vars <- scale_data %>%
  names()

num_vars <- scale_vars %>%
  length()

# impute missing data (if any) ----

prop_missing <-
  ((scale_data %>% is.na() %>% sum()) / (scale_data %>% dim() %>% (function(x) {
    x[[1]] * x[[2]]
  })))

sprintf("Proportion Missing = %.2f", prop_missing)

# impute missing values

if (prop_missing > 0) {
  # impute missing values
  
  set.seed(32431)
  mice_res <- mice(scale_data)
  
  predict_vars <- scale_data %>%
    map(is.na) %>%
    map_lgl(any) %>%
    which() %>%
    names()
  
  imp_info_df <- mice_res$imp[predict_vars] %>%
    map(apply, 1, median, na.rm = TRUE) %>%
    map(enframe, name = "row") %>%
    enframe(name = "var") %>%
    unnest(cols = c(value)) %>%
    mutate(row = as.numeric(row))
  
  imputed_data <- scale_data
  
  row_ind <- 1
  for (row_ind in 1:nrow(imp_info_df)) {
    imp_info <- imp_info_df[row_ind,]
    
    imputed_data[imp_info$row, imp_info$var] <- imp_info$value
    
  }
  
  min_val <- min(scale_data, na.rm = TRUE)
  max_val <- max(scale_data, na.rm = TRUE)
  
  imputed_data[imputed_data > max_val] <- max_val
  imputed_data[imputed_data < min_val] <- min_val
  
  scale_data <- imputed_data
  
  panel_data[scale_vars] <- scale_data
  
}

# remove low-variability variables ----

near_zero_vars <- scale_data %>%
  nearZeroVar() %>%
  names(scale_data)[.]

scale_data <- scale_data %>%
  select(-one_of(near_zero_vars))

panel_data <- panel_data %>%
  select(-one_of(near_zero_vars))

# next conduct ANOVAs variable by variable and remove variables with no stimulus effect  ----

num_id_x <- panel_data$ID_X %>%
  unique() %>%
  length()

num_session <- panel_data$Session %>%
  unique() %>%
  length()

if ((num_id_x > 1) | (num_session > 1)){ # if possible, run ANOVAs to test for stimulus effects
  
  decat_form <-
    str_c("~ Stim",
          ifelse(num_id_x > 1, " + ID_X", ""),
          ifelse(num_session > 1, " + Session", ""))
  
  res_decat <- panel_data %>%
    as.data.frame() %>%
    decat(
      formul = decat_form,
      firstvar = 4,
      proba = 1,
      graph = FALSE
    )
  
  # remove variables with no stimulus effect (as defined by bonferroni corrected p-value with alpha = alpha_thresh)
  
  alpha_thresh <- 0.1
  num_scale_vars <- ncol(panel_data) - 3
  bon_thresh <- alpha_thresh / num_scale_vars
  
  keep_vars <- res_decat$resF %>%
    rownames_to_column(var = "Variable") %>%
    as_tibble() %>%
    mutate(sig = (`P-value` < bon_thresh)) %>%
    filter(sig) %>%
    pull(Variable)
  
  panel_data <- panel_data %>%
    select(one_of(x_header_vars), one_of(keep_vars))
  
  scale_data <- scale_data %>%
    select(one_of(keep_vars))
  
}

if (num_session > 1){
  
  # quick check on internal consistency of panel ----
  
  tidy_panel_data <- panel_data %>%
    pivot_longer(starts_with("X"),
                 names_to = "Variable",
                 values_to = "Value")
  
  # consider spread in panelist responses for each stimulus
  
  panel_sd_plot <- tidy_panel_data %>%
    group_by(ID_X, Stim, Variable) %>%
    summarize(sd = sd(Value, na.rm = TRUE)) %>%
    ggplot(aes(x = sd)) +
    geom_histogram(bins = 50, col = "black") +
    labs(title = "Panelist Standard Deviation across Sessions", subtitle = "Should be mostly close to zero") +
    theme_minimal()
  
}

# consider correlations ----

ordered_names <- scale_data %>%
  corrgram(order = "HC") %>%
  row.names()

real_names <- prepped_keys$x %>%
  deframe() %>%
  .[ordered_names]

named_data <- scale_data[ordered_names] %>%
  set_names(real_names)

corr_plot <- named_data %>%
  ggcorr() +
  theme(legend.position = "bottom") +
  ggtitle("Correlogram - Ordered by Clustering") +
  guides(fill = guide_legend(
    title = "Correlation",
    title.position = "top",
    title.hjust = 0.5
  ))

# consider how many clusters to use

num_clusts <- named_data %>%
  find_num_clusts()

# make phylogenic tree

cluster_list <- named_data %>%
  make_cluster_list(num_clusts_k = num_clusts)

clust_info <- cluster_list$clust_inds %>%
  enframe(name = "var", value = "Cluster")

if (user_clusters){
  
  clust_info <- panel_details %>%  
    semi_join(clust_info %>% select(Variable = var)) %>% 
    select(var = Variable, Cluster)
  
  p <- cluster_list$plot 
  
  pb <- ggplot_build(p)
  
  poss_cols <- pb$data[[3]]$colour %>% 
    unique()
  
  new_cols <- pb$data[[3]]$label %>% 
    as.character() %>% 
    tibble(var = .) %>% 
    left_join(clust_info) %>% 
    pull(Cluster) %>% 
    poss_cols[.]
    
  pb$data[[2]]$colour <- new_cols
  pb$data[[3]]$colour <- new_cols

  q <- pb %>% 
    ggplot_gtable() %>% 
    ggplotify::as.ggplot()
  
  cluster_list$plot <- q
    
}

clust_info_list <-
  list(clust_data = named_data,
       num_clusts = num_clusts,
       clust_info = clust_info)

clust_out_dir <- file.path("output",
                           "data_exploration",
                           source_stem)

if (!dir.exists(clust_out_dir)){
  dir.create(clust_out_dir, recursive = TRUE)
}

save(
  clust_info_list,
  file = file.path(
    clust_out_dir,
    "clust_info_list.rdata"
  )
)

saveRDS(cluster_list$plot, file.path(clust_out_dir, "phylo_plot.rds"))

pdf(
  file.path("output", "data_exploration", source_stem, "correlations.pdf"),
  width = 17,
  height = 11
)

print(corr_plot)

dev.off()

# run PCA to get a sense of the dimensionality of the product space ----

scale_names <- prepped_keys$x %>%
  deframe() %>%
  .[names(scale_data)] %>%
  as.character()

pca_data <- panel_data %>%
  select(Stim, one_of(names(scale_data))) %>%
  group_by(Stim) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  left_join(prepped_keys$s) %>%
  select(-Stim) %>%
  column_to_rownames(var = "Stim_Name") %>%
  as.data.frame()

colnames(pca_data) <- scale_names

# run preliminary PCA to determine number of relevant components and perform initial plotting

num_prods <- pca_data %>%
  nrow()

pre_res_pca <-
  prcomp(
    pca_data,
    retx = TRUE,
    center = TRUE,
    scale = TRUE,
    rank = min(dim(pca_data))
  )

res_eig <- pre_res_pca %>%
  get_eigenvalue() %>%
  as_tibble()

min_var_exp <- 95

num_rel_comps <-
  (res_eig$cumulative.variance.percent > min_var_exp) %>%
  which() %>%
  min()

res_pca <-
  prcomp(
    pca_data,
    retx = TRUE,
    center = TRUE,
    scale = TRUE,
    rank = num_rel_comps
  )

# plot PCA results ----

# create scree plot (single plot)

scree_plot <- res_pca %>%
  fviz_screeplot(addlabels = TRUE)

# create importance plots (a list of plots) ----

imp_plot_list <- 1:num_rel_comps %>%
  map(~ fviz_contrib(res_pca, choice = "var", axes = .)) %>%
  set_names(str_c("Variable Importance: ", str_c("Dimension ", 1:num_rel_comps)))

# create biplots

dim_info <- 1:num_rel_comps %>%
  combn(2) %>%
  as_tibble(.name_repair = "unique")

make_pca_biplot <- function(dim_vect) {
  dim_1 <- dim_vect[[1]]
  dim_2 <- dim_vect[[2]]
  
  output <- res_pca %>%
    fviz_pca_biplot(
      repel = TRUE,
      ggtheme = theme_minimal(),
      axes = c(dim_1, dim_2),
      title = ""
    )
  
  return(output)
  
}

make_pca_titles <- function(dim_vect) {
  dim_1 <- dim_vect[[1]]
  dim_2 <- dim_vect[[2]]
  
  output <- str_c("Dimension ", dim_1, " vs Dimension ", dim_2)
  
  return(output)
  
}

pca_biplot_titles <- dim_info %>%
  map_chr(make_pca_titles) %>%
  as.character()

pca_biplot_list <- dim_info %>%
  map(make_pca_biplot) %>%
  set_names(pca_biplot_titles)

# collect PCA output ----

pca_ind_res <- res_pca %>%
  get_pca_ind()

pca_ind_mat <- pca_ind_res$coord

pca_ind_coords <- pca_ind_mat %>%
  as_tibble(rownames = "Stim_Name") %>%
  left_join(prepped_keys$s) %>%
  relocate(Stim) %>%
  select(-Stim_Name) %>%
  set_names(c("Stim", str_c("X", 1:(ncol(
    .
  ) - 1))))

pca_var_res <- res_pca %>%
  get_pca_var()

pca_var_contrib <- pca_var_res$contrib[, 1:num_rel_comps]
pca_var_cor <- pca_var_res$cor[, 1:num_rel_comps]

# determine correlations between original variables and pca variables

pca_cor_info <- pca_var_cor %>%
  as_tibble(rownames = "Variable") %>%
  pivot_longer(-Variable, names_to = "Component", values_to = "Correlation") %>%
  mutate(Variable = factor(str_to_title(Variable)),
         Component = factor(str_replace(Component, "Dim\\.", "Component ")))

# create correlation plot

res_hclust <- pca_data %>%
  scale() %>%
  t() %>%
  dist() %>%
  hclust(method = "ward.D2")

order_hclust <- res_hclust %>%
  order.hclust()

var_levels <-
  rownames(pca_data %>% t())[order_hclust] %>%
  str_to_title()

pca_cor_plot <- pca_cor_info  %>%
  mutate(Variable = factor(Variable, levels = var_levels)) %>%
  ggplot(aes(x = Component, y = Variable, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = gsk_blue,
                       high = gsk_pink,
                       name = "Correlation") +
  theme(
    axis.line = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.position = "bottom",
    axis.title = element_blank()
  )

pdf(
  file.path(output_dir, "PCA Correlation Results.pdf"),
  height = 8.5,
  width = 11
)
pca_cor_plot %>%
  print()
dev.off()

# determine how well we can recover the original data

comp_profiles <- res_pca$x %>%
  as_tibble() %>%
  mutate(Stim = 1:nrow(.))

pca_data_new <- res_pca$x %>%
  as_tibble() %>%
  mutate(Stim = 1:nrow(.)) %>%
  find_orig_profiles(res_pca) %>%
  select(-Stim) %>%
  set_names(names(res_pca$center))

rownames(pca_data_new) <- rownames(pca_data)

# compute largest difference between original and recovered variables
diff_res <-
  (pca_data_new - pca_data) %>% map(range) %>% map_dbl(diff) /
  (pca_data %>% map(range) %>% map_dbl(diff))

pca_plot_data <-
  pca_data %>%
  rownames_to_column(var = "Stim") %>%
  as_tibble() %>%
  mutate(Type = "Original") %>%
  bind_rows(
    pca_data_new %>%
      as.data.frame() %>%
      rownames_to_column(var = "Stim") %>%
      as_tibble() %>%
      mutate(Type = "Restored")
  ) %>%
  pivot_longer(-one_of(c("Stim", "Type")), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable), Type = factor(Type))

pca_comp_plot <- pca_plot_data %>%
  ggplot(aes(x = Value, y = Variable, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Stim)

pca_list <-
  list(
    cor_plot = pca_cor_plot,
    cor_info = pca_cor_info,
    comp_plot = pca_comp_plot,
    var_contrib = pca_var_contrib,
    ind_coords = pca_ind_coords,
    res_pca = res_pca,
    panel_data = panel_data,
    panel_keys_x = prepped_keys$x
  )

saveRDS(pca_list, file.path(output_dir, "pca_list.RData"))

# visualize range of responses by stimulus and scale ----

# preserve significance order from linear modeling
ordered_vars <- scale_data %>%
  names()

panel_data_list <- panel_data %>%
  pivot_longer(starts_with("X"),
               names_to = "Variable",
               values_to = "Values") %>%
  left_join(prepped_keys$x %>% rename(Variable = var)) %>%
  left_join(prepped_keys$s) %>%
  mutate(Variable = factor(Variable, levels = ordered_vars)) %>%
  split(., .$Variable)

make_box_plots <- function(panel_data_single) {
  var_name <- panel_data_single %>%
    pull(var_name) %>%
    unique()
  
  p <- panel_data_single %>%
    ggplot(aes(x = Stim_Name, y = Values)) +
    geom_boxplot() +
    ggtitle(var_name) +
    theme_minimal() +
    scale_x_discrete(name = "", guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(name = "")
  
  return(p)
}

box_plots <- panel_data_list %>%
  map(make_box_plots)

if (print_panel_plots) {
  graphics.off()
  pdf(file.path(output_dir,
                "panel_plots.pdf"),
      width = 11,
      height = 8.5)
  
  # print standard deviations
  panel_sd_plot %>%
    print()
  
  # print correlogram
  
  corr_plot %>%
    print()
  
  # write out box plots for inspection
  
  box_plots %>%
    walk(print)
  
  # print PCA results
  
  scree_plot %>%
    print()
  
  imp_plot_list %>%
    walk(print)
  
  pca_biplot_list %>%
    walk(print)
  
  # close file
  
  dev.off()
  
  
}

# At this point the panel data are cleaned and ready for further analysis

# consider variablity in consumer responses ----

cons_var_data <- prepped_data$y %>%
  group_by(ID_Y) %>%
  summarize(Var = var(Y)) %>%
  ungroup() %>%
  mutate(Rank = rank(Var)) %>%
  arrange(Rank)

# cons_var <- cons_var_data %>%
#   pull(Var)
# 
# # plot emperical cdf
# cons_var_cdf <- cons_var %>%
#   ecdf()
# 
# # drop consumers with lowest variability in liking responses
# 
# drop_prob <- 0.05
# 
# cut_off <- quantile(cons_var_data$Var, drop_prob) %>%
#   as.numeric()

keep_resp <- cons_var_data$ID_Y

# cons_var_data %>%
# filter(Var > cut_off) %>%
# arrange(ID_Y) %>%
# pull(ID_Y)

# examine and cull demographic information

demo_info <- prepped_data$d

# remove demographic variables with little to no variability

remove_ratio <- 20 / 1

near_zero_demo_vars <- demo_info %>%
  nearZeroVar(freqCut = remove_ratio) %>%
  names(demo_info)[.]

# check counts of variables marked for removal
demo_info[near_zero_demo_vars] %>%
  map(table)

# determine which demographic variables to keep
keep_demo_vars <- setdiff(names(demo_info), near_zero_demo_vars)

# check what will be removed
prepped_keys$d %>%
  filter(var %in% near_zero_demo_vars)

# clean up data so that only keep keep_vars and keep_resp remain in model data ----

cleaned_data <- vector("list")
cleaned_keys <- vector("list")

# stimulus keys don't change

cleaned_keys$s <- prepped_keys$s

# one hot encoding keys don't change

cleaned_keys$oh <- prepped_keys$oh

# clean up x information

keep_vars <- scale_data %>%
  names()

x_raw <- panel_data %>%
  pivot_longer(starts_with("X"),
               names_to = "var",
               values_to = "value") %>%
  filter(var %in% keep_vars) %>%
  mutate(var_fact = fct_inorder(as.factor(var))) %>%
  mutate(new_var = str_c("X", as.numeric(var_fact)))

cleaned_data$x <- x_raw %>%
  select(-var,-var_fact) %>%
  pivot_wider(names_from = new_var, values_from = value) %>%
  arrange(ID_X, Stim, Session)

cleaned_keys$x <- x_raw %>%
  select(var, new_var) %>%
  unique() %>%
  left_join(prepped_keys$x) %>%
  select(-var) %>%
  rename(var = new_var)

# clean up y information

y_raw <- prepped_data$y %>%
  arrange(ID_Y) %>%
  filter(ID_Y %in% keep_resp) %>%
  mutate(y_fact = fct_inorder(factor(ID_Y))) %>%
  mutate(new_y = as.factor(as.numeric(y_fact)))

cleaned_data$y <- y_raw %>%
  select(new_y, Stim, Y) %>%
  rename(ID_Y = new_y)

cleaned_keys$y <- prepped_keys$y

# clean up d information

d_raw <- y_raw %>%
  left_join(demo_info %>% select(one_of(keep_demo_vars))) %>%
  select(new_y, starts_with("D")) %>%
  rename(ID_Y = new_y) %>%
  unique()

cleaned_data$d <- d_raw %>%
  set_names(c("ID_Y", str_c("D", 1:(ncol(
    .
  ) - 1))))

cleaned_keys$d <- prepped_keys$d %>%
  filter(var %in% keep_demo_vars) %>%
  mutate(var = str_c("D", 1:(nrow(.))))

# run cluster analysis on demographic variables

# consider correlations ----

demo_key <- cleaned_keys$d %>%
  deframe()

demo_data <- cleaned_data$d %>%
  column_to_rownames(var = "ID_Y")

# consider correlations ----

ordered_names <- demo_data %>%
  corrgram(order = "HC") %>%
  row.names()

real_names <- demo_key[ordered_names]

named_data <- demo_data[ordered_names] %>%
  set_names(real_names)

corr_plot <- named_data %>%
  ggcorr() +
  theme(legend.position = "bottom") +
  ggtitle("Correlogram - Ordered by Clustering") +
  guides(fill = guide_legend(
    title = "Correlation",
    title.position = "top",
    title.hjust = 0.5
  ))

# consider how many clusters to use

# num_clusts <- named_data %>%
#   find_num_clusts()

num_clusts <- 2

# make phylogenic tree

demo_cluster_list <- named_data %>%
  make_cluster_list(num_clusts_k = num_clusts)

demo_clust_info <- demo_cluster_list$clust_inds %>%
  enframe(name = "var", value = "Cluster")

demo_clust_info_list <-
  list(clust_data = named_data,
       num_clusts = num_clusts,
       clust_info = demo_clust_info)

save(
  demo_clust_info_list,
  file = file.path(
    "output",
    "data_exploration",
    source_stem,
    "demo_clust_info_list.rdata"
  )
)

# run cluster analysis on remaining consumers ----

# check for missing values and impute if necessary before running cluster analysis
# note: cluster analysis requires complete data

# test_na_data <- cleaned_data$y %>%
#   select(ID_Y, Stim, Y) %>%
#   pivot_wider(names_from = Stim, values_from = Y)
# 
# if (any(is.na(test_na_data))) {
#   set.seed(12345)
# 
#   a_res <- Amelia::amelia(test_na_data, idvars = "ID_Y", p2s = 0)
# 
#   cluster_start_data <- a_res$imputations %>%
#     enframe() %>%
#     unnest(value) %>%
#     group_by(ID_Y) %>%
#     select(-name) %>%
#     summarize_if(is.numeric, mean) %>%
#     ungroup() %>%
#     pivot_longer(-ID_Y, names_to = "Stim", values_to = "Y")
# 
# } else {
#   cluster_start_data <- cleaned_data$y %>%
#     select(ID_Y, Stim, Y)
# }

# cluster based on demographics

d_cluster_vars <- cleaned_keys$d %>% 
  filter(str_detect(var_name, "Jar", negate = TRUE)) %>%
  # filter(!grepl("^jar[_ ]", var_name, ignore.case = TRUE)) %>%
  pull(var)

df_dist <- cleaned_data$d %>%
  column_to_rownames(var = "ID_Y") %>% 
  select(one_of(d_cluster_vars)) %>% 
  dist()

res_hc <- hclust(d = df_dist, method = "ward.D2")

num_clusts <- 2
cluster_assigns <- cutree(res_hc, k = num_clusts)

# rand_seed <- 12323
# set.seed(rand_seed)
# cluster_assigns <- sample(1:num_clusts, nrow(cleaned_data$d), replace = TRUE)

# choose number of clusters

# compute stability for chosen number

rand_seed <- 12323

capture.output(
  cboot_res <-
    clusterboot(
      data = df_dist,
      distances = TRUE,
      clustermethod = disthclustCBI,
      method = "average",
      k = num_clusts,
      seed = rand_seed
    ),
  file = "NUL"
)

cluster_eval <-
  tibble(Cluster = 1:num_clusts,
         Stability = cboot_res$bootmean)

print("Stability scores less 0.6 may indicate unstable clusters")
print(cluster_eval)

# # if we don't have at least one stable cluster, we revert to a single cluster, otherwise we accept these clusters
# 
# stability_thresh <- 0.6
# 
# if (!all(cluster_eval$Stability >= stability_thresh)) {
#   print("Reverting to a single cluster")
#   cluster_assigns <- rep(1, cleaned_data$d %>% nrow())
#   num_clusts <- 1
# } else {
#   print("Retaining clusters")
#   cluster_assigns <- cboot_res$partition
# }

# update the demographic dataset
cleaned_data$d <- cleaned_data$d %>%
  mutate(Cluster = cluster_assigns)

cluster_label_key <- cleaned_data$d$Cluster %>%
  enframe(value = "Cluster") %>%
  group_by(Cluster) %>%
  mutate(Cluster = as.character(Cluster),
         ClusLabel = str_c(Cluster, " (n = ", n(), ")")) %>%
  select(-name) %>%
  unique() %>%
  bind_rows(tibble(Cluster = "All", ClusLabel = "All"))

# update the demographic keys

cleaned_keys$d <- cleaned_keys$d %>%
  bind_rows(tibble(var = "Cluster",
                   var_name = "Cluster"))

# Consider differences in liking between samples across clusters

conf_level <- 0.95

num_subs <- cleaned_data$d %>%
  nrow()

cluster_all_label <- "All"

cluster_stim_data <- cleaned_data$y %>%
  left_join(cleaned_data$d) %>%
  left_join(cleaned_keys$s) %>%
  select(Cluster, Stim_Name, Y) %>% 
  mutate(Cluster = as.character(Cluster))

cluster_all_data <- cluster_stim_data %>%
  select(-Cluster) %>%
  mutate(Cluster = cluster_all_label) %>%
  select(Cluster, everything())

cluster_liking_plot <-
  cluster_stim_data %>%
  bind_rows(cluster_all_data) %>%
  left_join(cluster_label_key) %>%
  ggplot(aes(x = Y, fill = ClusLabel)) +
  geom_bar(aes(y = ..prop..)) +
  facet_grid(Cluster ~ Stim_Name, scales = "free_y") +
  scale_y_continuous(name = "", breaks = scales::pretty_breaks()) +
  theme_void() +
  theme(
    strip.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom"
  ) +
  labs(fill = "Cluster")

cluster_liking_plot

conf_int_res <- cluster_stim_data %>%
  group_by(Cluster, Stim_Name) %>%
  summarize(
    mean = mean(Y, na.rm = TRUE),
    n = n(),
    se = sd(Y) / sqrt(n)
  ) %>%
  mutate(conf_int_radius = se * qt(0.5 + conf_level / 2, n - 1)) %>%
  mutate(ci_low_bound = mean - conf_int_radius,
         ci_up_bound = mean + conf_int_radius) %>%
  ungroup() %>%
  select(Cluster, Stim_Name, ci_low_bound, mean, ci_up_bound)

conf_int_all <- cluster_stim_data %>%
  group_by(Stim_Name) %>%
  summarize(
    mean = mean(Y, na.rm = TRUE),
    n = n(),
    se = sd(Y) / sqrt(n)
  ) %>%
  mutate(conf_int_radius = se * qt(0.5 + conf_level / 2, n - 1)) %>%
  mutate(ci_low_bound = mean - conf_int_radius,
         ci_up_bound = mean + conf_int_radius) %>%
  ungroup() %>%
  mutate(Cluster = cluster_all_label) %>%
  select(Cluster, Stim_Name, ci_low_bound, mean, ci_up_bound)

cluster_ci_plot <- conf_int_res %>%
  select(Cluster, Stim_Name, ci_low_bound, mean, ci_up_bound) %>%
  bind_rows(conf_int_all) %>%
  left_join(cluster_label_key) %>%
  ggplot(aes(x = Stim_Name, y = mean, color = ClusLabel)) +
  geom_point(position = position_dodge(0.1)) +
  geom_errorbar(
    aes(ymin = ci_low_bound, ymax = ci_up_bound),
    width = 0.1,
    position = position_dodge(0.1)
  ) +
  scale_x_discrete(name = "", guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(name = "Mean") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  ) +
  labs(color = "Cluster")

cluster_ci_plot

# plot consumer results ----

if (print_cons_plots) {
  graphics.off()
  pdf(file.path(output_dir,
                "cons_plots.pdf"),
      width = 11,
      height = 8.5)
  
  # cons_var_cdf %>%
  #   plot(main = "Empirical CDF of Consumer Variability",
  #        ylab = "Cumulative Prob",
  #        xlab = "Var")
  # 
  # elbow_plot %>%
  #   print()
  # 
  # silhouette_plot %>%
  #   print()
  # 
  # gap_plot %>%
  #   print()
  # 
  # silhouette_res_plot %>%
  #   print()
  
  cluster_liking_plot %>%
    print()
  
  cluster_ci_plot %>%
    print()
  
  dev.off()
  
}

# save clean data as model data for future analysis

cleaned_data_path <- file.path(
  "input",
  "cleaned_data")

if (!dir.exists(cleaned_data_path)){
  dir.create(cleaned_data_path, recursive = TRUE)
}


save(cleaned_data,
     cleaned_keys,
     file = file.path(
       cleaned_data_path,
       str_c(source_stem, "_cleaned_data.Rdata")
     ))

# save plotting for output to PowerPoint

save(
  cluster_liking_plot,
  cluster_ci_plot,
  file = file.path(output_dir,
                   "cluster_analysis_plots.Rdata")
)

# NOTE: Having run this code, it would be wise to show the cleaned_keys$x table to a subject matter expert
# and see if she agrees that the scales are well chosen

cleaned_keys$x %>%
  openxlsx::write.xlsx(file = file.path(output_dir, "model_variables.xlsx"))

