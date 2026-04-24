#' Plot trait on maximum clade credibility tree
#'
#' @param data Tibble of D-PLACE data
#' @param mcc_tree Maximum clade credibility tree of D-PLACE societies
#'
#' @returns A ggplot object
#'
plot_trait_on_tree <- function(data, mcc_tree, variable) {
  # wrangle data
  d <-
    data |>
    transmute(var = as.factor(as.numeric(!!sym(variable)))) |>
    as.data.frame()
  rownames(d) <- data$xd_id
  # plot tree
  tree <-
    ggtree(
      tr = mcc_tree,
      layout = "circular",
      linewidth = 0.1
    )
  # add data to tree
  out <-
    gheatmap(
      p = tree,
      data = d,
      offset = -0.5,
      width = 0.05,
      colnames = FALSE,
      color = NA
    )
  if (is.ordered(data[[variable]])) {
    out <-
      out +
      scale_fill_brewer(
        name = str_to_sentence(str_replace_all(variable, "_", " ")),
        labels = function(x) levels(data[[variable]])[as.numeric(x)],
        type = "seq",
        palette = 7,
        na.value = "grey95"
      )
  } else {
    out <-
      out +
      scale_fill_manual(
        name = str_to_sentence(str_replace_all(variable, "_", " ")),
        labels = function(x) c("Absent", "Present")[as.numeric(x)],
        values = c("#ADD8E6", "#26667C")
      )
  }
  # taxa bookends for major language families
  taxa_bookends <- list(
    "Atlantic-Congo"          = c("xd10",   "xd253"),
    "Mande"                   = c("xd188",  "xd480"),
    "Athabaskan-Eyak-Tlingit" = c("xd1026", "xd1082"),
    "Algic"                   = c("xd1047", "xd1101"),
    "Uto-Aztecan"             = c("xd1118", "xd1292"),
    "Arawakan"                = c("xd1323", "xd1404"),
    "Afro-Asiatic"            = c("xd305",  "xd526"),
    "Indo-European"           = c("xd528",  "xd604"),
    "Dravidian"               = c("xd668",  "xd680"),
    "Uralic"                  = c("xd544",  "xd632"),
    "Nilotic"                 = c("xd2",    "xd406"),
    "Austronesian"            = c("xd687",  "xd749"),
    "Sino-Tibetan"            = c("xd639",  "xd704"),
    "Austroasiatic"           = c("xd661",  "xd724"),
    "Salishan"                = c("xd1071", "xd1146"),
    "Eskimo-Aleut"            = c("xd1022", "xd1067"),
    "Central Sudanic"         = c("xd155",  "xd358"),
    "Cariban"                 = c("xd1328", "xd1350")
  )
  # add clade labels for major language families
  for (family in names(taxa_bookends)) {
    # node number for most recent common ancestor
    node <- getMRCA(mcc_tree, taxa_bookends[[family]])
    # add clade label
    out <-
      out +
      geom_cladelab(
        node = node,
        label = family,
        offset = 10,
        offset.text = 2,
        barsize = 0.2,
        fontsize = 3,
        hjust = ifelse(
          family %in% c("Afro-Asiatic", "Indo-European", "Uralic", "Dravidian",
                        "Austronesian", "Austroasiatic", "Sino-Tibetan"),
          1, 0
        )
      )
  }
  # save
  ggsave(
    filename = paste0("plots/tree/tree_", variable, ".pdf"),
    plot = out,
    height = 10,
    width = 10
  )
  # return
  out
}
