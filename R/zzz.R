
utils::globalVariables(c(
  ".", ":=",
  # Validation variables
  ".coordinates_outOfRange", ".coordinates_empty", ".scientificName_empty",
  # Map selection variable
  "selected_",
  # Variables used for analysis
  "Latitude", "Longitude",
  ".__latitude", ".__longitude", ".__taxa", ".__year", ".id",
  ".__display_coord_accuracy", ".__display_taxa", ".__display_year",
  ".__selected", ".valid_year",
  "STATUS_CONR", "STATUS_DESC",
  "display_coord_accuracy", "display_taxa", "display_year",
  "calc_accuracy", "selected",
  "status", "matchtype",
  "citation", "author1", "author2", "author3",
  "validation_label", "validation_result", "year_description",
  "code", "code_x", "code_y", "confidence", "data_modif_d", "data_modif_m",
  "data_modif_y", "deg_min_sec", "fktax", "georef_final", "id", "id_tax_famclass",
  "idtax_f", "idtax_good_n", "idtax_n", "introduced_status", "kingdom", "morpho_species",
  "secondes", "species", "tax_esp", "tax_fam", "tax_famclass", "tax_gen", "tax_infra_level",
  "tax_infra_level_auth", "tax_nam01", "tax_nam02", "tax_order", "tax_rank01",
  "tax_rank02", "tax_sp_level", "tax_submitted", "tax_tax",
  "bbox", "bbox_sf", "table_name", "type", "description", "reference",
  "category", "pair_unique_coordinates", "EOO", "priority",
  "tax", "taxa",
  "protected",
  "polygon_method",
  "redlistcategory",
  "id_grid"
))

