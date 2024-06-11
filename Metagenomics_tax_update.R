####Taxonomy update code####

library(phyloseq)
library(rentrez)
library(taxize)


#Load in biom table
setwd('Your\\Working\\Directory\\')
LD_rat_table <- import_biom("table.biom")


#Update Taxonomy to match recent NCBI changes - insert your key
set_entrez_key("####")

# Extract the OTU IDs from the OTU table
otu_ids <- rownames(otu_table(LD_rat_table))
otu_ids <- as.numeric(otu_ids)
length(otu_ids)


# Create an empty list to store the updated taxonomy
updated_taxonomy <- list()

# Iterate over each OTU ID
for (h in 1:length(otu_ids)) {
  tryCatch({
    # Try to retrieve the classification information
    result <- classification(otu_ids[h], db = "ncbi")

    # Append the result to the updated_taxonomy list
    updated_taxonomy[[h]] <- result
  }, error = function(e) {
    # If an error occurs, append a blank entry to the updated_taxonomy list
    updated_taxonomy[[h]] <- data.frame(name = character(),
                                        rank = character(),
                                        id = character(),
                                        stringsAsFactors = FALSE)
  })
}

#Get the summary of updated_taxonomy
taxonomy_summary <- summary(updated_taxonomy)
taxonomy_summary

#Fill in any taxonomy updates that failed on 1st round i.e. which(taxonomy_summary[,1]==0)

which(taxonomy_summary[,1]==0)
#For any entries that fail, find taxID, example below is for row 8741 of tax_table
tax_table(LD_rat_table)[8741] 

#redo update, in this case row 8741 taxID = 1589297
new_classification <- classification(1589297, db ="ncbi")
#insert update into updated_taxonomy
updated_taxonomy[[8741]] <- new_classification

#Save updated taxonomy as RDS object 
saveRDS(updated_taxonomy, file = "LD_rat_table_ncbi_updated_taxonomy_table")

#Read in updated taxonomy 
updated_taxonomy <- readRDS("LD_rat_table_ncbi_updated_taxonomy_table")
summary(updated_taxonomy)


#Make any changes needed for sample data e.g. choosing factor levels, renaming taxonomy levels
head(tax_table(LD_rat_table))
colnames(tax_table(LD_rat_table)) <- c("superkingdom", "phylum", "class", "order", "family",  "genus", "species")
sample_names(LD_rat_table)
sample_names(LD_rat_table) <- c("11.1","11.2","11.3","6.1","6.2","6.3","Control.1","Control.2","Control.3")
sample_data(LD_rat_table)$DPI <- factor(sample_data(LD_rat_table)$DPI, levels = c("Control.6","6","11"))


# Assuming you have the updated_taxonomy list and the taxonomy table

# Check the number of rows in the taxonomy table
n_rows <- nrow(tax_table(LD_rat_table))

# Check the length of the updated_taxonomy list
n_taxa <- length(updated_taxonomy)

# Make sure the lengths match
if (n_rows != n_taxa) {
  stop("Lengths of taxonomy table and updated_taxonomy list do not match.")
}


for (i in 1:n_rows) {
  # Get the taxonomic information from the updated_taxonomy list for the current row
  updated_info <- updated_taxonomy[i]

  # Check if "superkingdom" rank exists in updated_info
  if ("superkingdom" %in% updated_info[[1]][[1]]$rank) {
    tax_table(LD_rat_table)[i, "superkingdom"] <- updated_info[[1]][[1]]$name[updated_info[[1]][[1]]$rank == "superkingdom"]
  } else {
    tax_table(LD_rat_table)[i, "superkingdom"] <- ""
  }

  # Check if "phylum" rank exists in updated_info
  if ("phylum" %in% updated_info[[1]][[1]]$rank) {
    tax_table(LD_rat_table)[i, "phylum"] <- updated_info[[1]][[1]]$name[updated_info[[1]][[1]]$rank == "phylum"]
  } else {
    tax_table(LD_rat_table)[i, "phylum"] <- ""
  }

  # Check if "class" rank exists in updated_info
  if ("class" %in% updated_info[[1]][[1]]$rank) {
    tax_table(LD_rat_table)[i, "class"] <- updated_info[[1]][[1]]$name[updated_info[[1]][[1]]$rank == "class"]
  } else {
    tax_table(LD_rat_table)[i, "class"] <- ""
  }

  # Check if "order" rank exists in updated_info
  if ("order" %in% updated_info[[1]][[1]]$rank) {
    tax_table(LD_rat_table)[i, "order"] <- updated_info[[1]][[1]]$name[updated_info[[1]][[1]]$rank == "order"]
  } else {
    tax_table(LD_rat_table)[i, "order"] <- ""
  }

  # Check if "family" rank exists in updated_info
  if ("family" %in% updated_info[[1]][[1]]$rank) {
    tax_table(LD_rat_table)[i, "family"] <- updated_info[[1]][[1]]$name[updated_info[[1]][[1]]$rank == "family"]
  } else {
    tax_table(LD_rat_table)[i, "family"] <- ""
  }

  # Check if "genus" rank exists in updated_info
  if ("genus" %in% updated_info[[1]][[1]]$rank) {
    tax_table(LD_rat_table)[i, "genus"] <- updated_info[[1]][[1]]$name[updated_info[[1]][[1]]$rank == "genus"]
  } else {
    tax_table(LD_rat_table)[i, "genus"] <- ""
  }

  # Check if "species" rank exists in updated_info
  if ("species" %in% updated_info[[1]][[1]]$rank) {
    tax_table(LD_rat_table)[i, "species"] <- updated_info[[1]][[1]]$name[updated_info[[1]][[1]]$rank == "species"]
  } else {
    tax_table(LD_rat_table)[i, "species"] <- ""
  }
}


##Save biom file with updated taxonomy 
saveRDS(LD_rat_table, file = "LD_rat_table_updated_taxonomy")