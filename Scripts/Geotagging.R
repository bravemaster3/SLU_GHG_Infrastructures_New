
photos_folder <- "photos_temp"

# Enhanced function to add GPS coordinates, Site, and Credit (preserve existing GPS)
add_metadata <- function(photo_path, latitude, longitude, site = NULL, credit = NULL) {
  # Check if file exists first
  if (!file.exists(photo_path)) {
    cat("File not found:", photo_path, "\n")
    return(FALSE)
  }
  
  # Check for existing GPS coordinates
  check_gps_cmd <- sprintf('exiftool -GPSLatitude -GPSLongitude -n "%s"', photo_path)
  existing_gps <- system(check_gps_cmd, intern = TRUE)
  
  has_existing_gps <- any(grepl("GPS Latitude|GPS Longitude", existing_gps)) && 
    any(grepl("\\d", existing_gps))
  
  cmd_parts <- c()
  
  # Add GPS coordinates only if they don't already exist
  if (has_existing_gps) {
    cat("Existing GPS coordinates found, preserving them for", basename(photo_path), "\n")
  } else {
    cat("No existing GPS coordinates found, adding new ones for", basename(photo_path), "\n")
    cmd_parts <- c(cmd_parts,
                   sprintf('-GPSLatitude=%f', abs(latitude)),
                   sprintf('-GPSLongitude=%f', abs(longitude)),
                   sprintf('-GPSLatitudeRef=%s', ifelse(latitude >= 0, "N", "S")),
                   sprintf('-GPSLongitudeRef=%s', ifelse(longitude >= 0, "E", "W")))
  }
  
  # Add Site information if provided
  if (!is.null(site) && !is.na(site) && site != "") {
    cmd_parts <- c(cmd_parts, sprintf('-XMP:Subject="%s"', site))  # Subject field
    cmd_parts <- c(cmd_parts, sprintf('-IPTC:Keywords="%s"', site))  # Keywords field
    cmd_parts <- c(cmd_parts, sprintf('-XMP:Location="%s"', site))  # Location field
  }
  
  # Add Credit information if provided
  if (!is.null(credit) && !is.na(credit) && credit != "") {
    cmd_parts <- c(cmd_parts, sprintf('-XMP:Creator="%s"', credit))  # Creator field
    cmd_parts <- c(cmd_parts, sprintf('-IPTC:By-line="%s"', credit))  # By-line field
    cmd_parts <- c(cmd_parts, sprintf('-EXIF:Artist="%s"', credit))  # Artist field
  }
  
  # Only run exiftool if we have something to add
  if (length(cmd_parts) > 0) {
    # Complete command
    cmd <- sprintf('exiftool %s -overwrite_original "%s"',
                   paste(cmd_parts, collapse = " "),
                   photo_path)
    
    cat("Running command:", cmd, "\n")
    result <- system(cmd, intern = FALSE)
    
    if (result == 0) {
      cat("Success: Metadata added to", basename(photo_path), "\n")
      return(TRUE)
    } else {
      cat("Error: Failed to add metadata to", basename(photo_path), "\n")
      return(FALSE)
    }
  } else {
    cat("No metadata changes needed for", basename(photo_path), "\n")
    return(TRUE)
  }
}

# Your updated coordinates with Site and Credit
# coordinates <- data.frame(
#   filename = c("HfM_aerial.jpg", "HfM_tower.jpg", "HfM_tower2.jpg", 
#                "Hmr_aerial.jpg", "Hmr_tower.jpg", "Hmr_chamber.jpg", "Hmr_chamber2.jpg", 
#                "Srj_tower.jpg", "Srj_tower2.jpg", "SVB_tower.jpg"),
#   latitude = c(64.159555, 64.159555, 64.159555, 64.159996, 64.159996, 64.1601, 64.1601, 64.174977, 64.174977, 64.256097),
#   longitude = c(19.551496, 19.551496, 19.551496, 19.569240, 19.569240, 19.5696, 19.5696, 19.563810, 19.563810, 19.774511),
#   Site = c("Halsingfors Stormyran", "Halsingfors Stormyran", "Halsingfors Stormyran",
#            "Halmyran", "Halmyran", "Halmyran", "Halmyran",
#            "Stortjarn", "Stortjarn", "Svartberget"),
#   Credit = c("Andreas Palmén", "Andreas Palmén", "Andreas Palmén",
#              "Andreas Palmén", "Andreas Palmén", "Andreas Palmén", "Andreas Palmén",
#              "Andreas Palmén", "Andreas Palmén", "Andreas Palmén")
# )




# coordinates <- data.frame(
#   filename = c("Tower.jpg", "BM_analyzers.jpg", "Entrance.jpg"),
#   latitude = c(64.022575, 64.022575, 64.022575),
#   longitude = c(20.569597, 20.569597, 20.569597),
#   Site = c("Bullmark drained peatland forest", "Bullmark drained peatland forest", "Bullmark drained peatland forest"),
#   Credit = c("Lei Gao", "Lei Gao", "Lei Gao")
# )


coordinates <- data.frame(
  filename = c("TBM_Tower.jpg", "TBM.jpg", "TBM2.jpg", "TBM_Chamber.jpg", "TBCC_DC2.jpg", "TBCC_DC3.jpg"),
  latitude = c(64.181021, 64.181021, 64.181021, 64.181242,64.177525, 64.17485),
  longitude = c(19.83733, 19.83733, 19.83733, 19.839874, 19.862569, 19.862563),
  Site = c("Trollberget mire", "Trollberget mire", "Trollberget mire", "Trollberget mire","Trollberget clearcut ditch cleaning", "Trollberget clearcut no ditch cleaning"),
  Credit = c("Alexander Pinkwart", "Alexander Pinkwart", "Alexander Pinkwart", "Alexander Pinkwart", "Alexander Pinkwart", "Alexander Pinkwart")
)


# Find all image files in current directory and subdirectories
all_photos <- list.files(photos_folder, pattern = "\\.(jpg|jpeg|png|tiff)$", 
                         ignore.case = TRUE, recursive = TRUE, full.names = TRUE)

cat("Found photos:\n")
print(all_photos)

# Process each photo in your coordinates list
for (i in 1:nrow(coordinates)) {
  filename <- coordinates$filename[i]
  
  # Find the photo in the list of all photos
  photo_match <- all_photos[basename(all_photos) == filename]
  
  if (length(photo_match) > 0) {
    photo_path <- photo_match[1]  # Use first match if multiple
    cat("\nProcessing:", filename, "at", photo_path, "\n")
    
    success <- add_metadata(photo_path, 
                            coordinates$latitude[i], 
                            coordinates$longitude[i],
                            coordinates$Site[i],
                            coordinates$Credit[i])
    
    if (success) {
      cat("✓ Successfully added metadata to", filename, "\n")
    }
  } else {
    cat("✗ Photo not found:", filename, "\n")
  }
}

# Enhanced verify function to show all metadata
verify_metadata <- function(photo_path) {
  if (file.exists(photo_path)) {
    cmd <- sprintf('exiftool -GPSLatitude -GPSLongitude -XMP:Subject -IPTC:Keywords -XMP:Location -XMP:Creator -IPTC:By-line -EXIF:Artist "%s"', photo_path)
    result <- system(cmd, intern = TRUE)
    
    if (length(result) > 0) {
      cat("Metadata for", basename(photo_path), ":\n")
      cat(paste(result, collapse = "\n"), "\n\n")
    } else {
      cat("No metadata found in", basename(photo_path), "\n")
    }
  }
}

cat("\n=== Verification ===\n")
# Verify a few photos
# for (filename in c("HfM_aerial.jpg", "Hmr_tower.jpg")) {
#   photo_match <- all_photos[basename(all_photos) == filename]
#   if (length(photo_match) > 0) {
#     verify_metadata(photo_match[1])
#   }
# }