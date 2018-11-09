library(tidyverse)
library(rvest)
library(XML)
library(RSelenium) # start a server with utility function
library(magrittr)
library(keyringr)
library(x3ptools)

source("UploadFunctions.R")

# Create the set before using this script
# setName <- "Hamby-44-CSAFE"
# datapath <- "~/Projects/CSAFE/BulletUploads/data"
# barrel_regex <- "((?:Barrel \\d{1,})|(Unknown))"
# meta_path <- "meta.csv"

setName <- "PhoenixPD"
datapath <- "/media/Sunny/CSAFE/phoenix-upload/x3ps-nist"
barrel_regex <- "((?:Unk)|([A-Z]\\d{1}[A-Z]\\d{1,}))"
meta_path <- "/media/Sunny/CSAFE/phoenix-upload/phoenix-meta.csv"

setSelector <- "#tableStudies_wrapper > div:nth-child(2) > div > div > div.DTFC_LeftWrapper > div.DTFC_LeftBodyWrapper > div > table > tbody > tr.even > td > a"

# --- Options ----
dropdown_options <- list(
  barrel_brand_options = c("AA Arms", "Al-Qadissiya", "Astra", "Beretta", 
                           "Bersa", "Browning", "Bryco", "Calico", "Canik", "Chiappa", "Colt", 
                           "CZ-USA", "Davis Industries", "Encom America", "Feather Industries", 
                           "FEG", "FM Argentine", "FN Herstal", "Glock", "Heckler & Koch", 
                           "Hi-Point", "Hungarian Arms Works", "IMI", "Ingram", "Intratec", 
                           "Kahr Arms", "Kel-Tec", "Kimber", "Llama", "Luger", "Maadi", 
                           "Norinco", "Para Ordnance", "Remington", "Republic Arms", "Rossi", 
                           "Ruger", "SCCY", "Sig Sauer", "Smith & Wesson", "Springfield Armory", 
                           "Stallard Arms", "Star", "Sten", "Steyr Arms", "Stoeger", "SWD", 
                           "Tanfoglio", "Taurus", "Walther", "Other"),
  
  bullet_brand_options = c("Aguila", "Bear", "CCI", "Federal", "Fiocchi", "FN", "Hornady", 
                           "Nosler", "PMC", "Remington", "Sellier & Bellot", "Speer", "Tulammo", 
                           "Weatherby", "Winchester", "Wolf", "Other"),
  
  bullet_weight_options = c("= 301", "101-150", "151-200", "201-250", "251-300", "30-50", "51-100", 
                            "Not specified"),
  
  surface_material_options = c("Brass", "Copper", "Lead", "Polymer", "Steel", "Not specified", "Other"),
  
  caliber_options = c("22LR", "25 Auto", "32 Auto", "357 Sig", "38/357", "380 Auto", 
                      "40/10 mm", "44 Spl/Mag", "45 Auto", "9 mm Luger", "Other"),
  
  breech_face_options = c("Arched", "Circular", "Cross Hatch", "Granular", "Smooth", "Striated", "Not specified", "Other"),
  
  n_lands_options = c(as.character(2:9), ">=10", "Not specified"),
  
  twist_options = c("Left", "Right", "Not specified")
)


# --- Browser setup ----

# Client option
rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]

# Docker option # It recognizes Docker's browser as a robot
# remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
# remDr$open()
remDr$navigate(url = "https://tsapps.nist.gov/NRBTD/")

# Establish a wait for an element
# remDr$setImplicitWaitTimeout(1000)

# Use: https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
# To set up keyringr with the correct password
nbtrd_login(remDr, "csafeISU", decrypt_gk_pw("db csafe user csafeISU"))

remDr$navigate("https://tsapps.nist.gov/NRBTD/Studies/Studies")

set_link <- remDr$findElement(using = "link text", value = setName)$getElementAttribute("href")[[1]]
remDr$navigate(set_link)

# --- Create Barrels ----
datafiles <- list.files(datapath, full.names = T)

barrels <- str_extract(datafiles, barrel_regex) %>%
  unique()

barrels <- barrels[!is.na(barrels)]

# DF of barrel info
firearm_info <- data_frame(
  idx = 1:length(barrels),
  name = barrels,
  brand = "Ruger",
  brand_other = NA,
  model = "P-95",
  caliber = "9 mm Luger",
  caliber_other = NA,
  consec_manufacture = FALSE,
  comments = "Land impressions are not indexed from bullet to bullet.", 
  cartridges = FALSE,
  bullets = TRUE,
  breech_face_class = NA,
  breech_face_other = NA,
  firing_pin_class = NA,
  firing_pin_other = NA,
  n_lands = "6",
  twist_direction = "Right"
) %>%
  nest(-idx, .key = "df")

firearm_info$url <- map(firearm_info$df, partial(create_firearms, rd = remDr, seturl = set_link))
firearm_info <- unnest(firearm_info)

# --- Create Bullets ----
# Get all barrels and collect edit links

remDr$navigate(set_link)
barrelList <- remDr$findElements(using = "css selector", "a[href*='Firearm'][href*='Details']")
barrelLinks <- data_frame(firearm_name = sapply(barrelList, function(x) unlist(x$getElementText())),
                          details_url = sapply(barrelList, function(x) unlist(x$getElementAttribute("href"))),
                          id = str_replace(details_url, fixed("https://tsapps.nist.gov/NRBTD/Studies/Firearm/Details/"), "")) %>%
  unique() %>%
  filter(firearm_name != "Bullet / CC")


elList <- remDr$findElements(using = "css selector", "a[href*='Firearm'][href*='Edit']")
editLinks <- data_frame(edit_text = sapply(elList, function(x) unlist(x$getElementText())),
                        edit_url = sapply(elList, function(x) unlist(x$getElementAttribute("href"))),
                        id = str_replace(edit_url, fixed("https://tsapps.nist.gov/NRBTD/Studies/Firearm/Edit/"), "")) %>%
  unique() %>%
  filter(edit_text == "Edit") %>%
  select(-edit_text)

firearm_links <- left_join(barrelLinks, editLinks)
rm(barrelList, barrelLinks, elList, editLinks)


bullet_info <- data_frame(
  brand = "American Eagle", 
  brand_other = NA,
  caliber = "9 mm",
  caliber_other = NA,
  grain = "101-150",
  cartridge_des = "9 mm Luger",
  surface_mat = "Copper",
  surface_mat_other = NA,
  firing_seq = "repeat fire",
  lot_no = "",
  comments = ""
)

metadata <- read_csv(meta_path)
metadata <- metadata %>%
  mutate(barrel = ifelse(bullet == "Bullet E", "Unknown", barrel)) %>%
  left_join(select(firearm_links, barrel = firearm_name, id, details_url)) %>%
  merge(bullet_info) %>%
  mutate_at(vars(barrel, bullet), str_replace_all, "[[:punct:]]", "")

bullets <- metadata %>% 
  select(-land, -land_str) %>%
  unique() %>%
  mutate(idx = 1:n()) %>%
  nest(-idx, .key = "bullet_only_info")
bullet_success <- map(bullets$bullet_only_info, partial(create_bullets, remDr = remDr)) %>% unlist()
bullets$bullet_link <- bullet_success
bullets <- unnest(bullets)


# --- Create Lands ----

get_land_info <- function(filename) {
  tmp <- read_x3p(filename)
  
  full_comment <- paste(
    c(paste0("Scan Comment: ", tmp$general.info$Comment[[1]]),
      paste0("Instrument Serial Number: ", tmp$general.info$Instrument$Serial[[1]]),
      paste0("Instrument Version: ",       tmp$general.info$Instrument$Version[[1]]),
      paste0("Instrument Calibration Date: ", tmp$general.info$CalibrationDate[[1]]),
      paste0("Probing System: ", tmp$general.info$ProbingSystem$Type[[1]]),
      paste0("Scanned by: ", tmp$general.info$Creator[[1]]),
      paste0("Creation Date: ", tmp$general.info$Date[[1]]),
      paste0("binary_md5checksum: ", tmp$matrix.info$DataLink$MD5ChecksumPointData[[1]])
    ),
    collapse = "\n")
  
  
  data_frame(
    filename = filename, 
    instrument_brand = tmp$general.info$Instrument$Manufacturer[[1]],
    instrument_model = tmp$general.info$Instrument$Model[[1]],
    lateral_res = tmp$header.info$incrementX*1e6,
    vertical_res = tmp$header.info$incrementY*1e6,
    obj = str_replace(tmp$general.info$ProbingSystem$Identification[[1]], "[xX]", "") %>% as.numeric(),
    aperture = NA,
    comment = full_comment
  )
}

default_land_info <- data_frame(
  creator = "Hofmann, Heike",
  nist_meas = "F",
  measurand = "3D Topography",
  lighting_dir = NA,
  lighting_dir_other = NA,
  meas_type = "Other",
  meas_type_other = "Confocal Light Microscope",
  roi = "Land Engraved Area"
)

indiv_land_info <- list.files("data/", full.names = T) %>%
  map_df(get_land_info)

indiv_land_info <- indiv_land_info %>%
  mutate(land_str = basename(filename)) %>%
  left_join(select(metadata, barrel, bullet, land, land_str)) %>%
  left_join(select(bullets, bullet, barrel, bullet_link)) %>%
  merge(default_land_info) %>%
  mutate(new_filename = sprintf("data/%s-%s-%s.x3p", barrel, bullet, land) %>%
           str_replace_all("(Barrel|Bullet|Land)\\s", "\\1_")) %>%
  mutate(idx = 1:n()) %>%
  nest(-idx, .key = "land_df")

land_success <- map(indiv_land_info$land_df, partial(create_lands, remDr = remDr)) %>% unlist()
indiv_land_info$land_link <- land_success
indiv_land_info <- unnest(indiv_land_info)


# --- Clean Up ----
remDr$closeall()

save(firearm_info, bullet_info, indiv_land_info, file = paste0(setName, "_Upload.Rdata"))
