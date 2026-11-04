library(dplyr)

# read original data from Google sheet
data_original = read.csv("data/Social Counselling in Munich - main table.csv")


# TRANSFORM DATA FOR THE MAP ---------

# compute the coordinates (takes a long time)
compute_coordinates = TRUE
if (compute_coordinates) {
  library(tidygeocoder)
  
  # find coordinates
  coord_ids = which(data_original$contact.address != "")
  data_map = paste0(data_original$contact.address[coord_ids], " München")
  data_map = data.frame(ID = coord_ids,
                        Name = data_original$Name[coord_ids],
                        address = data_map) %>%
    geocode(address, method = "osm")
  
  # add Names where missing
  for (row in 1:nrow(data_map)) {
    if (data_map$Name[row] == "") data_map$Name[row] = data_map$Name[row-1]
  }
  data_map = data_map %>% 
    mutate(Popup = paste0("<b>", Name, "</b><br>", gsub(" München$", "", address)))
  
  # save the coordinate
  write.csv2(data_map, "data/Data_for_Map.csv", row.names=F)
}




# TRANSFORM DATA FOR THE LIST -------

transform_for_list = function(data, linebreak="<br>", charlimit=100,
                              bold1="<b>", bold2="</b>",
                              header1="<span style='font-size: 24px;'><b>", header2="</b></span>",
                              mail1='<a href="mailto:', mail2='">', mail3='</a>',
                              url1='<a href="', url2='">Mehr Infos auf der Webseite</a>') {
  
  # save indices with and without names
  rows_without_names = which(data$Name == "")
  rows_with_names = setdiff(1:nrow(data), rows_without_names)
  
  # collapse multiple phone numbers
  data_list = data %>% 
    rowwise() %>%
    mutate(contact.phone = paste(na.omit(c(contact.phone.1, contact.phone.2, contact.phone.3))[c(contact.phone.1, contact.phone.2, contact.phone.3) != ""], collapse = linebreak)) %>%
    ungroup() %>%
    select(Name, description, contact.link,
           contact.name, contact.mail, 
           contact.phone, open.hours.phone,
           contact.address, open.hours.in.person,
           language, free.of.charge)
  names = colnames(data_list)
  
  # correct the linebreak in open hours
  data_list = data_list %>% 
    mutate(open.hours.phone = gsub("\n", linebreak, open.hours.phone)) %>% 
    mutate(open.hours.in.person = gsub("\n", linebreak, open.hours.in.person))
  
  # prepare language names
  # data_list %>% pull(language) %>% strsplit(", ") %>% unlist() %>% unique() %>% sort()
  data_list = data_list %>%
    mutate(language = gsub("Albanian", "Alban / Shqip", language)) %>% 
    mutate(language = gsub("Arabic", "Arab / العربية", language)) %>% 
    mutate(language = gsub("Assyrian/Aramaeic", "Aramit/Suret", language)) %>% 
    mutate(language = gsub("Bosnian", "Bosanski / босански", language)) %>% 
    mutate(language = gsub("Bulgarian", "Bulgarski / български", language)) %>% 
    mutate(language = gsub("Croatian", "Hrvatski", language)) %>% 
    mutate(language = gsub("Dari", "Dari / از", language)) %>% 
    mutate(language = gsub("Farsi", "Farsi / فارسی", language)) %>% 
    mutate(language = gsub("French", "Français", language)) %>% 
    mutate(language = gsub("Greek", "Ελληνικά", language)) %>% 
    mutate(language = gsub("Hindi", "Hindi / हिंदी", language)) %>% 
    mutate(language = gsub("Hungarian", "Shqip", language)) %>% 
    mutate(language = gsub("Italian", "Italiano", language)) %>% 
    mutate(language = gsub("Korean", "한국인", language)) %>% 
    mutate(language = gsub("Latvian", "Latviešu", language)) %>% 
    mutate(language = gsub("Macedonian", "македонски", language)) %>% 
    mutate(language = gsub("Persian", "Farsi / فارسی", language)) %>% 
    mutate(language = gsub("Polish", "Polski", language)) %>% 
    mutate(language = gsub("Portuguese", "Português", language)) %>% 
    mutate(language = gsub("Punjabi", "Punjabi / ਪੰਜਾਬੀ / پنجابی", language)) %>% 
    mutate(language = gsub("Romanian", "Română", language)) %>% 
    mutate(language = gsub("Russian", "Русский", language)) %>% 
    mutate(language = gsub("Serbian", "Српски", language)) %>% 
    mutate(language = gsub("Sorani", "Sorani / سۆرانی", language)) %>% 
    mutate(language = gsub("Spanish", "Español", language)) %>% 
    mutate(language = gsub("Swahili", "Swahili", language)) %>% 
    mutate(language = gsub("Thai", "Thai / แบบไทย", language)) %>% 
    mutate(language = gsub("Turkish", "Türkçe", language)) %>% 
    mutate(language = gsub("Ukrainian", "українська", language)) %>% 
    mutate(language = gsub("Vietnamese", "Tiếng Việt", language)) %>% 
    mutate(language = gsub("multiple", "Viele Sprachen / multiple languages", language))
 
  
  # transform data_list to a list
  data_list = apply(data_list, 1, as.list)
  
  # integrate rows without name into their "main" row with name
  for (row in rows_without_names) {
    main_row = max(rows_with_names[rows_with_names < row])
    names_to_change = names[unlist(lapply(data_list[[row]], function(x) x != ""))]
    for (n in names_to_change) {
      data_list[[main_row]][[n]] = c(data_list[[main_row]][[n]], data_list[[row]][[n]])
    }
  }
  
  # delete all rows without name
  data_list = data_list[rows_with_names]
  
  # create output for the list
  out = lapply(data_list, function(row) {
    
    # phone contact information
    m_contact = max(c(length(row$contact.name), length(row$contact.mail), length(row$contact.phone)))
    contact = character(0)
    for (i in seq_len(m_contact)) {
      contact[i] = paste0(
        ifelse(!is.na(row$contact.name[i]) & row$contact.name[i] != "",
               paste0(row$contact.name[i], linebreak), ""),
        ifelse(!is.na(row$contact.mail[i]) & row$contact.mail[i] != "",
               paste0(mail1, row$contact.mail[i], mail2, row$contact.mail[i], mail3, linebreak), ""),
        ifelse(!is.na(row$contact.phone[i]) & row$contact.phone[i] != "",
               paste0(row$contact.phone[i], linebreak), ""),
        ifelse(!is.na(row$open.hours.phone[i]) &
                 length(row$open.hours.phone[row$open.hours.phone != ""]) == m_contact,
               paste0(bold1, "Sprechzeiten:", bold2, linebreak, row$open.hours.phone[i], linebreak), "")
      )
    }
    
    # phone opening hours
    contact_open = ifelse(m_contact > 1 & length(row$open.hours.phone) == 1 &
                             row$open.hours.phone[[1]] != "",
                          paste0(bold1, "Sprechzeiten:", bold2, linebreak,
                                 row$open.hours.phone, linebreak), "")
    
    # address contact information
    m_address = max(c(length(row$contact.address), length(row$open.hours.in.person)))
    address = character(0)
    for (i in seq_len(m_address)) {
      address[i] = paste0(
        ifelse(!is.na(row$contact.address[i]) & row$contact.address[i] != "",
               paste0(bold1, row$contact.address[i], bold2, " München", linebreak), ""),
        ifelse(!is.na(row$open.hours.in.person[i]) &
               length(row$open.hours.in.person[row$open.hours.in.person != ""]) == m_contact,
               paste0("Öffnungszeiten:", linebreak, row$open.hours.in.person[i], linebreak), "")
        )
    }
    
    # address opening hours
    address_open = ifelse(m_address > 1 & length(row$open.hours.phone) == 1 &
                            row$open.hours.phone[[1]] != "",
                          paste0(bold1, "Öffnungszeiten:", bold2, linebreak,
                                 row$open.hours.in.person, linebreak), "")
    
    # language information
    m_language = max(c(length(row$lanuage), length(row$language)))
    language = character(0)
    for (i in seq_len(m_language)) {
      language[i] = paste0(
        ifelse(!is.na(row$language[i]) & row$language[i] != "",
               paste0(row$language[i]), "")  # TODO make fancier
      )
    }
    if (m_language < m_contact & m_language < m_address) {
      language = paste0(paste0(unique(unlist(strsplit(language, ", "))), collapse=", "))
    }
    
    # combine contact, address and language
    if (length(contact) == length(address)) {
      if (length(contact) == length(language) & length(address) == length(language)) {
        detail = paste0(
          paste0(paste0(contact, address,
                        ifelse(language[1] != "", paste0(language, linebreak), ""),
                        collapse=linebreak), collapse=""),
          contact_open, address_open
        )
      } else {
        detail = paste0(
          paste0(paste0(contact, address, collapse=linebreak), collapse=""),
          contact_open, address_open, linebreak, language
        )
      }
    } else {
      detail = paste0(paste0(contact, collapse=linebreak),
                 contact_open,
                 linebreak,
                 paste0(address, collapse=linebreak),
                 address_open,
                 ifelse(language != "", paste0(linebreak, language), "")
                 )
    }
    
    # complete list output
    detail = paste0(
      #header1, row$Name, header2,
      #linebreak, linebreak,
      row$description, linebreak, linebreak,
      url1, row$contact.link, url2, linebreak, linebreak,
      detail,
      ifelse(row$free.of.charge == "yes",
             paste0(linebreak, "Dieses Angebot ist kostenfrei."), "")
    )
    
    # for trouble-shooting:
    # if (length(detail) > 1) print(row$Name)
    
    return(detail)
  }) %>% unlist()
  
  # create shortened output for the list
  short = lapply(data_list, function(row) {
    paste0(
      header1, row$Name, header2
      # linebreak,
      # substr(row$description, 1, charlimit),
      # " ... ", url1, row$contact.link, url2
    )
  }) %>% unlist()
  
  # create output data frame
  data.frame(ID = rows_with_names, short_output = short, list_output = out)
}

# create data_list
data_list = transform_for_list(data_original)

# save data_list
write.csv2(data_list, "data/Data_for_list.csv", row.names=F)



# TRANSFORM DATA FOR THE ALGORITHM --------------


# transform the categories to a wide format
data_to_wide = function(data, answers) {
  
  # clean languages
  rows_without_names = which(data$Name == "")
  rows_with_names = setdiff(1:nrow(data), rows_without_names)
  for (row in rows_without_names) {
    main_row = max(rows_with_names[rows_with_names < row])
    data[main_row, "language"] = paste(c(data[main_row, "language"], data[row, "language"]), collapse = ", ")
  }
  
  # remove all entries without name
  data = data[rows_with_names,]
  
  # coarse language variable into binary
  data = data %>% mutate(language = ifelse(language == "", "", "language"))
  
  # initialize
  result = data.frame()
  filter_columns = c("Topic...1", "Topic...2", "Topic...3",
                     "Group...1", "Group...2", "Group...3",
                     "Specification...1", "Specification...2",
                     "language")
  
  # create a vector with all possible answers
  answers_names = data %>% select(all_of(filter_columns)) %>% as.matrix() %>% as.vector() %>% 
    unique()
  answers = rep(0, (length(answers_names) - 1)) %>%
    setNames(answers_names[answers_names != ""])
  
  # transform data to wide format
  for (i in 1:nrow(data)) {
    ls = row_to_wide(data[i, ], answers=answers, filter_columns)
    result = rbind(result, as.data.frame(t(ls)))
  }
  
  # create column "total" from row sums
  result[, "total"] = rowSums(result)
  
  return(result)
}

# helper function that transforms each row
row_to_wide = function(row, answers, filter_columns) {
  
  # Get the values of the filter columns
  all_values = unlist(row %>% select(all_of(filter_columns)))
  all_values = all_values[all_values != ""]
  
  # set true categories to 1
  out = answers
  out[] = 0
  out[names(out) %in% all_values] <- 1
  
  return(out)
}

# create wide format data for categories
data_wide = data_to_wide(data_original)

# save
write.csv2(data_wide, "data/Data_for_algo.csv", row.names=F)
