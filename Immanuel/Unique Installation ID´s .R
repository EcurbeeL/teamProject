train_data <- data.table::fread('/Users/immanuelspiess/Documents/HTWG /8/Teamprojekt/GIt/PBS-Kids/data-science-bowl-2019/2s_train.csv', stringsAsFactors = F)

unique(train_data$installation_id, incomparables = FALSE, nmax = NA) -> all_install_id
