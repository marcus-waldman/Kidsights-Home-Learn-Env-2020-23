maxseqdiff<-function(x){x %>% na.omit() %>% unique() %>% sort() %>% diff.default() %>% max()}
