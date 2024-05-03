# cross basket analysis2
tripDt <- fread("~/Downloads/subs_files/tripDt.csv") %>%
  cleanNames %>% 
  # .[quantity >= 1] %>%
  .[, .(tripid, itemnumber)] %>% unique

setkey(tripDt, tripid)

totalBaskets <- tripDt[, uniqueN(tripid)]

occurTogetherDt <- tripDt[tripDt, allow.cartesian = TRUE] %>%
  .[, .(n_baskets_both = uniqueN(tripid)), .(item1 = itemnumber,
                                                 item2 = i.itemnumber)]
item1Dt <- tripDt[tripDt, allow.cartesian = TRUE] %>%
  .[, .(n_baskets_item1 = uniqueN(tripid)), .(item1 = itemnumber)]
item2Dt <- tripDt[tripDt, allow.cartesian = TRUE] %>%
  .[, .(n_baskets_item2 = uniqueN(tripid)),
    .(item2 = i.itemnumber)]

outputDt <- merge(occurTogetherDt,
                  item1Dt, by = c("item1"), all.x = TRUE) %>%
  merge(item2Dt, by = c("item2"), all.y = TRUE) %>%
  .[item1 != item2]

# add on single baskets to adjust n_baskets_just_item_X numbers
singleDt <- fread("~/Downloads/subs_files/singleDt.csv") %>% 
  cleanNames %>%
  .[quantity >= 1] %>%
  .[, .(tripid, itemnumber)] %>% unique %>%
  .[, .(n_baskets_item = uniqueN(tripid)), keyby = itemnumber]

# add on item1
outputDt <- merge(outputDt, singleDt[, .(item1 = itemnumber,
                                         item1_basket_adds = n_baskets_item)],
                  by = "item1", all.x = TRUE) %>% 
  .[is.na(item1_basket_adds), item1_basket_adds := 0] %>%
  .[, n_baskets_item1 := item1_basket_adds + n_baskets_item1] %>%
  .[, item1_basket_adds := NULL]

# add on item2
outputDt <- merge(outputDt, singleDt[, .(item2 = itemnumber,
                                         item2_basket_adds = n_baskets_item)],
                  by = "item2", all.x = TRUE) %>% 
  .[is.na(item2_basket_adds), item2_basket_adds := 0] %>%
  .[, n_baskets_item2 := item2_basket_adds + n_baskets_item2] %>%
  .[, item2_basket_adds := NULL]



outputDt %>%
  .[, n_baskets_just_item_1 := n_baskets_item1 - n_baskets_both, by = .(item2, item1)] %>%
  .[, n_baskets_just_item_2 := n_baskets_item2 - n_baskets_both, by = .(item2, item1)] %>%
  .[, n_baskets_either      := n_baskets_just_item_1 + n_baskets_just_item_2, by = .(item2, item1)] %>%
  .[, n_baskets_neither     := totalBaskets - n_baskets_either, by = .(item2, item1)]



formulaNames <-  c("a", "b", "c", "d")
outputNames <- c("n_baskets_neither",
                 "n_baskets_just_item_1",
                 "n_baskets_just_item_2",
                 "n_baskets_both")
setnames(outputDt, old = outputNames,
         new = formulaNames)

outputDt[, a := as.double(a)] %>%
  .[, b := as.double(b)] %>%
  .[, c := as.double(c)] %>%
  .[, d := as.double(d)]

outputDt[, yules_y := (sqrt(a * d) - sqrt(b * c)) / (sqrt(a * d) + sqrt(b * c))]

outputDt <- merge(outputDt, attDt[, .(itemId, 
                                      item1_desc = item_description, 
                                      item1_subclass = subclass,
                                      item1_brand = brand)],
                  by.x = c("item1"), by.y = c("itemId"), all.x = TRUE) %>%
  merge(attDt[, .(itemId,
                  item2_desc = item_description, 
                  item2_subclass = subclass,
                  item2_brand = brand)],
        by.x = c("item2"),
        by.y = c("itemId"), 
        all.x = TRUE)

setnames(outputDt, old = formulaNames, new = outputNames)


merge(tripcount_pairs_table,
      tripcounts_table[, .(itemnumber, item1_count = n_trips)],
      by.x = "item1", by.y = "itemnumber") %>%
  merge(tripcounts_table[, .(itemnumber, item2_count = n_trips)],
        by.x = "item2", by.y = "itemnumber")