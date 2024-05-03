pogDt <- fread("C:/Users/William.Roberts/OneDrive - Unilever/Documents/hbl_transition_copy/planogram_table.csv")

pogDt[, dims := sub(".* ", "", pogTitle)]
pogDt[, strsplit(dims, split = "x"), by = dims]
pogDt[, c("width", "height", "depth") := tstrsplit(dims, "x")]

pogDt[grep("BLW", pogTitle, invert = TRUE), blw_ind := 0]
pogDt[grep("BLW", pogTitle), blw_ind := 1]
pogDt[grep("BLW2", pogTitle), blw_ind := 2]
pogDt[, gnd_ind := fifelse(grepl("GND", pogTitle), 1, 0)]
pogDt[, exp_ind := fifelse(grepl("EXP", pogTitle), 1, 0)]

pogDt[, bd_ind := fifelse(grepl("BD", pogTitle),
                          sub(".*:BD", "", pogTitle) %>% 
                            sub(pattern = ":.*", "", .) %>% 
                            as.numeric(),
                          0)]

depthVals <- seq(12, 24, by = 2) %>%
  .[which(. != 20)]
desc19Vals <- c("AS0392",
                "AS0394",
                "AS0394",
                "AS0393",
                "AS0391",
                "AS0391")
descsTable <- data.table(fixtureDepth = rep(depthVals, 4),
                         fixtureDesc19 = rep(desc19Vals, 4),
                         bd_flag = rep(0:1, 2) %>% sapply(FUN = rep, 6) %>% c,
                         fixtureWidth = c(36, 48) %>% sapply(FUN = rep, 12) %>% c)
descsTable[bd_flag == 1, fixtureDesc1 := paste0("BSNA", fixtureWidth)]
descsTable[bd_flag == 0 &
             fixtureWidth == 36, fixtureDesc1 := c("AS0026",
                                                   "AS0026",
                                                   "AS0027",
                                                   "AS0028",
                                                   "AS0019",
                                                   "AS0019")]
descsTable[bd_flag == 0 &
             fixtureWidth == 48, fixtureDesc1 := c("AS0001",
                                                   "AS0002",
                                                   "AS0003",
                                                   "AS0004",
                                                   "AS0006",
                                                   "AS0006")]

# pog_type == 1: BD + Shelves depth should match, given by bd_ind
# exp_ind == 1 with bd_ind
pogDt[exp_ind == 1 & bd_ind != 0, pog_type := 1]

# pog_type == 2: BD + Shelves should match and be what they are currently
pogDt[exp_ind == 1 & bd_ind == 0, pog_type := 2]

# pog_type == 3: BD + Shelf should match and be 18"
# - BLW2 POGs with gnd_ind == 1 + height != 67
# - nonblw pogs + blw pogs without EXP
# - nonblw pogs + blw pogs with EXP but where bd_ind == 0
# BLW1 pogs without EXP
pogDt[blw_ind == 2 & gnd_ind == 1 & exp_ind == 0, pog_type := 3]
pogDt[blw_ind <= 1 & exp_ind == 0, pog_type := 3]
pogDt[blw_ind <= 1 & exp_ind == 1 & bd_ind == 0, pog_type := 3]

# pog_type == 4; 16" BD, 14" else shelf
# - Only BLW2 stores
# - all BLW2 67" stores
# - All 84" where gnd_ind == 0
pogDt[blw_ind == 2 & height <= 67 & exp_ind == 0, pog_type := 4]
pogDt[blw_ind == 2 & height == 84 & gnd_ind == 0 & exp_ind == 0 , pog_type := 4]

# pog_type == 5; *SK stores
pogDt[grepl("[1-9]SK", pogTitle), pog_type := 5]


fixtureDt <- fread("C:/Users/William.Roberts/OneDrive - Unilever/Documents/hbl_transition_copy/fixture_table.csv") %>% unique
shelfDt <- fixtureDt[fixtureType == 0][fixtureColor == -1]
shelfDt[, min_depth := min(fixtureDepth), by = pogId]
shelfDt[, max_depth := max(fixtureDepth), by = pogId]
shelfDt[, bd_flag := fifelse(fixtureY == 6.18, 1, 0)]

shelfDtUpdate <- merge(copy(shelfDt), pogDt[, .(pogId, pog_type, bd_ind)])

shelfDtUpdate[pog_type == 1, fixtureDepth := bd_ind]
shelfDtUpdate[pog_type == 2 & (min_depth != max_depth), fixtureDepth := max_depth]
shelfDtUpdate[pog_type == 3, fixtureDepth := 18]
shelfDtUpdate[pog_type == 4, fixtureDepth := fifelse(bd_flag == 1, 16, 14)]
shelfDtUpdate <- shelfDtUpdate[, .(pogId, bd_flag, fixtureX, fixtureY, fixtureWidth, fixtureDepth)] %>%
  merge(descsTable, by = c("fixtureWidth", "fixtureDepth", "bd_flag"), all.x = FALSE)

shelfDtUpdateMelted <- melt(shelfDtUpdate, 
                            id.vars = c("pogId", "fixtureX", "fixtureY", "fixtureWidth", "bd_flag")) %>%
  setnames(old = "value", new = "value_update")

shelfDtOrigMelted <- melt(shelfDt, id.vars = c("pogId", "fixtureX", "fixtureY", "fixtureWidth", "bd_flag")) %>%
  setnames(old = "value", new = "value_original")

shelfDtMeltedMerged <- merge(shelfDtUpdateMelted, shelfDtOrigMelted, 
                             by = c("pogId", "fixtureX", "fixtureY", "fixtureWidth", "bd_flag", "variable"),
                             all.x = TRUE)

shelfDtOutput <- shelfDtMeltedMerged %>%
  .[value_update != value_original & (!is.na(value_update)),
    .(PogId = pogId,
      FixtureX = fixtureX,
      FixtureY = fixtureY,
      FixtureFieldName = variable,
      FixtureFieldValue = value_update)]


fwrite(shelfDtOutput, "C:/Users/William.Roberts/OneDrive - Unilever/Documents/hbl_transition_copy/03 - Tie Reports/configs/shelfConfig.csv")

# Check 1: check for any 36 width shelves with incorrect part #'s
# ruleset:

# Check 2: if it has BD** and "EXP" in it, then the depth is set to ** on all shelves
# update Desc1 and Desc19

# Check 3: if it has BD** in it, then the depth is set to bd_ind (since these are all EXP, all BD+shelf same)


