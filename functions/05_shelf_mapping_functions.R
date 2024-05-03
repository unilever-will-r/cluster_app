createNrbMap <- function(workingToTemplate,
                         workingFixtureDt,
                         templateFixtureDt) {
  shelfMapDtWithOrder <- data.table(TemplateId = NULL,
                                    TemplateFixtureId = NULL,
                                    WorkingID = NULL,
                                    WorkingFixtureID = NULL,
                                    MatchFlag = NULL,
                                    Xrank = NULL,
                                    Yrank = NULL,
                                    Seq = NULL)
  for (a_templateId in workingToTemplate[, unique(templateId)]) {
    templateShelfDt <- templateFixtureDt %>%
      .[pogId == a_templateId] %>%    
      .[order(fixtureX, fixtureY)]
    templateShelfDt[, xrank := as.numeric(factor(fixtureX), levels = sort(fixtureX)), by = pogId]
    templateShelfDt[, yrank := as.numeric(factor(fixtureY), levels = sort(fixtureY)), by = .(pogId, xrank)]
    templateShelfDt[, match_key := paste0("S_", xrank, "_", yrank)]
    t_rows <- templateShelfDt[, .N]
    
    for (a_workingId in workingToTemplate[templateId == a_templateId, unique(workingId)])
    {
      
      workingShelfDt <- workingFixtureDt %>%
        .[pogId == a_workingId] %>%    
        .[order(fixtureX, fixtureY)] %>%
        .[, working_shelf_flag := 1]
      workingShelfDt[, xrank := as.numeric(factor(fixtureX), levels = sort(fixtureX)), by = pogId]
      workingShelfDt[, yrank := as.numeric(factor(fixtureY), levels = sort(fixtureY)), by = .(pogId, xrank)]
      workingShelfDt[, match_key := paste0("S_", xrank, "_", yrank)]
      w_rows <- workingShelfDt[, .N]
      
      # match_flags: 
      # 2: the shelves are uniquely mapped, should work fine
      # 1: there are more shelves in the template than the working=
      mergedDt <- merge(templateShelfDt[, .(templateId = pogId, xrank, template_fixture_id = fixtureId)],
                        workingShelfDt[, .(workingId = pogId, xrank, working_fixture_id = fixtureId)],
                        by = c("xrank"))
      
      if (w_rows != t_rows) {
        mergedDt[, match_flag := 1]
      } else {
        mergedDt[, match_flag := 2]
      }
      
      shelfMapDt <- mergedDt[, .(WorkingId = workingId,
                                 TemplateId = templateId,
                                 WorkingFixtureId = working_fixture_id,
                                 TemplateFixtureId = template_fixture_id,
                                 MatchFlag = match_flag,
                                 Xrank = xrank,
                                 Yrank = 1)]
      
      # Order from left to right
      fillOrderDt <- templateShelfDt[, .(pogId, 
                                         fixtureId,
                                         xrank)] %>% 
        .[order(pogId,-xrank)] %>%
        .[, Seq := seq_len(.N), by = pogId] %>%
        .[, .(TemplateId = pogId,
              TemplateFixtureId = fixtureId,
              Seq)]
      
      shelfMapDtWithOrder <- rbind(merge(shelfMapDt,
                                         fillOrderDt,
                                         by = c("TemplateId", "TemplateFixtureId")),
                                   shelfMapDtWithOrder)
      
    }
  }
  return(
    shelfMapDtWithOrder
  )
}

createShelfMap <- function(workingToTemplate,
                           workingFixtureDt,
                           templateFixtureDt) {
  # Output: pogId1, pogId2, shelfId1, shelfId2
  # Output needs to have the same number of rows as workingFixtureDt has shelves
  
  shelfMapDtWithOrder <- data.table(TemplateId = NULL,
                                    TemplateFixtureId = NULL,
                                    WorkingID = NULL,
                                    WorkingFixtureID = NULL,
                                    MatchFlag = NULL,
                                    Xrank = NULL,
                                    Yrank = NULL,
                                    Seq = NULL)
  
  for (a_templateId in workingToTemplate[, unique(templateId)]) {
    templateShelfDt <- templateFixtureDt %>%
      .[pogId == a_templateId] %>%    
      filterFixtureDtToShelves %>% 
      .[order(fixtureX, fixtureY)]
    templateShelfDt[, xrank := as.numeric(factor(fixtureX), levels = sort(fixtureX)), by = pogId]
    templateShelfDt[, yrank := as.numeric(factor(fixtureY), levels = sort(fixtureY)), by = .(pogId, xrank)]
    templateShelfDt[, match_key := paste0("S_", xrank, "_", yrank)]
    
    for (a_workingId in workingToTemplate[templateId == a_templateId, unique(workingId)])
    {
      
      workingShelfDt <- workingFixtureDt %>%
        .[pogId == a_workingId] %>%    
        filterFixtureDtToShelves %>% 
        .[order(fixtureX, fixtureY)] %>%
        .[, working_shelf_flag := 1]
      workingShelfDt[, xrank := as.numeric(factor(fixtureX), levels = sort(fixtureX)), by = pogId]
      workingShelfDt[, yrank := as.numeric(factor(fixtureY), levels = sort(fixtureY)), by = .(pogId, xrank)]
      workingShelfDt[, match_key := paste0("S_", xrank, "_", yrank)]
      
      
      
      
      
      # We wanto to match in such a way that working shelves can be mapped to
      # the same template pog shelf, so we can double-stuff those shelves
      mergedDt <- merge(templateShelfDt[, .(templateId = pogId, xrank, yrank, template_fixture_id = fixtureId)],
                        data.table(workingId = a_workingId, templateId = a_templateId),
                        by = "templateId",
                        allow.cartesian = TRUE) %>%
        merge(workingShelfDt[, .(workingId = pogId, xrank, yrank, working_fixture_id = fixtureId)],
              by = c("workingId", "xrank", "yrank"),
              all.x = TRUE)
      
      # match_flags: 
      # 0: Don't try to autofill
      # 1: the shelves aren't uniquely mapped but it should work because about to fix
      # 2: the shelves are uniquely mapped, should work fine
      mergedDt[, match_flag := fifelse(sum(is.na(working_fixture_id)) > 0, 0, 2), by = workingId]
      
      
      # overstuffers
      # 1.1: 8 foot fit with 12 foot shelf
      #     - fix: stuff the rightmost shelf with leftover sections
      overstuffPogIds <- mergedDt[is.na(working_fixture_id)] %>% 
        .[, ':='(na_x_min = min(xrank), 
                 na_x_max = max(xrank), 
                 na_y_min = min(yrank), 
                 na_y_max = max(yrank)), by = .(workingId)] %>%
        .[na_x_min > 1 & na_y_min == 1, workingId] %>% unique
      mergedDt[workingId %in% overstuffPogIds, match_flag := 1]
      
      overstuffPogFixDt <- mergedDt[match_flag == 1] %>% 
        .[, max_with_col := max(xrank * !(is.na(working_fixture_id))), by = workingId] %>%
        .[is.na(working_fixture_id), .(workingId, 
                                       template_fixture_id,
                                       xrank = max_with_col, 
                                       yrank,
                                       templateId, 
                                       match_flag)] %>%
        merge(copy(mergedDt)[, template_fixture_id := NULL],
              by = c("workingId", "xrank", "yrank", "templateId", "match_flag"))
      
      
      # 1.2: the shelves are all 5-high but all the templates are 6
      #     - fix: stuff the top shelf with both or do nothing
      
      
      
      # Check that nrow merged shelf equals nrow workingShelfDt
      shelfMapDt <- rbind(mergedDt[match_flag != 1 | !is.na(working_fixture_id)],
                          overstuffPogFixDt) %>%
        .[, .(WorkingId = workingId,
              TemplateId = templateId,
              WorkingFixtureId = working_fixture_id,
              TemplateFixtureId = template_fixture_id,
              MatchFlag = match_flag,
              Xrank = xrank,
              Yrank = yrank)]
      
      # Order from upper-right to bottom-left
      fillOrderDt <- templateShelfDt[, .(pogId, fixtureId,
                                         xrank, yrank)] %>% 
        .[order(pogId,-xrank,-yrank)] %>%
        .[, Seq := seq_len(.N), by = pogId] %>%
        .[, .(TemplateId = pogId,
              TemplateFixtureId = fixtureId,
              Seq)]
      
      shelfMapDtWithOrder <- rbind(merge(shelfMapDt,
                                   fillOrderDt,
                                   by = c("TemplateId", "TemplateFixtureId")),
                                   shelfMapDtWithOrder)
      
    }
  }
  return(
    shelfMapDtWithOrder
  )
}


filterFixtureDtToShelves <- function(fixtureDt) {
  return(
    fixtureDt[fixtureType == 0 & fixtureColor %in% c(-1, 8421376) & fixtureY < 80]
    # fixtureDt[fixtureType == 0]
  )
}

checkForStrongMatch <- function(workingId, 
                                templateId,
                                workingFixtureDt,
                                templateFixtureDt) {
  # PogId | ShelfName | ItemId | LocationId (1 leftmost, etc.)
  # strong match flag==
  # 0: The shelves won't match at all, don't try to autofill
  # 1: The shelves aren't the same widths but there's the same number of them so fill
  # 2: The shelves match up  perfect, so fill
  # 3. The workingPog has at least as many shelves as the template and they're on the same vertical plane, so fill
  workingShelfDt <- workingFixtureDt[pogId == workingId] %>%
    filterFixtureDtToShelves %>% 
    .[order(fixtureX, fixtureY)] %>%
    .[, working_shelf_flag := 1]
  workingShelfDt[, xrank := as.numeric(factor(fixtureX), levels = sort(fixtureX))]
  workingShelfDt[, yrank := as.numeric(factor(fixtureY), levels = sort(fixtureY)), by = xrank]
  workingShelfDt[, fixtureId := paste0("S_", xrank, "_", yrank)]
  
  templateShelfDt <- templateFixtureDt[pogId == templateId] %>%
    filterFixtureDtToShelves %>% 
    .[order(fixtureX, fixtureY)]
  templateShelfDt[, xrank := as.numeric(factor(fixtureX), levels = sort(fixtureX))]
  templateShelfDt[, yrank := as.numeric(factor(fixtureY), levels = sort(fixtureY)), by = xrank]
  templateShelfDt[, fixtureId := paste0("S_", xrank, "_", yrank)]
  
  if (nrow(workingShelfDt) == nrow(templateShelfDt)) {
    mergedShelfDt <- merge(templateShelfDt,
                           workingShelfDt,
                           by = c("fixtureId",
                                  "fixtureWidth",
                                  "fixtureX",
                                  "fixtureY"))
    if (nrow(mergedShelfDt) == nrow(workingShelfDt)) {
      return(2)
    } else {
      return(1)
    }
  }
  else {
    # 
    mergedShelfDt <- merge(templateShelfDt,
                           workingShelfDt,
                           by = c("fixtureId"))
    if (NROW(mergedShelfDt) == NROW(templateShelfDt))
    {
      return(3)
    } else{
      return(0)
    }
  }
}

buildIndexOffShelfMap <- function(shelfMapDt, indexDt) {
  
  indexDt <- merge(indexDt[, .(WorkingId = display, width = width)],
                   shelfMapDt[, .(TemplateId, WorkingId, MatchFlag)] %>% unique, 
                   by = "WorkingId", all.y = TRUE) %>% 
    .[is.na(width), width := 0]
  indexDt[, N := .N, keyby = .(MatchFlag, TemplateId)]
  indexDt[, ProjectId := paste0(TemplateId, "_",
                                MatchFlag, "_", N)]
  indexDt[, ProjectId := paste0(ProjectId, fifelse(seq_len(.N) > 50, "_b", "")), ProjectId]
  
  
  templatePogs <- indexDt[, .(PogId = TemplateId, ProjectId, Seq = 1)] %>% unique
  nontemplatePogs <- indexDt[order(width), .(PogId = WorkingId, Seq = 1 + (seq_len(.N))), by = ProjectId]
  
  indexConfigOut <- rbind(templatePogs, nontemplatePogs) %>%
    .[order(ProjectId, Seq)]
}

