#'Calculate use metrics from stubble height and woody data.
#'
#'@param header Data frame. Use the data frame from the \code{header_build_lentic()} output.
#'@param annualuse_tall Data frame. Use the data frame from the \code{gather_annualuse()} output.
#'@param woody_tall Data frame. Use the data frame from the \code{gather_woodyspecies()} output.
#'@param hummocks Data frame. Use the data frame from the \code{gather_hummocks()} output.
#'@param unit String. The sampling unit by which data should be summarized. Should be `by_plot`, or `by_line`. `by_geosurface` is not an option for this calculation. Defaults to `by_plot`.
#'@param by_species Logical. If TRUE then results will be reported at the species-plot level. Defaults to FALSE.
#'@param tree_tall Optional data frame. Use the data frame from the \code{gather_tree()} output. For data from years prior to 2023, this data.frame will not exist.
#'@returns Data.frame of the summarized woody and annual use data by plot.

#' @export use_metrics
#' @rdname woodystructure_use_metrics
use_metrics <- function(header, annualuse_tall, woody_tall, masterspecieslist, by_line = F, by_species = F){

  #Allow to be calculated by line
  if (unit == "by_line") {
    level <- rlang::quos(PlotID, EvaluationID, LineKey)
    level_colnames <- c("PlotID", "EvaluationID", "LineKey")
  } else if (unit == "by_plot") {
    level <- rlang::quos(PlotID, EvaluationID)
    level_colnames <- c("PlotID", "EvaluationID")
  }else {
    stop("Incorrect unit. Height metrics can only calculated by_plot or by_line. ")
  }

  #select relevant columns from species list.
  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol, ends_with("WetStatus"), Species)

  #calculate use metrics from woodies, first by filtering woody species use to riparian woody species
  riparianwoody <- woody_tall%>%
    dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), UseClass != "")%>%
    dplyr::right_join(header%>%
                        dplyr::select(PlotID, EvaluationID, WetlandIndicatorRegion)%>%
                        sf::st_drop_geometry(),
                     .,
                     by = c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., masterspecieslist, by = c("RiparianWoodySpecies" = "Symbol"))%>%
    dplyr::mutate(RipStatus = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                        WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                        WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                        WetlandIndicatorRegion=="Alaska" ~ AK_WetStatus,
                                        TRUE ~ "REGIONMISSING"),
                  UnknownCodeKey = ifelse(Species %in% c(NA, ""), UnknownCodeKey, NA))%>%
    dplyr::select(-Species)

  if(by_species == F){

    #Calculate annual use on graminoids. First find dominant species excluding "N".
    dominantspecies <- annualuse_tall%>%
      dplyr::filter(StubbleHeightDominantSpecies != "N")%>%
      dplyr::group_by(!!!level, StubbleHeightDominantSpecies)%>%
      dplyr::summarize(Count = n(), .groups = "drop_last")%>%
      dplyr::mutate(rank = rank(-Count, ties.method = "random"))%>%
      dplyr::arrange(EvaluationID, desc(Count))%>%
      dplyr::filter(rank<=2)%>%
      pivot_wider(., id_cols = c(!!!level), values_from = StubbleHeightDominantSpecies, names_prefix = "AU_DominantGraminoid", names_from= rank)

    #Next calculate AnnualUse metrics
    annualusemetrics <- annualuse_tall%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(AU_SoilAlteration_Avg = round(mean(as.numeric(SoilAlteration), na.rm = T)/5*100, digits = 2),
                       AU_StubbleHgt_Avg = round(mean(StubbleHeight, na.rm = T), digits = 2),
                       AU_Grazed_Pct = round(sum(ifelse(Grazed == "Yes", 1, 0), na.rm = T)/sum(ifelse(Grazed %in% c("Yes", "No"), 1, 0))*100, digits = 2),
                       AU_StubbleHgt_Cnt = sum(ifelse(StubbleHeightDominantSpecies != "N", 1, 0)), .groups = "keep")%>%
      dplyr::left_join(., dominantspecies, by = level_colnames)

    #now put together dominant woody species
    dominantripwood <- riparianwoody%>%
      dplyr::filter(RipStatus %in% c("OBL", "FACW", "FAC"))%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies)%>%
      dplyr::summarize(Count = n(), .groups = "drop_last")%>%
      dplyr::mutate(rank = rank(-Count, ties.method = "random"))%>%
      dplyr::arrange(EvaluationID, desc(Count))%>%
      dplyr::filter(rank<=2)%>%
      tidyr::pivot_wider(., id_cols = c(!!!level), values_from = RiparianWoodySpecies, names_prefix = "AU_DominantWoody", names_from= rank)

    #Calculate Woody Metrics
    woodymetrics <- riparianwoody%>%
      dplyr::filter(RipStatus %in% c("OBL", "FACW", "FAC"))%>%
      dplyr::rowwise()%>%
      dplyr::mutate(TotalUseClass = sum(ifelse(!(UseClass %in% c(NA, "")), 1, 0)))%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(AU_WoodyNotAvailable_Pct = round(sum(ifelse(UseClass == "NA", 1, 0))/sum(TotalUseClass)*100, digits = 2),
                       AU_WoodyUseClass_Avg = round(mean(suppressWarnings(as.numeric(UseClass)), na.rm = T), digits = 2),
                       AU_WoodyUseClass_Cnt = sum(TotalUseClass),
                       .groups = "drop")

    #All metrics together
    UseMetrics <- annualusemetrics%>%
      dplyr::left_join(., woodymetrics, by = level_colnames)%>%
      dplyr::left_join(., dominantripwood, by = level_colnames)%>%
      dplyr::mutate(AU_WoodyUseClass_Cnt = ifelse(!is.na(AU_SoilAlteration_Avg), tidyr::replace_na(AU_WoodyUseClass_Cnt, 0), AU_WoodyUseClass_Cnt))

  } else{
    annualuse_tall <- annualuse_tall%>%
      dplyr::left_join(.,
                       masterspecieslist%>%dplyr::select(Symbol, Species),
                       by = c("StubbleHeightDominantSpecies"="Symbol"))%>%
      dplyr::mutate(UnknownCodeStubbleKey = ifelse(Species %in% c(NA, ""), UnknownCodeStubbleKey, NA))%>%
      dplyr::select(-Species)

    #No need to calculate "dominant species" when calculating averages by species.
    annualusemetrics <- annualuse_tall%>%
      dplyr::group_by(!!!level, StubbleHeightDominantSpecies, UnknownCodeStubbleKey)%>%
      dplyr::filter(StubbleHeightDominantSpecies !="N")%>%
      dplyr::summarize(AU_StubbleHgt_Avg = round(mean(StubbleHeight, na.rm = T), digits = 2),
                       AU_Grazed_Pct = round(sum(ifelse(Grazed == "Yes", 1, 0), na.rm = T)/
                                               sum(ifelse(Grazed %in% c("Yes", "No"), 1, 0))*100, digits = 2),
                       AU_StubbleHgt_Cnt = n())%>%
      dplyr::rename(Species = StubbleHeightDominantSpecies,
                    UnknownCodeKey = UnknownCodeStubbleKey)

    woodymetrics <- riparianwoody%>%
      dplyr::rowwise()%>%
      dplyr::mutate(TotalUseClass = sum(ifelse(!(UseClass %in% c(NA, "")), 1, 0)))%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::summarize(AU_WoodyNotAvailable_Pct = round(sum(ifelse(UseClass == "NA", 1, 0))/sum(TotalUseClass)*100, digits = 2),
                       AU_WoodyUseClass_Avg = round(mean(suppressWarnings(as.numeric(UseClass)), na.rm = T), digits = 2),
                       AU_WoodyUseClass_Cnt = sum(TotalUseClass))%>%
      dplyr::rename(Species = RiparianWoodySpecies)

    #Metrics together
    UseMetrics <- rbind(annualusemetrics, woodymetrics)
  }

  return(UseMetrics)
}

#' @export ageclass_metrics
#' @rdname woodystructure_use_metrics
ageclass_metrics <- function(header, woody_tall, tree_tall=NULL, masterspecieslist, unit = "by_plot", by_species = F){

  #allow metrics to be calculated at the plot level, line level, or species level.
  level <- rlang::quos(PlotID, EvaluationID)
  level_colnames <- c("PlotID", "EvaluationID")
  if(unit == "by_line"){
    level <- c(level, rlang::quos(LineKey))
    level_colnames <- c(level_colnames, "LineKey")
  } else if(!unit %in% c("by_line", "by_plot")){
    stop("Incorrect unit. Height metrics can only calculated by_plot or by_line. ")
  }
  if(by_species){
    level <- c(level, rlang::quos(RiparianWoodySpecies, UnknownCodeKey))
    level_colnames <- c(level_colnames, "RiparianWoodySpecies", "UnknownCodeKey")
  }

  # Calculate the number of quadrats for all lines with no woody species present.
  # Distinct columns are the same here independent of the level of reporting
  quadcalc <-
    woody_tall%>%
    dplyr::filter(WoodySpeciesPresent=="No")%>%
    dplyr::distinct(PlotID, EvaluationID, LineKey, LineLengthCM, interval)%>%
    dplyr::mutate(n = round(LineLengthCM/interval, digits = 0))

  # Calculate the number of quadrats for all lines with woody species present.
  # This will use the points in Annual Use Points Repeat to count by line.
  # Grouping for this table should either be plot level or transect level
  quadcount <-
    woody_tall%>%
    dplyr::filter(WoodySpeciesPresent=="Yes")%>%
    dplyr::distinct(PlotID, EvaluationID, LineKey, PointNbr)%>%
    dplyr::group_by(PlotID, EvaluationID, LineKey)%>%
    dplyr::count()%>%
    dplyr::bind_rows(., quadcalc)%>%
    dplyr::group_by(across(level_colnames[!level_colnames%in%c("RiparianWoodySpecies", "UnknownCodeKey")]))%>%
    dplyr::summarize(nquads = sum(n))

  #Join to master list to indicate which are ID'd to Species
  #This is only important for species-wise calcs, but it doesn't hurt plot and transect calcs.
  woody_tall <- woody_tall%>%
    dplyr::left_join(.,
                     masterspecieslist%>%dplyr::select(Symbol, Species),
                     by = c("RiparianWoodySpecies"="Symbol"))%>%
    dplyr::mutate(UnknownCodeKey = ifelse(Species %in% c(NA, ""), UnknownCodeKey, NA))

  if(!is.null(tree_tall)){
    tree_tall <- tree_tall%>%
      dplyr::left_join(.,
                       masterspecieslist%>%dplyr::select(Symbol, Species),
                       by = c("TreeSpecies"="Symbol"))%>%
      dplyr::filter(!is.na(TreeSpecies))%>%
      dplyr::mutate(RiparianWoodySpecies = TreeSpecies,
                    UnknownCodeKey = ifelse(Species %in% c(NA, ""), TreeUnknownCodeKey, NA),
                    OverhangingOrRooted = "Rooted-in",
                    AgeClass = dplyr::case_when(TreeIndivLiveDead == "D"~"Dead",
                                                (TreeMaxHeightClass %in% c("Height Class 0",
                                                                           "Height Class 1",
                                                                           "Height Class 2") &
                                                   DBHcm < 2.5) |
                                                  (TreeMaxHeightClassAK <= 1 & DBHcm < 2.5)~"Seedling",
                                                (TreeMaxHeightClass %in% c("Height Class 3",
                                                                           "Height Class 4",
                                                                           "Height Class 5",
                                                                           "Height Class 6") &
                                                   DBHcm > 7.6) |
                                                  (TreeMaxHeightClassAK > 1 & DBHcm > 7.6)~"Mature",
                                                TreeMaxHeightClass %in% c("Height Class 3",
                                                                          "Height Class 4",
                                                                          "Height Class 5",
                                                                          "Height Class 6") |
                                                  TreeMaxHeightClassAK > 1 |
                                                  (DBHcm >= 2.5 & DBHcm <= 7.6)~"Young"))

    #Tree age class counts to incorporate below.
    ageclass_trees <- tree_tall%>%
      dplyr::group_by(PlotID, EvaluationID, LineKey, RiparianWoodySpecies, UnknownCodeKey, OverhangingOrRooted, AgeClass)%>%
      dplyr::summarize(Count = n())%>%
      #pivot into a wider format to match ageclass_sum table below.
      tidyr::pivot_wider(.,
                         id_cols = c(PlotID, EvaluationID, LineKey, RiparianWoodySpecies, UnknownCodeKey, OverhangingOrRooted),
                         names_from = AgeClass, names_glue = "{AgeClass}Count",
                         values_from = Count)

    #Tree Max heights
    treemetrics <- tree_tall%>%
      #Reformat height classes to look like eventual metric names. AK heights need to be converted to classes.
      dplyr::mutate(TreeMaxHeightClass = dplyr::case_when(stringr::str_detect(TreeMaxHeightClass, "^Height Class")~stringr::str_replace_all(TreeMaxHeightClass, c(" "="", "Height" = "Hgt")),
                                                          TreeMaxHeightClassAK<=1~"HgtClass2",
                                                          TreeMaxHeightClassAK<=2~"HgtClass3",
                                                          TreeMaxHeightClassAK<=4~"HgtClass4",
                                                          TreeMaxHeightClassAK<=8~"HgtClass5",
                                                          TreeMaxHeightClassAK>8~"HgtClass6"),
                    TreeBasalArea = DBHcm^2 * 0.00007854)%>%
      dplyr::filter(!is.na(TreeMaxHeightClass), AgeClass != "Seedling")%>%
      dplyr::group_by(!!!level, TreeMaxHeightClass)%>%
      dplyr::summarise(Cnt = n(),
                       LiveCnt = sum(ifelse(TreeIndivLiveDead == "L", 1, 0)),
                       DeadCnt = sum(ifelse(TreeIndivLiveDead == "D", 1, 0)),
                       LiveBasalSum = sum(ifelse(TreeIndivLiveDead == "L", TreeBasalArea, 0)),
                       DeadBasalSum = sum(ifelse(TreeIndivLiveDead == "D", TreeBasalArea, 0)))%>%
      tidyr::pivot_wider(id_cols = all_of(level_colnames),
                         names_from = TreeMaxHeightClass, names_glue = "WS_TreeMax{TreeMaxHeightClass}_{.value}", names_sort = T,
                         values_from = c(Cnt, LiveCnt, DeadCnt, LiveBasalSum, DeadBasalSum))%>%
      dplyr::left_join(., quadcount, by = c(level_colnames[!level_colnames%in%c("RiparianWoodySpecies", "UnknownCodeKey")]))%>%
      dplyr::mutate(WS_Tree_Cnt = rowSums(dplyr::across(dplyr::ends_with("_Cnt")), na.rm = T),
                    #Basal area calcs
                    WS_TreeBasalAreaPerHectareLive = round(rowSums(dplyr::across(dplyr::ends_with("_LiveBasalSum")), na.rm = T)/(nquads *  0.0002),2),
                    WS_TreeBasalAreaPerHectareDead = round(
                      rowSums(dplyr::across(dplyr::ends_with("_DeadBasalSum")), na.rm = T)/(nquads *  0.0002),2),
                    #stem density calcs
                    WS_TreeStemDensityLive = round(
                      rowSums(dplyr::across(dplyr::ends_with("_LiveCnt")), na.rm = T)/(nquads *  0.0002),2),
                    WS_TreeStemDensityDead = round(
                      rowSums(dplyr::across(dplyr::ends_with("_DeadCnt")), na.rm = T)/(nquads *  0.0002),2),)%>%
      dplyr::select(-c(matches("_LiveCnt$|_DeadCnt$|_LiveBasalSum$|_DeadBasalSum$"), nquads))
  } else {
    message("No tree table was provided. Tree metrics will be excluded from output table. ")}

  #Next calculate age class totals for non rhizomatous species.
  ageclass_sum <- woody_tall%>%
    #bind tree species rows to add to age class counts
    {if(!is.null(tree_tall)) dplyr::bind_rows(., ageclass_trees) else .}%>%
    #join to master species list to remove any species that are not riparian.
    dplyr::left_join(.,
                     masterspecieslist%>%dplyr::select(Symbol, RipWoodList),
                     by = c("RiparianWoodySpecies" = "Symbol"))%>%
    dplyr::filter(OverhangingOrRooted == "Rooted-in", RipWoodList == "Y")%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(WS_Rhizomatous_Cnt = sum(ifelse(GrowthHabit == "Rhizomatous or Dwarf Shrub", 1, 0)),
                     WS_Seedling_Cnt = sum(SeedlingCount, na.rm = T),
                     WS_Young_Cnt = sum(YoungCount, na.rm = T),
                     WS_Mature_Cnt = sum(MatureCount, na.rm = T),
                     WS_Dead_Cnt = sum(DeadCount, na.rm = T))%>%
    dplyr::rowwise()%>%
    dplyr::mutate(WS_Seedling_Pct = round(WS_Seedling_Cnt/sum(WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, na.rm = T)*100, digits = 2),
                  WS_Young_Pct = round(WS_Young_Cnt/sum(WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, na.rm = T)*100, digits = 2),
                  WS_Mature_Pct = round(WS_Mature_Cnt/sum(WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, na.rm = T)*100, digits = 2))

  #Calculate percent of quadrats with riparian woody rhizomatous species
  rhiz_byquad <- woody_tall%>%
    dplyr::left_join(.,
                     masterspecieslist%>%dplyr::select(Symbol, RipWoodList),
                     by = c("RiparianWoodySpecies" = "Symbol"))%>%
    dplyr::filter(OverhangingOrRooted == "Rooted-in", GrowthHabit == "Rhizomatous or Dwarf Shrub", RipWoodList == "Y")%>%
    dplyr::group_by(!!!level)%>%
    #for each group, count the number of unique points where rhizomatous species were measured.
    summarize(n = dplyr::n_distinct(!!!level, LineKey, PointNbr))%>%
    dplyr::left_join(.,
                     quadcount,
                     by = c(level_colnames[!level_colnames%in%c("RiparianWoodySpecies", "UnknownCodeKey")]))%>%
    dplyr::mutate(WS_Rhizomatous_PctQdrts = round(n/nquads * 100, digits = 2))%>%
    dplyr::select(!!!level, WS_Rhizomatous_PctQdrts)

  #Height Class counts
  #Counts by individual height classes and all height classes
  woodyheightclass_byquad <- woody_tall%>%
    dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
    dplyr::mutate(HeightClass = as.numeric(stringr::str_extract(HeightClass, "[:digit:]")))%>%
    dplyr::distinct(!!!level, LineKey, PointNbr, HeightClass)%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(HeightClass0 = sum(ifelse(HeightClass == 0, 1, 0)),
                     HeightClass1 = sum(ifelse(HeightClass == 1, 1, 0)),
                     HeightClass2 = sum(ifelse(HeightClass == 2, 1, 0)),
                     HeightClass3 = sum(ifelse(HeightClass == 3, 1, 0)),
                     HeightClass4 = sum(ifelse(HeightClass == 4, 1, 0)),
                     HeightClass5 = sum(ifelse(HeightClass == 5, 1, 0)),
                     HeightClass6 = sum(ifelse(HeightClass == 6, 1, 0)),
                     #count unique line+pointnbr combinations per group
                     HeightClassAll = dplyr::n_distinct(LineKey, PointNbr))%>%
    dplyr::full_join(.,
                     quadcount,
                     by = c(level_colnames[!level_colnames%in%c("RiparianWoodySpecies", "UnknownCodeKey")]))%>%
    {if(by_species) dplyr::filter(., !is.na(RiparianWoodySpecies)) else .}%>%
    dplyr::mutate(WS_HgtClass0_PctQdrts = round(HeightClass0/nquads * 100, digits = 2),
                  WS_HgtClass1_PctQdrts = round(HeightClass1/nquads * 100, digits = 2),
                  WS_HgtClass2_PctQdrts = round(HeightClass2/nquads * 100, digits = 2),
                  WS_HgtClass3_PctQdrts = round(HeightClass3/nquads * 100, digits = 2),
                  WS_HgtClass4_PctQdrts = round(HeightClass4/nquads * 100, digits = 2),
                  WS_HgtClass5_PctQdrts = round(HeightClass5/nquads * 100, digits = 2),
                  WS_HgtClass6_PctQdrts = round(HeightClass6/nquads * 100, digits = 2),
                  WS_WoodySpp_PctQdrts = round(HeightClassAll/nquads * 100, digits = 2))%>%
    dplyr::select(!!!level,
                  dplyr::starts_with("WS_"))%>%
    dplyr::mutate(., dplyr::across(starts_with("WS_HgtClass"), ~tidyr::replace_na(., 0)))

  #Pull the dominant height class and total count of measurements.
  allwoodyheight <- woody_tall%>%
    dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
    dplyr::mutate(HeightClass = as.numeric(stringr::str_extract(HeightClass, "[:digit:]")))%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(WS_DominantHgtClass = paste("Height Class ",
                                                 names(which.max(table(HeightClass))),
                                                 case_when(names(which.max(table(HeightClass)))==0~" (0.0 - 0.2 m)",
                                                           names(which.max(table(HeightClass)))==1~" (0.2 - 0.5 m)",
                                                           names(which.max(table(HeightClass)))==2~" (>0.5 - 1.0 m)",
                                                           names(which.max(table(HeightClass)))==3~" (>1.0 - 2.0 m)",
                                                           names(which.max(table(HeightClass)))==4~" (>2.0 - 4.0 m)",
                                                           names(which.max(table(HeightClass)))==5~" (>4.0 - 8.0 m)",
                                                           names(which.max(table(HeightClass)))==6~" (>8.0 m)",
                                                           TRUE~""),
                                                 sep = ""),
                     WS_WoodyHgtClass_Cnt = n())

  #Join all the woody structure metrics
  WoodyStructureMetrics <- woodyheightclass_byquad%>%
    dplyr::left_join(., allwoodyheight, by = level_colnames)%>%
    dplyr::left_join(., ageclass_sum, by = level_colnames)%>%
    dplyr::left_join(., rhiz_byquad, by = level_colnames)%>%
    {if(!is.null(tree_tall)) dplyr::left_join(., treemetrics, by = level_colnames) else .}%>%
    {if(by_species) dplyr::rename(., Species = RiparianWoodySpecies) else .} %>%
    dplyr::relocate(c(WS_Rhizomatous_Cnt, WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, WS_Dead_Cnt,
                      WS_Rhizomatous_PctQdrts, WS_Seedling_Pct, WS_Young_Pct, WS_Mature_Pct),
                    .after = WS_WoodyHgtClass_Cnt)%>%
  #Fill in a 0 where no measurements were found. We've already filtered only to the records where woody structure was measured, so na's represent plots with no woody observations.
  dplyr::mutate(dplyr::across(dplyr::ends_with(c("Cnt","PctQdrts")), ~tidyr::replace_na(., 0)))

  return(WoodyStructureMetrics)
}

#' @export SGConifer_metrics
#' @rdname woodystructure_use_metrics
SGConifer_metrics <- function(tree_tall, masterspecieslist, unit = "by_plot"){

  #allow metrics to be calculated at the plot level, line level, or species level.
  level <- rlang::quos(PlotID, EvaluationID)
  level_colnames <- c("PlotID", "EvaluationID")
  if(unit == "by_line"){
    level <- c(level, rlang::quos(LineKey))
    level_colnames <- c(level_colnames, "LineKey")
  }

  sgconifermetrics <- tree_tall%>%
    dplyr::left_join(.,
                     masterspecieslist%>%dplyr::select(Symbol, Species, SG_Group),
                     by = c("TreeSpecies"="Symbol"))%>%
    dplyr::filter(!is.na(TreeSpecies), SG_Group == "Conifer")%>%
    dplyr::mutate(RiparianWoodySpecies = TreeSpecies,
                  UnknownCodeKey = ifelse(Species %in% c(NA, ""), TreeUnknownCodeKey, NA),
                  TreeMaxHeightClass = dplyr::case_when(stringr::str_detect(TreeMaxHeightClass, "^Height Class")~stringr::str_replace_all(TreeMaxHeightClass, c(" "="", "Height" = "Hgt")),
                                                        TreeMaxHeightClassAK<=1~"HgtClass2",
                                                        TreeMaxHeightClassAK<=2~"HgtClass3",
                                                        TreeMaxHeightClassAK<=4~"HgtClass4",
                                                        TreeMaxHeightClassAK<=8~"HgtClass5",
                                                        TreeMaxHeightClassAK>8~"HgtClass6"))%>%
    dplyr::filter(!is.na(RiparianWoodySpecies),
                  SG_Group == "Conifer",
                  !is.na(TreeMaxHeightClass),
                  TreeMaxHeightClass %in%c("HgtClass4", "HgtClass5", "HgtClass6"))%>%
    dplyr::group_by(!!!level, TreeMaxHeightClass)%>%
    dplyr::summarise(Cnt = n())%>%
    tidyr::pivot_wider(id_cols = all_of(level_colnames),
                       names_from = TreeMaxHeightClass, names_glue = "WS_Max{TreeMaxHeightClass}SGConifer_{.value}", names_sort = T,
                       values_from = c(Cnt))

  return(sgconifermetrics)
}

#' @export hummocks_metrics
#' @rdname woodystructure_use_metrics
hummocks_metrics <- function(hummocks, unit = "by_plot"){

  if (unit == "by_line") {
    level <- rlang::quos(PlotID, EvaluationID, LineKey)
  } else if (unit == "by_plot") {
    level <- rlang::quos(PlotID, EvaluationID)
  } else {
    stop("Incorrect unit. Height metrics can only calculated by_plot or by_line. ")
  }

  nohummockplots <- hummocks%>%
    dplyr::filter(HummocksPresentLine == "No")%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::count()%>%
    dplyr::filter(n==3)%>%
    dplyr::mutate(H_Hummock_Cnt = 0,
                  H_Hummock_Pct = 0)%>%
    dplyr::select(-n)

  hummocksmetrics <- hummocks%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(H_Hummock_Cnt = sum(ifelse(HummocksPresentLine=="Yes", 1, 0)),
              H_Hummock_Pct = round(sum(Width,na.rm = T)/7500*100, digits = 2),
              H_HummockHgt_Avg = ifelse(H_Hummock_Cnt > 0, round(mean(Height, na.rm = T), digits = 2), NA),
              H_HummockWidth_Avg = ifelse(H_Hummock_Cnt > 0, round(mean(Width, na.rm = T), digits = 2), NA),
              H_HummockTroughWidth_Avg = ifelse(H_Hummock_Cnt > 0, round((7500 - sum(Width,na.rm = T))/ H_Hummock_Cnt, digits = 2), NA),
              H_HummockSlope_Avg = ifelse(H_Hummock_Cnt > 0, round(mean(SlopeClass, na.rm = T), digits = 2), NA),
              H_HummockVegCover_Avg = ifelse(H_Hummock_Cnt > 0, round(mean(VegCover, na.rm = T), digits = 2), NA))%>%
    bind_rows(.,
              nohummockplots)

  return(hummocksmetrics)
}

