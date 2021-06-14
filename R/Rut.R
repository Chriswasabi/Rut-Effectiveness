require("tidyverse")

#' Get the preprocessing done
#' @export
#' @param database This is the full PMIS work history stored in the UT Database HUB
#' @param corrected_DFOS This is the database with the corrected DFOs found in the UT Database Hub
#' @param min_year This is the minimum year you want to analyze, we recommend 2018
#' @param max_year This is the maximum year you want to analyze, we recommend 2019
#' @param asphalt This parameter double checks that you are analyzing HMAs, by default it is true
pmis_preprocessing <- function(database, corrected_DFOS, min_year, max_year, asphalt=T)  {

  #Rename and select the variables of interest for ACP
  df <- database %>% select(FY=EFF_YEAR, hwy=ROUTE_NAME, distr=TX_DISTRICT_NUM_ID, county=TX_COUNTY_NBR,
                            dfof=OFFSET_FROM, dfot=OFFSET_TO, pvmt_type=TX_PVMNT_TYPE_DTL_RD_LIFE_CODE,
                            rutl=TX_ACP_RUT_LFT_WP_DPTH_MEAS,rutr=TX_ACP_RUT_RIT_WP_DPTH_MEAS,
                            cs=TX_CONDITION_SCORE,  iril=TX_IRI_LEFT_SCORE, irir=TX_IRI_RIGHT_SCORE,
                            lcr=TX_ACP_LONGITUDE_CRACKING_PCT)



  #Round the DFO's
  df <- df %>% mutate(dfof=round(dfof, digits = 1), dfot=round(dfot, digits = 1) )

  #Create the section length and Highway class variables
  df <- df %>% mutate(sec_len=dfot-dfof, HC=substr(hwy, start = 1, stop = 2))

  #Compute the network length per year
  netw_len <- df %>% group_by(FY) %>% filter(FY>=min_year & FY<=max_year) %>%
    summarise(hwy_mi=round(sum(sec_len), digits = 0))


  #Filter by years of interest. In this case PMIS 2017-2020
  df <- df %>% filter(FY >= min_year & FY<=max_year)

  #Filter by pvmt type, keep only ACP and remove composites and concrete pvmts

  if (asphalt==T) {
    df <- df %>% filter(pvmt_type>3)
  } else { stop("This algorithm only works on asphalt pavements")}


  #Create the length of highway variable and Filter hwys whose total length is less than 10 miles
  df <- df %>% group_by(hwy, FY) %>% mutate(hwy_len=sum(sec_len)) %>% ungroup() %>% filter(hwy_len>=10)

  #Filter entries whose sec length is 0 mi, (point measurement)
  df <- df %>%  filter(sec_len>0)

  #Add the corrected DFOF and DFOT
  t1 <- left_join(df, corrected_DFOS, by=c("hwy"="hwy", "dfof"="dfof_m","dfot"="dfot_m" ))

  return(t1)
}


#' Get the Rut work history
#' @export
#' @param database This is the output of the preprocessing function
#' @param max_year This is the maximum year you want to analyze, we recommend 2019
pmis_processing <- function(database, max_year) {
  #First arrange the db by hwy, dfof, dfot, year
  t1 <- database %>% arrange(hwy,UT_dfof, UT_dfot, FY)


  #Create a unique ID for every section based on hwy, DFO, district and county
  t1$sec_ID <- group_indices(t1,hwy,UT_dfof, UT_dfot, distr,county)

  #Compute the difference in performance measures year(i+1)-year(i)
  df <- t1 %>% group_by(sec_ID) %>% mutate( drutl=c(diff(rutl),NA), drutr=c(diff(rutr),NA), dlcr=c(diff(lcr),NA),
                                            diril=c(diff(iril),NA), dirir=c(diff(irir),NA), dcs=c(diff(cs),NA)) %>% ungroup()

  #Define the threshold value for each distress/performance measure (first "t" means threshold)
  trutl <- quantile(df$drutl, na.rm = T, probs = c(0.1)); trutr <- quantile(df$drutr, na.rm = T, probs = c(0.1))
  tcs <- quantile(df$dcs, na.rm = T, probs = c(0.9)); tiril <- quantile(df$diril, na.rm = T, probs = c(0.1)) ;
  tirir <- quantile(df$dirir, na.rm = T, probs = c(0.1));  tlcr <- quantile(df$dlcr, na.rm = T, probs = c(0.1))

  #Flag each section based on the difference in distress/performance measures from one year to the next
  df <- df %>% mutate(
    t1 = ifelse(dcs<=tcs, 1, 0), #Condition Score
    t2 = ifelse(diril<=tiril, 1, 0), #IRIL
    t3 = ifelse(dirir<=tirir, 1, 0), #IRIR
    t4 = ifelse(drutl<=trutl, 1.5, 0), #Rut Left
    t5 = ifelse(drutr<=trutr, 1.5, 0), #Rut Right
    t6 = ifelse(dlcr<=tlcr, 1, 0), #Rut Right
  )

  #Convert NA's into zeroes (fix)
  df$t1[is.na(df$t1)] <- 0
  df$t2[is.na(df$t2)] <- 0
  df$t3[is.na(df$t3)] <- 0
  df$t4[is.na(df$t4)] <- 0
  df$t5[is.na(df$t5)] <- 0
  df$t5[is.na(df$t6)] <- 0

  #Define the Flag score
  df <- df %>% mutate(flag_score=t1+t2+t3+t4+t5+t6)

  hist(df$flag_score)

  #Filter out any section whose flag score is less than the flag threshold
  tflag <- 2 ; df <- df %>%  filter(flag_score>=tflag)


  #Remove all unnecessary variables
  rm(trutl, trutr, tflag, tcs, tiril, tirir, tlcr)

  #Select only the variables that will be needed
  df1 <- df %>% select(FY,hwy,HC,distr,county,UT_dfof,UT_dfot,sec_len,hwy_len,
                       drutl, drutr, dcs, sec_ID, flag_score,
                       diril, dirir, dlcr) %>% arrange(FY,hwy,UT_dfof)

  #Find sections that are next to each other (1.5 miles apart) (Im still missing the final point)
  df1 <- df1 %>% mutate(t1 = ifelse((hwy == lead(hwy, 1) & abs(UT_dfof - lead(UT_dfof, 1)) < 1.6), 1, 0),
                        t2 = ifelse((hwy == lead(hwy, 2) & abs(UT_dfof - lead(UT_dfof, 2)) < 2.1), 1, 0),
                        t3 = ifelse((hwy == lead(hwy, 3) & abs(UT_dfof - lead(UT_dfof, 3)) < 2.6), 1, 0),
                        t4 = ifelse((hwy == lead(hwy, 4) & abs(UT_dfof - lead(UT_dfof, 4)) < 3.1), 1, 0),
                        t=t1 + t2 + t3 + t4 )

  #This variables will select those sections that contiguous to at least four other sections
  df1 <- df1 %>% mutate(v1 = ifelse((t==4), 1, 0),
                        v2 = ifelse(lag(v1,1)==1, 1, 0),
                        v3 = ifelse(lag(v2,1)==1, 1, 0),
                        v4 = ifelse(lag(v3,1)==1, 1, 0),
                        v5 = ifelse(lag(v4,1)==1, 1, 0),
                        v=v1 + v2 + v3 + v4 + v5
  )

  #Filter out sections that are not contiguous to four other sections
  df1 <- df1 %>% filter(v>0) %>% select(FY,hwy,HC,distr,county,UT_dfof,UT_dfot,sec_len,
                                        hwy_len, drutl, drutr, dcs, diril, dirir, dlcr, sec_ID)
  df1 <- df1 %>% filter(FY<max_year)

  #Create a project ID
  df2 <- df1
  proj_ID <- numeric(nrow(df2))
  k=1
  for (i in 1:nrow(df2)) {
    if (i==1) {
      proj_ID[i]=k
    } else { if (df2$hwy[i] == df2$hwy[i-1] & abs(df2$UT_dfof[i] - df2$UT_dfof[i-1]) < 1.6 ) {
      proj_ID[i]=k
    } else {
      k=k+1
      proj_ID[i]=k
    }
    }
  }

  #Assign the project ID
  df2$proj_ID <- proj_ID

  # Remove NA's
  df2$drutl[is.na(df2$drutl)] <- 0
  df2$drutr[is.na(df2$drutr)] <- 0
  df2$diril[is.na(df2$diril)] <- 0
  df2$dirir[is.na(df2$dirir)] <- 0
  df2$dlcr[is.na(df2$dlcr)] <- 0
  df2$dcs[is.na(df2$dcs)] <- 0

  options(digits = 2)

  df2 <- df2 %>% mutate( rutl=ifelse(drutl<(-0.25),1,ifelse(drutl>0.25,-1,round(drutl/(-0.25), 2))),
                         rutr=ifelse(drutr<(-0.25),1,ifelse(drutr>0.25,-1,round(drutr/(-0.25), 2))),
                         cs=ifelse(dcs<(-50),1,ifelse(dcs>50,-1,round(dcs/(-50)*.75,2))),
                         iril=ifelse(diril<(-50),1,ifelse(diril>50,-1,round(diril/(-50)*.75,2))),
                         irir=ifelse(dirir<(-50),1,ifelse(dirir>50,-1,round(dirir/(-50)*.75,2))),
                         lcr=ifelse(dlcr<(-60),1,ifelse(dlcr>60,-1,round(dlcr/(-60)*.75,2))),
                         improv = (rutl+rutr + cs +iril + irir + lcr)/6)

  #Compute project limits, project length

  df3 <- df2 %>% group_by(FY, hwy, proj_ID) %>% summarise(proj_len = max(UT_dfot)- min(UT_dfof),
                                                          proj_beg = min(UT_dfof),
                                                          proj_end = max(UT_dfot),
                                                          rutl_sum = sum(rutl)/n(),
                                                          rutr_sum = sum(rutr)/n(),
                                                          T_improv_sum= sum(improv)/n()) %>% ungroup()

  df3 <- df3 %>% mutate ( Work_type = ifelse(T_improv_sum> quantile(df3$T_improv_sum, probs = 0.93),"HR",
                                             ifelse(T_improv_sum> quantile(df3$T_improv_sum, probs = 0.88), "MR",
                                                    ifelse(T_improv_sum> quantile(df3$T_improv_sum, probs = 0.80,), "LR", "PM"))))

  return(df3)
}

#' Get the Rut work history and prepare for plotting
#' @export
#' @param database This is the output of the preprocessing function
#' @param workhistory This is the output of the processing function
treat_assing <- function(database, workhistory) {

  db.1 = database %>% filter(FY==2018)
  df <- workhistory %>% select(hwy,proj_beg,proj_end,Work_type)

  data <- left_join(db.1, df, by=c("hwy"="hwy"))
  data1 <- data %>% filter(UT_dfof>= proj_beg & UT_dfot<= proj_end) %>% unique() %>% select(hwy, UT_dfof, UT_dfot, Work_type)

  df1 <- left_join(db.1, data1, by=c("hwy"="hwy", "UT_dfof"="UT_dfof", "UT_dfot"="UT_dfot")) %>% distinct()

  df1$Work_type[is.na(df1$Work_type)] <- "DN"

  t <- database %>% filter(FY==2019) %>% select(hwy, UT_dfof, UT_dfot, rutl19 = rutl, rutr19 = rutr)
  t$UT_dfof <- na_interpolation(t$UT_dfof); t$UT_dfot <- na_interpolation(t$UT_dfot)
  t$rutl19[is.na(t$rutl19)] = 0 ; t$rutr19[is.na(t$rutr19)] = 0

  t12 <- inner_join(df1 ,t, by=c("hwy"="hwy", "UT_dfof"="UT_dfof", "UT_dfot"="UT_dfot") )


  return(t12)
}

#' Get the Rut work history and prepare for plotting
#' @export
#' @param database This is the output of the treat_assign function
#' @param workhistory This is the output of the processing function
plotter <- function(database, workhistory) {

  setwd("C:/Users/Owner/Desktop/Graphs")


  data <- db1 %>% select(proj_ID, hwy, proj_beg, proj_end, Work_type) %>% distinct()

  #Isolate the beginning of the project begin and end
  hwy_list <- data$hwy %>% unique()

  i=1

  for (i in 1:length(hwy_list)) {

    p <- database %>% filter(hwy == hwy_list[i])
    beg = data %>% filter(hwy == hwy_list[i]) %>% select(proj_beg)
    beg = beg$proj_beg
    end = data %>% filter(hwy == hwy_list[i]) %>% select(proj_end)
    end = end$proj_end
    mid = (end+beg)/2
    work = data %>% filter(hwy == hwy_list[i]) %>% select(Work_type)
    work = work$Work_type

    p1 = ggplot() + geom_rect(aes(xmin = beg, xmax = end ,ymin = rep(0,length(beg)), ymax = rep(Inf,length(beg))), fill ="blue", alpha=0.2) +
      geom_point(data = p, aes(y=rutl, x =UT_dfof), color="red") +
      geom_point(data = p, aes(y=rutl19, x =UT_dfof), color ="black") +
      geom_line(data = p, aes(y=rutl, x =UT_dfof), color="red") +
      geom_line(data = p, aes(y=rutl19, x =UT_dfof), color ="black") + scale_y_reverse() +
      ylab("Rutting (in.)") + xlab("Distance from Origin") +
      annotate("text", x=mid, y=rep(0.9, length(mid)), label= work)



    p2 = ggplot() + geom_rect(aes(xmin = beg, xmax = end ,ymin = rep(0,length(beg)), ymax = rep(Inf,length(beg))), fill ="blue", alpha=0.2) +
      geom_point(data = p, aes(y=rutr, x =UT_dfof), color="red") +
      geom_point(data = p, aes(y=rutr19, x =UT_dfof), color ="black") +
      geom_line(data = p, aes(y=rutr, x =UT_dfof), color="red") +
      geom_line(data = p, aes(y=rutr19, x =UT_dfof), color ="black") + scale_y_reverse() +
      ylab("Rutting (in.)") + xlab("Distance from Origin") +
      annotate("text", x=mid, y=rep(0.9, length(mid)), label= work)

    p3 = plot_grid(
      p1,p2, nrow=2,
      labels = c("Left Wheelpath", "Right Wheelpath"),
      label_size = 12,
      label_x = 0, label_y = 0,
      hjust = -0.5, vjust = -0.5)

    p3

    save_plot(filename = paste0("Plot ", i, ".jpg"), p3, ncol = 1, nrow = 1, base_height = 3.71*2,
              base_asp = 1.618)

    print(paste0("Graph ", i, " done"))

  }

}
