#--------------------------
## Generate Pausibility score (ENA Software score)
#--------------------------

calcul_quality_score <- function(read_lines_file){
  ##Select the part in the as file where the data is
  pos <- grep("mdy", read_lines_file, ignore.cas=TRUE)
  if(length(pos) == 0){
    pos <- grep("dmy", read_lines_file, ignore.cas=TRUE)
  }
  pos2 <- grep("\\Planning:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
  df <- read_lines_file[(pos+1):(pos2-1)]
  df<- strsplit(df, split = "\t")
  df <- lapply(df, `length<-`, max(lengths(df)))
  df <- as.data.frame(do.call(rbind, df))
  names(df) <- df[1,]
  df <- df[-1,]
  df <- df[,c("SURVDATE", "CLUSTER", "TEAM","ID" , "HH" , "SEX", "BIRTHDAT", "MONTHS",
              "WEIGHT","HEIGHT","EDEMA","MUAC","WAZ" ,"HAZ" ,"WHZ")]
  df[, c("MONTHS","WEIGHT","HEIGHT","MUAC","WAZ" ,"HAZ" ,"WHZ", 'CLUSTER')] <- sapply(df[, c("MONTHS","WEIGHT","HEIGHT","MUAC","WAZ" ,"HAZ" ,"WHZ", 'CLUSTER')], function(x) gsub(",", ".", x))
  df[, c("MONTHS","WEIGHT","HEIGHT","MUAC","WAZ" ,"HAZ" ,"WHZ", 'CLUSTER')] <- sapply(df[, c("MONTHS","WEIGHT","HEIGHT","MUAC","WAZ" ,"HAZ" ,"WHZ", 'CLUSTER')], as.numeric)
  df <- df |> tidyr::drop_na('WHZ')
  df <- df |> tidyr::drop_na('MONTHS')
  df <- df |> tidyr::drop_na('CLUSTER')
  if(nrow(df) == 0){
    return(NA)
  }
  if(length(df$CLUSTER) == 0){
    df$CLUSTER <- 1
  }else{
    for(ind in 1:length(df$CLUSTER)){
      if(all(is.na(df$CLUSTER))){
        df$CLUSTER <- 1
      }else{
        if(is.na(df$CLUSTER[ind])){
          if(ind != 1 & is.na(df$CLUSTER[ind-1]) == FALSE){
            df$CLUSTER[ind] <- df$CLUSTER[ind-1]
          }else if(is.na(df$CLUSTER[ind+1]) == FALSE & ind != length(df$CLUSTER)){
            df$CLUSTER[ind] <- df$CLUSTER[ind+1]
          }
        }
      }

    }
  }

  mean_WHZ <- mean(df$WHZ)
  df['flag_WHZ'] <- ifelse(df$WHZ > 3 +mean_WHZ | df$WHZ <(-3) +mean_WHZ, 1, 0)
  flagged_data <- sum(df$flag_WHZ)/length(df$flag_WHZ)*100
  if(is.na(flagged_data)){
    flagged_data <- 0
  }

  report_gam <- sum(ifelse(df$WHZ < -2, 1, 0))/length(df$WHZ)
  report_sam <- sum(ifelse(df$WHZ < -3, 1, 0))/length(df$WHZ)

  df$SEX <- ifelse(df$SEX == 'm', 1, 0)
  overall_sex <- chisq.test(c(sum(df$SEX), length(df$SEX)-sum(df$SEX)))$p.value

  df$AGE_u30<- ifelse(df$MONTHS >= 6 & df$MONTHS <= 29, 1, 0)
  df$AGE_o30 <- ifelse(df$MONTHS > 29 & df$MONTHS <= 59, 1, 0)
  overall_age <- round(prop.test(sum(df$AGE_u30), length(df$AGE_u30), p = 0.459495)$p.value,3)


  skew <- round( nipnTK::skewKurt(df[df$flag_WHZ == 0,]$WHZ)$s, 2)
  kur <- round( nipnTK::skewKurt(df[df$flag_WHZ == 0,]$WHZ)$k, 1)
  std_weight <- round(sd(df[df$flag_WHZ == 0, ]$WHZ),2)

  dps_height <- round( nipnTK::digitPreference(df$HEIGHT)$dps)
  dps_weight <- round( nipnTK::digitPreference(df$WEIGHT)$dps)
  if(all(is.na(df$MUAC))){dps_muac <- 0}else{dps_muac <- round( nipnTK::digitPreference(as.numeric(df$MUAC), digits = 0)$dps)}

  count <- c()
  for (var in unique(df$CLUSTER)){
    count <- append(count, sum(ifelse(df$CLUSTER == var & df$WHZ < -2 & df$flag_WHZ == 0, 1, 0)))
  }
  count[is.na(count)] <- 0

  if(length(count) == 1){
    poisson_score <- 1
  }else{
    poisson_score <- round(chisq.test(as.table(count))$p.value,3)
  }

  final_score <- 0
  if(flagged_data > 2.5 & flagged_data <= 5){
    final_score <- final_score + 5
  }else if(flagged_data > 5 & flagged_data <= 7.5){
    final_score <- final_score + 10
  }else if(flagged_data > 7.5){
    final_score <- final_score +20
  }

  if(overall_sex > 0.05 & overall_sex < 0.1){
    final_score <- final_score + 2
  }else if(overall_sex > 0.001 & overall_sex < 0.05){
    final_score <- final_score + 4
  }else if(overall_sex <= 0.001){
    final_score <- final_score + 10
  }


  if(overall_age > 0.05 & overall_age < 0.1){
    final_score <- final_score + 2
  }else if(overall_age >= 0.001 & overall_age < 0.05){
    final_score <- final_score + 4
  }else if(overall_age < 0.001){
    final_score <- final_score + 10
  }

  if(dps_weight >= 8 & dps_weight <= 12){
    final_score <- final_score + 2
  }else if(dps_weight >= 13 & dps_weight <= 20){
    final_score <- final_score + 4
  }else if(dps_weight > 20){
    final_score <- final_score + 10
  }

  if(dps_height >= 8 & dps_height <= 12){
    final_score <- final_score + 2
  }else if(dps_height >= 13 & dps_height <= 20){
    final_score <- final_score + 4
  }else if(dps_height > 20){
    final_score <- final_score + 10
  }

  if(dps_muac >= 8 & dps_muac <= 12){
    final_score <- final_score + 2
  }else if(dps_muac >= 13 & dps_muac <= 20){
    final_score <- final_score + 4
  }else if(dps_muac > 20){
    final_score <- final_score + 10
  }
  if(is.na(std_weight)){return(0)}
  if(std_weight >= 1.1 & std_weight <= 1.15){
    final_score <- final_score + 5
  }else if(std_weight > 1.15 & std_weight < 1.2){
    final_score <- final_score + 10
  }else if(std_weight >= 1.2){
    final_score <- final_score + 20
  }

  if((skew > -0.4 & skew <= -0.2) | (skew < 0.4 & skew > 0.2)){
    final_score <- final_score + 1
  }else if((skew > -0.6 & skew < -0.4) | (skew < 0.6 & skew > 0.4)){
    final_score <- final_score + 3
  }else if((skew < -0.6) | (skew > 0.6)){
    final_score <- final_score + 5
  }

  if((kur > -0.4 & kur <= -0.2) | (kur < 0.4 & kur >= 0.2)){
    final_score <- final_score + 1
  }else if((kur > -0.6 & kur <= -0.4) | (kur < 0.6 & kur >= 0.4)){
    final_score <- final_score + 3
  }else if((kur <= -0.6) | (kur >= 0.6)){
    final_score <- final_score + 5
  }

  if(poisson_score > 0.01 & poisson_score < 0.05){
    final_score <- final_score + 1
  }else if(poisson_score >= 0.001 & poisson_score < 0.01){
    final_score <- final_score + 3
  }else if(poisson_score < 0.001){
    final_score <- final_score + 5
  }

  return(1-0.01*final_score)

}
