# Decoding MODIS QA flags from here:
# https://stevemosher.wordpress.com/2012/12/05/modis-qc-bits/
QC_Data <- data.frame(Integer_Value = 0:255,
                      Bit7 = NA,
                      Bit6 = NA,
                      Bit5 = NA,
                      Bit4 = NA,
                      Bit3 = NA,
                      Bit2 = NA,
                      Bit1 = NA,
                      Bit0 = NA,
                      QA_word1 = NA,
                      QA_word2 = NA,
                      QA_word3 = NA,
                      QA_word4 = NA
                      )

for(i in QC_Data$Integer_Value){
  AsInt <- as.integer(intToBits(i)[1:8])
  QC_Data[i+1,2:9]<- AsInt[8:1]
}

QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==0] <- "LST GOOD"
QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==1] <- "LST Produced,Other Quality"
QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==0] <- "No Pixel,clouds"
QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==1] <- "No Pixel, Other QA"

QC_Data$QA_word2[QC_Data$Bit3 == 0 & QC_Data$Bit2==0] <- "Good Data"
QC_Data$QA_word2[QC_Data$Bit3 == 0 & QC_Data$Bit2==1] <- "Other Quality"
QC_Data$QA_word2[QC_Data$Bit3 == 1 & QC_Data$Bit2==0] <- "TBD"
QC_Data$QA_word2[QC_Data$Bit3 == 1 & QC_Data$Bit2==1] <- "TBD"

QC_Data$QA_word3[QC_Data$Bit5 == 0 & QC_Data$Bit4==0] <- "Emiss Error <= .01"
QC_Data$QA_word3[QC_Data$Bit5 == 0 & QC_Data$Bit4==1] <- "Emiss Err >.01 <=.02"
QC_Data$QA_word3[QC_Data$Bit5 == 1 & QC_Data$Bit4==0] <- "Emiss Err >.02 <=.04"
QC_Data$QA_word3[QC_Data$Bit5 == 1 & QC_Data$Bit4==1] <- "Emiss Err > .04"

QC_Data$QA_word4[QC_Data$Bit7 == 0 & QC_Data$Bit6==0] <- "LST Err <= 1"
QC_Data$QA_word4[QC_Data$Bit7 == 0 & QC_Data$Bit6==1] <- "LST Err > 2 LST Err <= 3"
QC_Data$QA_word4[QC_Data$Bit7 == 1 & QC_Data$Bit6==0] <- "LST Err > 1 LST Err <= 2"
QC_Data$QA_word4[QC_Data$Bit7 == 1 & QC_Data$Bit6==1] <- "LST Err > 4"

FINAL <- QC_Data[QC_Data$QA_word1=="LST GOOD" & !(QC_Data$QA_word4=="LST Err > 4" | QC_Data$QA_word4=="LST Err > 2 LST Err <= 3"),]
FINAL

# Right now, just ignoring the worst
flags.good <- QC_Data[(QC_Data$QA_word1=="LST GOOD" & QC_Data$QA_word2=="Good Data") |
                        (QC_Data$QA_word2=="Good Data" & !QC_Data$QA_word3 %in% c("Emiss Err > .04")) |
                           (QC_Data$QA_word2=="Good Data" & !QC_Data$QA_word3 %in% c("LST Err > 4")),"Integer_Value"]

flags.good <- QC_Data[QC_Data$QA_word2=="Good Data", "Integer_Value"]
