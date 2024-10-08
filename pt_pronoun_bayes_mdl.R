library(readxl)

df <- read_excel("quest_EP_BP_pronouns.xlsx")

df$response <- ifelse (df$answer == 0, "object", "subject" )

prob_m_c <- function (subj, obj) { 
 subject = subj/100
 object = obj/100
 return(c(subject, object))}

subject<- prob_m_c(50,50)[1]
object<- prob_m_c(50,50)[2]

prob_2 <- function (dframe, column, covert_subj, overt_subj, covert_obj, overt_obj) {
  vec <- dframe[,column]
  dframe$Prob_F_MrCq <- ifelse (vec == "object", subject*(overt_subj/covert_obj),
                                object*(covert_subj/overt_obj))
  return(dframe)
}
