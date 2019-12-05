# The text of this file provides the R code for analyses in the text,
# tables, and figures of the “Enough evidence and other endings” paper.

# Files and data for this project are at GitHub:
#  https://github.com/hildabast/enough-evidence

# There are 9 data files: S1_File_Meta-data.docx includes a guide
# to the files and their meta-data

# Two packages are used in the following code:

library (tidyverse)
library (reshape2)

#Methods text: level of assessment of reasons for reviews being declared stable reviews

#Import S5_File

# Proportions of stable reviews by level of author assessment

stable <- S5_File

stable %>%
  group_by(assessed) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 1))

#Figure 1: The proportion of stable reviews

#Data reproduced here is also contained in S3_File

df <- data.frame(
  Reviews = rep(c("Stable", "Normal"), each = 2),
  Year = rep(c("2013", "2019"), 2),
  Number = c(180, 507, 4957, 7138))

jpeg('figures/fig1.jpeg')

p <- ggplot(df, aes(x= Year, y= Number))+
  geom_col(aes(fill = Reviews), position = position_stack(reverse = TRUE), width = 0.7)+
  geom_area(aes(fill = Reviews))+
  scale_fill_discrete(breaks = c("Stable", "Normal"))

p +theme_bw()+annotate("text", label = "bw()",
                       col="black", size=4)

dev.off()

#Table 1 and related text: stable reviews across CRGs

# Number of stable reviews in each CRG

# Import S5_File

Stable <- S5_File

#Import S6_File

AllCRGs <- S6_File

#Create file combining CRGs with stable reviews with all CRGs

WithStable <- Stable %>%
  group_by(crg)%>%
  summarise(n=n())

StableXcrg <- left_join(AllCRGs,WithStable,by="crg")
StableXcrg2 <- StableXcrg %>%
  mutate(n = replace_na(n, 0))

# Range, median and IQR of stable reviews per CRG

summary(StableXcrg2)

#CRGs with more than 20 stable reviews

StableXcrg2 %>%
  filter(n>20)%>%
  summarise(n=n())%>%
  mutate(proportion = round(100 * n/sum(53), 1))
view(StableXcrg2)

#CRGs with 0 stable reviews

StableXcrg2 %>%
  filter(n==0)%>%
  summarise(n=n())%>%
  mutate(proportion = round(100 * n/sum(53), 1))

#CRGs with 1-10 stable reviews

StableXcrg2 %>%
  filter(n>0,n<11)%>%
  summarise(n=n())%>%
  mutate(proportion = round(100 * n/sum(53), 1))

#CRGs with 11-20 stable reviews

StableXcrg2 %>%
  filter(n>10,n<21)%>%
  summarise(n=n())%>%
  mutate(proportion = round(100 * n/sum(53), 1))

#Stable reviews in CRGs with more than 20

Over20 <- StableXcrg2 %>%
  filter(n>20)
summary(Over20)
RevsOver20 <- Over20$n
sum(RevsOver20)
n <- sum(RevsOver20)
round(100 * n/sum(507), 1)

#Stable reviews in CRGs with 1-10

OneToTen <- StableXcrg2 %>%
  filter(n>0,n<11)
RevsOneToTen <- OneToTen$n  
sum(RevsOneToTen)
n <- sum(RevsOneToTen)
round(100 * n/sum(507), 1)

#Stable reviews in CRGs with 11-20

Eleven <- StableXcrg2 %>%
  filter(n>10,n<21)
RevsEleven <- Eleven$n  
sum(RevsEleven)
n <- sum(RevsEleven)
round(100 * n/sum(507), 1)

#Stable reviews in top 3 CRGs

Top3 <- Over20 %>%
  filter(n>39)
RevsTop3 <- Top3$n
sum(RevsTop3)
n <- sum(RevsTop3)
round(100 * n/sum(507), 1)

#Stable reviews in top CRG

219/507

#Figure 2 diagram of 2013 stable reviews in 2019 and text

# Number of non-withdrawn Cochrane reviews in 2013 is in S3_File

# Total number of reviews stable in both 2013 and 2019

# Import S4_File

AllStable <- S4_File

# Reviews stable in 2013
AllStable2013 <- AllStable%>%
  filter(stable_2013 == "y")

# 2013 stable reviews withdrawn by 2019

Withdrawn <- AllStable2013%>%
  filter(other_status_2019 == "Withdrawn") %>%
  summarise(n = n())

head(Withdrawn)

# 2013 stable reviews no longer stable in 2019

NoLongerStable <- AllStable2013%>%
  filter(other_status_2019 == "Normal")%>%
  summarise(n = n())

head(NoLongerStable)

# 2013 stable reviews still stable in 2019

StillStable <- AllStable2013%>%
  filter(other_status_2019 !="Normal")%>%
  filter(other_status_2019 !="Withdrawn")%>%
  summarise(n = n())

head(StillStable)

# Summary data for 2019 status of reviews stable in 2013

df <- data.frame(
  Reviews = rep(c("Withdrawn", "No longer stable", "Still stable")),
  Number = c(5, 16, 159))

head(df)

n <- df %>%
  select(Number)

round(100 * n/sum(180), 1)


#Table 2 formerly stable reviews reverted to normal status

#Import S7_File

formerly <- S7_File

# Number and percentage of formerly stable reviews reverted to normal status by inclusion of new studies

NonWithdrawnNormal <- formerly%>%
  filter(events_category != "Withdrawal")%>%
  filter(events_category != "Stable")%>%
  group_by(new_included_studies)%>%
  summarise(n = n())%>%
  mutate(proportion = round(100 * n/sum(16), 1))

head(NonWithdrawnNormal)

# Row 1 of Table 2

Row1 <- formerly%>%
  filter(new_included_studies == "no")%>%
  filter(events_category == "Unrenewed")%>%
  summarise(n = n())%>%
  mutate(proportion = round(100 * n/sum(16), 1))

head(Row1)

# Row 2 of Table 2

Row2 <- formerly%>%
  filter(new_included_studies == "yes")%>%
  filter(events_category == "Same conclusion")%>%
  summarise(n = n())%>%
  mutate(proportion = round(100 * n/sum(16), 1))

head(Row2)

# Row 3 of Table 2

Row3 <- formerly%>%
  filter(new_included_studies == "yes")%>%
  filter(events_category == "Changed conclusion")%>%
  summarise(n = n())%>%
  mutate(proportion = round(100 * n/sum(16), 1))

head(Row3)

# Row 4 of Table 2

Row4 <- formerly%>%
  filter(events_category == "Criticism")%>%
  summarise(n = n())%>%
  mutate(proportion = round(100 * n/sum(16), 1))

head(Row4)

# Totals for Table 2

df <- data.frame(
  Reviews = rep(c("Row 1", "Row 2", "Row 3", "Row 4")),
  Number = c(10, 3, 2, 1))

head(df)

n <- df %>%
  select(Number)%>%
  sum()

head(n)

round(100 * n/sum(16), 1)

head(n)

# Table 3 – reasons for declaring reviews stable


# Import S5_File

stable <- S5_File

# All stable reviews where reason reported

stable_all <- stable %>%
  filter(reason != "U") %>%
  select(reason)
# Table 3 results

stable_all %>%
  group_by(reason) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(rel.freq = round(100 * n/sum(n), 1))

# Most common reasons

df <- data.frame(
  Reason = rep(c("B", "D", "E")),
  Number = c(147, 85, 99))

head(df)

n <- df %>%
  select(Number)%>%
  sum()

head(n)

round(100 * n/sum(505), 1)

# Figure 4 comparison of reasons with and without one CRG

# Import S5_File

stable <- S5_File

# All reviews with known stable status

stable_all <- stable %>%
  filter(reason != "U") %>%
  select(reason)

comp_first <- stable_all %>%
  group_by(reason) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 1))


# Reviews with known stable status without one CRG

stable_minus <- stable %>%
  filter(reason != "U") %>%
  filter(crg != "Pain, Palliative and Supportive Care Group") %>%
  select(reason)

comp_second <- stable_minus %>%
  group_by(reason) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 1))

reasoncompared <- bind_cols(list(comp_first, comp_second))

# Visualisation of comparison

df <-select(reasoncompared, c(reason, rel.freq, rel.freq1)) 

head(df)

df1 <- data.frame(c(df$reason), c(df$rel.freq), c(df$rel.freq1))

colnames(df1) <- c("Reasons", "All", "Without one CRG")

df2 <- melt(df1, id="Reasons")

jpeg('figures/fig4.jpeg')

ggplot()+
  geom_bar(data = df2, aes(x=Reasons, y=value, fill=variable), position = "dodge", stat = "identity")+
  theme_bw()+annotate("text", label = "bw()",
                      col="black", size=4)+
  ylab("Percent")+
  labs(fill="Stable reviews")+
  scale_fill_manual(values = c("blue", "orange"))

dev.off()

# Reviews with firm conclusions – text

# Proportion of non-withdrawn reviews in 2019 that were stable

39/7645

# Reviews with firm conclusions by CRG

# Import S8_File

Conclusive <-S8_File

Conclusive %>%
  group_by(crg) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 1))

# Proportion of reviews with firm conclusion of benefit

Conclusive %>%
  group_by(concluded_effect) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 1))

# Number of reviews with firm conclusions, recommendations for further research on the subject of the firm conclusion(s)

FutureResearch <- Conclusive %>%
  group_by(research_needed)%>%
  summarise(n = n())%>%
  mutate(rel.freq = round(100 * n/39, 1))

head(FutureResearch)

# Number of reviews with firm conclusions, recommending no further research on the subject of the firm conclusion(s), by whether or not research is needed on other questions

OtherQs <- Conclusive %>%
  filter(research_needed == "no")%>%
  group_by(if_no_other_qs)%>%
  summarise(n = n())%>%
  
  head(OtherQs)

# Proportion of reviews with firm conclusion reporting using an analytic method

Conclusive %>%
  group_by(analytic_method) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = round(100 * n/sum(n), 1))

# Table 4 – reviews with firm conclusions with summary of findings (sof) table

# Number by firm conclusion’s concluded effect

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  group_by(concluded_effect)%>%
  summarise(n = n())
head(sof)

# Row 1, columns 2 and 3 (further research)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "Benefit")%>%
  group_by(research_needed)%>%
  summarise(n = n())

head(sof)

# Row 2, columns 2 and 3 (further research)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "Some evidence of benefit")%>%
  group_by(research_needed)%>%
  summarise(n = n())

head(sof)

# Row 3, columns 2 and 3 (further research)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "Some evidence of benefit, adverse effects")%>%
  group_by(research_needed)%>%
  summarise(n = n())

head(sof)

# Row 4, columns 2 and 3 (further research)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "No evidence of superiority")%>%
  group_by(research_needed)%>%
  summarise(n = n())

head(sof)

# Row 5, columns 2 and 3 (further research)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "No evidence of benefit, adverse effects")%>%
  group_by(research_needed)%>%
  summarise(n = n())

head(sof)

# Row 1, columns 4 and 5 (quality of evidence)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "Benefit")%>%
  group_by(evidence_rating)%>%
  summarise(n = n())

head(sof)

# Row 2, columns 4 and 5 (quality of evidence)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "Some evidence of benefit")%>%
  group_by(evidence_rating)%>%
  summarise(n = n())

head(sof)

# Row 3, columns 4 and 5 (quality of evidence)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "Some evidence of benefit, adverse effects")%>%
  group_by(evidence_rating)%>%
  summarise(n = n())

head(sof)

# Row 4, columns 4 and 5 (quality of evidence)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "No evidence of superiority")%>%
  group_by(evidence_rating)%>%
  summarise(n = n())

head(sof)

# Row 5, columns 4 and 5 (quality of evidence)

sof <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(concluded_effect == "No evidence of benefit, adverse effects")%>%
  group_by(evidence_rating)%>%
  summarise(n = n())

head(sof)

# Number of reviews with uncontradictory results

Uncontradictory <- Conclusive %>%
  filter(sof == "yes")%>%
  filter(research_needed == "no")%>%
  group_by(evidence_rating)%>%
  summarise(n = n())%>%
  mutate(rel.freq = round(100 * n/(14), 1))

head(Uncontradictory)

# Number of reviews without sof table

NoSOF <- Conclusive %>%
  filter(sof == "no")%>%
  summarise(n = n())

head(NoSOF)

# Number of reviews without sof table, by evidence rating

NoSOF1 <- Conclusive %>%
  filter(sof == "no")%>%
  group_by(evidence_rating)%>%
  summarise(n = n())

head(NoSOF1)

# Number of reviews without sof table, by recommendation on future research

NoSOF2 <- Conclusive %>%
  filter(sof == "no")%>%
  group_by(research_needed)%>%
  summarise(n = n())

head(NoSOF2)

# Use of formal analytic methods in Cochrane protocols

# Import S9_File

Protocols <- S9_File

# Number of protocols using analytic methods

Protocols %>%
  group_by(analytic_method)%>%
  summarise(n = n())

# Proportion of all Cochrane protocols using analytic methods

Protocols %>%
  filter(analytic_method == "Yes")%>%
  summarise(n = n())%>%
  mutate(rel.freq = round(100 * n/(2415), 1))

# Number of CRGs with protocols using analytic methods, and proportion of protocols per CRG

ByCRG <- Protocols %>%
  filter(analytic_method == "Yes")%>%
  group_by(crg)%>%
  summarise(n = n())%>%
  mutate(rel.freq = round(100 * n/sum(n), 1))

summary(ByCRG)

# Percentage of CRGs with protocols using analytic methods

19/53

# Table 5 – type and proportion of analytic methods used

Type <- Protocols %>%
  filter(analytic_method == "Yes")%>%
  group_by(type)%>%
  summarise(n = n())%>%
  mutate(rel.freq = round(100 * n/sum(n), 1))%>%
  mutate(rel.freq = round(100 * n/(2415), 1))

head(Type)

# Percentage of use of TSA by one CRG

79/107

# To prepare this compendium's description

library(holepunch)

write_compendium_description()

# Ends


