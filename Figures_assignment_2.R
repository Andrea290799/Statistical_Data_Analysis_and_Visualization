library(dplyr)
library(ggplot2) 
library(tidyr)

require(reshape) 

#### FUNCTIONS --------------------------------------------------------------

plot_heatmap = function(df, variable, title){
  # Function that plots a heatmap
  # args:
  ## df: the dataframe to use
  ## variable: the studied aspect
  ## title: the title of the graph
  
  # transform from string to numbers
  for (i in 1:length(df[,1])){
    for (j in 1:length(df[1,])){
      df[i,j] = as.numeric(df[i,j])
    }
  }
  
  #apropiate format to use ggplot
  m = melt(as.matrix(df))
  
  colnames(m) = c("States", variable, "Rate")
  
  ggplot(data = data.frame(m), aes(x = data.frame(m)[,2], y = States, fill= Rate)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue") +  
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title=element_text(size=20))+
    ylab("States")+
    xlab(variable)+
    ggtitle(title) +
    theme(plot.title = element_text(size=16))
}


plot_boxplot = function(df, variable, title){
  # Function that plots a boxplot
  # args:
  ## df: the dataframe to use
  ## variable: the studied aspect
  ## title: the title of the graph
  
  # transform from string to numbers
  for (i in 1:length(df[,1])){
    for (j in 1:length(df[1,])){
      df[i,j] = as.numeric(df[i,j])
    }
  }
  
  #apropiate format to use ggplot
  m = melt(as.matrix(df))
  
  colnames(m) = c("States", variable, "Rate")
  
  ggplot(data = data.frame(m), aes(x = data.frame(m)[,2], y = Rate,
                                   fill= data.frame(m)[,2])) +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title=element_text(size=20))+
    ylab("Rates (per 100.000)")+
    xlab(variable)+
    geom_boxplot()+scale_fill_brewer(palette="Pastel 2")+
    ggtitle(title) + theme(plot.title = element_text(size=16))
  
}

get_sex_matrices = function(data){
  
  #This function takes the sex data divided
  #in age groups and puts it together
  
  matrix = matrix(0,51,1)
  for (i in 1:length(data[,1])){
    matrix[i,1] = sum(data[i,])
  }
  
  rownames(matrix) = rownames(data)
  return(matrix)
}

# CODE --------------------------------------------------------------------

df = read.csv("cancer.csv", row.names = 1)

# The name of the states
states = rownames(df)
# The features in the dataset
columns = colnames(df)

# Ethnicities -------------------------------------------------------------


# we select the columns of interest
selected_data = df[,c(40,41,42,43,44,45,46,54,55,56,57,58,59,60,68,69,70,
                         71,72,73,74)]

cancer1 = selected_data[,c(1,2,3,4,5,6,7)] #breast
colnames(cancer1) = c("White", "White non hispanic", "Black", "Black non hispanic", 
                 "Asian", "Indigenous", "Hispanic")
cancer2 = selected_data[,c(8,9,10,11,12,13,14)] #colorectal
colnames(cancer2) = colnames(cancer1)
cancer3 = selected_data[,c(15,16,17,18,19,20,21)] #lung
colnames(cancer3) = colnames(cancer1)

### Bar diagram 

medias1 = c()
medias2 = c()
medias3 = c()

#we calculate the mean for each column in the matrices
for (j in 1:length(cancer1[1,])){
  medias1 = c(medias1, mean(cancer1[,j]))
  medias2 = c(medias2, mean(cancer2[,j]))
  medias3 = c(medias3, mean(cancer3[,j]))
}

medias = rbind(medias1, medias2, medias3)
rownames(medias) = c("Breast cancer", "Colorectal cancer", "Lung cancer")
colnames(medias) = c("White", "White non hispanic", "Black", "Black non hispanic", 
                   "Asian", "Indigenous", "Hispanic")


#we transform the matrix in a suitable format
m_medias = melt(medias)
colnames(m_medias) = c("Cancer", "Races", "Rate")

ggplot(data = data.frame(m_medias),
    mapping = aes(x = Races, y=Rate, fill = Cancer))+
  geom_bar(stat = "identity", position = 'dodge')+
  ylab("Rates (per 100.000)")+
  ggtitle("Cases of breast, colorectal and lung cancer") +
  theme(plot.title = element_text(size=16))+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title=element_text(size=14))

### Boxplot death

# Use the gather method to transform the columns of the ethnic groups into a new 
# column with the name Race and the value of those groups in a column named Population.Rate
# We store that information in the race_df
race_df<-df %>% gather(key='Race',value='Population.Rate',  
                       Rates.Race.White:Rates.Race.Indigenous)

# Select only the Race and Population.Rate columns (The ones for the plot)
race_df <- race_df[c("Race", "Population.Rate")]

# Convert the Race column values to factors
race_df$Race <- factor(race_df$Race, levels=c("Rates.Race.White", 
                                              "Rates.Race.White.non.Hispanic",  
                                              "Rates.Race.Black", 
                                              "Rates.Race.Asian",
                                              "Rates.Race.Indigenous"))

# The labels for the X-axis and the legend
race_labels = c("White","White non Hispanic","Black", "Asian","Indigenous")

# Create a boxplot with the ethnic group in the X axis and population death rate in the Y axis
ggplot(data=race_df, aes(x=Race,y=Population.Rate, fill=Race)) +
  geom_boxplot() +
  labs(title="Cancer death rates") +
  ylab("Rates (per 100.000)")+
  scale_x_discrete(labels=race_labels) +
  scale_fill_brewer(palette = 'PuBu', labels = race_labels)


### Cases heatmaps

plot_heatmap(df = cancer1, variable = "Race", title = "Breast cancer cases")
plot_heatmap(df = cancer2, variable = "Race", title = "Colorectal cancer cases")
plot_heatmap(df = cancer3, variable = "Race", title = "Lung cancer cases")

### Cases boxplots

plot_boxplot(df = cancer1, variable = "Race", title = "Breast cancer cases")
plot_boxplot(df = cancer2, variable = "Race", title = "Colorectal cancer cases")
plot_boxplot(df = cancer3, variable = "Race", title = "Lung cancer cases")




# Ages --------------------------------------------------------------------

data_age = df[,c(37,38,39,48,49,50,51,52,53,62,63,64,65,66,67)]

cancer1 = data_age[,c(1,2,3)]#breast
c1g1 = cancer1[,c(1)]#18-44
c1g2 = cancer1[,c(2)]#44-65
c1g3 = cancer1[,c(3)]#>64

# we sum up males and females in the next two cases
cancer2 = data_age[,c(4,5,6,7,8,9)]#colorectal
c2g1 = rowSums(cancer2[,c(1,2)])#18-44
c2g2 = rowSums(cancer2[,c(3,4)])#44-65
c2g3 = rowSums(cancer2[,c(5,6)])

cancer3 = data_age[,c(10,11,12,13,14,15)]#lung
c3g1 = rowSums(cancer3[,c(1,2)])#18-44
c3g2 = rowSums(cancer3[,c(3,4)])#44-65
c3g3 = rowSums(cancer3[,c(5,6)])#>64

cancer1_total = cbind(c1g1,c1g2,c1g3)
cancer2_total = cbind(c2g1,c2g2,c2g3)
cancer3_total = cbind(c3g1,c3g2,c3g3)

### Bar diagram

medias1 = c()
medias2 = c()
medias3 = c()

for (j in 1:length(cancer1_total[1,])){
  medias1 = c(medias1, mean(cancer1_total[,j]))
  medias2 = c(medias2, mean(cancer2_total[,j]))
  medias3 = c(medias3, mean(cancer3_total[,j]))
}


medias = rbind(medias1, medias2, medias3)
rownames(medias) = c("Breast cancer", "Colorectal cancer", "Lung cancer")
colnames(medias) = c("18-44", "44-65", ">64")

m_medias = melt(medias)

colnames(m_medias) = c("Cancer", "Age", "Rate")

m_medias$Age =  factor (m_medias$Age, levels = c("18-44", "44-65", ">64"))

ggplot(data = data.frame(m_medias),
       mapping = aes(x = Age, y=Rate,
                     fill = Cancer))+
  geom_bar(stat = "identity", position = 'dodge')+
  ylab("Rates (per 100.000)")+
  ggtitle("Cases of breast, colorectal and lung cancer") +
  theme(plot.title = element_text(size=16))+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title=element_text(size=14))

### Boxplot death

# Use the gather method to transform the columns of the age groups into a new column with the
# name Age and the value of those groups in a column named Population.Rate
# We store that information in the age_df
age_df<-df %>% gather(key='Age',value='Population.Rate',  Rates.Age...18:Rates.Age...64)

# Select only the Age and Population.Rate columns (The ones for the plot)
age_df <- age_df[c("Age", "Population.Rate")]

# Convert the Age column values to factors
age_df$Age <- factor(age_df$Age, levels=c("Rates.Age...18", "Rates.Age.18.45",  "Rates.Age.45.64", "Rates.Age...64"))

# To see if the factors are created correctly
levels(age_df$Age)

# The labels for the X-axis and the legend
age_labels = c("0-17","18-44","45-64",">65")

# Create a boxplot with the age group in the X axis and population death rate in the Y axis
ggplot(data=age_df, aes(x=Age,y=Population.Rate, fill=Age)) +
  geom_boxplot() +
  labs(title="Cancer death rates", x= "Age (years)", y="Population Rate (per 100.000)") +
  ylab("Rates (per 100.000)")+
  scale_x_discrete(labels=age_labels) + 
  scale_fill_brewer(palette="PuBu", labels = age_labels)

# BOXPLOT INCIDENCE COLORECTAL CANCER BY AGE GROUPS

# The information for the colorectal cancer by age group is divided by sex, so we need to
# create a new column that is the sum of those two columns to represent the total
# population with colorectal cancer, we use the dyplr method mutate to achieve this
# The new dataframe is called df2
df %>% mutate(Types.Colorectal.Age.18...44 = Types.Colorectal.Age.and.Sex.Male.18...44 + Types.Colorectal.Age.and.Sex.Female.18...44) -> df2
df2 %>% mutate(Types.Colorectal.Age.45...64 = Types.Colorectal.Age.and.Sex.Female.45...64 + Types.Colorectal.Age.and.Sex.Male.45...64) -> df2
df2 %>% mutate(Types.Colorectal.Age...64 = Types.Colorectal.Age.and.Sex.Female...64 + Types.Colorectal.Age.and.Sex.Male...64) -> df2

# Create a new column that has the age categories of the newly created columns and the values
# In a column called Population.Rate. The result is the age_df
age_df <- df2 %>% gather(key='Colorectal.Age.group',value='Population.Rate',  Types.Colorectal.Age.18...44:Types.Colorectal.Age...64)

# Select only the column with the age categories and the population rate
age_df <- age_df[c('Colorectal.Age.group', "Population.Rate")]

# Convert the age categories column into factors
age_df$Age <- factor(age_df$Colorectal.Age.group, levels=c("Types.Colorectal.Age.18...44", "Types.Colorectal.Age.45...64",  "Types.Colorectal.Age...64"))

# To see if the factors are created correctly
levels(age_df$Age)

# Labels for the age groups in the X-axis and the legend
age_labels = c("18-44","45-64",">65")

# Create a boxplot with the age group in the X axis and population rate with colorectal
# cancer in the Y axis
ggplot(data=age_df, aes(x=Age,y=Population.Rate, fill=Age)) + 
  geom_boxplot() +
  labs(title="Colorectal cancer cases", x= "Age (years)", y="Rates (per 100.000)") +
  scale_x_discrete(labels=age_labels) + ggtitle("Colorectal cancer cases") +
  scale_fill_brewer(palette = "PBu", labels = age_labels)+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title=element_text(size=20))+
  theme(plot.title = element_text(size=16))


# HEATMAP PLOT INCIDENCE COLORECTAL CANCER BY AGE GROUP

# Convert the rownames into a columns with the state names
# And apply the tidyr method pivot_longer to transform the columns into a single column with
# each category.
heat_df <-df2 %>% 
  tibble::rownames_to_column(var = "States") %>% 
  pivot_longer(Types.Colorectal.Age.18...44:Types.Colorectal.Age...64, names_to='Age.group',values_to='Rate')

# Select the columns for the heatmap
heat_df <- heat_df[c("States","Age.group", "Rate")]

# Convert the age groups to factors
heat_df$Age.group <- factor(heat_df$Age.group, levels=c("Types.Colorectal.Age.18...44", "Types.Colorectal.Age.45...64",  "Types.Colorectal.Age...64"))

# Create the heatmap with the age group in the X-axis, each state in the Y-axis and
# the population rate incidence for colorectal cancer as the fill values.
ggplot(heat_df, aes(x = Age.group, y=States, fill= Rate)) + 
  geom_tile() +
  scale_x_discrete(labels=age_labels) + 
  labs(title="Colorectal cancer cases",  x= "Age (years)",)  + scale_fill_gradient(low = "white", high = "steelblue") +  
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 8), axis.text.y = element_text(size = 6))+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title=element_text(size=20))+theme(plot.title = element_text(size=16))


# BOXPLOT INCIDENCE LUNG CANCER BY AGE GROUPS

# The information for the lung cancer by age group is divided by sex, so we need to
# create a new column that is the sum of those two columns to represent the total
# population with lung cancer, we use the dyplr method mutate to achieve this
# The new dataframe is called df2
df %>% mutate(Types.Lung.Age.18...44 = Types.Lung.Age.and.Sex.Male.18...44 + Types.Lung.Age.and.Sex.Female.18...44) -> df2
df2 %>% mutate(Types.Lung.Age.45...64 = Types.Lung.Age.and.Sex.Female.45...64 + Types.Lung.Age.and.Sex.Male.45...64) -> df2
df2 %>% mutate(Types.Lung.Age...64 = Types.Lung.Age.and.Sex.Female...64 + Types.Lung.Age.and.Sex.Male...64) -> df2

# Create a new column that has the age categories of the newly created columns and the values
# of the incidence in lung cancer in a column called Population.Rate.
# The result is the age_df
age_df <- df2 %>% gather(key='Age',value='Population.Rate',  Types.Lung.Age.18...44:Types.Lung.Age...64)

# Select only the column with the age categories and the population rate
age_df <- age_df[c('Age', "Population.Rate")]

# Convert the Age column values to factors
age_df$Age <- factor(age_df$Age, levels=c("Types.Lung.Age.18...44", "Types.Lung.Age.45...64",  "Types.Lung.Age...64"))

# Create a boxplot with the age group in the X axis and population rate with lung
# cancer in the Y axis
ggplot(data=age_df, aes(x=Age,y=Population.Rate, fill=Age)) + 
  geom_boxplot() +
  labs(title="Lung cancer cases", x= "Age (years)", y="Rates (per 100.000)") +
  scale_x_discrete(labels=age_labels) + ggtitle("Lung cancer cases") +
  scale_fill_brewer(palette = "PBu", labels = age_labels)+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title=element_text(size=20))+
  theme(plot.title = element_text(size=16))


# HEATMAP PLOT INCIDENCE LUNG CANCER BY AGE GROUP

# Convert the rownames into a columns with the state names
# And apply the tidyr method pivot_longer to transform the columns into a single column with
# each category of the age group.
heat_df <-df2 %>% 
  tibble::rownames_to_column(var = "States") %>% 
  pivot_longer(Types.Lung.Age.18...44:Types.Lung.Age...64, names_to='Age.group',values_to='Rate')

# Select the columns for the heatmap
heat_df <- heat_df[c("States","Age.group", "Population.Rate")]

# Convert the age groups to factors
heat_df$Age.group <- factor(heat_df$Age.group, levels=c("Types.Lung.Age.18...44", "Types.Lung.Age.45...64",  "Types.Lung.Age...64"))

# Create the heatmap with the age group in the X-axis, each state in the Y-axis and
# the population rate incidence for lung cancer as the fill values.
ggplot(heat_df, aes(x = Age.group, y=States, fill= Rate)) + 
  geom_tile() +
  scale_x_discrete(labels=age_labels) + 
  labs(title="Lung cancer cases",  x= "Age (years)",)  + scale_fill_gradient(low = "white", high = "steelblue") +  
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 8), axis.text.y = element_text(size = 6))+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title=element_text(size=20))+theme(plot.title = element_text(size=16))


# BOXPLOT INCIDENCE BREAST CANCER BY AGE GROUPS

# Create a new column that has the age categories of the newly created columns and the values
# of the incidence in breast cancer in a column called Population.Rate.
# The result is the age_df
age_df <- df %>% gather(key='Breast.Age.group',value='Population.Rate',  Types.Breast.Age.18...44:Types.Breast.Age...64)

# Select only the column with the age categories and the population rate
age_df <- age_df[c('Breast.Age.group', "Population.Rate")]

# Convert the Age column values to factors
age_df$Age <- factor(age_df$Breast.Age.group, levels=c("Types.Breast.Age.18...44", "Types.Breast.Age.45...64",  "Types.Breast.Age...64"))

# Create a boxplot with the age group in the X axis and population rate with breast
# cancer in the Y axis
ggplot(data=age_df, aes(x=Age,y=Population.Rate, fill=Age)) + 
  geom_boxplot() +
  labs(title="Breast cancer cases", x= "Age (years)", y="Rates (per 100.000)") +
  scale_x_discrete(labels=age_labels) + ggtitle("Breast cancer cases") +
  scale_fill_brewer(palette = "PBu", labels = age_labels)+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title=element_text(size=20))+
  theme(plot.title = element_text(size=16))


# HEATMAP PLOT INCIDENCE BREAST CANCER BY AGE GROUP

# Convert the rownames into a columns with the state names
# And apply the tidyr method pivot_longer to transform the breast cancer columns into a single column with
# each category of the age group.
heat_df <-df2 %>% 
  tibble::rownames_to_column(var = "States") %>% 
  pivot_longer(Types.Breast.Age.18...44:Types.Breast.Age...64, names_to='Age.group',values_to='Rate')

# Select the columns for the heatmap
heat_df <- heat_df[c("States","Age.group", "Rate")]

# Convert the age groups to factors
heat_df$Age.group <- factor(heat_df$Age.group, levels=c("Types.Breast.Age.18...44", "Types.Breast.Age.45...64",  "Types.Breast.Age...64"))

# Create the heatmap with the age group in the X-axis, each state in the Y-axis and
# the population rate incidence for breast cancer as the fill values.
ggplot(heat_df, aes(x = Age.group, y=States, fill= Rate)) + 
  geom_tile() +
  scale_x_discrete(labels=age_labels) + 
  labs(title="Breast cancer cases",  x= "Age (years)",)  + scale_fill_gradient(low = "white", high = "steelblue") +  
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 8), axis.text.y = element_text(size = 6))+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title=element_text(size=20))+theme(plot.title = element_text(size=16))


# Gendres -----------------------------------------------------------------

data_sex = df[,c(37, 38, 39, 48,49,50, 51, 52, 53, 62, 63, 64, 65, 66, 67)]

cancer1_sex = data_sex[,c(1,2,3)]#breast

cancer2_sex = data_sex[,c(4,5,6,7,8,9)]#colorectal
cancer2_male = cancer2_sex[,c(1,3,5)]
cancer2_female = cancer2_sex[,c(2,4,6)]

cancer3_sex = data_sex[,c(10,11,12,13,14,15)]#lung
cancer3_male = cancer3_sex[,c(1,3,5)]
cancer3_female = cancer3_sex[,c(2,4,6)]


cancer1_matrix = get_sex_matrices(cancer1_sex)
cancer1_total = cbind(cancer1_matrix, rep(0,3)) #there is no data for men
colnames(cancer1_total) = c("Female", "Male")

cancer2_matrix_M = get_sex_matrices(cancer2_male)
cancer2_matrix_F = get_sex_matrices(cancer2_female)
cancer2_total = cbind(cancer2_matrix_F, cancer2_matrix_M)
colnames(cancer2_total) = c("Female", "Male")

cancer3_matrix_M = get_sex_matrices(cancer3_male)
cancer3_matrix_F = get_sex_matrices(cancer3_female)
cancer3_total = cbind(cancer3_matrix_F, cancer3_matrix_M)
colnames(cancer3_total) = c("Female", "Male")

# Bar plot

medias1 = c()
medias2 = c()
medias3 = c()

#mean calculation
for (j in 1:length(cancer1_total[1,])){
  medias1 = c(medias1, mean(cancer1_total[,j]))
  medias2 = c(medias2, mean(cancer2_total[,j]))
  medias3 = c(medias3, mean(cancer3_total[,j]))
}

names(medias1) = c("Female", "Male")
names(medias2) = c("Female", "Male")
names(medias3) = c("Female", "Male")

medias = rbind(medias1, medias2, medias3)
rownames(medias) = c("Breast cancer", "Colorectal cancer", "Lung cancer")

#right format
m_medias = melt(medias)

colnames(m_medias) = c("Cancer", "Sex", "Rate")

ggplot(data = data.frame(m_medias),
       mapping = aes(x = Sex, y=Rate, fill = Cancer))+
  geom_bar(stat = "identity", position = 'dodge')+
  ylab("Rates (per 100.000)")+
  ggtitle("Cases of breast, colorectal and lung cancer") +
  theme(plot.title = element_text(size=16))+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title=element_text(size=14))

### Cases heatmaps

plot_heatmap(df = cancer1_total, variable = "Sex", title = "Breast cancer cases")
plot_heatmap(df = cancer2_total, variable = "Sex", title = "Colorectal cancer cases")
plot_heatmap(df = cancer3_total, variable = "Sex", title = "Lung cancer cases")

### Cases boxplots

plot_boxplot(df = cancer1_total, variable = "Sex", title = "Breast cancer cases")
plot_boxplot(df = cancer2_total, variable = "Sex", title = "Colorectal cancer cases")
plot_boxplot(df = cancer3_total, variable = "Sex", title = "Lung cancer cases")
