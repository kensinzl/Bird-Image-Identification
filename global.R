library(shiny)
library(shinydashboard)
library(rsconnect)
library(keras)
library(tensorflow)
library(tidyverse)


model = load_model_tf("www/bird_mod")
#load("www/label_list.RData")
#label_list = dir("/Users/zhaol/Downloads/Image_Bird/train")
load("www/label_list.RData")
target_size = c(224,224,3)
options(scipen=999) #prevent scientific number formatting