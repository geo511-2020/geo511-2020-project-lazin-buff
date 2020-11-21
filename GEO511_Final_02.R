
##REQUIRED PACKAGES

library(usmap)
library(tibble)
library(tidytext)
library(dplyr)
library(tm)
library(rtweet)
library(ggplot2)
library(e1071)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tmap)
library(spData)
library(sf)
library(Cairo)
library(ggwordcloud)
library(pacman)
library(tidyverse)
library(geojsonio)
library(rgdal)
library(broom)
library(rgeos)
library(mapproj)

#LOAD DATA

txt_al<-read.csv("txt_al.csv")
txt_ar<-read.csv("txt_ar.csv")
txt_az<-read.csv("txt_az.csv")
txt_ca<-read.csv("txt_ca.csv")
txt_cn<-read.csv("txt_cn.csv")
txt_co<-read.csv("txt_co.csv")
txt_dc<-read.csv("txt_dc.csv")
txt_de<-read.csv("txt_de.csv")
txt_fl<-read.csv("txt_fl.csv")
txt_ga<-read.csv("txt_ga.csv")
txt_ia<-read.csv("txt_ia.csv")
txt_id<-read.csv("txt_id.csv")
txt_il<-read.csv("txt_il.csv")
txt_in<-read.csv("txt_in.csv")
txt_ks<-read.csv("txt_ks.csv")
txt_ky<-read.csv("txt_ky.csv")
txt_la<-read.csv("txt_la.csv")
txt_ma<-read.csv("txt_ma.csv")
txt_md<-read.csv("txt_md.csv")
txt_me<-read.csv("txt_me.csv")
txt_mi<-read.csv("txt_mi.csv")
txt_mn<-read.csv("txt_mn.csv")
txt_mo<-read.csv("txt_mo.csv")
txt_ms<-read.csv("txt_ms.csv")
txt_mt<-read.csv("txt_mt.csv")
txt_nc<-read.csv("txt_nc.csv")
txt_nd<-read.csv("txt_nd.csv")
txt_ne<-read.csv("txt_ne.csv")
txt_nh<-read.csv("txt_nh.csv")
txt_nj<-read.csv("txt_nj.csv")
txt_nm<-read.csv("txt_nm.csv")
txt_nv<-read.csv("txt_nv.csv")
txt_ny<-read.csv("txt_ny.csv")
txt_oh<-read.csv("txt_oh.csv")
txt_ok<-read.csv("txt_ok.csv")
txt_or<-read.csv("txt_or.csv")
txt_pa<-read.csv("txt_pa.csv")
txt_ri<-read.csv("txt_ri.csv")
txt_sc<-read.csv("txt_sc.csv")
txt_sd<-read.csv("txt_sd.csv")
txt_tn<-read.csv("txt_tn.csv")
txt_tx<-read.csv("txt_tx.csv")
txt_ut<-read.csv("txt_ut.csv")
txt_va<-read.csv("txt_va.csv")
txt_vt<-read.csv("txt_vt.csv")
txt_wa<-read.csv("txt_wa.csv")
txt_wi<-read.csv("txt_wi.csv")
txt_wv<-read.csv("txt_wv.csv")
txt_wy<-read.csv("txt_wy.csv")

## NLP SETUP

stop_twitter<-as.data.frame(c("https","t.co","rt","amp"))
stop_candidate<-as.data.frame(c("trump","biden","election"))
names(stop_twitter)<-"word"
names(stop_candidate)<-"word"

data("us_states")
states_us<-us_states
albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

nrc<-get_sentiments("nrc")
bing<-get_sentiments("bing")
afinn<-get_sentiments("afinn")

mean_afinn<-function(afinn_state){
  avg_state<-mean(afinn_state$value)
  return(avg_state)
}

## ALL STATES

all_states<-rbind(txt_al,txt_ar,txt_az,txt_ca,txt_cn,txt_co,txt_dc,txt_de,txt_fl,txt_ga,txt_ia,txt_id,txt_il,txt_in,txt_ks,txt_ky,txt_la,txt_ma,txt_md,txt_me,txt_mi,txt_mn,txt_mo,txt_ms,txt_mt,txt_nc,txt_nd,txt_ne,txt_nh,txt_nj,txt_nm,txt_nv,txt_ny,txt_oh,txt_ok,txt_or,txt_pa,txt_ri,txt_sc,txt_sd,txt_tn,txt_tx,txt_ut,txt_va,txt_vt,txt_wa,txt_wi,txt_wy)
all_states[]<-lapply(all_states,as.character)
untk_states<-all_states%>%
  unnest_tokens(word,text)

sw_states<-untk_states%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)

#NATIONAL WORD FREQUENCY
count_states<-sw_states%>%
  count(word,sort=TRUE)%>%
  head(12)
print(count_states)

#NATIONAL SENTIMENT
nrc_states<-sw_states%>%
  inner_join(nrc)%>%
  count(sentiment,sort=TRUE)
print(nrc_states)

afinn_states<-sw_states%>%
  inner_join(afinn)

## EACH STATE

txt_al[]<-lapply(txt_al,as.character)
untk_al<-txt_al%>%
    unnest_tokens(word,text)%>%
    anti_join(stop_words)%>%
    anti_join(stop_twitter)%>%
    anti_join(stop_candidate)%>%
    inner_join(afinn)
untk_al$meanVal<-mean_afinn(untk_al)
avg_al<-mean_afinn(untk_al)

txt_ar[]<-lapply(txt_ar,as.character)
untk_ar<-txt_ar%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ar$meanVal<-mean_afinn(untk_ar)
avg_ar<-mean_afinn(untk_ar)

txt_az[]<-lapply(txt_az,as.character)
untk_az<-txt_az%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_az$meanVal<-mean_afinn(untk_az)
avg_az<-mean_afinn(untk_az)

txt_ca[]<-lapply(txt_ca,as.character)
untk_ca<-txt_ca%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ca$meanVal<-mean_afinn(untk_ca)
avg_ca<-mean_afinn(untk_ca)

txt_cn[]<-lapply(txt_cn,as.character)
untk_cn<-txt_cn%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_cn$meanVal<-mean_afinn(untk_cn)
avg_cn<-mean_afinn(untk_cn)

txt_co[]<-lapply(txt_co,as.character)
untk_co<-txt_co%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_co$meanVal<-mean_afinn(untk_co)
avg_co<-mean_afinn(untk_co)

txt_dc[]<-lapply(txt_dc,as.character)
untk_dc<-txt_dc%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_dc$meanVal<-mean_afinn(untk_dc)
avg_dc<-mean_afinn(untk_dc)

txt_de[]<-lapply(txt_de,as.character)
untk_de<-txt_de%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_de$meanVal<-mean_afinn(untk_de)
avg_de<-mean_afinn(untk_de)

txt_fl[]<-lapply(txt_fl,as.character)
untk_fl<-txt_fl%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_fl$meanVal<-mean_afinn(untk_fl)
avg_fl<-mean_afinn(untk_fl)

txt_ga[]<-lapply(txt_ga,as.character)
untk_ga<-txt_ga%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ga$meanVal<-mean_afinn(untk_ga)
avg_ga<-mean_afinn(untk_ga)

txt_ia[]<-lapply(txt_ia,as.character)
untk_ia<-txt_ia%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ia$meanVal<-mean_afinn(untk_ia)
avg_ia<-mean_afinn(untk_ia)

txt_id[]<-lapply(txt_id,as.character)
untk_id<-txt_id%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_id$meanVal<-mean_afinn(untk_id)
avg_id<-mean_afinn(untk_id)

txt_il[]<-lapply(txt_il,as.character)
untk_il<-txt_il%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_il$meanVal<-mean_afinn(untk_il)
avg_il<-mean_afinn(untk_il)

txt_in[]<-lapply(txt_in,as.character)
untk_in<-txt_in%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_in$meanVal<-mean_afinn(untk_in)
avg_in<-mean_afinn(untk_in)

txt_ks[]<-lapply(txt_ks,as.character)
untk_ks<-txt_ks%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ks$meanVal<-mean_afinn(untk_ks)
avg_ks<-mean_afinn(untk_ks)

txt_ky[]<-lapply(txt_ky,as.character)
untk_ky<-txt_ky%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ky$meanVal<-mean_afinn(untk_ky)
avg_ky<-mean_afinn(untk_ky)

txt_la[]<-lapply(txt_la,as.character)
untk_la<-txt_la%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_la$meanVal<-mean_afinn(untk_la)
avg_la<-mean_afinn(untk_la)

txt_ma[]<-lapply(txt_ma,as.character)
untk_ma<-txt_ma%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ma$meanVal<-mean_afinn(untk_ma)
avg_ma<-mean_afinn(untk_ma)

txt_md[]<-lapply(txt_md,as.character)
untk_md<-txt_md%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_md$meanVal<-mean_afinn(untk_md)
avg_md<-mean_afinn(untk_md)

txt_me[]<-lapply(txt_me,as.character)
untk_me<-txt_me%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_me$meanVal<-mean_afinn(untk_me)
avg_me<-mean_afinn(untk_me)

txt_mi[]<-lapply(txt_mi,as.character)
untk_mi<-txt_mi%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_mi$meanVal<-mean_afinn(untk_mi)
avg_mi<-mean_afinn(untk_mi)

txt_mn[]<-lapply(txt_mn,as.character)
untk_mn<-txt_mn%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_mn$meanVal<-mean_afinn(untk_mn)
avg_mn<-mean_afinn(untk_mn)

txt_mo[]<-lapply(txt_mo,as.character)
untk_mo<-txt_mo%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_mo$meanVal<-mean_afinn(untk_mo)
avg_mo<-mean_afinn(untk_mo)

txt_ms[]<-lapply(txt_ms,as.character)
untk_ms<-txt_ms%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ms$meanVal<-mean_afinn(untk_ms)
avg_ms<-mean_afinn(untk_ms)

txt_mt[]<-lapply(txt_mt,as.character)
untk_mt<-txt_mt%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_mt$meanVal<-mean_afinn(untk_mt)
avg_mt<-mean_afinn(untk_mt)

txt_nc[]<-lapply(txt_nc,as.character)
untk_nc<-txt_nc%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_nc$meanVal<-mean_afinn(untk_nc)
avg_nc<-mean_afinn(untk_nc)

txt_nd[]<-lapply(txt_nd,as.character)
untk_nd<-txt_nd%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_nd$meanVal<-mean_afinn(untk_nd)
avg_nd<-mean_afinn(untk_nd)

txt_ne[]<-lapply(txt_ne,as.character)
untk_ne<-txt_ne%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ne$meanVal<-mean_afinn(untk_ne)
avg_ne<-mean_afinn(untk_ne)

txt_nh[]<-lapply(txt_nh,as.character)
untk_nh<-txt_nh%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_nh$meanVal<-mean_afinn(untk_nh)
avg_nh<-mean_afinn(untk_nh)

txt_nj[]<-lapply(txt_nj,as.character)
untk_nj<-txt_nj%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_nj$meanVal<-mean_afinn(untk_nj)
avg_nj<-mean_afinn(untk_nj)

txt_nm[]<-lapply(txt_nm,as.character)
untk_nm<-txt_nm%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_nm$meanVal<-mean_afinn(untk_nm)
avg_nm<-mean_afinn(untk_nm)

txt_nv[]<-lapply(txt_nv,as.character)
untk_nv<-txt_nv%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_nv$meanVal<-mean_afinn(untk_nv)
avg_nv<-mean_afinn(untk_nv)

txt_ny[]<-lapply(txt_ny,as.character)
untk_ny<-txt_ny%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ny$meanVal<-mean_afinn(untk_ny)
avg_ny<-mean_afinn(untk_ny)

txt_oh[]<-lapply(txt_oh,as.character)
untk_oh<-txt_oh%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_oh$meanVal<-mean_afinn(untk_oh)
avg_oh<-mean_afinn(untk_oh)

txt_ok[]<-lapply(txt_ok,as.character)
untk_ok<-txt_ok%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ok$meanVal<-mean_afinn(untk_ok)
avg_ok<-mean_afinn(untk_ok)

txt_or[]<-lapply(txt_or,as.character)
untk_or<-txt_or%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_or$meanVal<-mean_afinn(untk_or)
avg_or<-mean_afinn(untk_or)

txt_pa[]<-lapply(txt_pa,as.character)
untk_pa<-txt_pa%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_pa$meanVal<-mean_afinn(untk_pa)
avg_pa<-mean_afinn(untk_pa)

txt_ri[]<-lapply(txt_ri,as.character)
untk_ri<-txt_ri%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ri$meanVal<-mean_afinn(untk_ri)
avg_ri<-mean_afinn(untk_ri)

txt_sc[]<-lapply(txt_sc,as.character)
untk_sc<-txt_sc%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_sc$meanVal<-mean_afinn(untk_sc)
avg_sc<-mean_afinn(untk_sc)

txt_sd[]<-lapply(txt_sd,as.character)
untk_sd<-txt_sd%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_sd$meanVal<-mean_afinn(untk_sd)
avg_sd<-mean_afinn(untk_sd)

txt_tn[]<-lapply(txt_tn,as.character)
untk_tn<-txt_tn%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_tn$meanVal<-mean_afinn(untk_tn)
avg_tn<-mean_afinn(untk_tn)

txt_tx[]<-lapply(txt_tx,as.character)
untk_tx<-txt_tx%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_tx$meanVal<-mean_afinn(untk_tx)
avg_tx<-mean_afinn(untk_tx)

txt_ut[]<-lapply(txt_ut,as.character)
untk_ut<-txt_ut%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_ut$meanVal<-mean_afinn(untk_ut)
avg_ut<-mean_afinn(untk_ut)

txt_va[]<-lapply(txt_va,as.character)
untk_va<-txt_va%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_va$meanVal<-mean_afinn(untk_va)
avg_va<-mean_afinn(untk_va)

txt_vt[]<-lapply(txt_vt,as.character)
untk_vt<-txt_vt%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_vt$meanVal<-mean_afinn(untk_vt)
avg_vt<-mean_afinn(untk_vt)

txt_wa[]<-lapply(txt_wa,as.character)
untk_wa<-txt_wa%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_wa$meanVal<-mean_afinn(untk_wa)
avg_wa<-mean_afinn(untk_wa)

txt_wi[]<-lapply(txt_wi,as.character)
untk_wi<-txt_wi%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_wi$meanVal<-mean_afinn(untk_wi)
avg_wi<-mean_afinn(untk_wi)

txt_wv[]<-lapply(txt_wv,as.character)
untk_wv<-txt_wv%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_wv$meanVal<-mean_afinn(untk_wv)
avg_wv<-mean_afinn(untk_wv)

txt_wy[]<-lapply(txt_wy,as.character)
untk_wy<-txt_wy%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  anti_join(stop_twitter)%>%
  anti_join(stop_candidate)%>%
  inner_join(afinn)
untk_wy$meanVal<-mean_afinn(untk_wy)
avg_wy<-mean_afinn(untk_wy)

## MAP MAKING

afinn_eachState_list<-round(c(avg_al,avg_ar,avg_az,avg_ca,avg_cn,avg_co,avg_dc,avg_de,avg_fl,avg_ga,avg_ia,avg_id,avg_il,avg_in,avg_ks,avg_ky,avg_la,avg_ma,avg_md,avg_me,avg_mi,avg_mn,avg_mo,avg_ms,avg_mt,avg_nc,avg_nd,avg_ne,avg_nh,avg_nj,avg_nm,avg_nv,avg_ny,avg_oh,avg_ok,avg_or,avg_pa,avg_ri,avg_sc,avg_sd,avg_tn,avg_tx,avg_ut,avg_va,avg_vt,avg_wa,avg_wi,avg_wv,avg_wy),1)
state_names<-c("Alabama","Arkansas","Arizona","California","Connecticut","Colorado","District of Columbia","Delaware","Florida","Georgia","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky","Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota","Missouri","Mississippi","Montana","North Carolina","North Dakota","Nebraska","New Hampshire","New Jersey","New Mexico","Nevada","New York","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Virginia","Vermont","Washington","Wisconsin","West Virginia","Wyoming")
state_ecv<-as.numeric(c("9","6","11","55","7","9","3","3","29","16","6","4","20","11","6","8","8","11","10","4","16","10","10","6","3","15","3","5","4","14","5","6","29","18","7","7","20","4","9","3","11","38","6","13","3","12","10","5","3"))
state_result<-as.vector(c("R","R","D","D","D","D","D","D","R","D","R","R","D","R","R","R","R","D","D","D","D","D","R","R","R","R","R","R","D","D","D","D","D","R","R","D","D","D","R","R","R","R","R","D","D","D","D","R","R"))
state_swing<-as.vector(c("R","R","S","D","D","D","D","D","S","S","S","R","D","R","R","R","R","D","D","D","S","S","R","R","R","R","R","R","S","D","S","S","D","R","R","D","S","D","R","R","R","S","R","D","D","D","S","R","R"))
#https://ballotpedia.org/U.S._House_battlegrounds,_2020
tUp_house<-as.numeric(c("1","1","1","2","0","0","0","0","0","0","2","0","1","1","0","0","0","0","0","0","1","2","1","0","0","0","0","1","0","1","1","0","4","1","1","0","1","0","0","0","0","3","1","1","0","0","0","0","0"))
house_swing<-tUp_house*.25

states_afinn<-tibble(state_names,afinn_eachState_list,state_result,state_ecv,state_swing,tUp_house,house_swing)
names(states_afinn)<-c("NAME","AFINN","RESULT","EC_VOTES","SWING_FORECAST","TOSS_UP","TU_alpha")
states_us<-states_us[order(states_us$NAME),]
us_states_afinn<-merge(states_us,states_afinn,by="NAME")

us_states_afinn$REP_PER_POP<-us_states_afinn$EC_VOTES/us_states_afinn$total_pop_15
sum_rep<-sum(us_states_afinn$REP_PER_POP)
us_states_afinn$VOTE_WEIGHT<-round(as.numeric((us_states_afinn$REP_PER_POP/sum_rep)*10),2)

trans_states<-st_transform(us_states_afinn,albers)
shape_states<-tm_shape(trans_states)+
  tm_borders(col="black")+
  tm_layout(main.title="AFINN Sentiments of 2020 Election",main.title.position="center",frame=F)+
  tm_polygons('AFINN',midpoint=NA)

#SENTIMENT MAP (TMAPS: https://tlorusso.github.io/geodata_workshop/tmap_package)
shape_states

swing_col<-c("powderblue","lightpink","lavender")
result_col<-c("blue","red")

trans_states<-st_transform(us_states_afinn,albers)
bubbles_states<-tm_shape(trans_states)+
  tm_layout(main.title="Forecasts, Results, and Votes in 2020",
            main.title.position=c("left","top"))+
  tm_bubbles(size="EC_VOTES",
             col="SWING_FORECAST",
             midpoint=NA,
             palette=swing_col,
             legend.hist=TRUE,
             scale=4)+
  tm_shape(trans_states)+
  tm_bubbles(size=0.25,
             col="RESULT",
             midpoint=NA,
             alpha=0.7,
             scale=1,
             palette=result_col,
             legend.hist=TRUE)+
  tm_layout(legend.position = c(0.1,0.1),
            legend.outside=TRUE,
            frame=FALSE)

#FORECAST RESULT AND OUTCOME MAP  
bubbles_states

#HEXGRID TUTORIAL: https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)
library(mapproj)
library(viridis)

my_palette <- c("turquoise","salmon")
my_palette2 <- c("turquoise","salmon","violet")
my_palette3<-rev(magma(20))
my_palette4<-magma(8)

hex_ec_voteWeight
hex_ec_forecast
hex_ec_swingHouse
hex_ec_concentration

#LOAD IN HEXGRID US: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
plot(spdf)

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")
names(spdf_fortified)<-c("long","lat","order","hole","piece","group","NAME")
spdf_fort_noHIAK<-spdf_fortified[-c(8:14,78:84),]
spdf_fort_L48<-merge(spdf_fort_noHIAK,us_states_afinn,by="NAME")
t_ec<-sum(state_ecv)
#Messed with algorithm to make alphas appear visually
spdf_fort_L48$EC_WT<-(round((spdf_fort_L48$EC_VOTES/t_ec),3)*10)+.2

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
cntr<-centers[-c(27,50),]

#MAKING PLOTS
hex_ec_voteWeight<-ggplot() +
  geom_polygon(data = spdf_fort_L48, aes(fill=RESULT, x = long, y = lat, group = group,alpha=VOTE_WEIGHT+0.2))+
  geom_text(data=cntr, aes(x=x, y=y, label=id)) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette, 
    name="2020 Election", 
    guide = guide_legend(keyheight = unit(3, units = "mm"), 
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1))+
  scale_alpha_identity(
    name="Weight in EC",
    guide = guide_legend(keyheight = unit(3, units = "mm"),
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1))+
  coord_quickmap()+
  ggtitle( "Citizen Vote Weight \nin Electoral College" ) +
  theme(
    legend.position = c(0.6, 1.0),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "ivory", color = NA), 
    panel.background = element_rect(fill = "ivory", color = NA), 
    legend.background = element_rect(fill = "ivory", color = NA),
    plot.title = element_text(size= 22, hjust=0.2, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

hex_ec_forecast<-ggplot() +
  geom_polygon(data = spdf_fort_L48, aes(fill=SWING_FORECAST, x = long, y = lat, group = group,alpha=EC_WT))+
  geom_text(data=cntr, aes(x=x, y=y, label=id)) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette2, 
    name="Dem / Rep / Swing", 
    guide = guide_legend(keyheight = unit(3, units = "mm"), 
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1))+
  scale_alpha_identity(
    name="Electoral College Points \n Share x 10",
    guide = guide_legend(keyheight = unit(3, units = "mm"),
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1))+
  coord_quickmap()+
  ggtitle( "2020 Electoral College \nForecasted Votes" ) +
  theme(
    legend.position = c(0.7, 1.0),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "ivory", color = NA), 
    panel.background = element_rect(fill = "ivory", color = NA), 
    legend.background = element_rect(fill = "ivory", color = NA),
    plot.title.position="plot",
    plot.title = element_text(size= 22, hjust=0.2, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

hex_ec_swingHouse<-ggplot() +
  geom_polygon(data = spdf_fort_L48, aes(fill=SWING_FORECAST, x = long, y = lat, group = group,alpha=TU_alpha))+
  geom_text(data=cntr, aes(x=x, y=y, label=id)) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette2,
    name="Dem / Rep / Swing", 
    guide = guide_legend(keyheight = unit(3, units = "mm"), 
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1))+
  scale_alpha_identity(
    name="2020 Toss Up \nRaces per State",
    guide = guide_legend(keyheight = unit(3, units = "mm"),
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1))+
  coord_quickmap()+
  ggtitle( "Swing Races Per \nState from 0 to 4" ) +
  theme(
    legend.position = c(0.7, 1.0),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "ivory", color = NA), 
    panel.background = element_rect(fill = "ivory", color = NA), 
    legend.background = element_rect(fill = "ivory", color = NA),
    plot.title.position="plot",
    plot.title = element_text(size= 22, hjust=0.2, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

spdf_fort_L48$ECbin<-cut(spdf_fort_L48$EC_VOTES,
                         breaks=c(0,6,12,18,24,Inf), 
                         labels=c("0-6","7-12","13-18","19-24","30+"), 
                         include.lowest=TRUE)

hex_ec_concentration<-ggplot() +
  geom_polygon(data = spdf_fort_L48, aes(fill=ECbin, x = long, y = lat, group = group))+
  geom_text(data=cntr, aes(x=x, y=y, label=id)) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette3, 
    name="Concentration", 
    guide = guide_legend(keyheight = unit(3, units = "mm"), 
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1))+
  coord_quickmap()+
  ggtitle( "Electoral College" ) +
  theme(
    legend.position = c(0.6, 1.0),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "ivory", color = NA), 
    panel.background = element_rect(fill = "ivory", color = NA), 
    legend.background = element_rect(fill = "ivory", color = NA),
    plot.title = element_text(size= 22, hjust=0.2, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

hex_afinn<-ggplot() +
  geom_polygon(data = spdf_fort_L48, aes(fill=as.factor(AFINN), x = long, y = lat, group = group))+
  geom_text(data=cntr, aes(x=x, y=y, label=id),color="white") +
  theme_void() +
  scale_fill_manual( 
    values=my_palette4, 
    name="Sentiment + / -", 
    guide = guide_legend(keyheight = unit(3, units = "mm"), 
                         keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top',
                         nrow=1))+
  coord_quickmap()+
  ggtitle( "Tweet Sentiment by State" ) +
  theme(
    legend.position = c(0.6, 1.0),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "ivory", color = NA), 
    panel.background = element_rect(fill = "ivory", color = NA), 
    legend.background = element_rect(fill = "ivory", color = NA),
    plot.title = element_text(size= 22, hjust=0.2, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
hex_afinn


#WORD CLOUDS https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
feel_swatch= ifelse(bing_swing$sentiment %in% "positive","red","darkgreen")
set.seed(511)

swing_wfqc<-rbind(untk_pa,untk_ga,untk_az,untk_fl,untk_ia,untk_mi,untk_mn,untk_nm,untk_nv,untk_tx,untk_wi)%>%
  count(word,sort=TRUE)
bing_swing<-swing_wfqc%>%
  inner_join(bing)
cloud_swing<-wordcloud(word=bing_swing$word,freq=bing_swing$n,min.freq=0,max.words=200,ordered.colors=F,random.order=F,colors=feel_swatch)

pa_wfqc<-untk_pa%>%
  count(word,sort=TRUE)
bing_pa<-pa_wfqc%>%
  inner_join(bing)
cloud_pa<-wordcloud(word=bing_pa$word,freq=bing_pa$n,min.freq=0,max.words=200,ordered.colors=F,random.order=F,colors=feel_swatch)

mn_wfqc<-untk_mn%>%
  count(word,sort=TRUE)
bing_mn<-mn_wfqc%>%
  inner_join(bing)
cloud_mn<-wordcloud(word=bing_mn$word,freq=bing_mn$n,min.freq=0,max.words=200,ordered.colors=F,random.order=F,colors=feel_swatch)

wv_wfqc<-untk_wv%>%
  count(word,sort=TRUE)
bing_wv<-wv_wfqc%>%
  inner_join(bing)
cloud_wv<-wordcloud(word=bing_wv$word,freq=bing_wv$n,min.freq=0,max.words=200,ordered.colors=F,random.order=F,colors=feel_swatch)

ga_wfqc<-untk_ga%>%
  count(word,sort=TRUE)
bing_ga<-ga_wfqc%>%
  inner_join(bing)
cloud_ga<-wordcloud(word=bing_ga$word,freq=bing_ga$n,min.freq=0,max.words=200,ordered.colors=F,random.order=F,colors=feel_swatch)