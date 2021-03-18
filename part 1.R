#list library 
library(dplyr)
library(ggplot2)
library(tidyr)

 #Data preparation

driver_activity <- read.csv("data raw/Hourly_DriverActivity_1.csv")
colnames(driver_activity) <- c(
  'date', 'active_drivers',	'online_hour', 'has_booking_hour', 'waiting_hour', 'busy_hour',
  'hours_per_active_driver', 'rides_per_online_hour',	'finished_ride')
driver_activity[is.na(driver_activity)] <- 0 #replace NA to 0
driver_activity$data_date <- driver_activity$date

overview_search <- read.csv("data raw/Hourly_OverviewSearch_1.csv")
colnames(overview_search) <- c('date','people_with_no_driver','people_with_driver','coverage_rate')

#merge both data frame and split date column to data_date and time
#define demand as total passanger with no driver and total passanger with drivers

df <- 
  driver_activity %>%
  left_join(overview_search) %>%
  separate(date, c("date", "time"), sep = " ") %>%
  mutate(demand = people_with_no_driver + people_with_driver, 
         sdr = active_drivers/demand,
         sdd = abs(active_drivers - demand),
         ls = sdd-(sdd*sdr),
         is_weekend = if_else(
           format(as.Date(date),'%u') %in% c(6,7),'weekend','weekday')
         )

df$data_week  <- cut(as.Date(df$date),'week')
  
  
#show how SDD and SDR works
ggplot(df, aes(x=sdd, y=sdr))+
  geom_point() +
  labs(x='Supply Demand Difference', y='Supply Demand Ratio', subtitle = 'SDD vs SDR')+
  scale_y_log10()+
  scale_x_continuous(limits = c(0,100),breaks = seq(0,100,25))+
  geom_hline(yintercept = 1, linetype =2, color='red')+
  annotate('text',x=55, y= 5.5, label='Oversupplied', color='red',)+
  annotate('text',x=55, y= 0.35, label='Undersupplied', color='red')+
  theme_bw(base_size = 20)


#find 36 hour most undersupplied time for each week
df %>%
  group_by(data_week) %>%
  mutate(rank = dense_rank(desc(ls))) %>%
  ungroup() %>%
  filter(rank <=36, sdr<=1) %>%
  ggplot(aes(y=ls,x=rank,fill=is_weekend))+
  geom_bar(stat = 'identity')+
  geom_text(aes(x=rank, y=40, label=paste('hour:',data_date)))+
  scale_x_continuous(limits = c(0,36),breaks = seq(1,36,1))+
  coord_flip()+
  labs(y='Lack of supply', subtitle = 'Sort top 36 undersupplied hour per isoweek\nGraph only shows data points with SDR <1')+
  facet_wrap(~data_week, nrow =1)+
  theme_bw(base_size = 15)



#investigate why undersupply is getting worse
df %>%
  group_by(is_weekend,data_week) %>%
  summarise(ls =sum(if_else(ls<0,1,ls), na.rm = TRUE),
            online =sum(online_hour, na.rm=TRUE),
            demand = sum(demand, na.rm = TRUE),
            finished_ride = sum(finished_ride, na.rm=TRUE),
            avg_online =mean(hours_per_active_driver),
            avg_sdr = mean(sdr, na.rm = TRUE),
            avg_coverage = mean(coverage_rate, na.rm = TRUE)) %>%
  mutate(pct_loss = ls/demand)

# 24hour curve supply demand

plot_curve_sd <- function (df, daytype ='weekday'){
df %>%
  select(data_week, time, active_drivers,demand, is_weekend) %>%
  group_by(data_week,time, is_weekend) %>%
  summarise(avg_supply = mean(active_drivers, na.rm = TRUE),
            avg_demand = mean(demand, na.rm = TRUE)) %>%
  mutate(line_color = if_else(avg_supply >= avg_demand, 'enough supply','low supply')) %>%
  ungroup()%>%
  pivot_longer(-c(data_week,time,is_weekend,line_color)) %>%
  filter(is_weekend==daytype) %>%
  #start ploting
  ggplot(aes(x=time,y=value))+
  geom_point(aes(color=name))+
  geom_line(aes(color=line_color))+
  scale_color_manual(values = c('black','purple','green','red'))+
  facet_wrap(~is_weekend+data_week, nrow = 2)+
  theme_bw(base_size = 15)+
  theme(legend.position = 'bottom')
}

plot_curve_sd(df,'weekend') 
plot_curve_sd(df,'weekday')

## create heat map weekly and 24 hour

ggplot(df %>% mutate(ls= if_else(ls< 350*-1,2,(if_else(ls<0,1,0)))) ,
       aes(time, date)) + 
  geom_tile(aes(fill = ls),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = 'red', high = 'green') +  
  guides(fill=guide_legend(title="Indicator Scale")) +
  theme_bw() +
  labs(title = "Heatmap supply condition",
       subtitle = 'Indicator | 0:undersupply; | 1:supply is sufficient; | 2: oversupply',
       x = "Hour", y = "Date") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## estimate number of supply hour to achive higher coverage rate
# the idea is estimating best ratio for cover all incoming booking with certain driver
# show there are fake alarm, that make problematic bucket not visible

ggplot(df, aes(x=demand, y=sdr))+
  geom_point(alpha=0.5)+
  scale_y_continuous(limits = c(0,18), breaks = seq(0,20,1))+
  scale_x_continuous(limits = c(0,280), breaks= seq(0,300, 50))+
  labs(x='#of demand frequency', y= 'supply demand ratio',
       title = 'Supply Demand Ratio based on number of demand',
       subtitle = 'Graph presented on hourly basis')+
  geom_hline(yintercept=1, linetype =2, color='red')+
  annotate("text", x = 200, y = 0.5, label = 'Problematic hour',alpha=10, color='red', size=5)+
  geom_vline(xintercept = 20, linetype = 2, color='orange')+
  annotate("text", x = 12, y = 10, label = 'False Alarm',alpha=10, color='orange', angle=90, size=5)+
  theme_bw(base_size = 15)
  

# construct ND-SDR

find_nd_sdr <- function (df) {
  df %>%
    na.omit() %>%
    select(sdr,demand,active_drivers, date) %>%
    group_by(date) %>%
    mutate(
      size_weight = demand/sum(demand),
      partial_sdr = size_weight*sdr,
      mean_sdr = sum(active_drivers)/sum(demand),
      relative_dispersion = exp(-4*sdr/mean_sdr),
      sum_relative_dispersion = sum(relative_dispersion),
      weight_product = size_weight * relative_dispersion/sum_relative_dispersion,
      sum_weight_product = sum(weight_product),
      total_weight_product = weight_product/sum_weight_product,
      partial_sdr_weight = sdr*total_weight_product
    ) %>% 
    ungroup() %>%
    group_by(date) %>%
    summarise(nd_sdr = sum(partial_sdr_weight))  
}

# construnct daily coverage rate
find_daily_coverage_rate <- function(df) {
  df %>%
    na.omit() %>%
    select(demand, people_with_driver, date) %>%
    group_by(date) %>%
    summarise(daily_coverage_rate = sum(people_with_driver)/sum(demand))
}

#plot relationship both

plot_actual <- function(df){
df %>%
  find_nd_sdr() %>%
  left_join(df %>%  find_daily_coverage_rate()) %>%
  ggplot(aes(x=nd_sdr, y=daily_coverage_rate))+
  scale_x_continuous(limits = c(0.4,2.5))+
  geom_point()+
  labs(x='Adjusted Supply Demand Ratio', y='Daily Coverage Rate',
       title = 'Estimate daily coverage rate based on avaibilty drivers',
       subtitle  = 'Actual Data')+
  theme_bw(base_size = 15)
}

# summary nd_sdr 
summary(df %>%
  find_nd_sdr())

# coverage estimation using glm binomial
log_data <- 
  df %>%
  find_nd_sdr() %>%
  left_join(df %>%  find_daily_coverage_rate()) %>%
  mutate(y = if_else(daily_coverage_rate<0.78,0,1)) # cut off is acquired from median value


# build model
logmod <- (glm(data = log_data,
           family='binomial',
           y ~ nd_sdr))

summary(logmod)
#simulate using model built
sim_df <- data.frame(nd_sdr = seq(0,2.5,0.05))
sim_df$expected_coverage <- predict(logmod, sim_df, type='response')

# get simulation plot
plot_sim <- function(df){
ggplot(sim_df, aes(x=nd_sdr, y=expected_coverage))+
  geom_point()+
  scale_x_continuous(limits = c(0.4,2.5))+
  labs(x='Adjusted Supply Demand Ratio', y='Daily Coverage Rate',
       title = 'Estimate daily coverage rate based on avaibilty drivers',
       subtitle = 'Simulation Data: Generated by sigmoid function')+
  theme_bw(base_size = 15)
}


#combine all graph
gridExtra::grid.arrange(
  gridExtra::arrangeGrob(plot_actual(df),plot_sim(sim_df), ncol=1, nrow=2))

#find best yield gains for each incremental sdr
sim_df %>%
  mutate(lag_expected_coverage = lag(expected_coverage),
         gain_coverage_rate = expected_coverage-lag_expected_coverage) %>%
  ggplot(aes(x=nd_sdr, y=gain_coverage_rate))+
  geom_col()+
  scale_x_continuous(limits = c(0,2), breaks = seq(0,2,0.25))+
  geom_vline(xintercept = 0.875, linetype =2, color='red', alpha=100)+
  geom_vline(xintercept = 1.1, linetype=2, color='orange')+
  geom_vline(xintercept = 1.35, linetype=2, color='dark green')+
  geom_hline(yintercept = 0.01, linetype=2, color='blue')+
  geom_hline(yintercept = 0.05, linetype=2, color='blue')+
  annotate('text',x=1.7, y= 0.052, label='for each 0.1 SDR coverage gains is less than 5%', size=4.5)+
  annotate('text',x=1.7, y= 0.012, label='for each 0.1 SDR coverage gains is less than 1%', size=4.5)+
  scale_y_continuous(limits = c(0,0.11), breaks = seq(0,0.1, 0.01))+
  labs(x='Adjusted Supply Demand Ratio', y='Coverage rate gains',
       title = 'Find the best yield to decide best ratio for each upcoming booking',
       subtitle = 'Simulation Data: Generated by sigmoid function')+
  theme_bw(base_size = 15)


# predict supply hour needed
regmod <- lm(data=df, online_hour ~ active_drivers)
summary(regmod)


df %>%
  filter(data_week =='2016-12-12',is_weekend=='weekday') %>%
  group_by(time) %>%
  summarise(avg_demand = mean(demand),
            avg_online_hour = mean(online_hour),
            avg_coverge_rate = mean(coverage_rate)) %>%
  ungroup() %>%
  mutate(driver_needed = avg_demand *1., #1.1 is best yield of sdr
         online_hour_needed = (0.406*driver_needed - 1.86),
         lack_of_supply_hour = avg_online_hour-online_hour_needed
         )

  
# calculate minimum revenue
# before we calculate MRG, first we need to estimate number potential revenue
get_data_mrg <- function(df){
  df %>%
    select(rides_per_online_hour,finished_ride) %>%
    mutate(revenue = finished_ride * 10,#dollar
           demand_hour = finished_ride* rides_per_online_hour)
}

mod_mrg <- (lm(data = df %>% get_data_mrg(),
           revenue ~ demand_hour))

simul_mrg <- data.frame(demand_hour = seq(0,150))
simul_mrg$revenue <- predict(mod_mrg,simul_mrg)

#estimate earning per hour 
sum(df$finished_ride)*10/ sum(df$online_hour)

supply_hour_need <- 45.7 + 43.4 +25.8
RA=round(sum(df$finished_ride)*10/ sum(df$online_hour)*(29.6+32.2+36.2),2)

eq = paste0("Revenue per additional demand hour= ", round(coef(mod_mrg)[[2]],2),' * demand hour + ' ,round(coef(mod_mrg)[[1]],2))
eq2 = paste0("(Maximum) Revenue with current supply")

ggplot(simul_mrg, aes(x=demand_hour, y=revenue))+
  geom_line()+
  ggtitle('ERG Illustration')+
  labs(x='demand hour', y= 'revenue')+
  geom_abline(slope = coef(mod_mrg)[[2]], intercept = coef(mod_mrg)[[1]])+
  geom_hline(yintercept = RA)+
  annotate(
    "text",
    x = 35,
    y = 45 *  coef(mod_mrg)[[2]] * 1.2,
    angle = 22,#atan(coef(mod_mrg)[[2]]) * 180/pi,
    label =eq
  ) +
  annotate('text',x=35,y=RA*1.05,label=eq2)+
  annotate('text',x=100,y=300,label='Additional Hour (AS)')+
  annotate('text',x=supply_hour_need*1.02,y=750,label='Additional Revenue (AR)', angle=90)+
  annotate('text',x=supply_hour_need*1.02,y=300,label='Estimated revenue Current Supply (CuR)', angle=90)+
  geom_vline(xintercept =77.2,linetype=2,color='orange')+
  geom_vline(xintercept =supply_hour_need,linetype=2,color='red')+
  theme_bw(base_size = 15)

#estimate MRG
df %>%
  filter(data_week=='2016-12-12') %>%
  mutate(
    lab = if_else((is_weekend == 'weekday'& time %in% c('07','08','09')), 'weekday_morning',
                   if_else(is_weekend=='weekday' & time %in% c('18','19','20'),'weekday_evening',
                           if_else(is_weekend=='weekend' & time %in% c('00','01','02'),'weekend_midnight','skip')))
      
    ) %>%
  filter(lab != 'skip') %>%
  group_by(lab) %>%
  mutate(EPH = sum(finished_ride *10)/sum(online_hour),
         CuR = online_hour *EPH,
         ideal_online_hour = 1.86+0.4*demand*1.2,
         AS = ideal_online_hour - online_hour,
         PR = ideal_online_hour *EPH,
         AR = PR - CuR,
         estimated_driver_per_hour = active_drivers
  ) %>%
  select(lab, EPH,CuR, ideal_online_hour,AS,PR,CuR,estimated_driver_per_hour) %>%
  group_by(lab,EPH) %>%
  summarise(CuR = sum(CuR),
            ideal_online_hour = sum(ideal_online_hour),
            AS = sum(AS),
            PR = sum(PR),
            estimated_driver_per_hour = sum(estimated_driver_per_hour),
            EGR = (PR/estimated_driver_per_hour) *0.8/3
            )
  

#get data for case study
reg_df <- function(df) {
df %>%
  dplyr::filter(data_week == '2016-12-12', is_weekend=='weekday') %>%
  select (date, time, sdr, demand, online_hour,active_drivers) %>%
  group_by(time) %>%
  summarise(avg_online_hour = mean(online_hour),avg_driver=mean(active_drivers), avg_demand =mean(demand))
}

summary(lm(data=df %>% reg_df(),avg_online_hour~avg_driver))


#summarise the driver utilization
df %>%
  group_by( is_weekend,data_week) %>%
  summarise(utilization = sum(has_booking_hour)/sum(online_hour),
            sdr = sum(active_drivers, na.rm = TRUE)/sum(demand, na.rm = TRUE))
  

