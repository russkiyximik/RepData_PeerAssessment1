library(tidyverse)
unzip('activity.zip')
activity <- read_csv('activity.csv')

filled <- activity[!is.na(activity$steps),]
stepsperday <- aggregate(steps ~ date, data=filled, sum)
g1 <- ggplot(data=stepsperday, aes(x=steps))
g1 + geom_histogram()
mean(stepsperday$steps)
median(stepsperday$steps)

stepsperint <- aggregate(steps ~ interval, data=filled, mean)
plot(stepsperint$interval, stepsperint$steps, type='l')
stepsperint$interval[which.max(stepsperint$steps)]

sum(is.na(activity$steps))
# We will replace all NA's with the mean value for that interval
complete <- activity %>% 
	left_join(stepsperint, by='interval', suffix=c('','_mean')) %>% 
	mutate(steps=ifelse(is.na(steps), steps_mean, steps)) %>% 
	select(-steps_mean)
completestepsperday <- aggregate(steps ~ date, data=complete, sum)
g2 <- ggplot(data=completestepsperday, aes(x=steps))
g2 + geom_histogram()
mean(completestepsperday$steps)
median(completestepsperday$steps)

byday <- complete %>% mutate(day=ifelse(weekdays(date) %in% 
	c('Saturday', 'Sunday'), 'weekend', 'weekday'))
completestepsperintday <- aggregate(steps ~ interval + day, data=byday, mean)
g3 <- ggplot(data=completestepsperintday, aes(interval, steps))
g3 + facet_wrap(.~day, nrow=2) + geom_line()
		    