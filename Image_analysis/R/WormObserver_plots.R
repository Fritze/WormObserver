
# this takes a while...
#calculate_overview_statistics(offset,max_number_gaps,duration)
#angle between 1s, only minimum 10s tracked
calculate_overview_statistics(5,5,50)

#function(offset,selected_minute,which_dataset,max_number_gaps,duration,which_frame,color_path,with_image) 
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"frame","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"no_mean","with_image")
plot_image_with_path(25, 200, "2018-12-07_16-00-07",5,0,10,"sd_local_angle","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"mean","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"ID","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"displacement","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"velocity","with_image")
#plot_image_with_path(25, 100, "2018-12-07_16-00-07",2,250,10,"length","with_image")
#plot_image_with_path(25, 200, "2018-12-07_16-00-07",2,250,10,"local_velocity","with_image")
plot_image_with_path(25, 200, "2018-12-07_16-00-07",2,250,10,"sd_local_velocity","with_image")



