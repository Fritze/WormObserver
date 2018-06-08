import os.path
import datetime as dt
from time import sleep
from picamera import PiCamera

camera=PiCamera()

timelapse_length = 300
timelapse_window = 30
timelapse_step = 150

timelapse_length = timelapse_length * 60

timelapses = "timelapses/"
timelapse_folder = dt.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
save_path= os.path.join(timelapses,timelapse_folder)
if not os.path.exists(save_path):
	os.makedirs(save_path)
	print("Path was created.")


wrm_line = raw_input("Please enter the wormline used.\n")
print "Used wormline is ", wrm_line
tmp = raw_input("At which temperature were those cultured previously?\n")
print "Temperature was ", tmp, "degrees Celsius"
date = raw_input("Since when where the worms cultured on the plate?\n")
print "Worms were cultured since ", date
plate_type = raw_input("What type of plate is used for the timelapse?\n")
print "Worms are now on", plate_type
time_elapsed = raw_input ("what is the elapsed time (in minutes) since the worms were plated after SDS selection?\n")
print "Time elapsed:", time_elapsed

txt_path = os.path.join(save_path, "metadata.txt")
txt_file = open(txt_path, 'w')
metadata = "Used line: " + wrm_line + "\n" +  "Previously culture at (degrees Celsius): " + tmp + "\n" + "Cultured since: " + date + "\n" + "Timelapse plate type: " + plate_type + "\n" + "Time elpased previous to timelapse start (minutes): " + time_elapsed
txt_file.write(metadata)
txt_file.close()



print("Starting a  new timelapse")
start=dt.datetime.now()
print("Started at ", str(dt.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")))
while (dt.datetime.now() - start).seconds < timelapse_length:
	filename = dt.datetime.now().strftime("%Y-%m-%d_%H-%M-%S.h264")
	save_path2 = os.path.join(save_path, filename)
        number_files = len(os.listdir(save_path))
        camera.resolution = (1024, 1024)
        print("Showing preview")
        camera.start_preview()
        sleep(5)
        camera.stop_preview()
        print("Now recording " + filename + "(#" + str(number_files) +")")
        print("Running for " + str((dt.datetime.now() - start).seconds / 60) + " minutes")
        camera.start_recording(save_path2)
	camera.wait_recording(timelapse_window)
	camera.stop_recording()
	print(filename + " was created.")
        print("Now waiting for " + str(timelapse_step) +  " seconds.")
        sleep(timelapse_step)
print("Finished timelapse after " + str((dt.datetime.now() - start).seconds / 60) + " minutes.")	
