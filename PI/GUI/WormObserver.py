from PyQt5 import QtCore, QtGui, QtWidgets, uic
import sys
import os
import datetime as dt
import time
import picamera
from picamera import mmal
import shutil
import ctypes as ct


qtCreatorFile = "WormObserver_gui.ui"  # Enter file here.
Ui_MainWindow, QtBaseClass = uic.loadUiType(qtCreatorFile)


class MyApp(QtWidgets.QMainWindow, Ui_MainWindow):
    def __init__(self):
        QtWidgets.QMainWindow.__init__(self)
        Ui_MainWindow.__init__(self)

        self.setupUi(self)
        self.stop = False
        self.cancel_button.clicked.connect(QtWidgets.QApplication.instance().quit)
        self.cancel_button.clicked.connect(self.stopClicked)
        self.confirm_button.clicked.connect(self.details_entered)

        self.fps_slider_input.valueChanged.connect(self.update_fps_slider)
        self.fps_input.valueChanged.connect(self.update_fps)

        self.timepoints_slider_input.valueChanged.connect(self.update_timepoints_slider)
        self.timepoints_input.valueChanged.connect(self.update_timepoints)

        self.timepoint_length_slider_input.valueChanged.connect(self.update_timepoint_length_slider)
        self.timepoint_length_input.valueChanged.connect(self.update_timepoint_length)

        self.timestep_slider_input.valueChanged.connect(self.update_timestep_slider)
        self.timestep_input.valueChanged.connect(self.update_timestep)

    def stopClicked(self):
        self.stop = True


    def update_fps_slider(self):
        self.fps_input.setValue(self.fps_slider_input.value())

    def update_fps(self):
        self.fps_slider_input.setValue(self.fps_input.value())

    def update_timepoints_slider(self):
        self.timepoints_input.setValue(self.timepoints_slider_input.value())

    def update_timepoints(self):
        self.timepoints_slider_input.setValue(self.timepoints_input.value())

    def update_timepoint_length_slider(self):
        self.timepoint_length_input.setValue(self.timepoint_length_slider_input.value())

    def update_timepoint_length(self):
        self.timepoint_length_slider_input.setValue(self.timepoint_length_input.value())

    def update_timestep_slider(self):
        self.timestep_input.setValue(self.timestep_slider_input.value())

    def update_timestep(self):
        self.timestep_slider_input.setValue(self.timestep_input.value())

    def details_entered(self):
        wrm_line = self.wrm_line_input.text()
        self.message_output.appendPlainText("Used wormline is " + wrm_line + ".")
        tmp = self.tmp_input.text()
        self.message_output.appendPlainText("Worms were previously cultured at " + tmp + " degrees.")
        date = self.date_input.text()
        self.message_output.appendPlainText("Worms were cultured since " + date + ".")
        plate_type = self.plate_type_input.text()
        self.message_output.appendPlainText("Plate type is " + plate_type + ".")
        plate_batch = self.plate_batch_input.text()
        self.message_output.appendPlainText("Plates are from  " + plate_batch + ".")
        number_worms = self.number_worms_input.text()
        self.message_output.appendPlainText("Number of worms on plate: " + number_worms + ".")
        time_elapsed = self.time_elapsed_input.text()
        self.message_output.appendPlainText("Worms are on plate for " + time_elapsed + " minutes.")
        magnification = self.magnification_input.text()
        self.message_output.appendPlainText("Magnification (zoom) is " + magnification + ".")

        self.message_output.appendPlainText('\nThere will be %s timepoints, each consisting of %s seconds. One timepoint will be acquired every %s minute(s) at %s fps.\n' % (self.timepoints_input.value(), self.timepoint_length_input.value(), self.timestep_input.value(), self.fps_input.value()))
        self.message_output.appendPlainText('The total length of the experiment will be ca. %d hours.\n' % ((self.timepoint_length_input.value() * self.timepoints_input.value() + (self.timepoints_input.value() - 1) * self.timestep_input.value() * 60) / 3600))



        os.chdir('timelapses')
        timelapse_id = dt.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
        if not os.path.exists(timelapse_id):
            os.makedirs(timelapse_id)
            self.message_output.appendPlainText('The folder %s was created on the RaspberryPi' % (timelapse_id))
        os.chdir(timelapse_id)

        # on workstation
        path_on_workstation = self.path_on_workstation_input.text() + timelapse_id
        scp_path_on_workstation = self.workstation_input.text() + ":" + path_on_workstation

        os.system('ssh %s mkdir %s' % (self.workstation_input.text(), path_on_workstation))
        self.message_output.appendPlainText("The folder %s was created on the workstation.\n" % (path_on_workstation))

        #######

        # Create file with metadata specific for each experiment
        txt_file_name = timelapse_id + "_metadata.txt"
        txt_file = open(txt_file_name, 'w')
        metadata = "Used line: " + wrm_line + "\n" + \
                   "Previously culture at (degrees Celsius): " + tmp + "\n" + \
                   "Cultured since: " + date + "\n" + \
                   "Timelapse plate type: " + plate_type + "\n" + \
                   "Pate batch: " + plate_batch + "\n" + \
                   "Number of worms on the plate: " + number_worms + "\n" + \
                   "Time elapsed previous to timelapse start (minutes): " + time_elapsed + "\n" + "\n" + \
                   "Number of timepoints: %s." % (self.timepoints_input.value()) + "\n" + \
                   "Timepoint length: %s." % (self.timepoint_length_input.value()) + "\n" + \
                   "Timestep is one timepoint every %s minutes." % (self.timestep_input.value()) + "\n" + \
                   "Magnification is: %s x." % (magnification) + "\n" + \
                   "Framerate is: %s fps." % (self.fps_input.value())

        txt_file.write(metadata)
        txt_file.close()
        # Copy this file to the respective folder on the workstation
        os.system('scp -r %s %s' % (txt_file_name, scp_path_on_workstation))
        self.message_output.appendPlainText("Metadata file was copied to %s." % (scp_path_on_workstation))
        QtWidgets.QApplication.processEvents()
        #######

        tp = 1
        #temporary fix needed for PiNoir camera
        class PiCamera2(picamera.PiCamera):
            AWB_MODES = {
                    "off":          mmal.MMAL_PARAM_AWBMODE_OFF,
                    "auto":         mmal.MMAL_PARAM_AWBMODE_AUTO,
                    "greyworld":    ct.c_uint32(10)
                    
                    }


        while tp <= self.timepoints_input.value():
            with PiCamera2() as camera:
                camera.awb_mode = "greyworld"
                camera.resolution = (1024, 1024)
                camera.framerate = self.fps_input.value()
                folder = '%s%d' % ('tp_', tp)
                self.message_output.appendPlainText("\nCreating " + folder)
                if not os.path.exists(folder):
                    os.makedirs(folder)
                os.chdir(folder)
                file_name = '%s%d.h264' % ('tp_', tp)
                start = time.time()
                self.message_output.appendPlainText('Recording now timepoint %d' % (tp))
                camera.start_recording(file_name)
                self.message_output.appendPlainText('Camera started at ' + time.ctime())
                QtWidgets.QApplication.processEvents()
                camera.wait_recording(self.timepoint_length_input.value())
                self.message_output.appendPlainText('Recording done at ' + time.ctime())
                camera.stop_recording()
                self.message_output.appendPlainText('Recording stoped at ' + time.ctime())
                QtWidgets.QApplication.processEvents()
            os.chdir('..')
            self.message_output.appendPlainText('Now starting copying process')
            os.system('scp -r  %s %s' % (folder, scp_path_on_workstation))
            self.message_output.appendPlainText('%s was copied.' % (folder))
            if self.keep_videos_input.isChecked() == False:
                try:
                    shutil.rmtree(folder)
                    self.message_output.appendPlainText('Timepoint %s was deleted from RaspberryPi SD card.' % (folder))
                except OSError as e:
                    self.message_output.appendPlainText("Error: %s - %s." % (e.filename, e.strerror))
                finish = time.time()
            if tp < self.timepoints_input.value():
                #for i in range(self.timestep_input.value() * 60 - int(finish - start)):
                    #self.message_output.appendPlainText("\rWaiting for %d seconds." % (self.timestep_input.value() * 60 - int(finish - start) - i))
                    #sys.stdout.flush()
                self.message_output.appendPlainText("\rWaiting for %d seconds." % (self.timestep_input.value() * 60))
                QtWidgets.QApplication.processEvents()
                time.sleep(self.timestep_input.value() * 60)
                #sys.stdout.write("\n")
            if tp == self.timepoints_input.value():
                self.message_output.appendPlainText("DONE")
                os.chdir('/home/pi')
            QtWidgets.QApplication.processEvents()
            tp += 1
            if self.stop:
                self.stop = True
                break



if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    window = MyApp()
    window.showMaximized()
    sys.exit(app.exec_())

