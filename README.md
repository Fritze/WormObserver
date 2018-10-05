# The WormObserver

For any question, please drop me a line at:
Friedrich.Preusser[at]mdc-berlin.de

![Sketch](https://github.com/Fritze/WormObserver/blob/master/Images/WormObserver_sketch.png)

# A lightweight solution for adding a Raspberry Pi Camera to a stereoscopic microscope

## The set-up

![Parts](https://github.com/Fritze/WormObserver/blob/master/Images/Overview_WormObserver.jpg)

This is an adapted version of an adapter allowing coupling of a standard Raspberry Pi camera to a Zeiss Stereo Discover V8 microscope. However, by changing the inner diameter of the eyepiece sleeve accordingly, the design can easily adjusted to any other microscope.
The design is inspired by [this](https://www.thingiverse.com/thing:2007339) design, previously published on the open source thingiverse platform.
The Raspberry Pi camera can be either glued directly to the printed holder or fixed with screws through the dedicated holes on the camera chip.

![Set-up](https://github.com/Fritze/WormObserver/blob/master/Images/Microscope_WormObserver.jpg)

### Attaching the camera to the Raspberry Pi

To allow convenient allignment of the camera with respect to the focal plane of the objective, it is recommended to use [the longer connection cable](https://www.adafruit.com/product/1731) when connecting the camera to the Raspberry Pi. 

### Alligning the camera

It is recommended to allign the camera via the live preview mode directly within the Raspberry Pi GUI. To allow a maximum of flexibility, the Rasperry Pi can be directly coupled to a 7'' Touchscreen Display and a keyboard. To protect the display from the potential hazardous environment of a typical benchspace a custom [3D-printable case is available on thingiverse](https://www.thingiverse.com/thing:1585924).


### Cost estimation (excluding the microscope)

35$ - [Raspberry Pi](https://www.adafruit.com/product/3775)

30$ - [Raspberry Pi Camera](https://www.adafruit.com/product/3099)

3$ - [Camera Flex Cable](https://www.adafruit.com/product/1731)

80$ - [7'' Touchscreen Display](https://www.adafruit.com/product/2718)

Total. 148$

## Image analysis pipeline

![workflow](https://github.com/Fritze/WormObserver/blob/master/Images/WormObserver_workflow_sketch.png)

![KNIME](https://github.com/Fritze/WormObserver/blob/master/Images/WormObserver_screen.png)

![analysis](https://github.com/Fritze/WormObserver/blob/master/Images/analysis_overview.png)
