# Delphi-Navigation-Google-and-Android
 Some Projects that examine Mobile Device Sensors and GPS Navigation
## Aim of Projects 
 A smart phone is potentially packed with sensors and Delphi claims to provide cross platform access to those sensors.

This project was aimed at discovering this capability and what can be achieved.  A number of libraries have been developed in the process of looking at Mobile sensors. The plan was to make the application and libraries available as open source on GitHub. 

A major learning curve was how the user interface needs to be adapted for the very small screens.  I chose a Multi Layer Tab control template "Tabbed with Navigation" offered with Tokyo as a basis for my trail app. I had to add the functionality to support the hardware back button and was not very successful with gesture use opting in the end for Forward and Back Speed buttons so more to learn there.  The Templates were not offered with my Rio install.

First  developed was an about page which shows which sensors are available on the device. This was later expanded to enable all sensors to be turned off and then activated separately enabling examination of the battery impact of individual sensors. 

Later additional primary tabs were added, one to show primary location functions, a second to attempt to "Home" in on a static location and finally a "Map" to access Google Maps with location data.

Overall the initial program aims were not achieved. The effort of adapting to the changing mobile devices especially their increasing security demands and the apparent lack of accuracy in the GPS units defeated me. This was never really about developing a supper mobile app but more about learning what is and what is not possible. 

Along the way I developed some Delphi code I will continue to use and I think is worth sharing, I partially got my head around  how to do a UI on a mobile device and gathered lots of information on the actual capabilities of the sensors in those devices.    

## Libraries
In playing with the GPS and other mobile sensors I coded my slowly acquired knowledge into code libraries. 

They are 
* Navigation Utilities, 
* Mobile Sensor Utilities,
* FMX Graphics Utilities and 
* Android Permission Utilities.

The code for these is in the LibraryCode directory. 

## Example Projects
The main project is targeted at the Android Phone and is included as  *AndroidExamineSensors*. It runs in Windows using "Dummy" sensors incorporated in the Mobile Sensor Library. More documentation and explanation is available at http://www.innovasolutions.com.au/DownLoad_Android_Apps/AndroidHTML/index.html.

Two example Windows projects demonstrates and tests the Navigation Library and its interaction with Google.

### GoogleDirForm
Really simple form. put in Long/Lat references and get Google Directions in Browser.

### PlayGoogleTabbedForm
Compile and run in Windows, You can put any Long/Lat co-ordinates and then view them in the Browser via Google Maps or imput another co-ordinate and get not only "Google Directions" but also the Great circle distance and bearing. A few aviation navigational aids are preprogrammed by buttons and if you have the appropiate maps you can check/compare the values given.    
