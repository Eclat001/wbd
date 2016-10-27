'''
Created on Oct 26, 2016

@author: eclat
'''
from genericpath import isfile
import datetime
import time
import xml.etree.ElementTree as ET
import math
from operator import itemgetter

class Fix():
    def __init__(self, logFile = 'log.txt'):
        self.logFile = logFile
        self.sightingFile = None
        if not isinstance(logFile, str):
            raise ValueError('Fix.__init__:  "logFile" is a string')
        if (len(logFile) < 1):
            raise ValueError('Fix.__init__:  "logFile" should have a length .GE. 1')
        if not isfile(logFile):
            try:
                log = open(logFile, 'w')
            except:
                raise ValueError('Fix.__init__:  "logFile" can not be created')            
        else:
            try:
                log = open(logFile, 'a')    
            except:
                raise ValueError('Fix.__init__:  "logFile" can not be opened for appending')
        logEntry = self.message("Start of log")
        try:
            log.write(logEntry)
        except:
            raise ValueError('Fix.__init__:  "logFile" can not be appended')
        log.close()
    
    def message(self, message):
        start = "LOG:\t"
        date = datetime.datetime.now().strftime("%y-%m-%d %H:%M:%S")
        timezone = "-06:00\t"
        result = start + date + timezone + message + "\n"
        return result


    def setSightingFile(self, sightingFile = None):
        if sightingFile == None:
            raise ValueError('Fix.setSightingFile:  "sightingFile" can not be empty')
        if not isinstance(sightingFile, str):
            raise ValueError('Fix.setSightingFile:  the file name violates the parameter specification')
        extend = sightingFile[-4:]
        if not extend == ".xml":
            raise ValueError('Fix.setSightingFile:  the file name violates the parameter specification')
        if len(sightingFile) < 5:
            raise ValueError('Fix.setSightingFile:  the file name violates the parameter specification')
        try:
            sighting = open(sightingFile, 'r')
        except:
            raise ValueError('Fix.setSightingFile:  the file can not be opened')
        logEntry = self.message("Start of sighting file:\t" + sightingFile)
        try:
            log = open(self.logFile, 'a+') 
        except:
            raise ValueError('Fix.setSightingFile:  "logFile" can not be opened for appending')
#         log = open(self.logFile, 'a+') 
        try:
            log.write(logEntry)
        except:
            raise ValueError('Fix.setSightingFile:  "logFile" can not be appended')
        
        log.close()    
        sighting.close()
        self.sightingFile = sightingFile
        return sightingFile

    def getSightings(self):
        approximateLatitude = "0d0.0"
        approximateLongitude = "0d0.0"
        try:
            log = open(self.logFile, 'a+') 
        except:
            raise ValueError('Fix.getSightings:  "logFile" can not be opened for appending')
        sighting_tuples = []
        try:
            tree = ET.parse(self.sightingFile)
        except:
            raise ValueError('Fix.getSightings:  "sightingFile" does not exist')
        root = tree.getroot()
        if root.tag == "fix":
            for child in tree.findall("sighting"):
                #body content
                bodyTag = child.find("body")
                if bodyTag == None:
                    raise ValueError('Fix.getSightings:  "body" tag is missing')
                else:
                    body = bodyTag.text
                    if body == None:
                        raise ValueError('Fix.getSightings:  "body" content is missing')
                #date content
                dateTag = child.find("date")   
                if dateTag == None:
                    raise ValueError('Fix.getSightings:  "date" tag is missing')
                else:
                    date = dateTag.text
                    try:
                        time.strptime(date, "%Y-%m-%d")  
                    except:  
                        raise ValueError('Fix.getSightings:  "date" is wrong formated')   
                #time content
                timeTag = child.find("time")
                if timeTag == None:
                    raise ValueError('Fix.getSightings:  "time" tag is missing')
                else:
                    timee = timeTag.text
                    try:
                        time.strptime(timee, "%H:%M:%S")
                    except:
                        raise ValueError('Fix.getSightings:  "time" is wrong formated') 
                #observation content   
                observationTag = child.find("observation")   
                if observationTag == None:
                    raise ValueError('Fix.getSightings:  "observation" tag is missing')  
                else:
                    observation = observationTag.text
                    if observation.find("d") == -1:
                        raise ValueError('Fix.getSightings:  missing separator')
                    xdy = observation.split("d")
                    if len(xdy) != 2:
                        if observation.find("d") == 0:
                            raise ValueError('Fix.getSightings:  missing degrees')
                        if observation.find("d") == len(observation) - 1:
                            raise ValueError('Fix.getSightings:  missing minutes')
                    else:
                        try:
                            xdy[0] = int (xdy[0])
                        except:
                            raise ValueError('Fix.getSightings:  degree is an integer')
                        if xdy[0] < 0:
                            raise ValueError('Fix.getSightings:  degree must be positive')
                        if xdy[0] >= 90:
                            raise ValueError('Fix.getSightings: degree must be less than 90')
                        try:
                            xdy[1] = float (xdy[1])
                        except:
                            raise ValueError('Fix.getSightings:  minute is an integer or float')
                        if xdy[1] < 0.0:
                            raise ValueError('Fix.getSightings:  minute must be positive')
                        if xdy[1] >= 60.0:
                            raise ValueError('Fix.getSightings: degree must be less than 90')
                        if xdy[1] * 10 % 1 != 0:
                            raise ValueError('Fix.getSightings:  minutes must have only one decimal place')
                        if xdy[0] == 0 and xdy[1] < 0.1:
                            raise ValueError('Fix.getSightings:  altitude is extremely small')
                        observationAltitude = xdy[0] + xdy[1] / 60.0
                #height content
                heightTag = child.find("height")
                if heightTag == None:
                    height = 0
                else:
                    height = heightTag.text
                    if height == "":
                        height = 0
                    try:
                        height = float (height)
                    except:
                        raise ValueError('Fix.getSightings:  height must be a numeric')
                    if height < 0:
                        raise ValueError('Fix.getSightings:  height must be .GE. 0')
                #temperature content
                temperatureTag = child.find("temperature")
                if temperatureTag == None:
                    temperature = 72
                else:
                    temperature = temperatureTag.text
                    if temperature == "":
                        temperature = 72
                    try:
                        temperature = int (temperature)
                    except:
                        raise ValueError('Fix.getSightings:  temperature must be an integer')
                    if temperature < -20:
                        raise ValueError('Fix.getSightings:  temperature must be .GE. -20')
                    if temperature > 120:
                        raise ValueError('Fix.getSightings:  temperature must be .LE. 120')
                #pressure content
                pressureTag = child.find("pressure")
                if pressureTag == None:
                    pressure = 1010
                else:
                    pressure = pressureTag.text
                    if pressure == "":
                        pressure = 1010
                    try:
                        pressure = int (pressure)
                    except:
                        raise ValueError('Fix.getSightings:  pressure must be an integer')
                    if pressure < 100:
                        raise ValueError('Fix.getSightings:  pressure must be .GE. 100')
                    if pressure > 1100:
                        raise ValueError('Fix.getSightings:  pressure must be .LE. 1100')
                #horizon content
                horizonTag = child.find("horizon")
                if horizonTag == None:
                    horizon = "natural"
                else:
                    horizon = horizonTag.text
                    if horizon == "":
                        horizon = "natural"
                    horizon = horizon.lower()
                    if horizon != "artificial" and horizon != "natural":
                        raise ValueError('Fix.getSightings:  horizon must be either"artificial" or "natural"')
                    
                if horizon == "natural":
                    dip = (-0.97 * math.sqrt(height)) / 60
                else:
                    dip = 0
                Ctemperature = (temperature - 32) / 1.8   
                refraction = (-0.00452 * pressure) / (273 + Ctemperature) / math.tan(observationAltitude * math.pi / 180.0)
                adjustedAltitude = observationAltitude + dip + refraction
                degree = int (adjustedAltitude)
                minute = round((adjustedAltitude - degree) * 60, 1)
                altitude = str (degree) + "d" + str (minute)
                
                sighting = (body, date, timee, altitude)
                sighting_tuples.append(sighting)
            sighting_tuples = sorted(sighting_tuples, key = itemgetter(1, 2, 0))
#             print sighting_tuples
#             print ("-----------------------")
            for s in sighting_tuples:
                message = self.readtuple(s)
                logEntry = self.message(message)
                try:
                    log.write(logEntry)
                except:
                    raise ValueError('Fix.getSightings:  "logFile" can not be appended')
                                
        endEntry = self.message("End of sighting file:\t" + self.sightingFile)
        try:
            log.write(endEntry)
        except:
            raise ValueError('Fix.setSightingFile:  "logFile" can not be appended')
        log.close()
        return (approximateLatitude,approximateLongitude)
    
    def readtuple(self, tuples):
        message = ""
        for item in tuples:
            message = message + str(item) + "\t"
        return message.rstrip()