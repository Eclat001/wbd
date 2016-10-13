from __builtin__ import int

class Angle():
    def __init__(self):
        self.angle = 0       #set to 0 degrees 0 minutes
    def setDegrees(self, degrees = 0.0):        
        if (not isinstance(degrees, int) and not isinstance(degrees, float)):
            raise ValueError('Angle.setDegrees:  "degrees" violates the parameter specifications')
        else:
            self.angle = float (degrees % 360)
            degree = int (self.angle)
            minute = round((self.angle - degree) * 60, 1)
            self.angle = degree + minute / 60.0
            return self.angle
                    
    def setDegreesAndMinutes(self, angleString):
        if (len(angleString) == 0):
            raise ValueError("Angle.setDegreesAndMinutes: null string")
        if (angleString.find("d") == -1):
            raise ValueError("Angle.setDegreesAndMinutes: missing separator")
        xdy = angleString.split("d")
        if (len(xdy) != 2):
            if (angleString.find("d") == 0):
                raise ValueError("Angle.setDegreesAndMinutes: missing degrees")
            if (angleString.find("d") == len(angleString) - 1):
                raise ValueError("Angle.setDegreesAndMinutes: missing minutes")   
        else:
            try:
                degree = float (xdy[0])
            except:
                raise ValueError("Angle.setDegreesAndMinutes: degrees must be an integer")    
            try:
                minute = float (xdy[1])
            except:
                raise ValueError("Angle.setDegreesAndMinutes: minutes must be an integer")

            if (degree % 1 != 0):
                raise ValueError("Angle.setDegreesAndMinutes: degrees must be an integer")
            if (minute < 0):
                raise ValueError("Angle.setDegreesAndMinutes: minutes must be positive")
            if (xdy[1].find(".") < len(xdy[1]) - 2):
                raise ValueError("Angle.setDegreesAndMinutes: minutes must have only one decimal place")  
            else:
                if degree < 0:
                    self.angle =  int ((degree + minute / 60) % 360) - minute % 60 / 60.0
                else:
                    self.angle =  int ((degree + minute / 60) % 360) + minute % 60 / 60.0
                return self.angle
    
    def add(self, angle = None):
        if angle == None:
            raise ValueError('Angle.add:  "angle" can not be empty')
        if (not isinstance(angle, Angle)):
            raise ValueError('Angle.add:  "angle" is not a valid instance of Angle')
        if (not isinstance(angle.angle, int) and not isinstance(angle.angle, float)):
            raise ValueError('Angle.add:  "angle" is not a valid instance of Angle')
        self.angle = (self.angle + angle.angle) % 360
        return self.angle
    
    def subtract(self, angle=None):
        if angle == None:
            raise ValueError('Angle.subtract:  "angle" can not be empty')
        if (not isinstance(angle, Angle)):
            raise ValueError('Angle.subtract:  "angle" is not a valid instance of Angle')
        if (not isinstance(angle.angle, int) and not isinstance(angle.angle, float) and not isinstance(angle, str)):
            raise ValueError('Angle.subtract:  "angle" is not a valid instance of Angle')
        self.angle = (self.angle - angle.angle) % 360
        return self.angle
    
    def compare(self, angle=None):
        if angle == None:
            raise ValueError('Angle.compare:  "angle" can not be empty')
        if (not isinstance(angle, Angle)):
            raise ValueError('Angle.compare:   "angle" is not a valid instance of Angle')
        if (not isinstance(angle.angle, int) and not isinstance(angle.angle, float)):
            raise ValueError('Angle.compare:   "angle" is not a valid instance of Angle')
        if (self.angle < angle.angle):
            return -1
        if (self.angle == angle.angle):
            return 0
        if (self.angle > angle.angle):
            return 1
    
    def getString(self):
        degree = int (self.angle) % 360
        minute = round((self.angle - degree) * 60, 1)
        return str (degree) + "d" + str (minute)
    
    def getDegrees(self):
        self.angle = self.angle % 360
        return self.angle       