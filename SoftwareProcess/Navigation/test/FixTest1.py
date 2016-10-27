import unittest
import Navigation.prod.Fix as F

class FixTest(unittest.TestCase):
    def setUp(self):
        self.className = "Fix."
    def tearDown(self):
        pass 
    
#    Acceptance Test: 100
#        Analysis - Constructor
#            inputs
#                logFile
#            outputs
#                instance of Fix
#            state change
#                write "Start of log" to the log file
#
#            Happy path
#                nominal case for string:  F()
#                nominal case for string:  F(normal.txt)
#                nominal case for "Start of log" be written in
#            Sad path
#                logFile
#                    logFile cannot be opended or created
#                    not a string:  F(123)
#                    empty string: F("")                   
#    Happy path
    def test100_010_ShouldCreatInstanceOfFix(self):
        self.assertIsInstance(F.Fix(), F.Fix) 
    def test100_020_ShouldCreateInstanceOfFix(self):
        self.assertIsInstance(F.Fix("normal.txt"), F.Fix)
    def test100_030_ShouldWriteStartLogToLogFile(self):
        aF = F.Fix("normal.txt")
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("Start of log")
        self.assertEquals(last_line, expectedLine)
#    Sad path
    def test100_910_ShouldRaiseExceptionOnEmtpyString(self):
        expectedDiag = self.className + "__init__:"
        with self.assertRaises(ValueError) as context:
            aF = F.Fix("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test100_920_ShouldRaiseExceptionOnNonString(self):
        expectedDiag = self.className + "__init__:"
        with self.assertRaises(ValueError) as context:
            aF = F.Fix(123)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
#-----------------------------------------------------------------
#    Acceptance Test: 200
#        Analysis - setSightingFile
#            inputs
#                sightingFile expressed as a string in form of x.xml
#                    x has a length .GE. 1
#                    .xml is literal
#            outputs
#                a string having the value passed as the "sightingFile"
#            state change
#                write entry log "Start of sighting file f.xml" to the log file
#
#            Happy path
#                nominal case for string:  setSightingFile("sighting.xml")
#                nominal case for "Start of sighting file" be written in
#            Sad path
#                sightingFile
#                    not string:  setSightingFile(123)
#                    empty string:  setSightingFile("")
#                    violate .xml specification:  setSighting("t13w")
#                    violate .xml specification:  setSighting("t2.w")
#                    violate .xml specification:  setSighting("t3.txt")
#                    violate x specification:  setSighting(".xml")
#    Happy path    
    def test200_010_ShouldReturnString(self):
        aF = F.Fix()  
        self.assertIsInstance(aF.setSightingFile("sightingFile.xml"), str)
    def test200_020_ShouldWriteStartLogToLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("Start of sighting file:\tsightingFile.xml")
        self.assertEquals(last_line, expectedLine)
#    Sad path
    def test200_910_ShouldShouldRaiseExceptionOnEmtpyString(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_920_ShouldShouldRaiseExceptionOnNonString(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile(123)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_930_ShouldShouldRaiseExceptionOnMissingExtension(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile("t13w")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_940_ShouldShouldRaiseExceptionOnShortExtension(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile("t2.w")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_950_ShouldShouldRaiseExceptionOnWrongExtension(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile("t3.txt")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_960_ShouldShouldRaiseExceptionOnMissingNameOfTheFile(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile(".xml")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
#-----------------------------------------------------------------
#    Acceptance Test: 300
#        Analysis - getSightings
#            inputs
#                none
#            outputs
#                a tuple consisting of the latitude and longitude of the approximate location
#            state change
#                Navigational calculations are written to the log file
#
#            Happy path
#                nominal case for return (0d0.0, 0d0.0)
#                nominal case for height missing, "End of sighting file" be written in
#                nominal case for temperature missing, "End of sighting file" be written in
#                nominal case for pressure missing, "End of sighting file" be written in
#                nominal case for horizon missing, "End of sighting file" be written in
#                nominal case for "End of sighting file" be written in
#                nominal case: log are sorted and written in
#            Sad path
#                "body" tag is missing
#                "date" tag is missing
#                "time" tag is missing
#                "observation" tag is missing
#                "body" content is an empty string
#                "date" content is not in yyyy-mm-dd format
#                "time" content is not in hh:mm:ss format
#                "observation" content is not in xdy.y format where
#                    x is an integer .GE. 0 and .LT. 90 :
#                        x is missing
#                        x is not an integer, e.g. a float
#                        x is an integer <0
#                        x is an integer >=90
#                    d is a literal :
#                        d is missing
#                    y.y can be either integer or float with one decimal .GE. 0.0 and .LT. 60.0 :
#                        y.y is missing
#                        y.y <0.0
#                        y.y >=60.0
#                "height" content is not a numeric
#                "height" content is a numeric but not .GE. 0
#                "temperature" content is not an integer, e.g. a string, a float
#                "temperature" content is an integer, but not in the range .GE. -20 and .LE. 120
#                "pressure" content is not an integer, e.g. a string, a float
#                "pressure" content is an integer, but not in the range .GE. 100 and .LE. 1100
#                "horizon" content is a string, but case-insensitive neither "artificial" nor "natural"
#                "observation" altitude is .LT. 0d0.1
#    Happy path      
    def test030_010_ShouldReturn0d0(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        self.assertEquals(aF.getSightings(), ("0d0.0","0d0.0"))
    def test030_020_ShouldWriteEndOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("End of sighting file:\tsightingFile.xml")
        self.assertEquals(last_line, expectedLine)
    def test030_030_HeightMissingShouldWriteEndOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_heightmissing.xml")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("End of sighting file:\tsightingFile_heightmissing.xml")
        self.assertEquals(last_line, expectedLine)
    def test030_040_TemperatureMissingShouldWriteEndOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_temperaturemissing.xml")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("End of sighting file:\tsightingFile_temperaturemissing.xml")
        self.assertEquals(last_line, expectedLine)
    def test030_050_PressureMissingShouldWriteEndOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_pressuremissing.xml")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("End of sighting file:\tsightingFile_pressuremissing.xml")
        self.assertEquals(last_line, expectedLine)
    def test030_060_HorizonMissingShouldWriteEndOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_horizonmissing.xml")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("End of sighting file:\tsightingFile_horizonmissing.xml")
        self.assertEquals(last_line, expectedLine)
    def test030_070_ShouldWriteSortedLogOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
            last_thirdline = lines[-3]
        f.close()
        expectedSecondLine = aF.message("Peacock\t2016-03-02\t00:05:05\t45d11.9")
        expectedThirdLine = aF.message("Aldebaran\t2016-03-01\t23:40:01\t15d1.5")
        self.assertEquals(last_secondline, expectedSecondLine)
        self.assertEquals(last_thirdline,expectedThirdLine)
#    Sad path
    def test030_910_TagBodyIsMissing(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_bodymissing.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test030_920_TagBodyBad(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_bodybad.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test030_930_TagDateIsMissing(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_datemissing.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test030_940_TagDateIsMissing(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_datebad.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test030_950_TagTimeIsMissing(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_timemissing.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test030_960_TagTimeIsMissing(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_timebad.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test030_970_TagObservationIsMissing(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationmissing.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test030_980_ObservationDIsMissing(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationdmissing.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_990_ObservationXIsMissing(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationxmissing.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_911_ObservationXBad(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationxbad.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_912_ObservationXNegative(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationxnegative.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
    def test030_913_ObservationXG90(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationxG90.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_914_ObservationYIsMissing(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationy.ymissing.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_915_ObservationYNegative(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationy.ynegative.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
    def test030_916_ObservationYG60(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationy.yG60.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_917_ObservationYBad(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationy.ybad.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
    def test030_918_HeightBad(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_heightstring.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_919_HeightNegative(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_heightnegative.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_921_TemperatureBad(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_temperaturestring.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_922_TemperatureLN20(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_temperatureL-20.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_923_TemperatureLG120(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_temperatureG120.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_924_PressureBad(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_pressurestring.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_925_PressureL100(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_pressureL100.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_926_PressureG1100(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_pressureG1100.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test030_927_HorizonBad(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_horizonbad.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test030_928_ShouldRaiseExceptionOnExtremeSmallAltitude(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile_extremesmallaltitude.xml")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
             
         