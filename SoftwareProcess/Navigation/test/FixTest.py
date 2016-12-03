import unittest
import Navigation.prod.Fix as F
import os

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
        expectedLine = aF.message("Log file:\t" + os.path.abspath(aF.logFile))
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
#                a string whose value is the absolute filepath of the file specified by the parameter.
#            state change
#                write entry log "Sighting file:\t" + the absolute filepath to the sighting file to the log file
#
#            Happy path
#                nominal case for string:  setSightingFile("sighting.xml")
#                nominal case for "Sighting file" + path be written in
#            Sad path
#                sightingFile
#                    not string:  setSightingFile(123)
#                    empty string:  setSightingFile("")
#                    violate .xml specification:  setSightingFile("t13w")
#                    violate .xml specification:  setSightingFile("t2.w")
#                    violate .xml specification:  setSightingFile("t3.txt")
#                    violate x specification:  setSightingFile(".xml")
#    Happy path    
    def test200_010_ShouldReturnString(self):
        aF = F.Fix()  
        self.assertEquals(aF.setSightingFile("sightingFile.xml"), os.path.abspath("sightingFile.xml"))
    def test200_020_ShouldWriteStartLogToLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("Sighting file:\t" + os.path.abspath("sightingFile.xml"))
        self.assertEquals(last_line, expectedLine)
#    Sad path
    def test200_910_ShouldShouldRaiseExceptionOnEmtpyString(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_911_ShouldShouldRaiseExceptionOnNonString(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile(123)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_912_ShouldShouldRaiseExceptionOnMissingExtension(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile("t13w")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_913_ShouldShouldRaiseExceptionOnShortExtension(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile("t2.w")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_914_ShouldShouldRaiseExceptionOnWrongExtension(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile("t3.txt")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test200_915_ShouldShouldRaiseExceptionOnMissingNameOfTheFile(self):
        expectedDiag = self.className + "setSightingFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setSightingFile(".xml")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
#-----------------------------------------------------------------
#    Acceptance Test: 300
#        Analysis - setAriesFile
#            inputs
#                AriesFile expressed as a string in form of x.txt
#                    x has a length .GE. 1
#                    .txt is literal
#            outputs
#                a string whose value is the absolute filepath of the file specified by the parameter.
#            state change
#                write entry log "Aries file:\t" + the absolute filepath to the aries file to the log file
#
#            Happy path
#                nominal case for string:  setAriesFile("aries.txt")
#                nominal case for "Aries file" + path be written in
#            Sad path
#                sightingFile
#                    not string:  setAriesFile(123)
#                    empty string:  setAriesFile("")
#                    violate .txt specification:  setAriesFile("t13w")
#                    violate .txt specification:  setAriesFile("t3.xml")
#                    violate x specification:  setAriesFile(".txt")
#    Happy path    
    def test300_010_ShouldReturnString(self):
        aF = F.Fix()  
        self.assertEquals(aF.setAriesFile("aries.txt"), os.path.abspath("aries.txt"))
    def test300_020_ShouldWriteAbsolutePathToLogFile(self):
        aF = F.Fix()
        aF.setAriesFile("aries.txt")
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("Aries file:\t" + os.path.abspath("aries.txt"))
        self.assertEquals(last_line, expectedLine)
#    Sad path
    def test300_910_ShouldShouldRaiseExceptionOnEmtpyString(self):
        expectedDiag = self.className + "setAriesFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setAriesFile("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test300_911_ShouldShouldRaiseExceptionOnNonString(self):
        expectedDiag = self.className + "setAriesFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setAriesFile(123)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test300_912_ShouldShouldRaiseExceptionOnMissingExtension(self):
        expectedDiag = self.className + "setAriesFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setAriesFile("t13w")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test300_913_ShouldShouldRaiseExceptionOnWrongExtension(self):
        expectedDiag = self.className + "setAriesFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setAriesFile("t3.xml")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test300_914_ShouldShouldRaiseExceptionOnMissingNameOfTheFile(self):
        expectedDiag = self.className + "setAriesFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setAriesFile(".txt")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])       
#-----------------------------------------------------------------
#    Acceptance Test: 400
#        Analysis - setStarFile
#            inputs
#                StarFile expressed as a string in form of x.txt
#                    x has a length .GE. 1
#                    .txt is literal
#            outputs
#                a string whose value is the absolute filepath of the file specified by the parameter.
#            state change
#                write entry log "Star file:\t" + the absolute filepath to the aries file to the log file
#
#            Happy path
#                nominal case for string:  setStarFile("aries.txt")
#                nominal case for "Star file" + path be written in
#            Sad path
#                sightingFile
#                    not string:  setStarFile(123)
#                    empty string:  setStarFile("")
#                    violate .txt specification:  setStarFile("t13w")
#                    violate .txt specification:  setStarFile("t3.xml")
#                    violate x specification:  setStarFile(".txt")
#    Happy path    
    def test400_010_ShouldReturnString(self):
        aF = F.Fix()  
        self.assertEquals(aF.setAriesFile("stars.txt"), os.path.abspath("stars.txt"))
    def test400_020_ShouldWriteAbsolutePathToLogFile(self):
        aF = F.Fix()
        aF.setStarFile("stars.txt")
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("Star file:\t" + os.path.abspath("stars.txt"))
        self.assertEquals(last_line, expectedLine)
#    Sad path
    def test400_910_ShouldShouldRaiseExceptionOnEmtpyString(self):
        expectedDiag = self.className + "setStarFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setStarFile("")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test400_911_ShouldShouldRaiseExceptionOnNonString(self):
        expectedDiag = self.className + "setStarFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setStarFile(123)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test400_912_ShouldShouldRaiseExceptionOnMissingExtension(self):
        expectedDiag = self.className + "setStarFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setStarFile("t13w")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test400_913_ShouldShouldRaiseExceptionOnWrongExtension(self):
        expectedDiag = self.className + "setStarFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setStarFile("t3.xml")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)]) 
    def test400_914_ShouldShouldRaiseExceptionOnMissingNameOfTheFile(self):
        expectedDiag = self.className + "setStarFile:"
        aF = F.Fix()
        with self.assertRaises(ValueError) as context:
            aF.setStarFile(".txt")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
#-----------------------------------------------------------------
#    Acceptance Test: 500
#        Analysis - getSightings
#            inputs
#                none
#            outputs
#                a tuple consisting of the latitude and longitude of the approximate location
#            state change
#                Navigational calculations are written to the log file
#
#            Happy path
#                nominal case for approximate latitude and longitude are missing
#                nominal case for height missing, sighting error not change
#                nominal case for temperature missing, sighting error not change
#                nominal case for pressure missing, sighting error not change
#                nominal case for horizon missing, sighting error not change
#                nominal case: log are sorted and written in
#                "body" tag is missing, sighting error++
#                "date" tag is missing, sighting error++
#                "time" tag is missing, sighting error++
#                "observation" tag is missing, sighting error++
#                "body" content is an empty string, sighting error++
#                "date" content is not in yyyy-mm-dd format, sighting error++
#                "time" content is not in hh:mm:ss format, sighting error++
#                "observation" content is not in xdy.y format, sighting error++
#                   where
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
#                "height" content is not a numeric, sighting error++
#                "height" content is a numeric but not .GE. 0, sighting error++
#                "temperature" content is not an integer, e.g. a string, a float, sighting error++
#                "temperature" content is an integer, but not in the range .GE. -20 and .LE. 120, sighting error++
#                "pressure" content is not an integer, e.g. a string, a float, sighting error++
#                "pressure" content is an integer, but not in the range .GE. 100 and .LE. 1100, sighting error++
#                "horizon" content is a string, but case-insensitive neither "artificial" nor "natural", sighting error++
#                "observation" altitude is .LT. 0d0.1, sighting error++
#            Sad path
#                setSightingFile must be called before
#                setAriesFile must be called before
#                setStarFile must be called before
#                approximate latitude or longitude is not a string
#                approximate latitude is a string but not in format hxdy.y
#                approximate longitude is a string but not in format xdy.y
#    Happy path      
    def test500_010_ShouldReturnApproximateLatitudeLongitude(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        self.assertEquals(aF.getSightings("N27d59.5","85d33.4"), ("S20d15.5","79d19.9"))
#     def test500_020_ShouldWriteEndOfSightingFileInLogFile(self):
#         aF = F.Fix()
#         aF.setSightingFile("sightingFile.xml")
#         aF.setAriesFile("aries.txt")
#         aF.setStarFile("stars.txt")
#         aF.getSightings()
#         with open(aF.logFile, 'r') as f:
#             lines = f.readlines()
#             last_line = lines[-1]
#         f.close()
#         expectedLine = aF.message("End of sighting file:\tsightingFile.xml")
#         self.assertEquals(last_line, expectedLine)
    def test500_020_ShouldWriteApproximateAnlgeInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings("N27d59.5", "85d33.4")
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-1]
        f.close()
        expectedLine = aF.message("Approximate latitude:\tS20d15.5\tApproximate longitude:\t79d19.9")
        self.assertEquals(last_line, expectedLine)
    def test500_030_HeightMissingShouldWriteEndOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_heightmissing.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-2]
        f.close()
        expectedLine = aF.message("Sighting errors:\t1")
        self.assertEquals(last_line, expectedLine)
    def test500_040_TemperatureMissingShouldWriteEndOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_temperaturemissing.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-2]
        f.close()
        expectedLine = aF.message("Sighting errors:\t1")
        self.assertEquals(last_line, expectedLine)
    def test500_050_PressureMissingShouldWriteEndOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_pressuremissing.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-2]
        f.close()
        expectedLine = aF.message("Sighting errors:\t1")
        self.assertEquals(last_line, expectedLine)
    def test500_060_HorizonMissingShouldWriteEndOfSightingFileInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_horizonmissing.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_line = lines[-2]
        f.close()
        expectedLine = aF.message("Sighting errors:\t1")
        self.assertEquals(last_line, expectedLine)
#     def test500_070_ShouldWriteSortedLogOfSightingFileInLogFile(self):
#         aF = F.Fix()
#         aF.setSightingFile("sightingFile.xml")
#         aF.setAriesFile("aries.txt")
#         aF.setStarFile("stars.txt")
#         aF.getSightings()
#         with open(aF.logFile, 'r') as f:
#             lines = f.readlines()
#             last_secondline = lines[-2]
#             last_thirdline = lines[-3]
#             last_fourthline = lines[-4]
#         f.close()
#         expectedSecondLine = aF.message("Sighting errors:\t1")
#         expectedThirdLine = aF.message("Sirius\t2017-04-17\t09:30:30\t45d11.9\t-16d44.5\t247d6.2")
#         expectedFourthLine = aF.message("Pollux\t2017-04-14\t23:50:14\t15d1.5\t27d59.1\t84d33.4")
#         self.assertEquals(last_secondline, expectedSecondLine)
#         self.assertEquals(last_thirdline, expectedThirdLine)
#         self.assertEquals(last_fourthline, expectedFourthLine)
    def test500_070_ShouldWriteSightingFileEntryInLogFile(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings("N27d59.5", "85d33.4")
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_thirdline = lines[-3]
        f.close()
        expectedThirdLine = aF.message("Pollux\t2017-04-17\t23:50:14\t15d1.5\t27d59.1\t87d30.8\tN27d59.5\t85d33.4\t7d21.1\t-2919")
        self.assertEquals(last_thirdline, expectedThirdLine)
    def test500_080_TagBodyIsMissing(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_bodymissing.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
    def test500_090_TagBodyBad(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_bodybad.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
    def test500_011_TagDateIsMissing(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_datemissing.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
    def test500_012_TagDateBad(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_datebad.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
    def test500_013_TagTimeIsMissing(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_timemissing.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
    def test500_014_TagTimeIsMissing(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_timebad.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
    def test500_015_TagObservationIsMissing(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationmissing.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
    def test500_016_ObservationDIsMissing(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_observationdmissing.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
#     def test020_924_ObservationXIsMissing(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_observationxmissing.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
#     def test020_925_ObservationXBad(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_observationxbad.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
#     def test020_926_ObservationXNegative(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_observationxnegative.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
#     def test020_927_ObservationXG90(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_observationxG90.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
#     def test020_928_ObservationYIsMissing(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_observationy.ymissing.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
#     def test020_929_ObservationYNegative(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_observationy.ynegative.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
#     def test020_930_ObservationYG60(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_observationy.yG60.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
#     def test020_931_ObservationYBad(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_observationy.ybad.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
    def test500_017_HeightBad(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_heightstring.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
#     def test020_933_HeightNegative(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_heightnegative.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test500_018_TemperatureBad(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_temperaturestring.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
#     def test020_935_TemperatureLN20(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_temperatureL-20.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
#     def test020_936_TemperatureLG120(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_temperatureG120.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test500_019_PressureBad(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_pressurestring.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
#     def test020_938_PressureL100(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_pressureL100.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
#     def test020_939_PressureG1100(self):
#         expectedDiag = self.className + "setSightingFile:"
#         aF = F.Fix()
#         with self.assertRaises(ValueError) as context:
#             aF.setSightingFile("sightingFile_pressureG1100.xml")
#         self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])   
    def test500_021_HorizonBad(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_horizonbad.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
    def test500_022_ShouldRaiseExceptionOnExtremeSmallAltitude(self):
        aF = F.Fix()
        aF.setSightingFile("sightingFile_extremesmallaltitude.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        aF.getSightings()
        with open(aF.logFile, 'r') as f:
            lines = f.readlines()
            last_secondline = lines[-2]
        f.close()
        expectedSecondLine = aF.message("Sighting errors:\t2")
        self.assertEquals(last_secondline, expectedSecondLine)
    def test500_023_ShouldReturnApproximateLatitudeLongitude(self):
        aF = F.Fix()
        aF.setSightingFile("sighting.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        self.assertEquals(aF.getSightings("S53d38.4","74d35.3"), ("S13d28.0","101d42.2"))
#    Sad path
    def test500_900_ShouldRaiseExceptionOnNotSettingSightingsFile(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
    def test500_910_ShouldRaiseExceptionOnNotSettingAriesFile(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setStarFile("stars.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_920_ShouldRaiseExceptionOnNotSettingStarFile(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings()
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_930_ShouldRaiseExceptionOnAssumedLatitudeNotString(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings(123, "")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])  
    def test500_940_ShouldRaiseExceptionOnAssumedLongitudeNotString(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings("", 123)
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_950_ShouldRaiseExceptionOnAssumedLatitudeBadFormatString1(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings("34d4", "")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_960_ShouldRaiseExceptionOnAssumedLatitudeBadFormatString2(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings("N0d0.0", "")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_970_ShouldRaiseExceptionOnAssumedLatitudeBadFormatString3(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings("N34d4.12", "")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_980_ShouldRaiseExceptionOnAssumedLongitudeBadFormatString1(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings("N34d4", "N34d4")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
    def test500_990_ShouldRaiseExceptionOnAssumedLongitudeBadFormatString2(self):
        expectedDiag = self.className + "getSightings:"
        aF = F.Fix()
        aF.setSightingFile("sightingFile.xml")
        aF.setAriesFile("aries.txt")
        aF.setStarFile("stars.txt")
        with self.assertRaises(ValueError) as context:
            aF.getSightings("N34d4", "34d4.12")
        self.assertEquals(expectedDiag, context.exception.args[0][0:len(expectedDiag)])
         