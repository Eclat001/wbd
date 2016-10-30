import datetime
import time
today = datetime.date.today().strftime("%m/%d/%y")
today1 = datetime.datetime.strptime(today, "%m/%d/%y")
oneday = datetime.timedelta(days = 1)
tomorrow = today1 + oneday
tomorrow1 = tomorrow.strftime("%m/%d/%y")
print tomorrow1
