import serial
import csv
import os

#convert raw gps data to decimal latitude and longitude
def decode(lat,long,dir): 
    if lat != '':
        seg = lat.split(".")
        if len(seg) < 2:
            return ''
        degree1 = seg[0][0:2]
        degree2 = seg[0][2:]
        digit = seg[1]
        minute = float(degree2+'.'+digit)
        ten_digit = float(degree1)+minute/60
        if dir == "S":
            ten_digit = -ten_digit
    else:
        seg = long.split(".")
        if len(seg) < 2:
            return ''
        degree1 = seg[0][0:3]
        degree2 = seg[0][3:]
        digit = seg[1]
        minute = float(degree2+'.'+digit)
        ten_digit = float(degree1)+minute/60
        if dir == 'W':
            ten_digit = -ten_digit
    return ten_digit


def parse(data):
    if data[0:6] == "$GPGGA":
        record = data.split(",")
        satellite = record[7]
        timestamp = record[1][0:2] + ":" + record[1][2:4] + ":" + record[1][4:6]
        latitude = str(decode(record[2],'',record[3]))
        latitude_dir = record[3]
        longitude = str(decode('',record[4],record[5]))
        longitude_dir = record[5]
        altitude = record[9] + " m"

        print("Time(UTC): %s | Latitude: %s(%s) | Longitude:%s(%s)   Altitude:%s   (%s satellites)"
              % (timestamp, latitude, latitude_dir, longitude, longitude_dir, altitude, satellite))

        return [timestamp, latitude, latitude_dir, longitude, longitude_dir, altitude, satellite]

def read_logging(file):
    with open('GPSData.csv', mode='w') as GPSData:
        GPS_data_csv = csv.writer(GPSData, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        GPS_data_csv.writerow(['Timestamp', 'Latitude', 'LatitudeDirection', 'Longitude', 'LongitudeDirection',
                               'Altitude', 'SatelliteNo.'])
        with open(file, 'r') as f:
            for line in f:
                record = parse(line)
                if record and record[0][0] != ':' and record[1]!='' and record[3]!='':
                    # only write the record with timestamp
                    GPS_data_csv.writerow(record)


def read_serial():
    port = "/dev/ttyS0"
    serial_data = serial.Serial(port, baudrate=9600, timeout=0.5)
    while True:
        record = serial_data.readline()
        parse(record)


if __name__ == '__main__':
    read_logging('dataset/gpspipe_5_1.log')
