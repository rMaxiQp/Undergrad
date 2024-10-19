# ACSII Video Converter

An ACSII video converter can convert an image or a video into ASCII format.<br>

## HOW TO USE (for general user)

1. Send email attaching the image/video want to convert to 'ASCII.video.server@gmail.com'<br>
2. There will be email automatically sent back to inform if the attachment file is valid or not.<br>
3. After processed, there will be an email with link sent back. Open link to download the final product (login is needed).<br>

## Developer Guide

### Getting Started

It is recommended to run this application within a `virtualenv` virtual environment

To setup a virtual environment, please refer to [official documentation](https://docs.python.org/3/library/venv.html)

Current version of this package depend on following requirements:

- [Python3](https://realpython.com/installing-python/)
- [openCV](https://pypi.org/project/opencv-python/)
- [colorama](https://pypi.org/project/colorama/)

### Install Package

To install the package within another application, open your Command Line Interface and execute either of the following command:

```
python3 -m pip install --index-url https://test.pypi.org/simple/ --no-deps asciicvtv

pip install --index-url https://test.pypi.org/simple/ --no-deps asciicvtv
```

## Support IO

Input: Any format of image or video.<br>
Output: Terminal output package. Image/Video.<br>

## Require library

`smtplib`<br>
`imaplib`<br>
`cv2(openCV)`<br>
`requests`<br>
`mimetypes`<br>
`colorama`<br>

## Project Structure

This ASCII converter project is mainly divided into three components.<br>
**Email Server**<br>
**Converter**<br>
**Cloud Storage**<br>

### Email Server

**IMAP Receiver**<br>
Walk through unread email. Extract attachment metadata to check if format is valid and under size limit. Download to local for converter. <br><br>
**SMTP Sender**<br>
If the incoming file is valid, reply email to sender to inform the file has been received.<br>
If the file is invalid, reply sender with error message.<br><br>

### Converter

**Decompose Video**<br>
**Frame converting**<br>
Frame will be parsed into 2D array depends on pixel's grey intensity.<br>
Depends on its grey intensity, the pixel maps to different ASCII character.<br>
The converted array can be written into txt file.

### Cloud Storage

**Authenication**<br>
The Authenication for the storage includes:<br>
Use credential get _API key_<br>
_OAuth2_ get access token <br>
_Bearer token_ to make request<br><br>
**Storage Structure**<br>
Folders and files are stored in tree-like data structure. <br>
There is a fixed root folder file. <br>
Each node has its own unique id. <br><br>
**Share Link**
Share link will be created and send back to client to download. <br>
The file will be deleted after download or expire after limit time. <br><br>
