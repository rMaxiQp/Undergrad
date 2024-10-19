FROM python:3

WORKDIR /usr/src/app

RUN pip install opencv-python

RUN pip install --index-url https://test.pypi.org/simple/ --no-deps asciicvtv

COPY . .

CMD ["python", "./EmailServer/Server.py"]