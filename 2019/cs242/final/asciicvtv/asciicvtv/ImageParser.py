# loading built-in modules
import os
import sys
import time
import shutil
import traceback
import logging
from zipfile import ZipFile

# Config logger
FORMAT = '[ %(asctime)-15s ][%(levelname)s] %(message)s'
logging.basicConfig(stream=sys.stderr, format=FORMAT)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

# Loading other modules
try:
    import cv2
except ImportError as e:
    logger.critical(traceback.format_exc())
    logger.critical(e)
    sys.exit(1)

# Loading local modules
try:
    from .ImageOutput import ImageOutput
except ModuleNotFoundError as e:
    logger.critical('BROKEN PACKAGE')
    logger.critical(traceback.format_exc())
    logger.critical(e)
    sys.exit(1)


class ImageParser:

    def __init__(self, path):
        full_name = os.path.join(os.getcwd(), path)
        _, extension = os.path.splitext(full_name)

        if extension not in ['.mp4', '.jpg']:
            raise NotImplementedError

        self.__attr = {
            'path': full_name,
            'video': extension == '.mp4',
            'granularity': 1,
            'fps': 24,
            'terminal': False,
            'sleep': 0,
            'frames': None,
            'second': None,
            'name': 'output'
        }

    def set(self, key, value):
        """
        modify attr[key] with given value
        :param key: key of modifying attr
        :param value:  new value
        :return: None
        """
        if hasattr(self.__attr, key):
            self.__attr[key] = value
        else:
            raise NotImplementedError

    def get(self, key):
        """
        Getter function
        :param key: key of the attr
        :return: value of the given key
        """
        return self.__attr[key]

    def __unzip_with_extraction(self, directory):
        """
        A private function used to unzip video with apply background subtraction
        :param directory: target directory for extracted frames
        :return: None
        """
        logger.debug('Start converting Video into Frames ...')

        start = time.time()
        try:
            cap = cv2.VideoCapture(self.__attr['path'])

            # take total frame count of the input video
            max_frame = cap.get(cv2.CAP_PROP_FRAME_COUNT)

            # Set background subtraction mask
            kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (2, 2))
            mask = cv2.createBackgroundSubtractorKNN()

            logger.debug('Press Q to End Capturing Video...')

            for count in range(int(max_frame)):

                if cv2.waitKey(1) & 0xFF == ord('q'):
                    break

                if cap.isOpened():
                    _, frame = cap.read()

                    background = mask.apply(frame)
                    background = cv2.morphologyEx(background, cv2.MORPH_OPEN, kernel)

                    frame[background < 128] = 255

                    cv2.imshow('window', frame)
                    cv2.imwrite(os.path.join(directory, 'frame%d.jpg' % count), frame)
                else:
                    logger.error('Video is Closed by Exception')
                    break
            cap.release()
            cv2.destroyAllWindows()
        except Exception as e:
            logger.error(traceback.format_exc())
            logger.error(e)

        end = time.time()

        logger.debug('Convert to frames used: {0} seconds'.format(end - start))

    def __unzip_without_extraction(self, directory):
        """
        A private function used to unzip video without apply background subtraction
        :param directory: target directory for extracted frames
        :return: None
        """
        logger.debug('Start converting Video into Frames ...')

        start = time.time()
        try:
            cap = cv2.VideoCapture(self.__attr['path'])

            # take total frame count of the input video
            max_frame = cap.get(cv2.CAP_PROP_FRAME_COUNT)

            logger.debug('Press Q to End Capturing Video...')

            for count in range(int(max_frame)):

                if (cv2.waitKey(1) & 0xFF) == ord('q'):
                    break

                if cap.isOpened():
                    _, frame = cap.read()
                    cv2.imshow('window', frame)
                    cv2.imwrite(os.path.join(directory, 'frame%d.jpg' % count), frame)
                else:
                    logger.error('Video is Closed by Exception')
                    break

            cap.release()
            cv2.destroyAllWindows()
        except Exception as e:
            logger.error(traceback.format_exc())
            logger.error(e)

        end = time.time()

        logger.debug('Convert to frames used: {0} seconds'.format(end - start))

    def unzip(self, extraction=False):
        """
        This function is used to unzip video into individual frames using openCV2,
        extracted frames will be dumped into directory [ frames ]
        :param extraction: To apply background subtraction or not. Default as False
        :return: None
        """
        cwd = 'frames'

        if not self.__attr['video']:
            raise ValueError('Input file is not a Video file')

        if os.path.exists(cwd):
            shutil.rmtree(cwd)

        os.mkdir(cwd)

        if extraction:
            self.__unzip_with_extraction(cwd)
        else:
            self.__unzip_without_extraction(cwd)

    def __convert_image(self):
        """
        A private function used to convert original single image into ASCII image in forms of .txt and .jpg
        :return: None
        """
        start = time.time()
        logger.debug('Convert regular image to ASCII image')
        full_name = self.__attr['path']
        image = ImageOutput(full_name)
        image.convert()
        image.generate_output_image()
        image.write_output('.')
        end = time.time()

        logger.info('Convert single ASCII image used: {0} seconds'.format(end - start))

    def __convert_video(self):
        """
        A private function used to convert original frames into ASCII frames in forms of .txt and .jpg.
        It reads from directory [frames] and write converted image and text files to directory [output]
        :return: None
        """
        logger.debug('Start convert Frames to ASCII images...')
        start = time.time()
        cwd = 'frames'
        if not os.path.exists(cwd):
            raise ValueError('Directory not found')

        frames = os.listdir(cwd)

        new_cwd = 'output'
        if os.path.exists(new_cwd):
            shutil.rmtree(new_cwd)

        os.mkdir(new_cwd)

        for frame in frames:
            full_path = os.path.join(cwd, frame)
            image = ImageOutput(full_path, self.__attr['granularity'])
            image.convert()
            image.generate_output_image()
            image.write_output(os.path.join(new_cwd, frame))

        end = time.time()

        logger.info('Convert ASCII images used: {0} seconds'.format(end - start))

    def convert(self):
        """
        This function is used to convert original video frames or single image into ASCII forms
        :return: None
        """

        if not self.__attr['video']:
            self.__convert_image()
        else:
            self.__convert_video()

    def __zip_video(self):
        """
        A private function used to read converted frames from directory [ output ]
        and write into a .mp4 format video file with given output name, DEFAULT as output
        :return: None
        """
        start = time.time()
        cwd = 'output'
        frames = os.listdir(cwd)
        frames.sort(key=lambda x: int(''.join(filter(str.isdecimal, x))))

        img_frames = list(filter(lambda x: x[-4:] == '.jpg', frames))
        frame = cv2.imread(os.path.join(os.getcwd(), cwd, img_frames[0]))
        height, width, _ = frame.shape

        fps = self.__attr['fps']
        output = self.__attr['name']

        logger.debug('Start convert ASCII images to Video')

        # Configure Video Writer
        four_cc = cv2.VideoWriter_fourcc(*'mp4v')
        out = cv2.VideoWriter((output + '.mp4'), four_cc, float(fps), (width, height))

        # Write to video
        for frame in img_frames:
            frame_path = os.path.join('output', frame)
            img = cv2.imread(frame_path)
            out.write(img)

        out.release()
        cv2.destroyAllWindows()

        end = time.time()
        logger.info('Covert to Video used: {0} seconds'.format(end - start))

    def __zip_frame(self):
        """
        A private function used to read text files from directory [ output ]
        and zip them and the converted video file into a .zip file
        :return: None
        """
        start = time.time()

        logger.debug('Start zip frames')

        cwd = 'output'
        frames = os.listdir(cwd)
        frames.sort(key=lambda x: int(''.join(filter(str.isdecimal, x))))

        output = self.__attr['name']

        img_frames = list(filter(lambda x: x[-4:] == '.jpg', frames))

        # Zip txt files
        logger.debug('Start zipping files')

        with ZipFile('output.zip', 'w') as my_zip:
            for img in img_frames:
                my_zip.write(os.path.join(cwd, txt))
            my_zip.write(output + '.mp4')
        end = time.time()
        
        # Remove video file since it is included in zip file
        os.remove(output + '.mp4')

        logger.info('Zip txt files used: {0} seconds'.format(end - start))

    def zip(self):

        if not self.__attr['video']:
            raise ValueError('Input file is not a Video file')

        self.__zip_video()

        self.__zip_frame()

        if os.path.exists('frames'):
            shutil.rmtree('frames')

        if os.path.exists('output'):
            shutil.rmtree('output')
