import cv2
import numpy as np
import logging
import sys
import traceback

# Config logger
FORMAT = '[ %(asctime)-15s ][%(levelname)s] %(message)s'
logging.basicConfig(stream=sys.stderr, format=FORMAT)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

try:
    from .ImageProcessor import ImageProcessor
except ModuleNotFoundError as e:
    logger.critical('BROKEN PACKAGE')
    logger.critical(traceback.format_exc())
    logger.critical(e)
    sys.exit(1)




"""
Output converted image.
Support different granularity and font.
"""


class ImageOutput(ImageProcessor):
    def __init__(self, image_name, granularity=1):
        """
        Read granularity and resize to corresponding new size.
        """
        super().__init__(image_name)
        if granularity <= 0 or granularity > self.rows or granularity > self.cols:
            logger.error("Granularity is invalid. Reset to default value(1).")
            granularity = 1
        self.granularity = granularity
        self.output_image = np.zeros((self.cols, self.rows, 3), np.uint8)
        self.rows = int(self.rows/granularity)
        self.cols = int(self.cols/granularity)

        super().adapt_window(self.cols, self.rows)

    def convert(self):
        super(ImageOutput, self).convert()

    def generate_output_image(self, font=cv2.FONT_HERSHEY_SIMPLEX):
        """
        Param font: font of ASCII characters.
        Write converted image.
        """

        space = 0.028 * self.granularity
        color = (255, 255, 255)
        for c in range(self.cols):
            for r in range(self.rows):
                cv2.putText(self.output_image, self.output[r][c],
                            (c*self.granularity, r*self.granularity),
                            font, space, color, 1, cv2.LINE_AA)

    def output_txt(self, output_name='output.txt'):
        """
        Param output_name: output txt file name. (Default: output.txt)
        """
        if output_name != '' and output_name.split('.')[1] != 'txt':
            logger.error('Output file has incorrect format.')
        with open(output_name, 'w') as f:
            for n in self.output:
                f.write("%s\n" % ' '.join(n))

    def show_output(self):
        """
        Display new image.
        """
        cv2.imshow("Output", self.output_image)
        cv2.waitKey(0)
        cv2.destroyAllWindows()

    def write_output(self, output_name):
        """
        Param output_name: output file name.
        """
        cv2.imwrite(output_name, self.output_image)
