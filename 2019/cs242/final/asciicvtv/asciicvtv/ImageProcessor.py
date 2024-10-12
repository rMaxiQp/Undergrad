import cv2
import logging
import sys
import traceback

# Config logger
FORMAT = '[ %(asctime)-15s ][%(levelname)s] %(message)s'
logging.basicConfig(stream=sys.stderr, format=FORMAT)
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

try:
    from .Utils import map_char
except ModuleNotFoundError as e:
    logger.critical('BROKEN PACKAGE')
    logger.critical(traceback.format_exc())
    logger.critical(e)
    sys.exit(1)




"""
Process Image into pixel.
Convert into ASCII by grey intensity.
Process edges separately.
"""


class ImageProcessor:

    def __init__(self, image_name):
        """
        Read image file and convert into monochrome format.Process Image into pixel.
        Extract edges
        """
        self.image = cv2.imread(image_name, 0)
        (self.cols, self.rows) = self.image.shape
        self.edge = cv2.Canny(self.image, 100, 200)
        self.output = [[]]

    def show_image(self):
        """
        Display current image.
        """
        cv2.imshow("Image", self.image)
        cv2.waitKey(0)
        cv2.destroyAllWindows()

    def adapt_window(self, window_col, window_row):
        """
        Params window_col, window_row: window size

        Resize Image to adapt different windows
        """
        if window_col < 1 or window_row < 1:
            logger.error("Resize Failed: given size is invalid")
        else:
            self.image = cv2.resize(self.image, (window_col, window_row))
            self.edge = cv2.resize(self.edge, (window_col, window_row))

    def convert(self):
        """
        Convert image into ASCII format
        """
        (col, row) = self.image.shape
        for c in range(col):
            for r in range(row):
                pixel = self.image[c, r]
                if self.edge[c, r] != 0:  # edge pixel
                    self.output[c].append('%')  # paint the edge
                else:
                    self.output[c].append(map_char(float(pixel)))
            self.output.append([])
