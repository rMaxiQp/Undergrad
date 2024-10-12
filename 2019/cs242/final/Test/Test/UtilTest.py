import unittest
from asciiv2v.ImageProcessor import ImageProcessor


"""
Test: Input image reading and resize.
"""


class UtilTest(unittest.TestCase):
    def test_read_image(self):
        """
        Reading image and display.
        """
        self.image_converter = ImageProcessor("../sample1.jpg")
        self.image_converter.show_image()

    def test_adapt_window(self):
        """
        Resize window to valid size.
        """
        self.image_converter = ImageProcessor("../sample1.jpg")
        self.image_converter.adapt_window(100, 100)
        self.assertEqual(self.image_converter.image.shape, (100, 100))
        self.assertEqual(self.image_converter.edge.shape, (100, 100))

    def test_fail_adapt_window(self):
        """
        Resize window to invalid size. (should see logging message)
        """
        self.image_converter = ImageProcessor("../sample1.jpg")
        self.image_converter.adapt_window(-100, 100)

