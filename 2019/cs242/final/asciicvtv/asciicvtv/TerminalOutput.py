import os
import sys
import logging
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
Output converted image into terminal.
"""


class TerminalOutput(ImageProcessor):
    def __init__(self, image_name):
        """
        Read terminal size and resize image.
        """
        super().__init__(image_name)
        try:
            (self.cols, self.rows) = os.get_terminal_size()
            # terminal screen size is smaller than window size,
            # need to shrink it proportionally
            window_col = int(self.cols * 0.93)
            window_row = int(self.rows * 0.93)
            super(TerminalOutput, self).adapt_window(window_col, window_row)
        except OSError:
            logger.error("Cannot read current window size. Please run from terminal window.")

    def convert(self):
        super(TerminalOutput, self).convert()

    def output_txt(self, output_name='output.txt'):
        """
        Param output_name: output txt file name. (Default: output.txt)
        """
        if output_name != '' and output_name.split('.')[1] != 'txt':
            logger.error('Output file has incorrect format.')
        with open(output_name, 'w') as f:
            for n in self.output:
                f.write("%s\n" % ' '.join(n))

    def output_terminal(self):
        """
          Output into terminal.
        """
        # print(os.get_terminal_size())
        for line in self.output:
            print(''.join(line))
