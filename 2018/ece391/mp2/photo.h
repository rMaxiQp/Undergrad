/* tab:4
 *
 * photo.h - photo display header file
 *
 * "Copyright (c) 2011 by Steven S. Lumetta."
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE AUTHOR OR THE UNIVERSITY OF ILLINOIS BE LIABLE TO
 * ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL
 * DAMAGES ARISING OUT  OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION,
 * EVEN IF THE AUTHOR AND/OR THE UNIVERSITY OF ILLINOIS HAS BEEN ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE AUTHOR AND THE UNIVERSITY OF ILLINOIS SPECIFICALLY DISCLAIM ANY
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE
 * PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND NEITHER THE AUTHOR NOR
 * THE UNIVERSITY OF ILLINOIS HAS ANY OBLIGATION TO PROVIDE MAINTENANCE,
 * SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * Author:        Steve Lumetta
 * Version:       3
 * Creation Date: Fri Sep 9 21:45:34 2011
 * Filename:      photo.h
 * History:
 *    SL    1    Fri Sep 9 21:45:34 2011
 *        First written.
 *    SL    2    Sun Sep 11 09:59:23 2011
 *        Completed initial implementation.
 *    SL    3    Wed Sep 14 21:56:08 2011
 *        Cleaned up code for distribution.
 */
#ifndef PHOTO_H
#define PHOTO_H


#include <stdint.h>

#include "types.h"
#include "modex.h"
#include "photo_headers.h"
#include "world.h"


/* limits on allowed size of room photos and object images */
#define MAX_PHOTO_WIDTH   1024
#define MAX_PHOTO_HEIGHT  1024
#define MAX_OBJECT_WIDTH   160
#define MAX_OBJECT_HEIGHT  100

/* oct_trees buffer */
#define LVL_TWO_LENGTH 64
#define LVL_FOUR_LENGTH 4096
#define LVL_FOUR_TOTAL 128

/* RGB bit manipulation */
#define BIT_SIX  0x3F // 0b111111
#define BIT_FIVE 0x1F // 0b11111
#define BIT_FOUR 0x0F // 0b1111
#define BIT_TWO 0x03 // 0b11
#define RED(val) ((val >> 11) & BIT_FIVE) // remove 5 bits of blue and 6 bits of green
#define GREEN(val) ((val >> 5) & BIT_SIX) // remove 5 bits of blue
#define BLUE(val) (val & BIT_FIVE)
#define TAKE_TWO(r, g, b) (((r >> 3 & BIT_TWO) << 4) | ((g >> 4 & BIT_TWO) << 2) | (b >> 3 & BIT_TWO))
#define TAKE_FOUR(r, g, b) (((r >> 1 & BIT_FOUR) << 8) | ((g >> 2 & BIT_FOUR) << 4) | (b >> 1 & BIT_FOUR))

/* Fill a buffer with the pixels for a horizontal line of current room. */
extern void fill_horiz_buffer(int x, int y, unsigned char buf[SCROLL_X_DIM]);

/* Fill a buffer with the pixels for a vertical line of current room. */
extern void fill_vert_buffer(int x, int y, unsigned char buf[SCROLL_Y_DIM]);

/* Get height of object image in pixels. */
extern uint32_t image_height(const image_t* im);

/* Get width of object image in pixels. */
extern uint32_t image_width(const image_t* im);

/* Get height of room photo in pixels. */
extern uint32_t photo_height(const photo_t* p);

/* Get width of room photo in pixels. */
extern uint32_t photo_width(const photo_t* p);

/*
 * Prepare room for display(record pointer for use by callbacks, set up
 * VGA palette, etc.).
 */
extern void prep_room(const room_t* r);

/* Read object image from a file into a dynamically allocated structure. */
extern image_t* read_obj_image(const char* fname);

/* Read room photo from a file into a dynamically allocated structure. */
extern photo_t* read_photo(const char* fname);

/* Self-defined qsort cmp function*/
int node_cmp(const void *p1, const void *p2);

/* Function to find the index in the palette */
unsigned int palette_idx(const node_t *buffer, const uint16_t pixel);

/* Assign RGB to buffer */
void assign_RGB(node_t *buffer, const uint16_t pixel, const char flag);

/* Initialize Buffer*/
void initialize_buffer(node_t *buffer, const int size);

/* Fill palette */
void fill_palette(node_t *lvl_two, node_t *lvl_four, photo_t *p);

/*
 * N.B.  I'm aware that Valgrind and similar tools will report the fact that
 * I chose not to bother freeing image data before terminating the program.
 * It's probably a bad habit, but ... maybe in a future release (FIXME).
 * (The data are needed until the program terminates, and all data are freed
 * when a program terminates.)
 */

#endif /* PHOTO_H */
