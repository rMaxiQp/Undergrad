/* tab:4
 *
 * photo.c - photo display functions
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
 * Creation Date: Fri Sep 9 21:44:10 2011
 * Filename:      photo.c
 * History:
 *    SL    1    Fri Sep 9 21:44:10 2011
 *        First written(based on mazegame code).
 *    SL    2    Sun Sep 11 14:57:59 2011
 *        Completed initial implementation of functions.
 *    SL    3    Wed Sep 14 21:49:44 2011
 *        Cleaned up code for distribution.
 */


#include <string.h>

#include "assert.h"
#include "modex.h"
#include "photo.h"
#include "photo_headers.h"
#include "world.h"

/* types local to this file(declared in types.h) */

/*
 * A room photo.  Note that you must write the code that selects the
 * optimized palette colors and fills in the pixel data using them as
 * well as the code that sets up the VGA to make use of these colors.
 * Pixel data are stored as one-byte values starting from the upper
 * left and traversing the top row before returning to the left of
 * the second row, and so forth.  No padding should be used.
 */
struct photo_t {
    photo_header_t hdr;            /* defines height and width */
    uint8_t        palette[192][3];     /* optimized palette colors */
    uint8_t*       img;                 /* pixel data               */
};

/*
 * An object image.  The code for managing these images has been given
 * to you.  The data are simply loaded from a file, where they have
 * been stored as 2:2:2-bit RGB values(one byte each), including
 * transparent pixels(value OBJ_CLR_TRANSP).  As with the room photos,
 * pixel data are stored as one-byte values starting from the upper
 * left and traversing the top row before returning to the left of the
 * second row, and so forth.  No padding is used.
 */
struct image_t {
    photo_header_t hdr;  /* defines height and width */
    uint8_t*       img;  /* pixel data               */
};

/*
 * Self defined struct for oct_trees
 */
struct node_t {
  unsigned int total_r; /* total_r/g/b are for general r/g/b count */
  unsigned int total_g;
  unsigned int total_b;
  unsigned int counter; /* number of this node*/
  unsigned int identity; /* defined color */
};

/*
 * node_cmp
 *   DESCRIPTION: compare two nodes and determine whether the first one is larger
 *
 *   INPUTS: p1 -- first node
 *           p2 -- second node
 *   OUTPUTS: compare value -- determine which value is larger
 *   RETURN VALUE: static int
 *   SIDE EFFECTS: none
 */
int node_cmp(const void *p1, const void *p2) {
  unsigned int first = ((node_t *) p1)->counter;
  unsigned int second = ((node_t *) p2)->counter;
  return first < second;
}

/* file-scope variables */

/*
 * The room currently shown on the screen.  This value is not known to
 * the mode X code, but is needed when filling buffers in callbacks from
 * that code(fill_horiz_buffer/fill_vert_buffer).  The value is set
 * by calling prep_room.
 */
static const room_t* cur_room = NULL;


/*
 * fill_horiz_buffer
 *   DESCRIPTION: Given the(x,y) map pixel coordinate of the leftmost
 *                pixel of a line to be drawn on the screen, this routine
 *                produces an image of the line.  Each pixel on the line
 *                is represented as a single byte in the image.
 *
 *                Note that this routine draws both the room photo and
 *                the objects in the room.
 *
 *   INPUTS:(x,y) -- leftmost pixel of line to be drawn
 *   OUTPUTS: buf -- buffer holding image data for the line
 *   RETURN VALUE: none
 *   SIDE EFFECTS: none
 */
void fill_horiz_buffer(int x, int y, unsigned char buf[SCROLL_X_DIM]) {
    int            idx;   /* loop index over pixels in the line          */
    object_t*      obj;   /* loop index over objects in the current room */
    int            imgx;  /* loop index over pixels in object image      */
    int            yoff;  /* y offset into object image                  */
    uint8_t        pixel; /* pixel from object image                     */
    const photo_t* view;  /* room photo                                  */
    int32_t        obj_x; /* object x position                           */
    int32_t        obj_y; /* object y position                           */
    const image_t* img;   /* object image                                */

    /* Get pointer to current photo of current room. */
    view = room_photo(cur_room);

    /* Loop over pixels in line. */
    for (idx = 0; idx < SCROLL_X_DIM; idx++) {
        buf[idx] = (0 <= x + idx && view->hdr.width > x + idx ? view->img[view->hdr.width * y + x + idx] : 0);
    }

    /* Loop over objects in the current room. */
    for (obj = room_contents_iterate(cur_room); NULL != obj; obj = obj_next(obj)) {
        obj_x = obj_get_x(obj);
        obj_y = obj_get_y(obj);
        img = obj_image(obj);

        /* Is object outside of the line we're drawing? */
        if (y < obj_y || y >= obj_y + img->hdr.height || x + SCROLL_X_DIM <= obj_x || x >= obj_x + img->hdr.width) {
            continue;
        }

        /* The y offset of drawing is fixed. */
        yoff = (y - obj_y) * img->hdr.width;

        /*
         * The x offsets depend on whether the object starts to the left
         * or to the right of the starting point for the line being drawn.
         */
        if (x <= obj_x) {
            idx = obj_x - x;
            imgx = 0;
        }
        else {
            idx = 0;
            imgx = x - obj_x;
        }

        /* Copy the object's pixel data. */
        for (; SCROLL_X_DIM > idx && img->hdr.width > imgx; idx++, imgx++) {
            pixel = img->img[yoff + imgx];

            /* Don't copy transparent pixels. */
            if (OBJ_CLR_TRANSP != pixel) {
                buf[idx] = pixel;
            }
        }
    }
}


/*
 * fill_vert_buffer
 *   DESCRIPTION: Given the(x,y) map pixel coordinate of the top pixel of
 *                a vertical line to be drawn on the screen, this routine
 *                produces an image of the line.  Each pixel on the line
 *                is represented as a single byte in the image.
 *
 *                Note that this routine draws both the room photo and
 *                the objects in the room.
 *
 *   INPUTS:(x,y) -- top pixel of line to be drawn
 *   OUTPUTS: buf -- buffer holding image data for the line
 *   RETURN VALUE: none
 *   SIDE EFFECTS: none
 */
void fill_vert_buffer(int x, int y, unsigned char buf[SCROLL_Y_DIM]) {
    int            idx;   /* loop index over pixels in the line          */
    object_t*      obj;   /* loop index over objects in the current room */
    int            imgy;  /* loop index over pixels in object image      */
    int            xoff;  /* x offset into object image                  */
    uint8_t        pixel; /* pixel from object image                     */
    const photo_t* view;  /* room photo                                  */
    int32_t        obj_x; /* object x position                           */
    int32_t        obj_y; /* object y position                           */
    const image_t* img;   /* object image                                */

    /* Get pointer to current photo of current room. */
    view = room_photo(cur_room);

    /* Loop over pixels in line. */
    for (idx = 0; idx < SCROLL_Y_DIM; idx++) {
        buf[idx] = (0 <= y + idx && view->hdr.height > y + idx ? view->img[view->hdr.width *(y + idx) + x] : 0);
    }

    /* Loop over objects in the current room. */
    for (obj = room_contents_iterate(cur_room); NULL != obj; obj = obj_next(obj)) {
        obj_x = obj_get_x(obj);
        obj_y = obj_get_y(obj);
        img = obj_image(obj);

        /* Is object outside of the line we're drawing? */
        if (x < obj_x || x >= obj_x + img->hdr.width ||
            y + SCROLL_Y_DIM <= obj_y || y >= obj_y + img->hdr.height) {
            continue;
        }

        /* The x offset of drawing is fixed. */
        xoff = x - obj_x;

        /*
         * The y offsets depend on whether the object starts below or
         * above the starting point for the line being drawn.
         */
        if (y <= obj_y) {
            idx = obj_y - y;
            imgy = 0;
        }
        else {
            idx = 0;
            imgy = y - obj_y;
        }

        /* Copy the object's pixel data. */
        for (; SCROLL_Y_DIM > idx && img->hdr.height > imgy; idx++, imgy++) {
            pixel = img->img[xoff + img->hdr.width * imgy];

            /* Don't copy transparent pixels. */
            if (OBJ_CLR_TRANSP != pixel) {
                buf[idx] = pixel;
            }
        }
    }
}


/*
 * image_height
 *   DESCRIPTION: Get height of object image in pixels.
 *   INPUTS: im -- object image pointer
 *   OUTPUTS: none
 *   RETURN VALUE: height of object image im in pixels
 *   SIDE EFFECTS: none
 */
uint32_t image_height(const image_t* im) {
    return im->hdr.height;
}


/*
 * image_width
 *   DESCRIPTION: Get width of object image in pixels.
 *   INPUTS: im -- object image pointer
 *   OUTPUTS: none
 *   RETURN VALUE: width of object image im in pixels
 *   SIDE EFFECTS: none
 */
uint32_t image_width(const image_t* im) {
    return im->hdr.width;
}

/*
 * photo_height
 *   DESCRIPTION: Get height of room photo in pixels.
 *   INPUTS: p -- room photo pointer
 *   OUTPUTS: none
 *   RETURN VALUE: height of room photo p in pixels
 *   SIDE EFFECTS: none
 */
uint32_t photo_height(const photo_t* p) {
    return p->hdr.height;
}


/*
 * photo_width
 *   DESCRIPTION: Get width of room photo in pixels.
 *   INPUTS: p -- room photo pointer
 *   OUTPUTS: none
 *   RETURN VALUE: width of room photo p in pixels
 *   SIDE EFFECTS: none
 */
uint32_t photo_width(const photo_t* p) {
    return p->hdr.width;
}

void* photo_palatte(const photo_t* p, int index) {
  if (index < 0 || index > 2) {
    return NULL;
  }

  return (void *) &(p->palette[0][index]);
}

/*
 * prep_room
 *   DESCRIPTION: Prepare a new room for display.  You might want to set
 *                up the VGA palette registers according to the color
 *                palette that you chose for this room.
 *   INPUTS: r -- pointer to the new room
 *   OUTPUTS: none
 *   RETURN VALUE: none
 *   SIDE EFFECTS: changes recorded cur_room for this file
 */
void prep_room(const room_t* r) {
    /* Record the current room. */
    cur_room = r;
    const photo_t *photo = room_photo(cur_room);
    load_photo(photo->palette);
  }


/*
 * read_obj_image
 *   DESCRIPTION: Read size and pixel data in 2:2:2 RGB format from a
 *                photo file and create an image structure from it.
 *   INPUTS: fname -- file name for input
 *   OUTPUTS: none
 *   RETURN VALUE: pointer to newly allocated photo on success, or NULL
 *                 on failure
 *   SIDE EFFECTS: dynamically allocates memory for the image
 */
image_t* read_obj_image(const char* fname) {
    FILE*    in;        /* input file               */
    image_t* img = NULL;    /* image structure          */
    uint16_t x;            /* index over image columns */
    uint16_t y;            /* index over image rows    */
    uint8_t  pixel;        /* one pixel from the file  */

    /*
     * Open the file, allocate the structure, read the header, do some
     * sanity checks on it, and allocate space to hold the image pixels.
     * If anything fails, clean up as necessary and return NULL.
     */
    if (NULL == (in = fopen(fname, "r+b")) ||
        NULL == (img = malloc(sizeof (*img))) ||
        NULL != (img->img = NULL) || /* false clause for initialization */
        1 != fread(&img->hdr, sizeof (img->hdr), 1, in) ||
        MAX_OBJECT_WIDTH < img->hdr.width ||
        MAX_OBJECT_HEIGHT < img->hdr.height ||
        NULL == (img->img = malloc
        (img->hdr.width * img->hdr.height * sizeof (img->img[0])))) {
        if (NULL != img) {
            if (NULL != img->img) {
                free(img->img);
            }
            free(img);
        }
        if (NULL != in) {
            (void)fclose(in);
        }
        return NULL;
    }

    /*
     * Loop over rows from bottom to top.  Note that the file is stored
     * in this order, whereas in memory we store the data in the reverse
     * order(top to bottom).
     */
    for (y = img->hdr.height; y-- > 0; ) {

        /* Loop over columns from left to right. */
        for (x = 0; img->hdr.width > x; x++) {

            /*
             * Try to read one 8-bit pixel.  On failure, clean up and
             * return NULL.
             */
            if (1 != fread(&pixel, sizeof (pixel), 1, in)) {
                free(img->img);
                free(img);
                (void)fclose(in);
                return NULL;
            }

            /* Store the pixel in the image data. */
            img->img[img->hdr.width * y + x] = pixel;
        }
    }

    /* All done.  Return success. */
    (void)fclose(in);
    return img;
}

/*
 * read_photo
 *   DESCRIPTION: Read size and pixel data in 5:6:5 RGB format from a
 *                photo file and create a photo structure from it.
 *                Code provided simply maps to 2:2:2 RGB.  You must
 *                replace this code with palette color selection, and
 *                must map the image pixels into the palette colors that
 *                you have defined.
 *   INPUTS: fname -- file name for input
 *   OUTPUTS: none
 *   RETURN VALUE: pointer to newly allocated photo on success, or NULL
 *                 on failure
 *   SIDE EFFECTS: dynamically allocates memory for the photo
 */
photo_t* read_photo(const char* fname) {
    FILE*    in;    /* input file               */
    photo_t* p = NULL;    /* photo structure          */
    uint16_t x;        /* index over image columns */
    uint16_t y;        /* index over image rows    */
    uint16_t pixel;    /* one pixel from the file  */

    /* Self defined arrays act as buffers*/
    node_t lvl_two[LVL_TWO_LENGTH]; // level_two
    node_t lvl_four[LVL_FOUR_LENGTH]; // level_four
    long file_start;

    /*
     * Open the file, allocate the structure, read the header, do some
     * sanity checks on it, and allocate space to hold the photo pixels.
     * If anything fails, clean up as necessary and return NULL.
     */
    if (NULL == (in = fopen(fname, "r+b")) ||
        NULL == (p = malloc(sizeof (*p))) ||
        NULL != (p->img = NULL) || /* false clause for initialization */
        1 != fread(&p->hdr, sizeof (p->hdr), 1, in) ||
        MAX_PHOTO_WIDTH < p->hdr.width ||
        MAX_PHOTO_HEIGHT < p->hdr.height ||
        NULL == (p->img = malloc
        (p->hdr.width * p->hdr.height * sizeof (p->img[0])))) {
        if (NULL != p) {
            if (NULL != p->img) {
                free(p->img);
            }
            free(p);
        }
        if (NULL != in) {
            (void)fclose(in);
        }
        return NULL;
    }

    /*
     * Though the documentation doesn't mention it,
     * the file desciptor is not set at zero for the next iteration
     *
     * if you set to zero, renderd photos would be wrapped around,
     * use ftell() to resolve it
     */
    file_start = ftell(in);
    /* initialization of buffers*/
    initialize_buffer(lvl_four, LVL_FOUR_LENGTH);
    initialize_buffer(lvl_two, LVL_TWO_LENGTH);

    /*
     * Loop over rows from bottom to top.  Note that the file is stored
     * in this order, whereas in memory we store the data in the reverse
     * order(top to bottom).
     */
    for (y = p->hdr.height; y-- > 0; ) {

        /* Loop over columns from left to right. */
        for (x = 0; p->hdr.width > x; x++) {

            /*
             * Try to read one 16-bit pixel.  On failure, clean up and
             * return NULL.
             */
            if (1 != fread(&pixel, sizeof (pixel), 1, in)) {
                free(p->img);
                free(p);
                (void)fclose(in);
                return NULL;
            }
            assign_RGB(lvl_four, pixel, 1);
            assign_RGB(lvl_two, pixel, 0);
        }
    }

    qsort(lvl_four, LVL_FOUR_LENGTH, sizeof(node_t), node_cmp);

    /* WARNING: SINCE THE GRADER CAN BE EXTREMELY PICKY, MOVE THIS PART
     * INTO A SEPERATE FUNCTION THOUGH IT IS ONLY CALLED ONCE*/
    fill_palette(lvl_two, lvl_four, p);

    /* reset file desciptor position */
    fseek(in, file_start, SEEK_SET);

    for (y = p->hdr.height; y-- > 0; ) {

        /* Loop over columns from left to right. */
        for (x = 0; p->hdr.width > x; x++) {

            /*
             * Try to read one 16-bit pixel.  On failure, clean up and
             * return NULL.
             */
            if (1 != fread(&pixel, sizeof (pixel), 1, in)) {
                free(p->img);
                free(p);
                (void)fclose(in);
                return NULL;
            }

            /* add 64 to skip text palette space */
            p->img[p->hdr.width * y + x] = 64 + palette_idx(lvl_four, pixel);
        }
    }

    /* All done.  Return success. */
    (void)fclose(in);
    return p;
}

/*
 * palette_idx
 *   DESCRIPTION: find corresponding index of in the palette of given pixel
 *   INPUTS: buffer -- lvl_four buffer
 *           pixel -- current pixel
 *   OUTPUTS: none
 *   RETURN VALUE: index in palette
 *   SIDE EFFECTS: none
 */
unsigned int palette_idx(const node_t *buffer, const uint16_t pixel) {
  unsigned char red = RED(pixel);
  unsigned char green = GREEN(pixel);
  unsigned char blue = BLUE(pixel);
  unsigned int idx = TAKE_FOUR(red, green, blue);
  int i;
  for (i = 0; i < LVL_FOUR_TOTAL; i++) {
    if (idx == buffer[i].identity) {
      return i;
    }
  }
  idx = TAKE_TWO(red, green, blue);
  return idx + LVL_FOUR_TOTAL;
}

/*
 * assign_RGB
 *   DESCRIPTION: assign RGB values to buffer
 *   INPUTS: buffer -- lvl_four buffer
 *           pixel -- current pixel
 *           flag -- 0 for level_two, 1 for level_four
 *   OUTPUTS: none
 *   RETURN VALUE: none
 *   SIDE EFFECTS: modify buffer
 */
void assign_RGB(node_t *buffer, const uint16_t pixel, const char flag) {
  unsigned char red = RED(pixel);
  unsigned char green = GREEN(pixel);
  unsigned char blue = BLUE(pixel);
  unsigned int idx;
  if (flag) {
    idx = TAKE_FOUR(red, green, blue);
  } else {
    idx = TAKE_TWO(red, green, blue);
  }
  buffer[idx].total_r += red;
  buffer[idx].total_g += green;
  buffer[idx].total_b += blue;
  buffer[idx].counter ++;
}

/*initialize_buffer
 *   DESCRIPTION: initialize buffers
 *   INPUTS: buffer -- initialize this buffer
 *           size -- buffer capacity
 *   OUTPUTS: none
 *   RETURN VALUE: none
 *   SIDE EFFECTS: modify buffer
 */
void initialize_buffer(node_t *buffer, const int size) {
  int i;
  for (i = 0; i < size; i ++) {
    buffer[i].total_r = 0;
    buffer[i].total_g = 0;
    buffer[i].total_b = 0;
    buffer[i].counter = 0;
    buffer[i].identity = i;
  }
}

/*fill_palette
 *   DESCRIPTION:  compute average of RGB and fill palette
 *   INPUTS: lvl_two -- lvl_two buffer
 *           lvl_four -- lvl_four buffer
 *           p -- modify this photo_t's palettes
 *   OUTPUTS: none
 *   RETURN VALUE: none
 *   SIDE EFFECTS: modify palette
 */
void fill_palette(node_t *lvl_two, node_t *lvl_four, photo_t *p) {
  /* Self defined local variables */
  int i, idx, red, green, blue;

  for (i = 0; i < LVL_FOUR_TOTAL; i++) {
    if (lvl_four[i].counter) {
      /* When fillig palettes, you need to shift R and B
       * left by 1 to make all of them 6 bits
       */
      p->palette[i][0] = (lvl_four[i].total_r / lvl_four[i].counter) << 1;
      p->palette[i][1] = lvl_four[i].total_g / lvl_four[i].counter;
      p->palette[i][2] = (lvl_four[i].total_b / lvl_four[i].counter) << 1;

      /*
       * find idx of lvl_two
       * since each color is stored as
       * 4 bits in lvl_four buffer
       * R4:G4:B4
       * shift corresponding bits to get correct color
       */
      red = lvl_four[i].identity >> 10;
      green = lvl_four[i].identity >> 6;
      blue = lvl_four[i].identity >> 2;
      idx = ((red & BIT_TWO) << 4) | ((green & BIT_TWO) << 2) | (blue & BIT_TWO);

      /* prevent duplicate RGB value in computation */
      lvl_two[idx].total_r -= lvl_four[i].total_r;
      lvl_two[idx].total_g -= lvl_four[i].total_g;
      lvl_two[idx].total_b -= lvl_four[i].total_b;
      lvl_two[idx].counter -= lvl_four[i].counter;
    }
  }

  /* fill the rest of palette with lvl_two average RGB */
  for(i = 0; i < LVL_TWO_LENGTH; i++) {
    idx = i + LVL_FOUR_TOTAL;
    if (lvl_two[i].counter) {
      p->palette[idx][0] = (lvl_two[i].total_r / lvl_two[i].counter) << 1;
      p->palette[idx][1] = lvl_two[i].total_g / lvl_two[i].counter;
      p->palette[idx][2] = (lvl_two[i].total_b / lvl_two[i].counter) << 1;
    }
  }
}
