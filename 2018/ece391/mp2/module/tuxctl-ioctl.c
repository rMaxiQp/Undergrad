/*
 * tuxctl-ioctl.c
 *
 * Driver (skeleton) for the mp2 tuxcontrollers for ECE391 at UIUC.
 *
 * Mark Murphy 2006
 * Andrew Ofisher 2007
 * Steve Lumetta 12-13 Sep 2009
 * Puskar Naha 2013
 */

#include <asm/current.h>
#include <asm/uaccess.h>

#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/module.h>
#include <linux/fs.h>
#include <linux/sched.h>
#include <linux/file.h>
#include <linux/miscdevice.h>
#include <linux/kdev_t.h>
#include <linux/tty.h>
#include <linux/spinlock.h>

#include "tuxctl-ld.h"
#include "tuxctl-ioctl.h"
#include "mtcp.h"

#define debug(str, ...) printk(KERN_DEBUG "%s: " str, __FUNCTION__, ## __VA_ARGS__)

/* local functions */
int tux_init(struct tty_struct* tty);
int tux_button(struct tty_struct* tty, unsigned long arg);
int tux_led(struct tty_struct* tty, unsigned long arg);
void set_btn(unsigned b, unsigned c);
void tux_reset(struct tty_struct* tty);
void draw_led(struct tty_struct* tty);

const unsigned char LED_TABLE[16] = {
  0xE7, 0x06, 0xCB, 0x8F, 0x2E, 0xAD,
  0xED, 0x86, 0xEF, 0xAF, 0xEE, 0x6D,
  0xE1, 0x4F, 0xE9, 0xE8
};

static spinlock_t tuxctl_ioctl_lock;
static int  ack_flag = 0;
static unsigned  pressed_btn;
static unsigned  led_value[4];
static unsigned led_status; // low 4 bits for status
static unsigned decimal; // low 4 bits for decimal points

/************************ Protocol Implementation *************************/

/* tuxctl_handle_packet()
 * IMPORTANT : Read the header for tuxctl_ldisc_data_callback() in
 * tuxctl-ld.c. It calls this function, so all warnings there apply
 * here as well.
 */
void tuxctl_handle_packet (struct tty_struct* tty, unsigned char* packet) {
    unsigned a, b, c;
    unsigned long flags;

    a = packet[0]; /* Avoid printk() sign extending the 8-bit */
    b = packet[1]; /* values when printing them. */
    c = packet[2];

    spin_lock_irqsave(&tuxctl_ioctl_lock, flags);
    switch(a) {
      case MTCP_ACK:
        ack_flag = 1;
        break;
      case MTCP_RESET:
        tux_reset(tty);
        draw_led(tty);
        break;
      case MTCP_BIOC_EVENT:
        set_btn(b, c);
        break;
      default:
        break;
    }
    spin_unlock_irqrestore(&tuxctl_ioctl_lock, flags);
}

/******** IMPORTANT NOTE: READ THIS BEFORE IMPLEMENTING THE IOCTLS ************
 *                                                                            *
 * The ioctls should not spend any time waiting for responses to the commands *
 * they send to the controller. The data is sent over the serial line at      *
 * 9600 BAUD. At this rate, a byte takes approximately 1 millisecond to       *
 * transmit; this means that there will be about 9 milliseconds between       *
 * the time you request that the low-level serial driver send the             *
 * 6-byte SET_LEDS packet and the time the 3-byte ACK packet finishes         *
 * arriving. This is far too long a time for a system call to take. The       *
 * ioctls should return immediately with success if their parameters are      *
 * valid.                                                                     *
 *                                                                            *
 ******************************************************************************/

 /*tuxctl_ioctl
  * DESCRIPTION: It acts like a jumptable
  *   INPUTS: tty -- struct that is related to the driver
  *           file -- argument that is not used for our purpose, ignore it
  *           cmd -- used for switch
  *           arg -- used for corresponding subrountines
  *   OUTPUTS: none
  *   RETURN VALUE: -EINVAL on error, 0 on unimplemented case,
  *                 see subrountines descriptions for more deatils
  *   SIDE EFFECTS: effect varies by cmd, see subrountines descriptions
  */
int tuxctl_ioctl(struct tty_struct* tty, struct file* file,
                 unsigned cmd, unsigned long arg) {
    switch (cmd) {
        case TUX_INIT:
          return tux_init(tty);
        case TUX_BUTTONS:
          return tux_button(tty, arg);
        case TUX_SET_LED:
          return tux_led(tty, arg);
        case TUX_LED_REQUEST:
          return 0;
        case TUX_READ_LED:
          return 0;
        case TUX_LED_ACK:
          return 0;
        default:
            return -EINVAL;
    }
}

/*tux_init
 * DESCRIPTION: Takes no arguments. Initializes any variables associated with the driver and
 *              returns 0. Assume that any user-level code that interacts with your device
 *              will call this ioctl before any others.
 *   INPUTS: tty -- driver
 *   OUTPUTS: none
 *   RETURN VALUE: 0 on success, -EINVAL on failure
 *   SIDE EFFECTS: initializes the driver
 */
int tux_init(struct tty_struct* tty) {
  spin_lock_init(&tuxctl_ioctl_lock);
  pressed_btn = 0xFF; // set history button to all in-active
  tux_reset(tty);
  return 0;
}

/*tux_button
 * DESCRIPTION: Takes a pointer to a 32-bit integer. Returns -EINVAL error if this pointer
 *              is not valid. Otherwise, sets the bits of the low byte corresponding to
 *              the currently pressed buttons, as shown:
 *                 +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
 *                 | R | L | D | U | C | B | A | S |
 *                 +---+---+---+---+---+---+---+---+
 *   INPUTS: tty -- driver
 *           arg -- a pointer to a 32-bit integer
 *   OUTPUTS: none
 *   RETURN VALUE: 0 on success, -EINVAL on failure
 *   SIDE EFFECTS:
 */
int tux_button(struct tty_struct* tty, unsigned long arg) {
  unsigned long flags;

  if (arg == 0) {
    return -EINVAL;
  }

  spin_lock_irqsave(&tuxctl_ioctl_lock, flags);

  if(ack_flag) {
    copy_to_user((void *)arg , &pressed_btn, 4); // copy 4 bytes (32 bits)
  }

  spin_unlock_irqrestore(&tuxctl_ioctl_lock, flags);

  return 0;
}


/*tux_led
 * DESCRIPTION: The argument is a 32-bit integer of the following form: The low 16-bits specify
 *              a number whose hexadecimal value is to be displayed on the 7-segment displays.
 *              The low 4 bits of the third byte specifies which LED’s should be turned on.
 *              The low 4 bits of the highest byte (bits 27:24) specify whether the
 *              corresponding decimal points should be turned on. This ioctl should return 0.
 *
 *            +---+---+---+---+---+---+---+---+---+---+---+---+---+hex_value (16 bits)+---+---+
 *            |          dec_point|         led_status|   [3]   |   [2]   |   [1]    |  [0]   |
 *            +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 *   INPUTS: tty -- driver
 *           arg -- 32-bit integer
 *   OUTPUTS: none
 *   RETURN VALUE: always success, return 0
 *   SIDE EFFECTS: modify LED on TUX
 */
int tux_led(struct tty_struct* tty, unsigned long arg) {
  unsigned long flags;
  unsigned led_, dec_;

  /* check above diagram for bit masking reasons */
  led_ = (unsigned) ((arg >> 16) & 0x0F);
  dec_ = (unsigned) ((arg >> 24) & 0x0F);

  spin_lock_irqsave(&tuxctl_ioctl_lock, flags);
  if (ack_flag) {
    /*
     * store LED value, status and decial status
     * LED values take low 16 bits of the input, 4 bits for each
     */
    led_value[0] = (arg) & 0x0F;
    led_value[1] = (arg >> 4) & 0x0F;
    led_value[2] = (arg >> 8) & 0x0F;
    led_value[3] = (arg >> 12) & 0x0F;

    led_status = led_ ;
    decimal = dec_;

    draw_led(tty);
  }
  spin_unlock_irqrestore(&tuxctl_ioctl_lock, flags);

  return 0;
}

/*set_btn
 * DESCRIPTION: Takes two packages. Reset any variables associated with the driver.
 *   INPUTS: b -- Byte 1 in package
 *           c -- Byte 2 in package
 *        byte 1  +-7--4-+-3-+-2-+-1-+-0-+
 *            | 1 X X X | C | B | A | S |
 *            +---------+---+---+---+--+
 *        byte 2  +-7--4-+-3-+-2-+-1-+-0-+
 *            | 1 X X X | R | D | L | U |
 *            +--------+---+---+---+---+
 *
 *   OUTPUTS: pressed_btn -- the state of each button
 *   RETURN VALUE: none
 *   SIDE EFFECTS: modify global variable
 */
void set_btn(unsigned b, unsigned c) {
  unsigned char left, down;

  /* take out left and down bit */
  left = (c >> 1) & 0x1;
  down = (c >> 2) & 0x1;

  /* swap the position of left and down bit */
  pressed_btn = (((c >> 3 & 0x1) << 7) | (left << 6) | (down << 5) | ((c & 0x1) << 4) | (b & 0xF)) & 0xFF;
}

/*tux_reset
 * DESCRIPTION: Takes no arguments. Reset any variables associated with the driver.
 *   INPUTS: tty -- driver
 *   OUTPUTS: none
 *   RETURN VALUE: none
 *   SIDE EFFECTS: reset the driver
 */
void tux_reset(struct tty_struct* tty) {
  char buf[2];
  ack_flag = 1;
  buf[0] = MTCP_BIOC_ON;
  buf[1] = MTCP_LED_USR;
  tuxctl_ldisc_put(tty, buf, sizeof(buf));
}

/*draw_led
 * DESCRIPTION: Takes no arguments. draw LED using global variables
 *   INPUTS: tty -- driver
 *   OUTPUTS: none
 *   RETURN VALUE: none
 *   SIDE EFFECTS: set LED
 */

void draw_led(struct tty_struct* tty) {
  unsigned char counter, buffer_index, mask;
  /* Opcode + led_status + led_value (max 4) = 6 bytes*/
  unsigned char buf[6] = {0};
  mask = 0x01;
  ack_flag = 0;

  buf[0] = MTCP_LED_SET;
  buf[1] = led_status;
  buffer_index = 2;

  for (counter = 0; counter < 4; counter++, mask <<= 1) {
    if (led_status & mask) {
      buf[buffer_index] = LED_TABLE[led_value[counter]];
      if (decimal & mask) {
        buf[buffer_index] |= 0x10; // decimal bit is located as 5th bit
      }
      buffer_index ++;
    }
  }


  tuxctl_ldisc_put(tty, buf, buffer_index * sizeof(unsigned char));
}
