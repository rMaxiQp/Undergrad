# mp1.S - missile-command version
# Good luck, and have fun!

.data

    # Constants for accessing the fields of a struct missile,
    # struct missile is defined in rtc.h and mp1.h

	NEXT 	  = 0 # pointer to next missile in linked list
	X	  = 4
	Y	  = 8 # x,y position on screen
	VX	  = 12
	VY	  = 16 # x,y velocity vector
	DEST_X	  = 20
	DEST_Y	  = 24 # x,y destination location
	EXPLODED  = 28 # explosion duration counter
	C	  = 32 # missile character representation
	STRUCT_SIZE = 33
	EXPLOSION = 64 # Character to draw for an explosion - '@'

	ROW_PIXEL = 80
	COL_UPPER_BOUND = 24
	ROW_UPPER_BOUND = 79
	TWO_BYTES = 16

# Data shared between this file and rtc.c helper functions

# This '.globl' directive makes these symbols visible externally
.globl mp1_missile_list, base_alive, mp1_score

mp1_missile_list:	.long 0x0	# Head of list pointer
base_alive:		.long 0x0	# Array of 3 bytes, plus a padding byte
mp1_score:		.long 0x0	# Player's current score

# Data private to this  file

base_pic:     	    .string "/^^^\\"	# Picture of a live base
dead_base_pic: 	    .string "xxxxx"	# Picture of a dead base
crosshairs_x:		.long 0x0	# X-position of the crosshairs
crosshairs_y:		.long 0x0	# Y-position of the crosshairs
crosshairs_mark: .byte '+' # symbol of the crosshairs
empty_mark: .byte ' ' # character used to erase missile character representation
kernel_status: .long 0x0 # placeholder in getstatus
.text

# void mp1_poke(void);
# You can use this function to write to video memory.
#
# Interface: Register-based arguments (not C-style)
#    Inputs: %cl  - The byte you wish to write
#            %eax - Offset from the start of video memory that you wish
#                   to write to
#   Outputs: Text-mode video screen is written to at location %eax with
#            the byte in %cl
# Registers: Clobbers EDX

mp1_poke:
		movl    vmem_base_addr(,1),%edx
		movb    %cl,(%edx,%eax,1)
		ret

#   ----------------- Exported functions ---------------------

# void mp1_rtc_tasklet(unsigned long garbage);
# Performs three tasks:
#	(1) updates the list of missiles (implement this in update_missiles,
#           below, and call it from here).
#	(2) Redraw the bases - they may have been overwritten with missiles
#	(3) Redraw the crosshairs - it may have been overwritten with missiles
# Inputs   : none
# Outputs  : none
# Registers: Standard C calling convention

.globl mp1_rtc_tasklet
mp1_rtc_tasklet:
		pushl %ebp
		movl %esp, %ebp

		pushl %eax
		pushl %ecx
		pushl %edx
		call update_missiles # update the list of missiles

		call draw_base # draw first base

		call draw_crosshairs # draw crosshair
		popl %edx
		popl %ecx
		popl %eax

		leave
		ret

# int mp1_ioctl(unsigned long arg, unsigned int cmd)
# The dispatch function for the MP1 ioctls - should use the cmd argument
# and a jumptable to execute one of the specific ioctls implemented below.
# Inputs   : unsigned long arg - parameter to the mp1_ioctl_....
#          : unsigned int cmd  - specifies which mp1_ioctl_... function
#          :                     to execute
# Outputs  : Returns an integer - depends on which ioctl() function is called
# Registers: Standard C calling convention

.globl mp1_ioctl
mp1_ioctl:
		movl 8(%esp), %ecx # %ecx = cmd
		cmpl $4, %ecx # total 4 options
		ja ioctl_default_case # cmd > 5
		jmp * ioctl_jumptable(, %ecx, 4) # total 4 options

	ioctl_default_case:
		movl $-1, %eax # invalid cmd
		ret

	ioctl_jumptable: # using jumptable to call specific functions
		.long mp1_ioctl_startgame, mp1_ioctl_addmissile, mp1_ioctl_movexhairs, mp1_ioctl_getstatus, mp1_ioctl_endgame

# ----------------- Functions private to this file -------------------

# int compute_address(int X, int Y)
# return 2 * (X + Y * ROW_PIXEL)
# relative registers: %eax, %edx

compute_address: # computed address from %edx, %eax and store it into %eax
		imul $ROW_PIXEL, %eax
		addl %edx, %eax # %eax = row * 80 + col
		imul $2, %eax # 2 bytes per pixel
		ret

# void mp1_remove_missile(struct missile * prev_missile, struct missile * remove_missile)
# remove missile with previous missile provided
# previous_missile->next = remove_missile->next

mp1_remove_missile: # remove missile with previous missile provided
		pushl %ebp
		movl %esp, %ebp
		pushl %edi
		pushl %esi
		pushl %ebx

		movl 8(%ebp), %edi # %edi = prev_missile
		movl 12(%ebp), %esi # %esi = remove_missile
		movl NEXT(%esi), %ebx # %ebx = remove_missile->next
		movl %ebx, NEXT(%edi) # %edi->next = %ebx

		movl $0, NEXT(%esi) # %esi->next = NULL
		pushl %esi
		call mp1_free # free %esi
		popl %esi

		popl %ebx
		popl %esi
		popl %edi
		leave
		ret

# void update_missiles()
# traverse mp1_missile_list to update missiles
# 1) check boundary - may remove missile
# 2) check explosion - may remove missile
# 3) check destination - may trigger explosion
# 4) update location
update_missiles:
		pushl %ebp
		movl %esp, %ebp
		pushl %ebx
		pushl %edi
		pushl %esi
		movl mp1_missile_list, %ebx # %ebx(prev_missile) = *mp1_missile_list

	update_missiles_loop:
    movl NEXT(%ebx), %edi # %edi = current_missile
		cmpl $0, %edi
		je update_missiles_done # current_missle == NULL

		# 1) check boundary
		movl X(%edi), %ecx # %ecx = X [0 - 79]
		shrl $TWO_BYTES, %ecx # take high-16 bits
		cmpw $0, %cx
		jl update_missiles_remove # X < 0
		cmpw $ROW_UPPER_BOUND, %cx
		jg update_missiles_remove # X > 79
		movl Y(%edi), %ecx # %ecx = Y [0 - 24]
		shrl $TWO_BYTES, %ecx
		cmpl $0, %ecx
		jl update_missiles_remove # Y < 0
		cmpl $COL_UPPER_BOUND, %ecx
		jg update_missiles_remove # Y > 24

		# 2) check explosion
		movl EXPLODED(%edi), %edx
		cmpl $0, %edx
		jg explosion # exploded > 0

		# 3) check destination
		movl X(%edi), %edx
		movl DEST_X(%edi), %esi
		shrl $TWO_BYTES, %edx # take high-16 bits
		cmpw %dx, %si
		jne  update_missile_position # x != dest_x
		movl Y(%edi), %edx
		movl DEST_Y(%edi), %esi
		shrl $TWO_BYTES, %edx # take high-16 bits
		cmpw %dx, %si
		je explosion # reach destination

		# 4) update address
	update_missile_position:
		movl X(%edi), %edx
		movl Y(%edi), %eax
		shrl $TWO_BYTES, %edx
		shrl $TWO_BYTES, %eax
		call compute_address
		movb empty_mark, %cl
		call mp1_poke # clear previous character

		movl X(%edi), %ecx # %ecx = X
		movl VX(%edi), %edx # %edx = VX
		addl %edx, %ecx
		movl %ecx, X(%edi) # x += VX

		movl Y(%edi), %ecx # %ecx = Y
		movl VY(%edi), %edx # %edx = VY
		addl %edx, %ecx
		movl %ecx, Y(%edi) # Y += VY

		movl X(%edi), %edx
		movl Y(%edi), %eax
		shrl $TWO_BYTES, %edx
		shrl $TWO_BYTES, %eax
		call compute_address
		movb C(%edi), %cl
		call mp1_poke # draw character
		movl %edi, %ebx # prev_missile = current_missile
		jmp update_missiles_loop

		# remove missile
	update_missiles_remove:
		movl X(%edi), %edx
		movl Y(%edi), %eax
		shrl $TWO_BYTES, %edx
		shrl $TWO_BYTES, %eax
		call compute_address
		movb empty_mark, %cl
		call mp1_poke
		pushl %edi # remove missile
		pushl %ebx # previous missile
		movl %edi, %ebx
		call mp1_remove_missile
		popl %ebx
		popl %edi
    jmp update_missiles_loop

	explosion:
		pushl %edi
		call missile_explode
		popl %edi
		cmpl $0, %eax
		je explosion_draw
		call mp1_notify_user # notify user for non-zero return

	explosion_draw:
		movl EXPLODED(%edi), %edx # %edx = exploded
		subl $1, %edx
		cmpl $0, %edx # if exploded == 0
		je update_missiles_remove
		movl %edx, EXPLODED(%edi)
		movb $EXPLOSION, %cl # %cl = EXPLOSION
		movl X(%edi), %edx # %eax = X
		movl Y(%edi), %eax # %esi = Y
		shrl $TWO_BYTES, %edx
		shrl $TWO_BYTES, %eax # take high-16 bits
		call compute_address
		call mp1_poke
		movl NEXT(%ebx), %ebx
		jmp update_missiles_loop

	update_missiles_done:
		popl %esi
		popl %edi
		popl %ebx
		leave
		ret

# void draw_base()
# draw bases by base_alive
# 1) check base status
# 2) load relative base picture
# 3) draw base
draw_base: # draw bases
		pushl %ebp
		movl %esp, %ebp
		pushl %edi
		pushl %ebx
		pushl %esi
		movl $2, %edx # first base starts at 18; 18 - 16 = 2
		movl $24, %eax # y-axis is always at 24
		call compute_address
		movl base_alive, %edx

	load_pic: #2(78 + 24 * 80) = 3996
		xorl %esi, %esi
		add $32, %eax # the interval between each base is 16 pixel => 32 bytes
		cmpl $3996, %eax # the ideal fourth base starts at 1998
		jge draw_base_done
		movl %edx, %ecx
		shrl $8, %edx # find next base
		cmpb $0, %cl # compare lowest bit
		je load_dead_base
	  movl $base_pic, %edi # %edi = &base_pic
		jmp draw_base_loop

	load_dead_base:
		movl $dead_base_pic, %edi # %edi = &dead_base_pic

	draw_base_loop:
		cmpl $5, %esi # base takes 5 pixels
		je load_pic # finish drawing
		movb (%edi), %cl
		pushl %eax
		pushl %edx
		pushl %ecx
		call mp1_poke
		popl %ecx
		popl %edx
		popl %eax
		addl $2, %eax # move 2 bytes for next pixel
		addl $1, %edi
		addl $1, %esi
		jmp draw_base_loop

	draw_base_done:
		popl %esi
		popl %ebx
		popl %edi
		leave
		ret
# void draw_crosshairs()
# draw crosshairs with crosshairs_x and crosshairs_y
draw_crosshairs: # draw crosshairs
		pushl %ebp
		movl %esp, %ebp
		pushl %edi
		pushl %esi
		pushl %ebx
		movl crosshairs_x, %edx
		movl crosshairs_y, %eax
		call compute_address
		movb crosshairs_mark, %cl

		pushl %edx
		pushl %eax
		pushl %ecx
		call mp1_poke
		popl %ecx
		popl %eax
		popl %edx

		popl %ebx
		popl %esi
		popl %edi
		leave
		ret

# void mp1_ioctl_startgame(unsigned long ignored)
# initialize labels:
#			base_alive, mp1_score, mp1_missile_list, crosshairs_x, corsshairs_y
# append a sentinal node to mp1_missile_list
mp1_ioctl_startgame: # initialization and set crosshairds (40, 12)
		pushl %ebp
		movl %esp, %ebp
		pushl %edi
		pushl %ebx
		pushl %esi

		movl $40, crosshairs_x
		movl $12, crosshairs_y # crosshairs <= (40,12)
		movl $0x10101, base_alive # initialize all bases to alive
		movl $0, mp1_score
		movl $STRUCT_SIZE, %edi
		pushl %edi
		call mp1_malloc
		addl $4, %esp # pop 1 argument
		movl $0, NEXT(%eax)
		movl NEXT(%eax), %edi
		movl %eax, mp1_missile_list
		popl %esi
		popl %ebx
		popl %edi
		leave
		ret

# int mp1_ioctl_addmissile (struct missle* user_missile)
# add the new missile into the linked list
# 1) malloc new_missile
# 2) copy values from user_missile to new_missile
# 3) append new_missile after the sential node
#    new_missile->next = sential_node->next
#    sential_node->next = new_missile
mp1_ioctl_addmissile:
		pushl %ebp
		movl %esp, %ebp
		pushl %edi
		pushl %ebx
		pushl %esi
		movl First_ARG(%ebp), %esi # %esi = user_missile

		movl $STRUCT_SIZE, %edi
		pushl %edi
		call mp1_malloc
		add $4, %esp # pop 1 argument
		cmpl $0, %eax
		je malloc_fail # malloc fails
		movl %eax, %edi

		# copy values from %ecx to %edx
		movl $STRUCT_SIZE, %eax
		pushl %eax # size
		pushl %esi # from
		pushl %edi # to
		call mp1_copy_from_user # %eax = return value
		addl $12, %esp # pop 3 arguments
		cmpl $-1, %eax
		je copy_from_user_fail # copy_from_user fails

		# append missile to list
		movl mp1_missile_list, %ecx # %ecx = HEAD
		movl NEXT(%ecx), %edx # %edx = %ecx->next
		movl %edx, NEXT(%edi) # %esi->next = %edx
		movl %edi, NEXT(%ecx) # %ecx->next = %esi
		movl %ecx, mp1_missile_list
		movl $0, %eax
		jmp addmissile_done

 # failure case
	copy_from_user_fail:
		pushl %esi
		call mp1_free
		popl %esi

	malloc_fail:
		movl $-1, %eax

	addmissile_done:
		popl %esi
		popl %ebx
		popl %edi
		leave
		ret

# int mp1_ioctl_movexhairs(unsigned long xhair_delta_packed)
# moves the crosshairs
# remove the previous drawing for the crosshairs, then redraw the crosshairs at new locaiton
# Input : xhair_delta_packed - {y, x}, x => [0-79], y => [0-24]
# Output : return 0
mp1_ioctl_movexhairs:
		pushl %ebp
		movl %esp, %ebp
		pushl %edi
		pushl %ebx
		pushl %esi
		movl 8(%ebp), %ebx # %ebx = xhair_delta_packed

		movl crosshairs_x, %edi # %edi = crosshairs_x
		movl crosshairs_y, %esi # %esi = crosshairs_y
		movl %edi, %edx
		movl %esi, %eax
		call compute_address
		movb empty_mark, %cl # erase crosshair
		call mp1_poke

	movexhairs_x:
		addw %bx, %di
		cmpw $0, %di
		jb x_outbound # x < 0
		cmpw $ROW_UPPER_BOUND, %di
		ja x_outbound # x > 79

	movexhairs_y:
		shrl $TWO_BYTES, %ebx # take y part
		addw %bx, %si
		cmpw $0, %si
		jb y_outbound # y < 0
		cmpw $COL_UPPER_BOUND, %si
		ja y_outbound # y > 24

	movexhairs_done:
		movl %edi, %edx
		movl %esi, %eax
		call compute_address
		movb crosshairs_mark, %cl # redraw crosshair
		call mp1_poke
		movl %edi, crosshairs_x
		movl %esi, crosshairs_y
		popl %esi
		popl %ebx
		popl %edi
		movl $0, %eax # provide return value
		leave
		ret

	x_outbound:
		subw %bx, %di
		jmp movexhairs_y

	y_outbound:
		subw %bx, %si
		jmp movexhairs_done

# int mp1_ioctl_getstatus (unsigned long* user_status)
# copy the current score into the low 16-bits of that integer,
# and the status of three cities into bits 16, 17, 18.
# Input  : user_status - a pointer to a 32-bit integer in user space
# Output : return -1 when it fails
#		   return 0 on success
mp1_ioctl_getstatus:
		pushl %ebp
		movl %esp, %ebp
		pushl %ebx
		pushl %esi
		pushl %edi

		movl 8(%ebp), %edi # %edi = user_status
		xorl %ebx, %ebx
		xorl %edx, %edx # set %ebx, %edx to 0
		movl mp1_score, %ebx
		andl $0xFFFF, %ebx# zero out upper 16 bits (2 bytes)

		# Get bases status
		movl $base_alive, %esi # %esi = base_alive
		orl  2(%esi), %edx
		shll $1, %edx # %edx = _ 1|0 _
		orl  1(%esi), %edx
		shll $1, %edx # %edx = 1|0 1|0 _
		orl  0(%esi), %edx # %edx = 1|0 1|0 1|0
		shll $TWO_BYTES, %edx # move %edx to high-16 bits
		orl %edx, %ebx
		 # add base status to %ebx
		movl %ebx, kernel_status

		# return
		movl $kernel_status, %ebx
		movl $4, %eax # size of unsigned long
		pushl %eax # n
		pushl %ebx # from
		pushl %edi # to
		call mp1_copy_to_user # %eax has been assigned here
		popl %ecx
		popl %ecx
		popl %ecx
		popl %esi
		popl %ebx
		popl %edi
		cmpl $-1, %eax
		je getstatus_mem_fail
		movl $0, %eax
		leave
		ret

getstatus_mem_fail:
		pushl %edx
		call mp1_free
		popl %edx
		movl $-1, %eax
		leave
		ret
# int mp1_ioctl_endgame(unsigned long ignore)
# only called when the game ends
# free mp1_missile_list -> heap address should be freed manually
# stack allocation will be taken care after the process ends
mp1_ioctl_endgame: # free all the memory being used by the linked list
		pushl %ebp
		movl %esp, %ebp
		pushl mp1_missile_list
		call mp1_ioctl_cleanup # recursive cleanup
		popl %ecx
		movl $0, %eax
		leave
		ret

# int mp1_ioctl_cleanup(struct missile *head)
# helper function for mp1_ioctl_endgame()
# recursively free variables in head
# WARNING: doing this recursion may create issue like stack overflow
mp1_ioctl_cleanup:
		pushl %ebp
		movl  %esp, %ebp

		movl First_ARG(%ebp), %ecx # %ecx = head
		cmpl $0, %ecx # if(head == NULL) return
		je cleanup_done

		pushl NEXT(%ecx)
		call mp1_ioctl_cleanup # mp1_ioctl_cleanup(head->NEXT)
		popl %edx

		pushl %ecx
		call mp1_free # free current node
		popl %ecx

cleanup_done:
		leave
		ret
