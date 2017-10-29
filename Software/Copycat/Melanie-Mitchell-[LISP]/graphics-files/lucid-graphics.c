/* External Declarations */
#include <stdio.h>
#include <suntool/sunview.h>
#include <math.h>

/*
 * This file contains the simple graphics declarations.
 */

#define TRUE 1
#define FALSE 0

#define op_draw 150
#define op_noop 151
#define op_erase 154
#define op_invert_area 155
#define op_xor 156
#define op_clear 157
#define op_set 158

#define mouse_left_down 256
#define mouse_middle_down 257
#define mouse_right_down 258
#define mouse_move 259
#define mouse_still 260
#define mouse_left_up 261
#define mouse_middle_up 262
#define mouse_right_up 263
#define mouse_screen_enter 264
#define mouse_screen_exit 265

#define trap_mouse_buttons 1001
#define trap_mouse_moves 1002
#define trap_mouse_still 1003
#define trap_keyhits 1004
#define trap_screen_enter 1005
#define trap_screen_exit 1006

extern	struct pixfont *pw_pfsysopen();

static struct pixfont *font = NULL;
static struct pixfont *old_font = NULL;

static Pixwin *pw;
int origin_x;
int origin_y;
int screen_height;
int screen_width;

static int xmouse = 0;
static int ymouse = 0;

/************************************************************/

get_drawing_op(code)
int code;
{
	if (code == op_draw)
		return(PIX_SRC);
	if (code == op_noop)
		return(PIX_DST);
	if (code == op_erase)
		return(PIX_NOT(PIX_SRC) & PIX_DST);
	if (code == op_invert_area)
		return(PIX_NOT(PIX_DST));
	if (code == op_xor)
		return(PIX_SRC ^ PIX_DST);
	if (code == op_clear)
		return(PIX_CLR);
	if (code == op_set)
 		return(PIX_SET);
	(void) fprintf(stderr, "Illegal drawing op code: %d\n",code);
	return(PIX_SRC);
}

/************************************************************/

init_screen(newpixwin, new_screen_width, new_screen_height, new_origin_x, new_origin_y)
Pixwin *newpixwin;
int new_screen_width, new_screen_height, new_origin_x, new_origin_y;
{
	pw = newpixwin;
	screen_width = new_screen_width;
	screen_height = new_screen_height;
	origin_x = new_origin_x;
	origin_y = new_origin_y;
	default_font();
	return(0);
}

/************************************************************/

window_width()
{
  return(screen_width);
}

/************************************************************/

window_height()
{
  return (screen_height);
}

/************************************************************/

clear_window()
{
  pw_writebackground(pw, origin_x, origin_y, screen_width, screen_height, PIX_CLR);
}	

/************************************************************/

translated_pw_vector (x1, y1, x2, y2, op, code)
int x1, y1, x2, y2, op, code;
{
  pw_vector (pw, (x1 + origin_x), (y1 + origin_y), (x2 + origin_x), (y2 + origin_y), op, code);
}

/************************************************************/

op_line (x1, y1, x2, y2, op)
int x1, y1, x2, y2, op;
{
  translated_pw_vector (x1, y1, x2, y2, op, 1);
}

/************************************************************/

draw_line (x1, y1, x2, y2)
int x1, y1, x2, y2;
{
  op_line(x1, y1, x2, y2, (get_drawing_op(op_draw)));
}

/************************************************************/

erase_line (x1, y1, x2, y2)
int x1, y1, x2, y2;
{
  op_line(x1, y1 ,x2, y2, get_drawing_op(op_erase));
}

/************************************************************/

xor_line(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
  op_line(x1, y1, x2, y2, get_drawing_op(op_xor));
}

/************************************************************/

op_dashed_line(x1, y1, x2, y2, op, dash_len, space_len)
int x1, y1, x2, y2, op, dash_len, space_len;
{
	int deltax, deltay, len, x, y, dx1, dy1, dx2, dy2, t;

	if (dash_len <= 0)
		dash_len = 1;
	if (space_len <= 0)
		space_len = 1;
	deltax = x2 - x1;
	deltay = y2 - y1;
	len = int_sqrt((deltax * deltax) + (deltay * deltay));
	if (len <= 0) {
		translated_pw_vector(dx1, dy1, dx1, dy1, op, 1);
		return(0);
	}
	y = y1 * len;
	x = x1 * len;

	t = 0;
	while (t < len) {
		dx1 = ((t * deltax) + x) / len;
		dy1 = ((t * deltay) + y) / len;
		t = t + dash_len;
		if (t > len)
		    t = len;
		dx2 = ((t * deltax) + x) / len;
		dy2 = ((t * deltay) + y) / len;
		t = t + space_len;
		translated_pw_vector(dx1, dy1, dx2, dy2, op, 1);
	}
	return(0);
}		

/************************************************************/

draw_dashed_line(x1, y1, x2, y2, dash_len, space_len)
int x1, y1, x2, y2, dash_len, space_len;
{
  op_dashed_line(x1, y1, x2, y2, get_drawing_op(op_draw), dash_len, space_len);
}

/************************************************************/

erase_dashed_line(x1, y1, x2, y2, dash_len, space_len)
int x1, y1, x2, y2, dash_len, space_len;
{
  op_dashed_line(x1, y1, x2, y2, get_drawing_op(op_erase), dash_len, space_len);
}

/************************************************************/

xor_dashed_line(x1, y1, x2, y2, dash_len, space_len)
int x1, y1, x2, y2, dash_len, space_len;
{
  op_dashed_line(x1, y1, x2, y2, get_drawing_op(op_xor), dash_len, space_len);
}

/************************************************************/

op_jagged_line(x1, y1, x2, y2, op, jag_len, line_len)
int x1, y1, x2, y2, op, jag_len, line_len;
{
	int deltax, deltay, len, x, y, dx1, dy1, dx2, dy2, t;

	if (jag_len <= 0)
		jag_len = 1;
	deltax = x2 - x1;
	deltay = y2 - y1;
	len = int_sqrt((deltax * deltax) + (deltay * deltay));
        if (len <= 0) {
		translated_pw_vector(dx1, dy1, dx1, dy1, op, 1);
		return(0);
	}
        if (line_len <= 0)
	        line_len = len;
	y = y1 * len;
	x = x1 * len;

	t = 0;
	while (t < line_len) {
		dx1 = ((t * deltax) + x) / len;
		dy1 = ((t * deltay) + y) / len;
		t = t + jag_len;
		if (t > line_len)
		    t = line_len;
		dx2 = ((t * deltax) + x) / len;
		dy2 = ((t * deltay) + y) / len;
		translated_pw_vector(dx1, dy1, dx2, dy1, op, 1);
		translated_pw_vector(dx2, dy1, dx2, dy2, op, 1);
	}
	return(0);
}		

/************************************************************/

draw_jagged_line(x1, y1, x2, y2, jag_len, line_len)
int x1, y1, x2, y2, jag_len, line_len;
{
  op_jagged_line(x1, y1, x2, y2, get_drawing_op(op_draw), jag_len, line_len);
}

/************************************************************/

erase_jagged_line(x1, y1, x2, y2, jag_len, line_len)
int x1, y1, x2, y2, jag_len, line_len;
{
  op_jagged_line(x1, y1, x2, y2, get_drawing_op(op_erase), jag_len, line_len);
}

/************************************************************/

xor_jagged_line(x1, y1, x2, y2, jag_len, line_len)
int x1, y1, x2, y2, jag_len, line_len;
{
  op_jagged_line(x1, y1, x2, y2, get_drawing_op(op_xor), jag_len, line_len);
}

/************************************************************/

int set_font(font_name)
char *font_name;
{
       font = pf_open(font_name);
       return(0);
}

/************************************************************/

int default_font()
{
       font = pf_default();
       return(0);
}


/************************************************************/

int text_length(text)
char *text;
{
	struct pr_size prs, pf_textwidth();
	prs = pf_textwidth(strlen(text),font,text);
	return(prs.x);
}

/************************************************************/

int number_length(number)
int number;
{
	struct pr_size prs, pf_textwidth();
	char buffer[20];
	
	(void) sprintf(buffer,"%d",number);
	prs = pf_textwidth(strlen(buffer),font,buffer);
	return(prs.x);
}

/************************************************************/

translated_pw_text(x, y, op, buffer)
int x, y, op;
char buffer[20];
{
  pw_text(pw, (x + origin_x), (y + origin_y), op, font, buffer);
}  

/************************************************************/

op_number(x, y, number, op)
int x, y, number, op;
{
	char buffer[20];
	(void) sprintf(buffer,"%d",number);
               translated_pw_text(x, y, op, buffer);
}

/************************************************************/

draw_number (x, y, number)
int x, y, number;
{
  op_number(x, y, number, get_drawing_op(op_draw));
}

/************************************************************/

erase_number (x, y, number)
int x, y, number;
{
  op_number(x, y, number, get_drawing_op(op_erase));
}

/************************************************************/

xor_number (x, y, number)
int x, y, number;
{
  op_number(x, y, number, get_drawing_op(op_xor));
}



/************************************************************/

op_text(x, y, text, op)
int x, y, op;
char *text;
{
	translated_pw_text(x, y, op, text);
}

/************************************************************/

draw_text(x, y, text)
int x, y;
char *text;
{
  op_text(x, y, text, get_drawing_op(op_draw));
}

/************************************************************/

erase_text(x, y, text)
int x, y;
char *text;
{
  op_text(x, y, text, get_drawing_op(op_erase));
}

/************************************************************/

xor_text(x, y, text)
int x, y;
char *text;
{
  op_text(x, y, text, get_drawing_op(op_xor));
}


/************************************************************/

op_centered_number(x, y, number, limit, op)
int x, y, limit, op, number;
{
	int i, len;
	char buf[20],textbuffer[20];
	char *b = buf, *text = textbuffer;

	(void) sprintf(text,"%d",number);
	len = strlen(text);
	i = text_length(text);
	if (i > limit) {
		len = (len * limit) / i;	
		while (len-- > 0) 
			*b++ = *text++;
		*b = 0;
		translated_pw_text(x, y, op, buf);
	}
	else
		translated_pw_text(x + ((limit - i) / 2), y, op, text);
}

/************************************************************/

draw_centered_number (x, y, number, limit)
int x, y, number, limit;
{
  op_centered_number(x, y, number, limit, get_drawing_op(op_draw));
}

/************************************************************/

erase_centered_number (x, y, number, limit)
int x, y, number, limit;
{
  op_centered_number(x, y, number, limit, get_drawing_op(op_erase));
}

/************************************************************/

xor_centered_number (x, y, number, limit)
int x, y, number, limit;
{
  op_centered_number(x, y, number, limit, get_drawing_op(op_xor));
}

/************************************************************/

op_centered_text(x, y, text,limit,op)
int x, y, limit, op;
char *text;
{
	int i, len;
	char buf[80];
	char *b = buf;

	len = strlen(text);
	i = text_length(text);
	if (i > limit) {
		len = (len * limit) / i;	
		while (len-- > 0) 
			*b++ = *text++;
		*b = 0;
		translated_pw_text(x, y, op, buf);
	}
	else
		translated_pw_text(x + ((limit - i) / 2), y, op, text);
}



/************************************************************/

draw_centered_text (x, y, text, limit)
int x, y, limit;
char *text;
{
  op_centered_text(x, y, text, limit, get_drawing_op(op_draw));
}

/************************************************************/

erase_centered_text (x, y, text, limit)
int x, y, limit;
char *text;
{
  op_centered_text(x, y, text, limit, get_drawing_op(op_erase));
}

/************************************************************/

xor_centered_text (x, y, text, limit)
int x, y, limit;
char *text;
{
  op_centered_text(x, y, text, limit, get_drawing_op(op_xor));
}

/************************************************************/

translated_pw_writebackground (x1, y1, width, height, op)
int x1, y1, width, height, op;
{
  pw_writebackground(pw, (x1 + origin_x), (y1 + origin_y), width, height, op);
}

/************************************************************/

op_solid_rectangle(x1, y1, x2, y2, op)
int x1, y1, x2, y2, op;
{
	translated_pw_writebackground(x1, y1, (x2 - x1), (y2 - y1), op);
}

/************************************************************/

draw_solid_rectangle(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
  op_solid_rectangle(x1, y1, x2, y2, get_drawing_op(op_set));
}

/************************************************************/

erase_solid_rectangle(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
  op_solid_rectangle(x1, y1, x2, y2, get_drawing_op(op_clear));
}

/************************************************************/

xor_solid_rectangle(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
  op_solid_rectangle(x1, y1, x2, y2, get_drawing_op(op_invert_area));
}

/************************************************************/

op_point(x, y, op)
int x, y;
{
  op_solid_rectangle(x, y, (x+2), (y+2), op);
}

/************************************************************/

draw_point(x, y)
int x, y;
{
  op_point(x, y, get_drawing_op(op_set));
}

/************************************************************/

erase_point(x, y)
int x, y;
{
  op_point(x, y, get_drawing_op(op_clear));
}

/************************************************************/

xor_point(x, y)
int x, y;
{
  op_point(x, y, get_drawing_op(op_invert_area));
}  

/************************************************************/

op_unfilled_rectangle(x1, y1, x2, y2, op)
int x1, y1, x2, y2, op;
{
	struct rect rect;

	if (x1 > x2) {
		rect.r_left = x2;
		rect.r_width = x1 - x2 +1;
	}
	else {
		rect.r_left = x1;
		rect.r_width = x2 - x1 + 1;
	}
	if (y1 > y2) {
		rect.r_top = y2;
		rect.r_height = y1 - y2 + 1;
	}
	else {
		rect.r_top = y1;
		rect.r_height = y2 - y1 + 1;
	}
	pw_lock(pw,&rect)
	translated_pw_vector(x1, y1, x1, y2, op, 1);
	translated_pw_vector(x1, y1, x2, y1, op, 1);
	translated_pw_vector(x1, y2, x2, y2, op, 1);
	translated_pw_vector(x2, y1, x2, y2, op, 1);
	pw_unlock(pw);
}

/************************************************************/

draw_unfilled_rectangle(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
  op_unfilled_rectangle(x1, y1, x2, y2, get_drawing_op(op_draw));
}

/************************************************************/

erase_unfilled_rectangle(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
  op_unfilled_rectangle(x1, y1, x2, y2, get_drawing_op(op_erase));
}

/************************************************************/

xor_unfilled_rectangle(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
  op_unfilled_rectangle(x1, y1, x2, y2, get_drawing_op(op_xor));
}

/************************************************************/

int_sqrt(num)
int num;
{
	int x, old_x, counter, old_old_x;
	
	x = num;
	old_x = num;
	old_old_x = -1;

	counter = 0;

	if (x < 0) {
		fprintf(stderr,"?Cannot take square root of %d\n",num);
		x = 0 - x;
	}
	else if (x == 0) {
		return(0);
	}

	while (((x = (((x * x) + num) / (2 * x))) != old_x) &&
	       (counter < 1000) &&
	       (x != old_old_x)) {
		old_old_x = old_x;
		old_x = x;
		counter++;
	}

	if ((x != old_old_x) &&
	    (counter >= 1000)) {
		fprintf(stderr,"?Error calculating square root of %d\n",num);
		return(num);
	}
	else if (x < 0)
		return (0 - x);
	else
		return(x);
}


/************************************************************/

static int denom, a, b, c;

solve_parabola(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
	extern int denom, a, b, c;

	denom = (x1 - x2) * (x1 - x2);
	a = (y1 - y2);
	b = - (2 * (a * x2));
	c = (y1 * denom) - (a * (x1 * x1)) - (b * x1);
	return;
}

/************************************************************/

c_op_parabola(x1, x2, width, op)
int x1, x2, width, op;

{
	extern op_line();
	int counter, end, old_x, old_y, new_y, yy1, yy2, j;

	counter = x1;
	end = x2;
	old_x = counter;
	old_y = ((a * (counter * counter)) + (b * counter) + c) / denom;
	while (counter < end) 
	{
		counter = counter + 5;  /* 5 is a good value for the parabola detail */
		if (counter > end) 
			counter = end;
		new_y = ((a * (counter * counter)) + (b * counter) + c) / denom;
		yy1 = old_y;
		yy2 = new_y;
		j = 1;
		while (j++ <= width) 
		{
			op_line(old_x, yy1, counter, yy2, op);
			yy1++;
			yy2++;
		}
		old_x = counter;
		old_y = new_y;
	}
}

/************************************************************/

c_draw_parabola(x1, x2, width)
int x1, x2, width;
{
  c_op_parabola(x1, x2, width, get_drawing_op(op_draw));
}

/************************************************************/

c_erase_parabola(x1, x2, width)
int x1, x2, width;
{
  c_op_parabola(x1, x2, width, get_drawing_op(op_erase));
}

/************************************************************/

c_xor_parabola(x1, x2, width)
int x1, x2, width;
{
  c_op_parabola(x1, x2, width, get_drawing_op(op_xor));
}


/************************************************************/

c_op_dashed_parabola (x1, x2, width, dash_length, space_length, op)
int x1, x2, width, dash_length, space_length, op;

{
	extern op_line();
	int counter, end, old_x, old_y, new_y, yy1, yy2, j, skip_counter;

	counter = x1;
	end = x2;
	old_x = counter;
	old_y = ((a * (counter * counter)) + (b * counter) + c) / denom;
	skip_counter = space_length;
        while (counter < end) 
	{
          counter = counter + dash_length; 
          skip_counter = skip_counter - 1;
          if (counter > end) 
	    counter = end;
 	  new_y = ((a * (counter * counter)) + (b * counter) + c) / denom;
	  yy1 = old_y;
	  yy2 = new_y;
	  j = 1;
	  while (j++ <= width) 
	  {
  	    if (skip_counter == 0)
	    {
              op_line(old_x, yy1, counter, yy2, op);
	      skip_counter = space_length;
	    }
	    yy1 = yy1++;
	    yy2 = yy2++;
	  }
		  old_x = counter;
		  old_y = new_y;
	}
}

/************************************************************/

c_draw_dashed_parabola(x1, x2, width, dash_length, space_length)
int x1, x2, width, dash_length, space_length;
{
  c_op_dashed_parabola(x1, x2, width, dash_length, space_length, get_drawing_op(op_draw));
}

/************************************************************/

c_erase_dashed_parabola(x1, x2, width, dash_length, space_length)
int x1, x2, width, dash_length, space_length;
{
  c_op_dashed_parabola(x1, x2, width, dash_length, space_length, get_drawing_op(op_erase));
}

/************************************************************/

c_xor_dashed_parabola(x1, x2, width, dash_length, space_length)
int x1, x2, width, dash_length, space_length;
{
  c_op_dashed_parabola(x1, x2, width, dash_length, space_length, get_drawing_op(op_xor));
}


/************************************************************/

#define STEP 4

draw_circle_arc(ctr_x, ctr_y, radius, begin_angle, end_angle)
int ctr_x, ctr_y, radius, begin_angle, end_angle;

{  
    int  theta, int_x_coord, int_y_coord, 
         int_old_x_coord, int_old_y_coord;

    float init_rad_angle, rad_angle, x_coord, y_coord,
          old_x_coord, old_y_coord;

    extern op_line();

     init_rad_angle = 3.1416 * begin_angle / 180.0;


/*     op_number(100, 100, dummy_int , PIX_SRC);  */

     old_x_coord = ctr_x + radius * cos(init_rad_angle);
     old_y_coord = ctr_y - radius * sin(init_rad_angle);
     int_old_x_coord = old_x_coord;
     int_old_y_coord = old_y_coord;

    for (theta = begin_angle; 
         theta <= end_angle; 
         theta = theta + STEP)
    {
        rad_angle = 3.1416 * theta / 180.0;
        x_coord = ctr_x + (radius * cos(rad_angle));
        y_coord = ctr_y - (radius * sin(rad_angle));
        int_x_coord = x_coord;
        int_y_coord = y_coord;

        op_line(int_x_coord, int_y_coord, 
                int_old_x_coord, int_old_y_coord, PIX_SRC);


        int_old_x_coord = int_x_coord;
        int_old_y_coord = int_y_coord;
    }
}


/************************************************************/

erase_circle_arc(ctr_x, ctr_y, radius, begin_angle, end_angle)
int ctr_x, ctr_y, radius, begin_angle, end_angle;
{  
    int  theta, int_x_coord, int_y_coord, 
         int_old_x_coord, int_old_y_coord;

    float init_rad_angle, rad_angle, x_coord, y_coord,
          old_x_coord, old_y_coord;

    extern op_line();

     init_rad_angle = 3.1416 * begin_angle / 180.0;


/*     op_number(100, 100, dummy_int , PIX_SRC);  */

     old_x_coord = ctr_x + radius * cos(init_rad_angle);
     old_y_coord = ctr_y - radius * sin(init_rad_angle);
     int_old_x_coord = old_x_coord;
     int_old_y_coord = old_y_coord;

    for (theta = begin_angle; 
         theta <= end_angle; 
         theta = theta + STEP)
    {
        rad_angle = 3.1416 * theta / 180.0;
        x_coord = ctr_x + (radius * cos(rad_angle));
        y_coord = ctr_y - (radius * sin(rad_angle));
        int_x_coord = x_coord;
        int_y_coord = y_coord;

        op_line(int_x_coord, int_y_coord, 
                int_old_x_coord, int_old_y_coord, PIX_NOT(PIX_SRC) & PIX_DST);


        int_old_x_coord = int_x_coord;
        int_old_y_coord = int_y_coord;
    }
}

/************************************************************/
