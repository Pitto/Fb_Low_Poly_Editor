#ifndef NULL
	const NULL as any ptr = 0
#endif

#ifndef getPixelAddress
    #define getPixelAddress(img,row,col) cast(any ptr,img) + _
        sizeof(FB.IMAGE) + (img)->pitch * (row) + (img)->bpp * (col)
#endif

'define and consts______________________________________________________
#define APP_NAME 				"Low Poly Editor by Pitto"
#define APP_VERSION 			"Version 0.10"
#define SCR_W 					1024		
#define SCR_H 					600
#define MIN_SNAP_DIST			15
#define MIN_SNAP_TO_SNAP_DIST	5
#define MIN_EDGE_SNAP_DIST		20
#define RANDOM_POLYGONS_QTY		200
#define MAX_POLYGONS_NODES		10
#define BTN_W					90 'GUI standard button width
#define BTN_H					12 'GUI standard button heigth

#define KEYBOARD_PX_INCREMENTS	1


'colors
#define C_BLACK			&h000000
#define C_WHITE			&hFFFFFF
#define C_GRAY 			&h7F7F7F
#define C_DARK_GRAY		&h202020
#define C_RED			&hFF0000
#define C_BLUE 			&h0000FF
#define C_GREEN			&h00FF00
#define C_YELLOW		&hFFFF00
#define C_CYAN 			&h00FFFF
#define C_LILIAC		&h7F00FF
#define C_ORANGE		&hFF7F00
#define C_PURPLE		&h7F007F
#define C_DARK_RED 		&h7F0000
#define C_DARK_GREEN	&h005500
#define C_DARK_BLUE		&h00007F

const as double _PI = 4*atn(1)
const as double _2PI = 2*_PI
