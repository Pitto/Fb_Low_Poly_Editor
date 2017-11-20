'Low Poly Editor by Pitto

'This program is free software; you can redistribute it and/or
'modify it under the terms of the GNU General Public License
'as published by the Free Software Foundation; either version 2
'of the License, or (at your option) any later version.
'
'This program is distributed in the hope that it will be useful,
'but WITHOUT ANY WARRANTY; without even the implied warranty of
'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'GNU General Public License for more details.
'
'You should have received a copy of the GNU General Public License
'along with this program; if not, write to the Free Software
'Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
'Also add information on how to contact you by electronic and paper mail.

'#######################################################################

' Compiling instructions: fbc -w all -exx "%f"

#include "fbgfx.bi"

Using FB
randomize timer

#include "definitions.bi"
#include "enums.bi"
#include "types.bi"
#include "functions.bi"


'MAIN___________________________________________________________________

redim polygons(0 to 0) as polygon_proto
DIM workpage 				AS INTEGER
workpage = 0
Dim user_mouse 				as mouse_proto
dim view_area				as view_area_proto
Dim input_mode				as proto_input_mode
dim wallp_image				as any ptr

user_mouse.is_dragging = false
user_mouse.is_lbtn_released = false
user_mouse.is_lbtn_pressed = false

view_area.x = 0
view_area.y = 0
view_area.zoom = 1.0f
view_area.old_zoom = view_area.zoom

dim settings as settings_proto
settings.is_snap_active = true
settings.is_hand_active = false
settings.is_bitmap_visible = true
settings.is_centroid_visible = true
settings.is_wireframe_visible = true
settings.wireframe_color = C_WHITE
settings.is_vertex_visible = true
settings.is_debug = false



dim key(0 to 82) as key_proto

dim keycodes(0 to 82) as integer = {	SC_ESCAPE, SC_1, SC_2, SC_3, SC_4, _
								SC_5, SC_6, SC_7, SC_8, SC_9, SC_0,_
								SC_MINUS, SC_EQUALS, SC_BACKSPACE, _
								SC_TAB, SC_Q, SC_W, SC_E, SC_R, _
								SC_T,SC_Y,SC_U,SC_I,SC_O,SC_P,_
								SC_LEFTBRACKET, SC_RIGHTBRACKET,_
								SC_ENTER,SC_CONTROL,SC_A,_
								SC_S,SC_D,SC_F,SC_G,SC_H,SC_J,SC_K,SC_L,_
								SC_SEMICOLON,SC_QUOTE,SC_TILDE,SC_LSHIFT,_
								SC_BACKSLASH,SC_Z,SC_X,SC_C,SC_V,SC_B,SC_N,_
								SC_M,SC_COMMA,SC_PERIOD,SC_SLASH,SC_RSHIFT,_
								SC_MULTIPLY,SC_ALT,SC_SPACE,SC_CAPSLOCK,_
								SC_F1,SC_F2,SC_F3,SC_F4,SC_F5,SC_F6,SC_F7,_
								SC_F8,SC_F9,SC_F10,SC_NUMLOCK,SC_SCROLLLOCK,_
								SC_HOME,SC_UP,SC_PAGEUP,SC_LEFT,SC_RIGHT,SC_PLUS,_
								SC_END,SC_DOWN,SC_PAGEDOWN,SC_INSERT,SC_DELETE,_
								SC_F11,SC_F12 }
dim i as integer
for i = 0 to Ubound(key)-1
	key(i).code = keycodes(i)
	key(i).is_down = false
	key(i).old_is_down = false
	key(i).is_released= false
next i

screenres (SCR_W, SCR_H, 24)
SetMouse SCR_W\2, SCR_H\2, 0

dim as FB.Image ptr wallp_img = Load_bmp( "img/test.bmp" )
dim as fb.image ptr wallp_img_resized = ImageScale	(wallp_img,_
													wallp_img->width*view_area.zoom, _
													wallp_img->height*view_area.zoom)

dim head as point_proto ptr
input_mode = input_add_polygon

do
	if MULTIKEY (SC_Escape) then exit do
	
	dim as integer i, c
	static nearest_point as temp_point_proto
	static dist_from_nearest_point as Uinteger
	dim scalechange as single

	User_Mouse.res = 	GetMouse( 	User_Mouse.x, User_Mouse.y, _
									User_Mouse.wheel, User_Mouse.buttons,_
									User_Mouse.clip)
								
	keyboard_listener	(	@input_mode, user_mouse, @view_area,_
							@settings, key())
	mouse_listener		(@user_mouse, @view_area)
	
	if (int((timer)*100) MOD 2) = 0  then
		nearest_point = find_nearest_point(polygons(), user_mouse, view_area)
		dist_from_nearest_point = int (dist		(nearest_point.x,_
												nearest_point.y, _
												user_mouse.abs_x, _
												user_mouse.abs_y))
	end if
											
	'zoom in / out 
	if (view_area.old_zoom <> view_area.zoom) then
		wallp_img_resized = ImageScale (wallp_img,_
								wallp_img->width*view_area.zoom, _
								wallp_img->height*view_area.zoom)
								
		scalechange = view_area.zoom - view_area.old_zoom
		view_area.x += -(user_mouse.abs_x * scalechange)
		view_area.y += -(user_mouse.abs_y * scalechange)
	end if
	
	view_area.old_zoom = view_area.zoom
	
	if settings.is_hand_active then
		input_mode = input_hand
	end if
	
	select case input_mode
	
		case input_hand
			'####################### HAND TOOL #########################
			if (user_mouse.is_dragging) then
				line (user_mouse.x, user_mouse.y)-(user_mouse.old_X, user_mouse.old_y)
				view_area.x = view_area.old_x + (user_mouse.x - user_mouse.old_x)
				view_area.y = view_area.old_y + (user_mouse.y - user_mouse.old_y)
			else
				view_area.old_x = view_area.x
				view_area.old_y = view_area.y
			end if
			user_mouse.is_lbtn_released = false
			if not settings.is_hand_active then
				input_mode = input_add_polygon
			end if
	
		case input_add_polygon
		
			add_polygon(polygons())
			head = polygons(Ubound(polygons)-1).first_point
			polygons(Ubound(polygons)-1).fill_color = C_GRAY
			input_mode = input_add_point
	
		case input_add_point
			
			if (user_mouse.is_lbtn_released) then
				'snapping if mouse pointer is near to existing points
				if Cbool(dist_from_nearest_point < MIN_SNAP_DIST/view_area.zoom) and _
					settings.is_snap_active then
					polygons(Ubound(polygons)-1).first_point = _
					add_point(@head, nearest_point.x, nearest_point.y)
				else
					polygons(Ubound(polygons)-1).first_point = _
					add_point(@head, user_mouse.abs_x, user_mouse.abs_y)
				end if

				user_mouse.is_lbtn_released = false
			end if
			
			if (user_mouse.is_rbtn_released) then
				input_mode = input_close_polygon
				polygons(Ubound(polygons)-1).centroid = calculate_centroid(polygons(Ubound(polygons)-1).first_point)
				polygons(Ubound(polygons)-1).fill_color = _
				get_pixel_color	(	int(polygons(Ubound(polygons)-1).centroid.x * view_area.zoom), _
									int(polygons(Ubound(polygons)-1).centroid.y * view_area.zoom), _
									wallp_img_resized)
				user_mouse.is_rbtn_released = false
			end if
		
		case input_close_polygon
		
			input_mode = input_add_polygon
			
		case input_erase_all
			for c = 0 to Ubound(polygons)-1
				delete_all_points (polygons(c).first_point)
			next c
			redim polygons(0 to 0)
			input_mode = input_add_polygon
			
		case input_erase_polygon
			delete_all_points (polygons(Ubound(polygons)-1).first_point)
			polygons(Ubound(polygons)-1).first_point = callocate(sizeof(point_proto))
			'pop_polygon(polygons(), polygons(c))
			input_mode = input_add_polygon
			
		case input_export_as_svg
			export_as_svg(polygons(), "output.svg")
			input_mode = input_add_polygon
			
		case input_save_as_lpe_file
			save_as_lpe_file(polygons(), "output.lpe")
			input_mode = input_add_polygon
	end select
	
	screenlock ' Lock the screen
	screenset Workpage, Workpage xor 1 ' Swap work pages.

	cls
	
	if (settings.is_bitmap_visible) then
		put (view_area.x,view_area.y),wallp_img_resized,pset
	end if

	c=0

	'draw the artwork, but not the current drawing polygon
	if (Ubound(polygons)-1) > 0 then
	
		for c = 0 to Ubound(polygons)-2
			'fill each polygon
			fill_polygon(polygons(c).first_point, CULng(polygons(c).fill_color), view_area, settings)
			'draw the centroid of each polygon
			if (settings.is_centroid_visible) then
				draw_centroid(polygons(c).centroid, C_GREEN, view_area)
			end if
			'show/hide nodes
			if (settings.is_vertex_visible) then
				draw_vertices(polygons(c).first_point, C_WHITE, view_area)
			end if
			'draw some debug info
			if (settings.is_debug) then
				draw_list_points(polygons(c).first_point, 20, 20 + c*20)
			end if
		next c
		
	end if
	
	'highlight line of the polygon the user is currently drawing
	i = (Ubound(polygons)-1)
	if i < 0 then i = 0
	if (polygons(i).first_point <> NULL) then
		draw_wireframe(polygons(i).first_point, C_RED, view_area, settings)
		if (polygons(i).first_point->next_p <> NULL) then
			line 	(polygons(i).first_point->x*view_area.zoom + view_area.x, _
					polygons(i).first_point->y*view_area.zoom + view_area.y)- _
					(User_Mouse.x, User_Mouse.y), C_WHITE,, &b1100110011001100
		end if
	end if
	
	'highlight nearest point to mouse, skip if Left or right shift key is down
	if 	Cbool(dist_from_nearest_point < MIN_SNAP_DIST / view_area.zoom) and _
		settings.is_snap_active then
		circle (	nearest_point.x*view_area.zoom + view_area.x, _
					nearest_point.y*view_area.zoom + view_area.y), 4, C_GREEN, ,,,F
		line (user_mouse.x-5, user_mouse.y-5)-STEP(10,10), C_DARK_GREEN, B
		line (user_mouse.x-6, user_mouse.y-6)-STEP(12,12), C_GREEN, B
	end if

	'mouse graphical cross pointer
	if (user_mouse.is_lbtn_pressed) then
		line (user_mouse.x-5, user_mouse.y-5)-step(10, 10), C_ORANGE, BF
	end if
	
	line (user_mouse.x-5, user_mouse.y-1)-step(10, 2), C_BLACK, BF
	line (user_mouse.x-1, user_mouse.y-5)-step(2, 10), C_BLACK, BF
		
	line (user_mouse.x-10, user_mouse.y)-(user_mouse.x+10, user_mouse.y)
	line (user_mouse.x, user_mouse.y-10)-(user_mouse.x, user_mouse.y+10)
	
	draw string (20, SCR_H - 40), "absolute x " + str(user_mouse.abs_x) + ", y " + str(user_mouse.abs_y)
	draw string (20, SCR_H - 30), "mouse x " + str(user_mouse.x) + ", y " + str(user_mouse.y)
	draw string (20, SCR_H - 20), APP_NAME + " " + APP_VERSION, C_BLACK
	draw string (20, SCR_H - 21), APP_NAME + " " + APP_VERSION, C_WHITE
	
	if (int((timer)*10) MOD 2) = 0  then
		draw string (20, SCR_H - 10), str (int((timer)*10) MOD 2)
	end if
	
	workpage = 1 - Workpage ' Swap work pages.
	screenunlock
	sleep 20,1
	
LOOP


'free memory
dim c as integer
for c = 0 to Ubound(polygons)-1
	delete_all_points (polygons(c).first_point)
next c
deallocate(head)
redim polygons(0 to 0)

'destroy bitmaps from memory
ImageDestroy wallp_img
ImageDestroy wallp_img_resized
