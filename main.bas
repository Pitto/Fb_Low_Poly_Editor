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
dim console_message			as proto_console_message
dim wallp_image				as any ptr


user_mouse.is_dragging = false
user_mouse.is_lbtn_released = false
user_mouse.is_lbtn_pressed = false

view_area.x = 0
view_area.y = 0
view_area.zoom = 1.0f
view_area.old_zoom = view_area.zoom
view_area.refresh = true


dim settings as settings_proto
settings.is_snap_active = true
settings.is_hand_active = false
settings.is_bitmap_visible = true
settings.is_centroid_visible = true
settings.is_wireframe_visible = false
settings.wireframe_color = C_WHITE
settings.is_vertex_visible = true
settings.is_debug = false
settings.is_help_visible = false
settings.is_snap_point_available = false
settings.is_alpha_bitmap_visible = true

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

'setting up initial values of keys
for i = 0 to Ubound(key)-1
	key(i).code = keycodes(i)
	key(i).is_down = false
	key(i).old_is_down = false
	key(i).is_released= false
next i

dim console_messages_strings (0 to 31) as string = { _
											"Undefined error", _
											"TOOL: Pen", _
											"TOOL: Selection", _
											"TOOL: Direct Selection", _
											"TOOL: Hand", _
											"Point Added", _
											"Polygon Closed", _
											"Polygon/s deleted", _
											"FILE SAVED", _
											"FILE EXPORTED", _
											"Wireframe: ON", _
											"Wireframe: OFF", _
											"Centroids: ON", _
											"Centroids: OFF", _
											"Bitmap:    ON", _
											"Bitmap:    OFF", _
											"Points:	ON", _
											"Points:    OFF" }
											
dim on_screen_help() as string
load_whole_txt_file	("data/instructions.txt", on_screen_help())

screenres (SCR_W, SCR_H, 24)
SetMouse SCR_W\2, SCR_H\2, 0

dim as FB.Image ptr wallp_img = Load_bmp( "img/test.bmp" )
dim as fb.image ptr wallp_img_resized = ImageScale	(wallp_img,_
													wallp_img->width*view_area.zoom, _
													wallp_img->height*view_area.zoom)
													
dim icon_set (0 to 39) 		as Ulong ptr													
load_icon_set (icon_set(), 240, 96, 10, 4,"data/icon-set.bmp")

dim as Ulong ptr rasterized_artwork
rasterized_artwork = IMAGECREATE (SCR_W, SCR_H)


dim head as point_proto ptr
input_mode = input_add_polygon
console_message = cm_PEN_Tool

do
	if MULTIKEY (SC_Escape) then exit do
	
	dim as integer i, c
	static nearest_point as temp_point_proto
	static dist_from_nearest_point as Uinteger
	dim scalechange as single
	dim timer_diff as double
	dim timer_begin as double
	

	timer_begin = timer
	
	User_Mouse.res = 	GetMouse( 	User_Mouse.x, User_Mouse.y, _
									User_Mouse.wheel, User_Mouse.buttons,_
									User_Mouse.clip)
								
	keyboard_listener	(	@input_mode, user_mouse, @view_area,_
							@settings, key())
	
	'this to avoid unwanted clicks while mouse pointer goes outside the
	'working window
	if (User_Mouse.res = 0) then
		mouse_listener		(@user_mouse, @view_area)
	else
		user_mouse.x = user_mouse.old_x
		user_mouse.y = user_mouse.old_y
	end if
	'-------------------------------------------------------------------
	
	nearest_point = find_nearest_point(polygons(), user_mouse, view_area)
	dist_from_nearest_point = int (dist		(nearest_point.x,_
											nearest_point.y, _
											user_mouse.abs_x, _
											user_mouse.abs_y))
								
	'zoom in / out 
	if (view_area.old_zoom <> view_area.zoom) then
		wallp_img_resized = ImageScale (wallp_img,_
								wallp_img->width*view_area.zoom, _
								wallp_img->height*view_area.zoom)
								
		scalechange = view_area.zoom - view_area.old_zoom
		view_area.x += -(user_mouse.abs_x * scalechange)
		view_area.y += -(user_mouse.abs_y * scalechange)
		view_area.refresh = true
	end if
	
	view_area.old_zoom = view_area.zoom
	
	if settings.is_hand_active then
		input_mode = input_hand
		console_message = cm_TOOL_Hand
		view_area.refresh = true
	end if
	
	select case input_mode
	
		case input_load_lpe_file
			'delete existing artwork before load a new one
			for c = 0 to Ubound(polygons)-1
				delete_all_points (polygons(c).first_point)
			next c
			redim polygons(0 to 0)
		
			load_lpe_file("output.lpe", polygons())
			input_mode = input_add_polygon
			view_area.refresh = true
	
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
		
			'deselect all polygons
			for c = 0 to Ubound(polygons)-1
				polygons(c).is_selected = false
			next c
		
			add_polygon(polygons())
			head = polygons(Ubound(polygons)-1).first_point
			polygons(Ubound(polygons)-1).fill_color = C_GRAY
			'set currently editing polygon as selected
			polygons(Ubound(polygons)-1).is_selected = true
			input_mode = input_add_point
			console_message = cm_PEN_Tool
			view_area.refresh = true
			
		case input_add_point
			
			if (user_mouse.is_lbtn_released) then
				'snapping if mouse pointer is near to existing points
				if 	(settings.is_snap_active) and _
					Cbool(dist_from_nearest_point < MIN_SNAP_DIST/view_area.zoom) then
					
					polygons(Ubound(polygons)-1).first_point = _
					add_point(@head, nearest_point.x, nearest_point.y)
				else
					polygons(Ubound(polygons)-1).first_point = _
					add_point(@head, user_mouse.old_abs_x, user_mouse.old_abs_y)
				end if
				user_mouse.is_lbtn_released = false
				console_message = cm_Point_Added
				view_area.refresh = true
			end if
			
			'close polygon clicking on mouse's right button
			if (user_mouse.is_rbtn_released) then
				input_mode = input_close_polygon
				polygons(Ubound(polygons)-1).centroid = calculate_centroid(polygons(Ubound(polygons)-1).first_point)
				
				polygons(Ubound(polygons)-1).bounds = calculate_bounds (polygons(Ubound(polygons)-1).first_point, polygons(Ubound(polygons)-1).centroid) 
				polygons(Ubound(polygons)-1).fill_color = _
				get_average_color (	polygons(Ubound(polygons)-1).first_point, _
									view_area, wallp_img)
				user_mouse.is_rbtn_released = false
				console_message = cm_Polygon_Closed
				view_area.refresh = true
			end if

		case input_close_polygon
		
			input_mode = input_add_polygon
			console_message = cm_Polygon_Closed
			view_area.refresh = true
			
		case input_erase_all
			for c = 0 to Ubound(polygons)-1
				delete_all_points (polygons(c).first_point)
			next c
			redim polygons(0 to 0)
			input_mode = input_add_polygon
			console_message = cm_Polygon_s_deleted
			view_area.refresh = true
			
		case input_erase_polygon
			'erase selected polygons
			for c = 0 to Ubound(polygons)-1
				if polygons(c).is_selected then
					delete_all_points (polygons(c).first_point)
					polygons(c).first_point = callocate(sizeof(point_proto))
					polygons(c).centroid.x = 0
					polygons(c).centroid.y = 0
				end if
			next c
		
			input_mode = input_add_polygon
			console_message = cm_Polygon_s_deleted
			view_area.refresh = true
			
		case input_export_as_svg
			export_as_svg(polygons(), "output.svg")
			input_mode = input_add_polygon
			console_message = cm_FILE_EXPORTED
			view_area.refresh = true
			
		case input_save_as_lpe_file
			save_as_lpe_file(polygons(), "output.lpe")
			input_mode = input_add_polygon
			console_message = cm_FILE_SAVED
		
		'selects single nodes
		case input_direct_selection
			if user_mouse.is_lbtn_released then
							
				get_selection_bounds(@user_mouse)
			
				unmark_all_nodes(polygons())
				mark_selected_nodes (	user_mouse.bounding_x1, user_mouse.bounding_y1, _
										user_mouse.bounding_x2, user_mouse.bounding_y2, _
										polygons())
				
				view_area.refresh = true
			end if
			user_mouse.is_lbtn_released = false
		
		case input_move_node_up
			move_selected_node(0, -KEYBOARD_PX_INCREMENTS, polygons())
			view_area.refresh = true
			input_mode = input_direct_selection
			
		case input_move_node_down
			move_selected_node(0, +KEYBOARD_PX_INCREMENTS, polygons())
			view_area.refresh = true
			input_mode = input_direct_selection
		
		case input_move_node_left
			move_selected_node(-KEYBOARD_PX_INCREMENTS, 0, polygons())
			view_area.refresh = true
			input_mode = input_direct_selection
			
		case input_move_node_right
			move_selected_node(KEYBOARD_PX_INCREMENTS, 0, polygons())
			view_area.refresh = true
			input_mode = input_direct_selection
		
		case input_selection
			if user_mouse.is_lbtn_released then
				get_selection_bounds(@user_mouse)
				for c = 0 to Ubound(polygons)-1
					if 	polygons(c).centroid.x > user_mouse.bounding_x1 and _
						polygons(c).centroid.y > user_mouse.bounding_y1 and _
						polygons(c).centroid.x < user_mouse.bounding_x2 and _
						polygons(c).centroid.y < user_mouse.bounding_y2 then	
					
						polygons(c).is_selected = true
					else
						polygons(c).is_selected = false
					end if
					
				next c
				view_area.refresh = true
			end if
			user_mouse.is_lbtn_released = false
			
		case input_delete_node
			for c = 0 to Ubound(polygons)-1
				while count_selected_nodes (polygons(c).first_point )
					polygons(c).first_point = delete_selected_node (polygons(c).first_point)
				wend
				polygons(c).centroid = calculate_centroid (polygons(c).first_point)
			next c
			input_mode = input_direct_selection
			view_area.refresh = true
		
		case input_create_random_polygons
		
			create_random_polygons	(polygons(),RANDOM_POLYGONS_QTY, _
									MAX_POLYGONS_NODES, SCR_W, _
									SCR_H, view_area, wallp_img)
			input_mode = input_add_polygon
			view_area.refresh = true
	end select
	
	timer_diff = timer - timer_begin
	
	timer_begin = timer
	screenlock ' Lock the screen
	screenset Workpage, Workpage xor 1 ' Swap work pages.

	cls
	


	c=0

	
	if view_area.refresh then 
	
		if (settings.is_bitmap_visible) then
			put (view_area.x,view_area.y),wallp_img_resized,pset
		end if
	
		'draw the artwork, but not the current drawing polygon
		if (Ubound(polygons)-1) > 0 then
		
			for c = 0 to Ubound(polygons)-2
			
				'fill each polygon only if into current view area
				
				if (is_overlap(	polygons(c).bounds.x1, polygons(c).bounds.y1, _
								polygons(c).bounds.x2, polygons(c).bounds.y2,_
								-view_area.x/ view_area.zoom, -view_area.y/ view_area.zoom, _
								-view_area.x/ view_area.zoom + SCR_W/view_area.zoom, _
								-view_area.y/ view_area.zoom + SCR_H/view_area.zoom)) and _
								settings.is_wireframe_visible = false then
				
					fill_polygon   (polygons(c).first_point, _
									CULng(polygons(c).fill_color), _
									view_area, settings)
				
				end if
				
				if (settings.is_wireframe_visible) then
					draw_wireframe(polygons(c).first_point, C_WHITE, view_area, settings, true)
				end if

				'draw the centroid of each polygon
				if (settings.is_centroid_visible) then
					draw_centroid(polygons(c).centroid, C_GREEN, view_area)
				end if
				'highligt selected polygon
				if polygons(c).is_selected then
					draw_centroid(polygons(c).centroid, C_RED, view_area)
					draw_wireframe(polygons(c).first_point, C_GREEN, view_area, settings, true)
					draw_bounds(polygons(c).bounds, view_area)
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
		
		'alpha bitmap overlayed
		if (settings.is_alpha_bitmap_visible) then
			put (view_area.x,view_area.y),wallp_img_resized,alpha, 75
		end if
		
		view_area.refresh = false
		
		GET (0, 0)-(SCR_W - 1, SCR_H - 1), rasterized_artwork
	
	else
	
		put (0,0), rasterized_artwork, pset
	
	end if
	
	select case input_mode
	
		case input_add_point
			'highlight line of the polygon the user is currently drawing
			i = (Ubound(polygons)-1)
			if i < 0 then i = 0
			if (polygons(i).first_point <> NULL) then
				draw_wireframe(polygons(i).first_point, C_RED, view_area, settings, false)
				if user_mouse.is_lbtn_pressed = false then
					if (polygons(i).first_point->next_p <> NULL)  then
						line 	(polygons(i).first_point->x*view_area.zoom + view_area.x, _
								polygons(i).first_point->y*view_area.zoom + view_area.y)- _
								(User_Mouse.x, User_Mouse.y), C_WHITE,, &b1100110011001100
						line 	(polygons(i).first_point->x*view_area.zoom + view_area.x, _
								polygons(i).first_point->y*view_area.zoom + view_area.y)- _
								(User_Mouse.x, User_Mouse.y), C_BLACK,, &b0011001100110011
					end if
				end if
			end if
			
			'highlight nearest point to mouse, skip if Left or right shift key is down
			if 	Cbool(dist_from_nearest_point < MIN_SNAP_DIST / view_area.zoom) and _
				settings.is_snap_active then
				settings.is_snap_point_available = true
				
				circle (	nearest_point.x*view_area.zoom + view_area.x, _
							nearest_point.y*view_area.zoom + view_area.y), 4, C_GREEN, ,,,F
				circle (	nearest_point.x*view_area.zoom + view_area.x, _
							nearest_point.y*view_area.zoom + view_area.y), 2, C_RED, ,,,F
			else
				settings.is_snap_point_available = false
			end if

		case input_selection
			
			if (user_mouse.is_lbtn_pressed) then
				line (user_mouse.x, user_mouse.y)-(user_mouse.old_x, user_mouse.old_y), C_ORANGE, B
			end if
			
		case input_direct_selection
		
			if (user_mouse.is_lbtn_pressed) then
				line (user_mouse.x, user_mouse.y)-(user_mouse.old_x, user_mouse.old_y), C_ORANGE, B
			end if
			
	end select
	
	 draw_mouse_pointer	(	user_mouse.x, user_mouse.y,_
							user_mouse.is_lbtn_pressed, _
							settings.is_snap_point_available, _
							input_mode, icon_set())
							
	draw_bottom_info 	(console_messages_strings(console_message), _
						view_area, user_mouse, settings, timer_begin, _
						on_screen_help())
						
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
ImageDestroy rasterized_artwork

for c = 0 to Ubound(icon_set)-1
	ImageDestroy icon_set(c)
next c
