'functions declarations
declare function _abtp 			(x1 as integer,y1 as integer,x2 as integer,y2 as integer) as single
declare function add_point		(head as point_proto ptr ptr, x as single, y as single) as point_proto ptr
declare function average_color	(rgb_values() as Ulong) as Ulong
declare function calculate_centroid (head as point_proto ptr) as point_proto
declare function dist 				(x1 as single, y1 as single, x2 as single, y2 as single) as single
declare function find_nearest_point (array() as polygon_proto, user_mouse as mouse_proto, view_area as view_area_proto) as temp_point_proto
declare function get_pixel_color	 (x as integer, y as integer, img_name as any ptr) as ULong
'fbGFXAddon by D.J. Peters  
declare function ImageScale		(byval s as fb.Image ptr, _
								byval w as integer, _
								byval h as integer) as fb.Image ptr
								
declare function pDistance		(x as single, y as single, _
						x1 as single, y1 as single, _
						x2 as single, y2 as single,_
						view_area as view_area_proto) as temp_point_proto

'converts a string of RGB values in only one variable containing
'Unsigned Lonv values (32 bit platform independent)						
declare function string_to_rgb (rgb_input as string) as ULong

'Bmp load by noop
declare function Load_bmp( ByRef filename As Const String ) As Any Ptr

declare function calculate_bounds (head as point_proto ptr, centroid as point_proto) as segment_proto
declare function is_overlap( 	Ax1 as single, Ay1 as single, _
								Ax2 as single, Ay2 as single, _
								Bx1 as single, By1 as single, _
								Bx2 as single, By2 as single ) as boolean
								
declare function get_average_color (	head as point_proto ptr, _
										view_area as view_area_proto, _
										img_name as any ptr) as ULong

'subs declarations______________________________________________________

declare sub add_polygon			(array() as polygon_proto)
declare sub draw_centroid		(centroid as point_proto, stroke_color as Ulong, view_area as view_area_proto)
declare sub draw_list_points	(head as point_proto ptr, x as integer, y as integer)
declare Sub export_as_svg		(array() as polygon_proto, file_name as string)
declare Sub fill_polygon		(head as point_proto ptr, ByVal c As ULong, view_area as view_area_proto, settings as settings_proto)
declare sub draw_highlighted_points(head as point_proto ptr, ByVal c As ULong, w as Ulong)
declare sub keyboard_listener	(input_mode as proto_input_mode ptr, _
								user_mouse as mouse_proto, _
								view_area as view_area_proto ptr, _
								settings as settings_proto ptr, _
								key() as key_proto)
declare sub mouse_listener		(user_mouse as mouse_proto ptr, _
								view_area as view_area_proto ptr)
declare Sub pop_values_in_array	(array() as integer,_
								eval as integer)
declare Sub delete_all_points	(head as point_proto ptr)
declare sub quicksort(array() as temp_point_proto, _left as integer, _right as integer )
declare Sub draw_vertices(head as point_proto ptr, ByVal c As ULong, view_area as view_area_proto)
declare sub reset_key_status(key as key_proto ptr)
declare sub save_as_lpe_file(array() as polygon_proto, file_name as string)
declare sub draw_wireframe	(head as point_proto ptr, ByVal c As ULong, _
							view_area as view_area_proto, _
							settings as settings_proto, _
							close_path as boolean)
declare sub pop_polygon(array() as polygon_proto, polygon as polygon_proto)
declare sub load_lpe_file(filename as string, polygons() as polygon_proto)
declare sub create_random_polygons	(polygons() as polygon_proto,_
									max_polygons as integer, _
									max_nodes as integer, _
									artwork_max_w as integer, _
									artwork_max_h as integer,_
									view_area as view_area_proto, _
									img_name as any ptr)
declare sub draw_bounds 			(bounds	as segment_proto, _
									view_area as view_area_proto)
declare sub load_whole_txt_file			(Byref fn As String,  filearr() As String)
declare sub draw_button 			(x as integer, y as integer, w as integer,_
									h as integer, label as string,_
									is_selected as boolean)
declare sub draw_bottom_info (	console_message as string, _
								view_area as view_area_proto, _
								user_mouse as mouse_proto, _
								settings as settings_proto, _
								timer_begin as double, _
								on_screen_help() as string)
declare sub load_icon_set ( 	bmp() as Ulong ptr, w as integer, _
								h as integer, cols as integer, rows as integer, _
								Byref bmp_path as string)
declare sub draw_mouse_pointer	(	x as integer, y as integer, _
									lbtn_pressed as boolean, _
									is_snap_point_available as boolean, _
									input_mode as proto_input_mode, _
									icon_set() as Ulong ptr)
declare sub mark_selected_nodes (	x1 as integer, y1 as integer, _
									x2 as integer, y2 as integer, _
									array() as polygon_proto)
declare sub unmark_all_nodes (	array() as polygon_proto)
declare sub move_selected_node(x_offset as integer, y_offset as integer, array() as polygon_proto)	
declare function delete_selected_node (node as point_proto ptr) as point_proto ptr
declare function count_selected_nodes (head as point_proto ptr) as integer
declare sub get_selection_bounds (user_mouse as mouse_proto ptr)

'_______________________________________________________________________

'FUNCTIONS______________________________________________________________
function _abtp (x1 as integer,y1 as integer,x2 as integer,y2 as integer) as single
	return -Atan2(y2-y1,x2-x1)
end function

function add_point(head as point_proto ptr ptr, x as single, y as single) as point_proto ptr
    dim as point_proto ptr p = callocate(sizeof(point_proto))
    p->x = x
    p->y = y
    p->is_selected = false
	p->next_p = *head
    *head = p
    return p
end function

'Average color function:
'given an array of rgb colors values as argument
'returns the average color using the arithmetic mean
function average_color(rgb_values() as Ulong) as Ulong
	dim as integer r, g, b, c, arraylen
	
	arraylen = UBound(rgb_values) - LBound(rgb_values) + 1
	
	r = 0 : g = 0 : b = 0

	for c = Lbound(rgb_values) to Ubound(rgb_values)
	
		'get & sum each r, g, b value
		r += rgb_values(c) shr 16
		g += rgb_values(c) shr 8 and &hFF
		b += rgb_values(c) and &hFF
		
	next c
	
	r = r \ (arraylen)
	g = g \ (arraylen)
	b = b \ (arraylen)

	return rgb(r,g,b)

end function


function calculate_centroid (head as point_proto ptr) as point_proto

	'some part of this function is a
	'translation from a C implementation by squeamish ossifrage
	'https://stackoverflow.com/questions/19766485/how-to-calculate-centroid-of-polygon-in-c

	dim centroid as point_proto
	dim as single a, cx, cy, t
    dim as integer i, i1

	redim preserve 	x(0 to 0) as Long
	redim preserve 	y(0 to 0) as Long

	i = 0
   
	while head <> NULL
		if (head->next_p <> NULL) then
			x(i) = head->x
			y(i) = head->y
			redim preserve x(0 to  Ubound(x)+1)
			redim preserve y(0 to  Ubound(y)+1)
		end if
		head = head->next_p
		i+=1
	wend

	'this is the translated part

	'First calculate the polygon's signed area A
	a = 0.0
	i1 = 1

	for i = 0 to (Ubound(x)-1) step 1

		a += x(i) * y(i1) - x(i1) * y(i)
		i1 = (i1 + 1) mod (Ubound(x))

	next i

	a *= 0.5

	' Now calculate the centroid coordinates Cx and Cy */
	cx = cy = 0.0
	i1 = 1

	for i = 0 to (Ubound(x)-1) step 1

		t = x(i)*y(i1) - x(i1)*y(i)
		cx += (x(i)+x(i1)) * t
		cy += (y(i)+y(i1)) * t
		i1 = (i1 + 1) mod (Ubound(x))
		
	next i

	cx = cx / (6.0 * a)
	cy = cy / (6.0 * a)

	centroid.x = cx
	centroid.y = cy

	return centroid

end function

function dist (x1 as single, y1 as single, x2 as single, y2 as single) as single
    return Sqr(((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2)))
end function

function find_nearest_point (array() as polygon_proto, user_mouse as mouse_proto, view_area as view_area_proto) as temp_point_proto
	dim as integer i, j, min_dist, temp_dist, k
	dim as point_proto ptr head

	'store all segments of all polygons in an array
	'and find the distance of line to pointer for each
	redim preserve segments(0 to 0) as segment_proto
	dim close_point as point_proto

	for j = 0 to Ubound(array) - 1

		if (array(j).first_point) <> NULL then
			head = array(j).first_point
			close_point.x = head->x
			close_point.y = head->y
		else
			continue for
		end if

		while head->next_p <> NULL

			segments(i).x1 = head->x
			segments(i).y1 = head->y

			if (head->next_p->next_p <> NULL) then
				segments(i).x2 = head->next_p->x
				segments(i).y2 = head->next_p->y
			else
				'join last segment to the beginning of the path
				segments(i).x2 = close_point.x
				segments(i).y2 = close_point.y
			end if
				
			redim preserve segments(0 to (Ubound(segments)+1))
			i+=1
			head = head->next_p
			
		wend
		
	next j
	
	redim preserve nearest_points(0 to (Ubound(segments)+1)) as temp_point_proto
	
	for i = 0 to Ubound(nearest_points)-1
		nearest_points(i) = pDistance	(user_mouse.abs_x, user_mouse.abs_y, _
								segments(i).x1, _
								segments(i).y1, _
								segments(i).x2, _
								segments(i).y2, _
								view_area)
	next i
	
	quicksort (nearest_points(), Lbound(nearest_points), Ubound(nearest_points))
	
	if UBound(nearest_points) > 0 then
		return nearest_points(1)
	else
		return nearest_points(0)
	end if

end function

function get_pixel_color (x as integer, y as integer, img_name as any ptr) as ULong
	dim p as Uinteger 
	p = point(x,y, img_name)
	return p
	
end function


'fbGFXAddon by D.J. Peters
function ImageScale(byval s as fb.Image ptr, _
                    byval w as integer, _
                    byval h as integer) as fb.Image ptr
  #macro SCALELOOP()
  for ty = 0 to t->height-1
    ' address of the row
    pr=ps+(y shr 20)*sp
    x=0 ' first column
    for tx = 0 to t->width-1
      *pt=pr[x shr 20]
      pt+=1 ' next column
      x+=xs ' add xstep value
    next
    pt+=tp ' next row
    y+=ys ' add ystep value
  next
  #endmacro
  ' no source image
  if s        =0 then return 0
  ' source widh or height legal ?
  if s->width <1 then return 0
  if s->height<1 then return 0
  ' target min size ok ?
  if w<2 then w=1
  if h<2 then h=1
  ' create new scaled image
  dim as fb.Image ptr t=ImageCreate(w,h,RGB(0,0,0))
  ' x and y steps in fixed point 12:20
  dim as FIXED xs=&H100000*(s->width /t->width ) ' [x] [S]tep
  dim as FIXED ys=&H100000*(s->height/t->height) ' [y] [S]tep
  dim as integer x,y,ty,tx
  select case as const s->bpp
  case 1 ' color palette
    dim as ubyte    ptr ps=cptr(ubyte ptr,s)+32 ' [p]ixel   [s]ource
    dim as uinteger     sp=s->pitch             ' [s]ource  [p]itch
    dim as ubyte    ptr pt=cptr(ubyte ptr,t)+32 ' [p]ixel   [t]arget
    dim as uinteger     tp=t->pitch - t->width  ' [t]arget  [p]itch
    dim as ubyte    ptr pr                      ' [p]ointer [r]ow
    SCALELOOP()
  case 2 ' 15/16 bit
    dim as ushort   ptr ps=cptr(ushort ptr,s)+16
    dim as uinteger     sp=(s->pitch shr 1)
    dim as ushort   ptr pt=cptr(ushort ptr,t)+16
    dim as uinteger     tp=(t->pitch shr 1) - t->width
    dim as ushort   ptr pr
    SCALELOOP()
  case 4 ' 24/32 bit
    dim as ulong    ptr ps=cptr(uinteger ptr,s)+8
    dim as uinteger     sp=(s->pitch shr 2)
    dim as ulong    ptr pt=cptr(uinteger ptr,t)+8
    dim as uinteger     tp=(t->pitch shr 2) - t->width
    dim as ulong    ptr pr
    SCALELOOP()
  end select
  return t
  #undef SCALELOOP
end function

Function Load_bmp( ByRef filename As Const String ) As Any Ptr
	'Bmp load by noop
	'http://www.freebasic.net/forum/viewtopic.php?t=24586
    Dim As Long filenum, bmpwidth, bmpheight
    Dim As Any Ptr img

    '' open BMP file
    filenum = FreeFile()
    If Open( filename For Binary Access Read As #filenum ) <> 0 Then Return NULL

        '' retrieve BMP dimensions
        Get #filenum, 19, bmpwidth
        Get #filenum, 23, bmpheight

    Close #filenum

    '' create image with BMP dimensions
    img = ImageCreate( bmpwidth, Abs(bmpheight) )

    If img = NULL Then Return NULL

    '' load BMP file into image buffer
    If BLoad( filename, img ) <> 0 Then ImageDestroy( img ): Return NULL

    Return img

End Function

'SUBS
sub add_polygon(array() as polygon_proto)
	array(Ubound(array)).first_point = callocate(sizeof(point_proto))
	
	redim preserve array(Lbound(array) to Ubound(array)+1)
end sub

sub draw_centroid(centroid as point_proto, stroke_color as Ulong, view_area as view_area_proto)
	dim as integer x_offset, y_offset
	x_offset = centroid.x * view_area.zoom + view_area.x 
	y_offset = centroid.y * view_area.zoom + view_area.y 
	line (x_offset - 2,  y_offset)-step(4,0), stroke_color
	line (x_offset,  y_offset - 2)-step(0,4), stroke_color
end sub

Sub export_as_svg (array() as polygon_proto, file_name as string)

	Dim i as integer
	Dim head as point_proto ptr
	Dim ff As UByte
	ff = FreeFile
	Open file_name for output As #ff

	'SVG file header info
	
	Print #ff, "<?xml version='1.0' standalone='no'?>"
	Print #ff, "<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>"
	Print #ff, "<svg  version='1.1' xmlns='http://www.w3.org/2000/svg'>"
	Print #ff, "<desc>" + APP_NAME + APP_VERSION + " - Export file</desc>"

	for i = 0 to Ubound(array)-1
		dim as Ubyte r, g, b
		
		r = array(i).fill_color shr 16 and &hFF
		g = array(i).fill_color shr 8 and &hFF
		b = array(i).fill_color and &hFF

		Print #ff, "<polygon fill='rgb(" + str(r) + "," + str(g) + "," + str(b) + ")' "
					'hex(array(i).fill_color shr 16 and &hFF) + _
					'hex(array(i).fill_color shr 8 and &hFF) +_
					'hex(array(i).fill_color and &hFF) + "'"
		Print #ff, "points='"
		
		head = array(i).first_point
		
		'ignore first one pointer values since it's only a link to data
		while head->next_p <> NULL
		
			Print #ff, str(head->x) + "," + str(head->y) + " "
			head = head->next_p
			
		wend
		
		Print #ff, "' />"
		
	next i

	Print #ff, "</svg>"
	Close #ff

end sub

Sub save_as_lpe_file(array() as polygon_proto, file_name as string)

	Dim as integer i, j
	dim as Ubyte r, g, b
	
	Dim head as point_proto ptr
	Dim ff As UByte
	ff = FreeFile
	Open file_name for output As #ff
	
	for i = 0 to Ubound(array)-1
		dim line_output as string
		
		r = array(i).fill_color shr 16 'and &hFF
		g = array(i).fill_color shr 8 and &hFF
		b = array(i).fill_color and &hFF
	
		'save RGB values in
		'line_output = line_output + str(hex(array(i).fill_color shr 16 and &hFF)) + _
		line_output = str(r) + "," + str(g) + "," + str(b) + "; "
					'str(hex(array(i).fill_color shr 8 and &hFF)) +_
					'str(hex(array(i).fill_color and &hFF)) + "; "
		
		head = array(i).first_point
		
		redim temp_array(0 to 0) as point_proto

		'ignore first one pointer values since it's only a link to data
		while head->next_p <> NULL
		
			'Print #ff, str(head->x) + "," + str(head->y)
			temp_array(Ubound(temp_array)).x = head->x
			temp_array(Ubound(temp_array)).y = head->y
			
			head = head->next_p
			
			redim preserve temp_array(0 to Ubound(temp_array)+1) as point_proto
			
		wend
		
		for j = 0 to Ubound(temp_array) -1
			line_output = line_output + str(temp_array(j).x) + "," + str(temp_array(j).y) + "; "
		next j
		
		Print #ff, line_output

	next i

	Close #ff

end sub

Sub draw_vertices(head as point_proto ptr, ByVal c As ULong, view_area as view_area_proto)
  
	while head <> NULL
		if (head->next_p <> NULL) then
			'highlight the selected node
			if head->is_selected then
			circle	(head->x*view_area.zoom + view_area.x , _
					head->y*view_area.zoom + view_area.y ), 3, C_BLUE,,,,F
			
			end if
			
			circle	(head->x*view_area.zoom + view_area.x , _
					head->y*view_area.zoom + view_area.y), 2,C_WHITE,,,,F
			

		end if
		head = head->next_p
	wend
End Sub

function calculate_bounds (head as point_proto ptr, centroid as point_proto) as segment_proto

	dim bounds as segment_proto
	dim i as integer
	
	bounds.x1 = centroid.x
	bounds.x2 = centroid.x
	bounds.y1 = centroid.y
	bounds.y2 = centroid.y
	
	i = 0
	while head <> NULL
		if (head->next_p <> NULL) then
			if head->x < bounds.x1 then bounds.x1 = head->x 
			if head->y < bounds.y1 then bounds.y1 = head->y
			if head->x > bounds.x2 then bounds.x2 = head->x 
			if head->y > bounds.y2 then bounds.y2 = head->y 
		end if
		head = head->next_p
		i+=1
	wend
	
	return bounds

end function

Sub fill_polygon(head as point_proto ptr, ByVal c As ULong, view_area as view_area_proto, settings as settings_proto)
   'translation of a c snippet by Angad
   'source of c code:
   'http://code-heaven.blogspot.it/2009/10/simple-c-program-for-scan-line-polygon.html
   
   ' Thanks to MrSwiss for the corrections on the below code for 64/32 compiler
   redim preserve 	a(0 to 0, 0 to 1) as Long
   Dim As Long      i, j, k, dy, dx, x, y, temp
  
   
   i = 0
   while head <> NULL
		if (head->next_p <> NULL) then
			a(i, 0) = head->x*view_area.zoom + view_area.x
			a(i, 1) = head->y*view_area.zoom + view_area.y
			redim preserve a(0 to  Ubound(a)+1, 0 to 1)
		end if
		head = head->next_p
		i+=1
	wend
   
   Dim As Long      xi(0 to Ubound(a, 1))
   Dim As Single    slope(0 to Ubound(a, 1))
   'join first and last vertex
   a(Ubound(a, 1), 0) = a(0, 0)
   a(Ubound(a, 1), 1) = a(0, 1)

   For i = 0 To Ubound(a, 1) - 1
		
		
	dy = a(i+1, 1) - a(i, 1)
      dx = a(i+1, 0) - a(i, 0)

      If (dy = 0) Then slope(i) = 1.0
      If (dx = 0) Then slope(i) = 0.0

      If (dy <> 0) AndAlso (dx <> 0) Then slope(i) = dx / dy
    
   Next i

   For y = 0 to SCR_H - 1
      k = 0
      ' using FB's short-cut operators (which C doesn't have!)
      For i = 0 to Ubound(a, 1) - 1
         If (a(i, 1) <= y AndAlso a(i+1, 1) > y) OrElse _
             (a(i, 1) > y AndAlso a(i+1, 1) <= y) Then
            xi(k) = CLng(a(i, 0) + slope(i) * (y - a(i, 1)))
            k += 1
         End If
      Next i

      For j = 0 to k - 2
         'Arrange x-intersections in order
         For i = 0 To k - 2
            If (xi(i) > xi(i + 1)) Then
               temp = xi(i)
               xi(i) = xi(i + 1)
               xi(i + 1) = temp
            End If
         Next i
      Next j
      'line filling
      For i = 0 To k - 2 Step 2
         Line (xi(i), y)-(xi(i + 1) + 1, y), c
      Next i

   Next y
   
End Sub

sub keyboard_listener(	input_mode as proto_input_mode ptr, _
						user_mouse as mouse_proto, _
						view_area as view_area_proto ptr,_
						settings as settings_proto ptr, _
						key() as key_proto)
						
	dim i as integer
	'keyboard key released listener
	for i = 0 to Ubound(key)-1
		if multikey (key(i).code) then
			key(i).is_down = true
		else
			key(i).is_down = false
		end if
		if (key(i).is_down = false) and (key(i).old_is_down = true) then
			key(i).is_released = true
		end if	
	next i

	for i = 0 to Ubound(key)-1
		if (key(i).is_released) then
			
		
			select case key(i).code
				'show / hide on screen help
				case SC_F1
					settings->is_help_visible = not settings->is_help_visible
					view_area->refresh = true
					reset_key_status(@key(i))
				'show / hide debug info
				case SC_D
					settings->is_debug = not settings->is_debug
					view_area->refresh = true
					reset_key_status(@key(i))
				'show / hide centroids of polygons
				case SC_C
					settings->is_centroid_visible = not settings->is_centroid_visible
					view_area->refresh = true
					reset_key_status(@key(i))
				'show / hide wireframe
				case SC_W
					settings->is_wireframe_visible = not settings->is_wireframe_visible
					view_area->refresh = true
					reset_key_status(@key(i))
				'show / hide bitmap
				case SC_B
					settings->is_bitmap_visible = not settings->is_bitmap_visible
					view_area->refresh = true
					reset_key_status(@key(i))
				'show / hide alpha bitmap overlayed
				case SC_X
					settings->is_alpha_bitmap_visible = _
					not settings->is_alpha_bitmap_visible
					view_area->refresh = true
					reset_key_status(@key(i))
				'export as SVG
				case SC_E
					*input_mode = input_export_as_svg
					reset_key_status(@key(i))
				'save in a file
				case SC_S
					*input_mode = input_save_as_lpe_file
					reset_key_status(@key(i))
				'delete all
				case SC_DELETE
					if *input_mode = input_selection then
						*input_mode = input_erase_polygon
					elseif *input_mode = input_direct_selection then
						*input_mode = input_delete_node
					end if
					view_area->refresh = true
					reset_key_status(@key(i))
				'show / hide vertices
				case SC_Q
					settings->is_vertex_visible = not settings->is_vertex_visible
					view_area->refresh = true
					reset_key_status(@key(i))
				'selection mode
				case SC_V
					*input_mode = input_selection
					reset_key_status(@key(i))
				'pen tool
				case SC_P
					if (*input_mode <> input_add_polygon) then
						*input_mode = input_add_polygon
					end if
					reset_key_status(@key(i))
				'load lpe file
				case SC_L
					if (multikey(SC_CONTROL)) then
						*input_mode = input_load_lpe_file
					end if
					reset_key_status(@key(i))
				'create random polygons
				case SC_R
					if (multikey(SC_CONTROL)) then
						*input_mode = input_create_random_polygons
					end if
					reset_key_status(@key(i))
				'direct selection
				case SC_A
					*input_mode = input_direct_selection
					reset_key_status(@key(i))
					
				case SC_UP
					'direct selection - node selected + UP key
					if *input_mode = input_direct_selection then
						*input_mode = input_move_node_up
					end if
					reset_key_status(@key(i))
				
				case SC_DOWN
					'direct selection - node selected + DOWN key
					if *input_mode = input_direct_selection then
						*input_mode = input_move_node_down
					end if
					reset_key_status(@key(i))
					
				case SC_LEFT
					'direct selection - node selected + LEFT key
					if *input_mode = input_direct_selection then
						*input_mode = input_move_node_left
					end if
					reset_key_status(@key(i))
					
				case SC_RIGHT
					'direct selection - node selected + RIGHT key
					if *input_mode = input_direct_selection then
						*input_mode = input_move_node_right
					end if
					reset_key_status(@key(i))
				
			end select
		
			
		end if
	
		key(i).old_is_down = key(i).is_down
	next i

	'this is for the hand ovverride tool
	if multikey (SC_SPACE) then
		settings->is_hand_active = true
	else
		settings->is_hand_active = false
	end if


	if ((multikey(SC_LSHIFT)) or (multikey(SC_LSHIFT))) then
		settings->is_snap_active = false
	else
		settings->is_snap_active = true
	end if

	
end sub

sub draw_list_points(head as point_proto ptr, x as integer, y as integer)
	dim as integer c = 0
	
	while (head <> NULL)
		draw string (x + c*100, y), ">" +str(hex(head)), C_DARK_GRAY	
		draw string (x + c*100, y+8), " " + str(int(head->x)) + "," + str(int(head->y)), C_DARK_GRAY	
		head = head->next_p
		c += 1
	wend
end sub

Sub delete_all_points	(head as point_proto ptr)
	dim temp as point_proto ptr
	while (head <> NULL)
		temp = Head
		head = temp->next_p
		deallocate(temp)
	wend
end sub

sub mark_selected_nodes (	x1 as integer, y1 as integer, _
							x2 as integer, y2 as integer, _
							array() as polygon_proto)	
	dim i as integer
	dim head as point_proto ptr
		
	for i = 0 to Ubound(array)-1
			head = array(i).first_point
			while head->next_p <> NULL
				if 	head->x > x1 and  head->y > y1 and _
					head->x < x2 and head->y < y2 then
					
					head->is_selected = true
				end if
				
				head = head->next_p
			wend
	next i							
end sub

sub mouse_listener(user_mouse as mouse_proto ptr, view_area as view_area_proto ptr)
	static old_is_lbtn_pressed as boolean = false
	static old_is_rbtn_pressed as boolean = false
	static as integer old_x, old_y
	static store_xy as boolean = false
	static begin_store_xy as boolean = false
	dim as integer scalechange
	
	user_mouse->abs_x = int(user_mouse->x / view_area->zoom + (-view_area->x / view_area->zoom))
	user_mouse->abs_y = int(user_mouse->y / view_area->zoom + (-view_area->y / view_area->zoom))
	
	user_mouse->old_abs_x = int(user_mouse->old_x / view_area->zoom + (-view_area->x / view_area->zoom))
	user_mouse->old_abs_y = int(user_mouse->old_y / view_area->zoom + (-view_area->y / view_area->zoom))
	
	if User_Mouse->old_wheel < User_Mouse->wheel and view_area->zoom < 4 then
      view_area->zoom *= 2.0f
   end if
   if User_Mouse->old_wheel > User_Mouse->wheel and view_area->zoom > 0.25 then
      view_area->zoom *= 0.5f
   end if
   

	'recognize if the left button has been pressed
	if User_Mouse->buttons and 1 then
		User_Mouse->is_lbtn_pressed = true
		
		user_mouse->drag_x2 = user_mouse->abs_x
		user_mouse->drag_y2 = user_mouse->abs_y
	else
		User_Mouse->is_lbtn_pressed = false
	end if
	
	'recognize if the right button has been pressed
	if User_Mouse->buttons and 2 then
		User_Mouse->is_rbtn_pressed = true
	else
		User_Mouse->is_rbtn_pressed = false
	end if
	
	'recognize if the left button has been released
	if old_is_lbtn_pressed = false and User_Mouse->is_lbtn_pressed and store_xy = false then 
		store_xy = true
	end if
	
	if store_xy then
		user_mouse->old_x = user_mouse->x
		user_mouse->old_y = user_mouse->y

		store_xy = false
		begin_store_xy = false
	end if
	
	'recognize if the left button has been released
	if old_is_lbtn_pressed and User_Mouse->is_lbtn_pressed = false then 
		User_Mouse->is_lbtn_released = true
	end if
	
	'recognize if the right button has been released
	if old_is_rbtn_pressed and User_Mouse->is_rbtn_pressed = false then 
		User_Mouse->is_rbtn_released = true
	end if
	
	'recognize drag
	if (User_Mouse->is_lbtn_pressed) and CBool((old_x <> user_mouse->x) or (old_y <> user_mouse->y)) then
		user_mouse->is_dragging = true
		'cuspid node
		if multikey(SC_ALT) then
			user_mouse->oppo_x = user_mouse->old_oppo_x
			user_mouse->oppo_y = user_mouse->old_oppo_y
		'normal node
		else
			user_mouse->oppo_x = User_Mouse->old_x - _
						cos (_abtp (User_Mouse->old_x, User_Mouse->old_y, User_Mouse->x, User_Mouse->y)) * _
						(dist(User_Mouse->old_x, User_Mouse->old_y, User_Mouse->x, User_Mouse->y))
			user_mouse->oppo_y = User_Mouse->old_y - _
						-sin(_abtp (User_Mouse->old_x, User_Mouse->old_y, User_Mouse->x, User_Mouse->y)) * _
						(dist(User_Mouse->old_x, User_Mouse->old_y, User_Mouse->x, User_Mouse->y))
			user_mouse->old_oppo_x = user_mouse->oppo_x
			user_mouse->old_oppo_y = user_mouse->oppo_y
		end if			
		
	else
		user_mouse->is_dragging = false
	end if
	
	if user_mouse->is_dragging and begin_store_xy = false then
		begin_store_xy = true
		user_mouse->drag_x1 = user_mouse->abs_x
		user_mouse->drag_y1 = user_mouse->abs_y
	end if
	
	
	   'store the old wheel state
	User_Mouse->old_wheel = User_Mouse->wheel
	'store the old state of left button
	old_is_lbtn_pressed = User_Mouse->is_lbtn_pressed
	'store the old state of left button
	old_is_rbtn_pressed = User_Mouse->is_rbtn_pressed
	
	
end sub


sub pop_values_in_array(array() as integer, eval as integer)
	'given a monodimensional re-dimmable array, pops all the data
	'that are equal to eval and resizes the array
	dim as integer i, j
	
	'transverse whole array, if the array(i) value
	'matches the eval, shift non-eval values of the array on the left.
	for i = Lbound(array) to Ubound(array)
		if array(i) = eval then 
			for j = (i + 1) to Ubound(array)
				if array(j) <> eval then
					swap array(j), array (i)
					exit for
				end if 
			next j
		end if
	next i
	
	'find new first eval value location
	for i = Lbound(array) to Ubound(array)
		if array(i) = eval then 
			exit for
		end if
	next i
	
	'redim the array
	redim preserve array(Lbound(array) to i-1) as integer
	
end sub


sub quicksort(array() as temp_point_proto, _left as integer, _right as integer )
	dim as integer i, j
	dim as single x, y
	
	i = _left
	j = _right
	
	x = array((_left + _right)\2).distance
	
	do
		while ((array(i).distance < x) and (i < _right))
			i +=1
		wend
		
		while ((x < array(j).distance) and (j > _left))
			j -=1
		wend
		
		if (i <=j) then
			'y = array(i)
			swap array(i), array (j)
			'array(j) = y
			i += 1
			j -= 1
		end if
		
	loop while (i <= j)
	
	if (_left < j) then quicksort (array(), _left, j)
	if (i < _right) then quicksort (array(), i, _right)

end sub


function pDistance		(x as single, y as single, _
						x1 as single, y1 as single, _
						x2 as single, y2 as single, _
						view_area as view_area_proto) as temp_point_proto
	'translated from https://stackoverflow.com/questions/849211/
	'shortest-distance-between-a-point-and-a-line-segment
	
	dim as single A, B, C, D, xx, yy, dot, len_sq, param
	dim nearest_point as temp_point_proto
	
	A = x - x1
	B = y - y1
	C = x2 - x1
	D = y2 - y1

	dot = A * C + B * D
	len_sq = C * C + D * D
	param = -1
	
	if (len_sq <> 0) then 'in case of 0 length line
      param = dot / len_sq
    end if
	
	if (param < 0) then 
		nearest_point.x = x1
		nearest_point.y = y1
	elseif (param > 1) then
		nearest_point.x = x2
		nearest_point.y = y2
	else
		nearest_point.x = x1 + param * C
		nearest_point.y = y1 + param * D
	end if
	
	'snapping to the end of the segment
	if dist(x1, y1, x, y) < MIN_EDGE_SNAP_DIST / view_area.zoom then
		nearest_point.x = x1
		nearest_point.y = y1
		nearest_point.distance = dist (x1, y1,x, y)
	elseif dist(x2, y2, x, y) < MIN_EDGE_SNAP_DIST / view_area.zoom then
		nearest_point.x = x2
		nearest_point.y = y2
		nearest_point.distance = dist (x2, y2,x, y)
	'snapping along the edge the segment
	else
		nearest_point.distance = dist (x, y,nearest_point.x, nearest_point.y)
	end if
	
	return nearest_point
	
	
	
end function

sub reset_key_status (key as key_proto ptr)
	key->is_released = false
	key->is_down = false
	key->old_is_down = false
end sub


Sub draw_wireframe(	head as point_proto ptr, ByVal c As ULong, _
					view_area as view_area_proto, _
					settings as settings_proto, _
					close_path as boolean)

   redim preserve 	a(0 to 0) as point_proto
   Dim i As Long

   i = 0
   while head <> NULL
		if (head->next_p <> NULL) then
			a(i).x = head->x*view_area.zoom + view_area.x
			a(i).y = head->y*view_area.zoom + view_area.y
			redim preserve a(0 to  Ubound(a)+1)
		end if
		head = head->next_p
		i+=1
	wend
   

	if close_path then
	   'join first and last vertex
		a(Ubound(a)) = a(0)
		'draw joined wireframe
		For i = 0 To Ubound(a) - 1
			line(a(i+1).x,a(i+1).y)-(a(i).x,a(i).y),c
		next i
	else
		'join first and last vertex
		a(Ubound(a)) = a(0)
		'draw unjoined wireframe
		if Ubound(a) > 1 then
			For i = 0 To Ubound(a) - 2
				line(a(i+1).x,a(i+1).y)-(a(i).x,a(i).y),c
			next i
		end if
	end if



End Sub

sub pop_polygon(array() as polygon_proto, polygon as polygon_proto)

'some stuff here
	
end sub



sub load_lpe_file(filename as string, polygons() as polygon_proto)

	'modified version of a snippet by MrSwiss
	'Loading a CSV file into an array
	'https://www.freebasic.net/forum/viewtopic.php?t=25693
	
	dim as string textline, token, tokens()
	dim as integer pos1 = 1, pos2 , filenum, res, j

	filenum = Freefile
	res 	= Open (filename, For Input, As #filenum)

	j = 0
	if res = 0 then 
		While (Not Eof(filenum))
			
			Line Input #filenum, textline ' Get one whole text line
			j +=1
			redim tokens(0 to 0)
			
			do
				' next semicolor position
				pos2 = instr(pos1, textline, ";")
				' if new semicolon found,
				' take the substring between the last semicolon and it
				if pos2 > 0 Then
					token = mid(textline, pos1, pos2 - pos1)    ' calc. len (new)
				Else
					token = Mid(textline, pos1)
				end if
			   
				' add the token to the end of the array (slightly inefficient)
				redim preserve tokens(0 to ubound(tokens) + 1)
				tokens(ubound(tokens)) = token
			   
				pos1 = pos2 + 1 ' added + 1
			loop until pos2 = 0
			
			'skip if there is an empty line
			if Ubound(tokens) > 2 then
				add_polygon(polygons())
				
				dim head as point_proto ptr
				head = polygons(Ubound(polygons)-1).first_point
				
				'fill main data of polygon
				polygons(Ubound(polygons)-1).fill_color = string_to_rgb(tokens(1))
				polygons(Ubound(polygons)-1).is_selected = false

				'skip first point since it contains only color and centroid data
				for i as integer = ubound(tokens)-1 to 2 step -1
					dim as string x, y
					dim as integer comma_pos
					'print tokens(i)
					comma_pos = instr(tokens(i), ",")
					if (comma_pos) then
						x = mid(tokens(i), 2, comma_pos -2)
						y = mid(tokens(i), comma_pos + 1, len(tokens(i))-(comma_pos-1))
					end if
					
					polygons(Ubound(polygons)-1).first_point = _
							add_point(@head, cast(single, x), cast(single,y))
					
				next i
				'calculate again centroid and bound
				polygons(Ubound(polygons)-1).centroid = _
				calculate_centroid(polygons(Ubound(polygons)-1).first_point)
				polygons(Ubound(polygons)-1).bounds = _
				calculate_bounds (polygons(Ubound(polygons)-1).first_point, polygons(Ubound(polygons)-1).centroid) 
					
				
			end if
		Wend

		Close #filenum
	end if

end sub

sub create_random_polygons	(polygons() as polygon_proto,_
							max_polygons as integer, _
							max_nodes as integer, artwork_max_w as integer, _
							artwork_max_h as integer,_
							view_area as view_area_proto,_
							img_name as any ptr)

	dim head as point_proto ptr
	
	for i as integer = 0 to max_polygons
	
		add_polygon(polygons())
		head = polygons(Ubound(polygons)-1).first_point
		polygons(Ubound(polygons)-1).fill_color = C_GRAY
		polygons(Ubound(polygons)-1).is_selected = false
		
		'at least 3 nodes
		dim random_node_qty as integer
		random_node_qty = int(rnd * max_nodes)
		if random_node_qty < 4 then random_node_qty = 4
		
		'random place
		dim as integer x, y
		dim as single rotation
		
		x = rnd*artwork_max_w
		y = rnd*artwork_max_h
		rotation = 0.0f
		
		for j as integer = 0 to random_node_qty
			rotation += (_2PI / random_node_qty)
			
			dim as integer x1, y1
			
			x1 = x + cos(_abtp(x, y, x + cos(rotation)*10, y + -sin(rotation)*10))*10
			y1 = y + -sin(_abtp(x, y, x + cos(rotation)*10, y + -sin(rotation)*10))*10
			
			polygons(Ubound(polygons)-1).first_point = _
					add_point(@head, x1, y1)
		
		next j
		
		polygons(Ubound(polygons)-1).centroid = calculate_centroid(polygons(Ubound(polygons)-1).first_point)
		polygons(Ubound(polygons)-1).bounds = calculate_bounds (polygons(Ubound(polygons)-1).first_point, polygons(Ubound(polygons)-1).centroid) 
				
				polygons(Ubound(polygons)-1).fill_color = _
				get_average_color (	polygons(Ubound(polygons)-1).first_point, _
									view_area, img_name)
	next i
	
	

end sub

function string_to_rgb (rgb_input as string) as ULong

redim tokens(0 to 0) as string
dim as integer pos1 = 1, pos2
dim as string token
	
	do
		' next comma position
		pos2 = instr(pos1, rgb_input, ",")
		' if new comma found,
		' take the substring between the last comma and it
		if pos2 > 0 Then
			token = mid(rgb_input, pos1, pos2 - pos1)    ' calc. len (new)
		Else
			token = Mid(rgb_input, pos1)
		end if
	   
		' add the token to the end of the array
		redim preserve tokens(0 to ubound(tokens) + 1)
		tokens(ubound(tokens)) = token
	   
		pos1 = pos2 + 1 ' added + 1
		
	loop until pos2 = 0
	
	return (rgb(tokens(1), tokens(2), tokens(3)))

end function

sub draw_bounds (bounds	as segment_proto, view_area as view_area_proto)
	line 	(bounds.x1*view_area.zoom + view_area.x, _
			bounds.y1*view_area.zoom + view_area.y) - _
			(bounds.x2*view_area.zoom + view_area.x, _
			bounds.y2*view_area.zoom + view_area.y), _
			 C_WHITE, B, &b1111000011110000	 
end sub

function is_overlap( 	Ax1 as single, Ay1 as single, _
						Ax2 as single, Ay2 as single, _
						Bx1 as single, By1 as single, _
						Bx2 as single, By2 as single ) as boolean
						
	if (Ay2 < By1) or (Ay1 > By2) or (Ax2 < Bx1) or (Ax1 > Bx2) then
		return false
	else
		return true
	end if
	
end function


function get_average_color (	head as point_proto ptr, _
								view_area as view_area_proto, _
								img_name as any ptr) as ULong
								
	dim as Uinteger r, g, b, c, arraylen
	redim rgb_values(0 to 0) as ULong
	
	' Thanks to MrSwiss for the corrections on the below code for 64/32 compiler
   redim preserve 	a(0 to 0, 0 to 1) as Long
   Dim As Long      i, j, k, dy, dx, x, y, temp
  
   
   'translate point stored into linked list into points stored into an array
   i = 0
   while head <> NULL
		if (head->next_p <> NULL) then
			a(i, 0) = head->x
			a(i, 1) = head->y
			redim preserve a(0 to  Ubound(a)+1, 0 to 1)
		end if
		head = head->next_p
		i+=1
	wend
   
   Dim As Long      xi(0 to Ubound(a, 1))
   Dim As Single    slope(0 to Ubound(a, 1))
   'join first and last vertex
   a(Ubound(a, 1), 0) = a(0, 0)
   a(Ubound(a, 1), 1) = a(0, 1)

   For i = 0 To Ubound(a, 1) - 1

	dy = a(i+1, 1) - a(i, 1)
      dx = a(i+1, 0) - a(i, 0)

      If (dy = 0) Then slope(i) = 1.0
      If (dx = 0) Then slope(i) = 0.0

      If (dy <> 0) AndAlso (dx <> 0) Then slope(i) = dx / dy
    
   Next i

   For y = 0 to SCR_H - 1
      k = 0
      ' using FB's short-cut operators (which C doesn't have!)
      For i = 0 to Ubound(a, 1) - 1
         If (a(i, 1) <= y AndAlso a(i+1, 1) > y) OrElse _
             (a(i, 1) > y AndAlso a(i+1, 1) <= y) Then
            xi(k) = CLng(a(i, 0) + slope(i) * (y - a(i, 1)))
            k += 1
         End If
      Next i

      For j = 0 to k - 2
         'Arrange x-intersections in order
         For i = 0 To k - 2
            If (xi(i) > xi(i + 1)) Then
               temp = xi(i)
               xi(i) = xi(i + 1)
               xi(i + 1) = temp
            End If
         Next i
      Next j
      'pick rgb values of each line
		
      For i = 0 To k - 2 Step 2
		for j = xi(i) to xi(i + 1) + 1
			x = j
			rgb_values(Ubound(rgb_values)) = point(j ,y, img_name)
			redim preserve rgb_values(0 to Ubound(rgb_values)+1) as ULong
		next j
	Next i

   Next y
	
	arraylen = UBound(rgb_values) - LBound(rgb_values) + 1
	
	r = 0 : g = 0 : b = 0

	for c = Lbound(rgb_values) to Ubound(rgb_values)
	
		'get each r, g, b value
		r += rgb_values(c) shr 16 and &hFF
		g += rgb_values(c) shr 8 and &hFF
		b += rgb_values(c) and &hFF
		
	next c
	
	r = r \ (arraylen)
	g = g \ (arraylen)
	b = b \ (arraylen)

	return rgb(r,g,b)
	

end function

SUB load_whole_txt_file(Byref fn As String,  filearr() As String)
    'Thanks to bulrush for this very useful sub
    'this is a snippet based on his source
    'http://www.freebasic.net/forum/viewtopic.php?f=7&t=24284
    'this sub read a whole text file and put it into an array
    Dim As Integer filenum,res,outpos,i,ub
    Dim As String procname,ln

    outpos 	= 1
    filenum = Freefile
    res 	= Open (fn, For Input, As #filenum)

	While (Not Eof(filenum))
		Line Input #filenum, ln ' Get one whole text line
		Redim Preserve filearr(outpos)
		filearr(outpos)	= ln
		outpos += 1
	Wend

    Close #filenum

    ub = Ubound(filearr)
END SUB

sub draw_button (x as integer, y as integer, w as integer,_
							h as integer, label as string,_
							is_selected as boolean)
							
	Line (x,y)-(x+w,y+h),C_GRAY,BF
	Line (x,y)-(x+w,y+h),C_WHITE,B
	
	if (is_selected) then
		Line (x,y)-(x+w,y+h),C_DARK_GRAY,BF
		Line (x,y)-(x+w,y+h),C_GRAY,B
	end if
	
	draw string (x + (w \ 2) - len(label)*4, y + h\2 - 2), label
	
end sub

sub draw_bottom_info (	console_message as string, _
						view_area as view_area_proto, _
						user_mouse as mouse_proto, _
						settings as settings_proto, _
						timer_begin as double, _
						on_screen_help() as string)
						
	'zoom
	draw_button (0, SCR_H - BTN_H, BTN_W, BTN_H, str(view_area.zoom * 100) + " %", true)
	'tool used
	draw_button (BTN_W, SCR_H - BTN_H, BTN_W * 2, BTN_H, console_message,true)
	'bitmap show
	draw_button (BTN_W*3, SCR_H - BTN_H, BTN_W, BTN_H, "[B]ITMAP",	 settings.is_bitmap_visible)
	'wireframe show
	draw_button (BTN_W*4, SCR_H - BTN_H, BTN_W, BTN_H, "[W]IREFRAME",	 settings.is_wireframe_visible)
	'snap show
	draw_button (BTN_W*5, SCR_H - BTN_H, BTN_W, BTN_H, "SNAP",	 settings.is_snap_active)
	'centroid show
	draw_button (BTN_W*6, SCR_H - BTN_H, BTN_W, BTN_H, "[C]ENTROID",	 settings.is_centroid_visible)
	'centroid show
	draw_button (BTN_W*7, SCR_H - BTN_H, BTN_W, BTN_H, "[Q] POINTS",	 settings.is_vertex_visible)
	'absolute x / y of mouse
	draw_button (BTN_W*8, SCR_H - BTN_H, BTN_W*2, BTN_H, _
				"x: " + str(user_mouse.abs_x) + ", y: " + str(user_mouse.abs_y),_
				true)
	'FPS
	dim fps as single = abs(int(1.0f/(timer_begin-timer)))
	
	draw_button (BTN_W*10, SCR_H - BTN_H, BTN_W, BTN_H, _
				"FPS: " + str(fps),	true)
				
	'F1 - HELP infos
	dim i as integer
	
	if (settings.is_help_visible) then
		for i = 0 to SCR_H step 2
			line(0, i)-(SCR_W, i), C_DARK_GRAY
		next i
		for i = 0 to Ubound(on_screen_help)-1
			draw string (21, 21 + i * 12), on_screen_help(i), C_BLACK
			draw string (20, 20 + i * 12), on_screen_help(i), C_WHITE
		next i
	end if
end sub


sub load_icon_set ( 	bmp() as Ulong ptr, w as integer, _
						h as integer, cols as integer, rows as integer, _
						Byref bmp_path as string)
				
	dim as integer c, tiles, tile_w, tile_h, y, x
	tiles = cols * rows
	tile_w = w\cols
	tile_h = h\rows
	y = 0
	x = 0
	
	BLOAD bmp_path, 0
	
	for c = 0 to Ubound(bmp)
		if c > 0 and c mod cols = 0 then
			y+= tile_h 
			x = 0 
		end if
		bmp(c) = IMAGECREATE (tile_w, tile_h)
		GET (x, y)-(x + tile_w - 1, y + tile_h - 1), bmp(c)
		x += tile_w

	next c

end sub


sub draw_mouse_pointer	(	x as integer, y as integer, _
							lbtn_pressed as boolean, _
							is_snap_point_available as boolean, _
							input_mode as proto_input_mode, _
							icon_set() as Ulong ptr)
	dim icon as proto_icon
	
	select case input_mode
		case input_add_point
			if is_snap_point_available then
				if lbtn_pressed then
					icon = icon_pen_is_pressed
				else
					icon = icon_pen_snap
				end if
			else 
				if lbtn_pressed then
					icon = icon_pen_is_pressed
				else
					icon = icon_pen
				end if
			end if
		case input_selection
			icon = icon_direct_selection
		case input_hand
			if lbtn_pressed then
				icon = icon_hand_is_pressed
			else
				icon = icon_hand
			end if
		case else
			icon = icon_selection
	
	end select

	put (x-12, y), icon_set(icon), trans
	
end sub

sub unmark_all_nodes (array() as polygon_proto)

	dim i as integer
	dim head as point_proto ptr
		
	for i = 0 to Ubound(array)-1
			head = array(i).first_point
			while head->next_p <> NULL					
				head->is_selected = false
				head = head->next_p
			wend
	next i	

end sub

sub move_selected_node(x_offset as integer, y_offset as integer, array() as polygon_proto)	

	dim i as integer
	dim head as point_proto ptr

		
	for i = 0 to Ubound(array)-1
		head = array(i).first_point
		while head->next_p <> NULL
			if 	head->is_selected then
				head->x += x_offset
				head->y += y_offset
			
			end if
			head = head->next_p
		'compute again the centroid of the modified polygon
		array(i).centroid = calculate_centroid(array(i).first_point)
		wend
	next i	
	

end sub

function delete_selected_node (node as point_proto ptr) as point_proto ptr

	if (node = NULL) then ' Found the tail
       return NULL
	   
	elseif (node->is_selected = true) then
	   
	   dim next_node as point_proto ptr
		' Found one to delete
		next_node = node->next_p
		
		deallocate(node)
		
		return next_node
   else
	 ' Just keep going
		node->next_p = delete_selected_node(node->next_p)
		return node
	end if
	
end function

function count_selected_nodes (head as point_proto ptr) as integer

	dim i as integer
	i = 0

	while head <> NULL
		if (head->next_p <> NULL) then
			if head->is_selected then
				i+=1
			end if
		end if
		head = head->next_p
	wend
	
	return i
	
end function

sub get_selection_bounds (user_mouse as mouse_proto ptr)

	user_mouse->bounding_x1 = user_mouse->drag_x1
	user_mouse->bounding_y1 = user_mouse->drag_y1 
	user_mouse->bounding_x2 = user_mouse->drag_x2
	user_mouse->bounding_y2 = user_mouse->drag_y2
	if user_mouse->bounding_x1 > user_mouse->bounding_x2 then
		swap user_mouse->bounding_x1, user_mouse->bounding_x2
	end if
	if user_mouse->bounding_y1 > user_mouse->bounding_y2 then
		swap user_mouse->bounding_y1, user_mouse->bounding_y2
	end if

end sub
