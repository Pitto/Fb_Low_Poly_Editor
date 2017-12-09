'types__________________________________________________________________
type point_proto
	x 			as single
	y 			as single
	next_p  	as point_proto ptr
end type

type temp_point_proto
	x 				as single
	y 				as single
	distance		as single
end type

type segment_proto
	as single x1,y1,x2,y2
end type

type polygon_proto
	first_point		as point_proto ptr
	centroid		as point_proto
	fill_color		as Ulong
	stroke_color	as Ulong
	is_selected		as boolean
	bounds			as segment_proto
end type

type view_area_proto
    x 		as single
    y 		as single
    old_x 	as single
    old_y 	as single
    w 		as single
    h 		as single
    speed 	as single
    rds 	as single
	zoom 	as single
	old_zoom 	as single
	'average_fps(0 to 49)	as integer
end type

type key_proto
	code			as integer
	is_down		 	as boolean
	old_is_down		as boolean
	is_released		as boolean
end type

Type mouse_proto
    As Integer 		res, x, y, old_x, old_y, wheel, clip, _
					old_wheel, diff_wheel, abs_x, abs_y
    as single 		oppo_x, oppo_y, old_oppo_x, old_oppo_y, _
					drag_x1, drag_y1, drag_x2, drag_y2, _
					bounding_x1, bounding_y1, bounding_x2, bounding_y2
    as boolean is_dragging
    as boolean is_lbtn_released
    as boolean is_lbtn_pressed
    as boolean is_rbtn_released
    as boolean is_rbtn_pressed
    Union
        buttons 		As Integer
        Type
            Left:1 		As Integer
            Right:1 	As Integer
            middle:1 	As Integer
        End Type
    End Union
End Type

Type settings_proto
	is_snap_active as boolean
	is_hand_active as boolean
	is_centroid_visible as boolean
	is_wireframe_visible as boolean
	is_bitmap_visible as boolean
	wireframe_color as Ulong
	is_vertex_visible as boolean
	is_debug as boolean
	is_help_visible as boolean
end type

type FIXED as long ' 12:20
