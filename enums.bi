'enums__________________________________________________________________

enum proto_input_mode
	input_error = 0
	input_add_polygon = 1
	input_add_point = 2
	input_close_polygon = 3
	input_selection
	input_hand
	add_vertex
	del_vertex
	add_edge 
	del_edge
	move_vertex
	set_start
	set_end
	input_save_as_lpe_file
	input_erase_all
	input_erase_polygon
	input_export_as_svg
	input_load_lpe_file
	input_create_random_polygons
end enum

enum proto_console_message
	cm_undefined_error = 0
	cm_PEN_Tool
	cm_TOOL_Selection
	cm_TOOL_Direct_Selection
	cm_TOOL_Hand
	cm_Point_Added
	cm_Polygon_Closed
	cm_Polygon_s_deleted
	cm_FILE_SAVED
	cm_FILE_EXPORTED
	cm_Wireframe_ON
	cm_Wireframe_OFF
	cm_Centroids_ON
	cm_Centroids_OFF
	cm_Bitmap_ON
	cm_Bitmap_OFF
	cm_Points_ON
	cm_Points_OFF
end enum
