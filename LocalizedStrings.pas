unit LocalizedStrings;

{$MODE Delphi}

interface
resourcestring
  res_application_title   = 'AutoREALM';
  res_main_editing_symbol = '%s - EDITING SYMBOL %s';
  res_main_autorlm_hlp =    'Autorlm.hlp'; //use the name autorlm_(localization).hlp
  res_main_undo =           '&Undo %s';
  res_main_redo =           '&Redo %s';
  res_main_add_overlay =    'Add Overlay';
  res_main_maximum_overlays_reached = 'Only %d overlays are allowed';
  res_main_maximum_overlay_title =    'Overlay limit';
  res_main_del_overlay =    'Delete the "%s" overlay?';
  res_main_sure =           'Are you sure?';
  res_main_remove_objects = 'Remove all objects within the "%s" overlay?';
  res_main_deleting =       ' (DELETING)';
  res_main_color_change_outline = 'Change Outline Color';
  res_main_grid_remove =    'Remove Grid';
  res_main_grid_bold =      'Change Bold Grid';
  res_main_grid_style1 =    'Change Primary Grid Style';
  res_main_grid_style2 =    'Change Secondary Grid Style';
  res_main_grid_default =   'default';
  res_main_grid_resize =    'Grid Resize';
  res_main_grid_squares =   'squares';
  res_main_grid_hexes =     'hexes';
  res_main_grid_triangles = 'triangles';
  res_main_grid_per_cm    = '/cm';
  res_main_grid_per_inch  = '/inch';
  res_main_file_nosave =    'Failed to save %s';
  res_main_file_noload =    '%s is not a valid map file.';
  res_main_file_incompatible = '%s was saved with an incompatible version of AutoREALM.';
  res_main_file_noopen =    'Failed to open %s';
  res_main_file_noinsert =  'Cannot insert %s';
  res_main_file_exist =     '%s already exists.  Overwrite?';
  res_main_file_exist2 =    'File already exists';
  res_main_file_changes =   'Save current map?  Changes include:';
  res_main_file_changes2 =  'Existing Map has changed';
  res_main_rotate_degrees = 'Rotate %g°'; //g evoid FloattoStr
  res_main_color_change_backgd = 'Change Background Color';
  res_main_color_change_grid = 'Change Grid Color';
  res_main_print_new_file = 'AutoREALM : New file';
  res_main_big_bitmap = 'Bitmap too large to be generated.  Try again with a smaller bitmap.';
  res_main_big_bitmap_title = 'Error Creating Bitmap';

  res_main_pushpin =        'You can not display a push pin until it has been placed.'#13#10+
                            'Right-click on the map and select "Place Push Pin".';

  res_main_reload =         'This action requires AutoREALM to reload itself.'#13#10+
                            'Your current work will be saved into a temporary file,'#13#10+
                            'and you will lose the ability to undo.  Continue?';

  res_main_hyperlink_doc_not_found = 'Error opening '#39'%s.'#39;
  res_main_hyperlink_exe_not_found = 'Error opening '#39'%s'#39#13#10+
                                     ' with command-line parameters '#39'%s'#39'.';
  res_main_hyperlink_hints         = #13#10'(Note: If your hyperlink filename contains spaces, surround the entire filename with'#13#10 +
                                           ' double quotes, e.g. "%s".)';

  res_main_warning =        'Warning';
  res_main_view_saved =     'Display a saved view';
  res_main_view_save =      'Save View';
  res_main_view_delete =    'Delete View';
  res_main_move_sel =       'Move Selection';
  res_main_readme =         'readme.doc';
  res_main_figure_close =   'Close Figures';
  res_main_line_reverse =   'Reverse Line Direction';
  res_main_define_empty_symbol='No objects are selected.  Do you really wish to define an empty symbol?';
  res_main_set_x_size =     'Change selection x (and maybe y) size';
  res_main_set_y_size =     'Change selection y (and maybe x) size';

  res_cprint_default =      'Default printer; ';
  res_cprint_ready =        'Ready';
  res_cprint_paused =       'Paused';
  res_cprint_error =        'Error';
  res_cprint_paper_jam =    'Paper jam';
  res_cprint_out_of_paper = 'Out of paper';
  res_cprint_paper_problem= 'Paper problem';
  res_cprint_offline =      'Offline';
  res_cprint_output_bin_full = 'Output bin full';
  res_cprint_not_available= 'Not available';
  res_cprint_toner_low =    'Toner low';
  res_cprint_no_toner =     'No toner';
  res_cprint_out_of_memory= 'Out of memory';
  res_cprint_door_open =    'Door open';
  res_cprint_unknown_server='Unknown server';
  res_cprint_power_save =   'Power save';
  res_defsymbol_group =     'A symbol cannot be defined without any groups.  Please create some groups in the symbol library and try again.';
  res_defsymbol_group_sel = 'You must select a group for this symbol';
  res_graphgrid_centi =     'Centimeters';  //appear in the dialoge Box "Map Settings"
  res_graphgrid_inche =     'Inches';
  res_graphgrid_feet =      'Feet';
  res_graphgrid_cubit =     'Cubits';
  res_graphgrid_yards =     'Yards';
  res_graphgrid_meter =     'Meters';
  res_graphgrid_fatho =     'Fathoms';
  res_graphgrid_rods  =     'Rods';
  res_graphgrid_chain =     'Chains';
  res_graphgrid_furlo =     'Furlongs';
  res_graphgrid_kilom =     'Kilometers';
  res_graphgrid_stadi =     'Stadia';
  res_graphgrid_miles =     'Miles';
  res_graphgrid_naumi =     'Nautical Miles';
  res_graphgrid_leagu =     'Leagues';
  res_graphgrid_dbf1  =     'Days by foot on rugged terrain, burdened';
  res_graphgrid_dbf2  =     'Days by foot, burdened';
  res_graphgrid_dbf3  =     'Days by foot on rugged terrain';
  res_graphgrid_dbw   =     'Days by wagon';
  res_graphgrid_dbf4  =     'Days by foot';
  res_graphgrid_dbh1  =     'Days by war horse';
  res_graphgrid_dbg1  =     'Days by oared galley';
  res_graphgrid_dbh2  =     'Days by horse';
  res_graphgrid_dbg2  =     'Days by sailed galley';
  res_graphgrid_au    =     'AU';
  res_graphgrid_ly    =     'Light years';
  res_graphgrid_parse =     'Parsecs';
  res_linetool_line_add =   'Add Line';
  res_linetool_linef_add =  'Add Fractal Line';
  res_linetool_arc_add   =  'Add Arc';
  res_linetool_linepoly_add='Add PolyLine';
  res_linetool_linefree_add='Add Freehand Line';
  res_linetool_linefpoly_add = 'Add Fractal PolyLine';
  res_linetool_lineffree_add = 'Add Fractal Freehand Line';
  res_linetool_curve_add =  'Add Curve';
  res_linetool_curvef_add = 'Add Fractal Curve';
  res_linetool_curvepoly_add='Add Poly Curve';
  res_linetool_curvefpoly_add = 'Add Fractal PolyCurve';
  res_linetool_circle_add = 'Add Circle';
  res_linetool_rect_add =   'Add Rectangle';
  res_linetool_polygon_add= 'Add Polygon';
  res_linetool_rosette_add= 'Add Rosette Grid';
  res_mapobj_view_sav =     '(View when map was last saved)';
  res_mapobj_undo_fail =    'Failed Undo Record';
  res_mapobj_undo_restore = '*RESTORE*';
  res_mapobj_o_del =        '* Objects were deleted';
  res_mapobj_o_add =        '* Objects were added';
  res_mapobj_o_chd =        '* Objects were changed';
  res_mapobj_o_symbols =    '* Symbols were added';
  res_mapobj_o_overlays =   '* Overlays were changed';
  res_mapobj_o_comments =   '* Comments';
  res_mapobj_o_map =        '* Map settings';
  res_mapobj_o_grid =       '* Grid attributes';
  res_mapobj_o_backgd =     '* Background color';
  res_mapobj_o_units =      '* Units of measurement';
  res_mapobj_o_map2 =       '* Map scale';
  res_mapobj_o_view =       '* Views were added, deleted, or changed';
  res_mapobj_o_overlays2 =  '* Visible or frozen overlays have changed';
  res_mapobj_o_print =      '* Page orientation (i.e. Portrait/Landscape)';
  res_mapobj_o_viewport =   '* Viewport (i.e. window was zoomed and/or panned)';
  res_mapobj_undo_del =     'Delete';
  res_mapobj_undo_del_overlay = 'Delete Overlay';
  res_mapobj_array_create = 'Create Array %u x %u';

  res_mapobj_overlay_inv1 = 'Warning: you have just added an object to a invisible overlay.'#13#10+
                            'The overlay will be made visible.';

  res_mapobj_overlay_inv2 = 'Invisible Overlay';
  res_mapobj_undo_style =   'Change Style';
  res_mapobj_undo_color =   'Change Color';
  res_mapobj_undo_colorf =  'Change Fill Color';
  res_mapobj_undo_seed =    'Change Seed';
  res_mapobj_undo_rough =   'Change Roughness';
  res_mapobj_undo_overlay = 'Change Overlay';
  res_mapobj_undo_group =   'Group';
  res_mapobj_undo_back =    'Send To Back';
  res_mapobj_undo_front =   'Bring To Front';
  res_mapobj_undo_backward_one ='Send Backwards';
  res_mapobj_undo_forward_one=  'Send Forwards';
  res_mapobj_undo_group_no ='Ungroup';
  res_mapobj_undo_decompose='Decompose';
  res_mapobj_file_chunk_no= 'Error: Expecting chunk missing. Cannot load file.';
  res_mapobj_file_end =     'Error: file ended prematurely!  File contents up to end of file have been read, however data is missing.  Try opening your .BAK file for a more complete copy.';
  res_mapobj_file_chunk_un ='Error: file contains unknown chunk id!  Cannot load file.';
  res_mapobj_undo_paste =   'Paste';
  res_mapobj_file_name_in = '%s is not a valid map file.';
  res_mapobj_file_ver_in =  '%s was saved with an incompatible version of AutoREALM.';
  res_mapobj_undo_file_insert =  'Insert of %s';               // Replaced with metafile name
  res_mapobj_metafile_comment='Created by AutoREALM';
  res_mapobj_metafile_title  ='%d x %d Map';                   // Replaced with width & height in pixels
  res_mapset_undo_comment = 'Change Comments';
  res_primitives_combine1 =   'Combine operation failed';
  res_primitives_types =     'Objects to be combined are different types.';
  res_primitives_combine2 =  'Failure to combine objects';
  res_primitives_convert =   'Failure to convert objects';
  res_selfont_font_name =    'Font Name Change';
  res_selfont_font_size =    'Font Size Change';

  res_seltool_freez = 'You are trying to select items in a frozen overlay.'#13#10+
                      'Try unfreezing an overlay (for example, in this case, "%s"'+#13#10+
                      'by clicking on the overlay'#39's gray checkbox.'#13#10#13#10+
                      'If you no longer wish to see this message, click "Ignore",'#13#10+
                      'otherwise, click "OK".';

  res_seltool_undo_handle =  'Move Handle';
  res_seltool_undo_strech =  'Stretch Selection';
  res_seltool_undo_rotate =  'Rotate Selection';
  res_seltool_undo_move =    'Move Selection';
  res_seltool_undo_glue =    'Glue';
  res_seltool_undo_scalpel1 ='Scalpel Delete Node';
  res_seltool_undo_scalpel2 ='Scalpel Separate Node';
  res_seltool_undo_scalpel3 ='Scalpel Add Intersection Node';
  res_seltool_undo_scalpel4 ='Scalpel Slice Along Line';
  res_symbolfile_file_notvalid = '%s is not a valid symbol file.';
  res_symbollib_lose = 'WARNING: This will lose all changes to the symbol library.  Continue?';
  res_symbollib_lose_title = 'Revert to Saved';
  res_symbollib_del =        'Do you want to delete the %s group and all symbols in it?';
  res_symbollib_del_title =  'Delete Group';
  res_symbollib_restart =    'Your changes to favorites will not take place until you restart AutoREALM.';
  res_symbollib_note =       'NOTE';
  res_symbollib_del_symbol = 'Do you want to delete the %s symbol from the %s group?';
  res_symbollib_del_symbol_title = 'Delete Symbol';
  res_symbollib_caption_done='Don&e';
  res_symbollib_caption_cancel='Cance&l';
  res_symbollib_caption_edit='&Edit Properties';
  res_symbollib_caption_del= 'De&lete';
  res_texttool_symbol_add =  'Add Symbol';
  res_texttool_symbols_adds= 'Add Multiple Symbols';
  res_texttool_text_add=     'Add Text';
  res_texttool_text_c_add=   'Add Curved Text';
  res_selectfont_font_name = 'Font Name Change';
  res_selectfont_font_size = 'Font Size Change';
  res_selectfont_font_bold = 'Font Bold';
  res_selectfont_font_italic = 'Font Italic';
  res_selectfont_font_underline = 'Font Underline';
  res_selectfont_align_left = 'Align Text Left';
  res_selectfont_align_center = 'Align Text Center';
  res_selectfont_align_right = 'Align Text Right';
  res_selectfont_text_change = 'Text Changes';

  res_ct_translate           = 'Translate Colors';
  res_ct_inv_translate       = 'Inverse Translate Colors';

  res_mapobj_o_pushpin =    '* Push pin names, locations, or waypoints';
  res_texttool_hyperlink_add= 'Add Hyperlink';
  res_main_hyperlink_drop=    'Drag/drop Hyperlink(s)';
  res_hyperlink_change =      'Hyperlink Changes';
  res_fail_hyperlink =        'Hyperlink Failed to Execute';

  res_pushpin_out_of_range   = 'Push pin index of history out of range';
  res_pushpinwaypointcaption = 'Push Pin Waypoint';
  res_pushpinwaypointprompt  = 'Enter the note you want associated with this waypoint:';
  res_pushpinwaypointerror   = 'You can not set a waypoint until the push pin has been placed.'#13#10+
                               'Right-click on the map and select "Place Push Pin".';
  res_pushpinclearprompt     = 'Push Pin History Clear';
  res_pushpinclearcaption    = 'Are you sure you want to clear %s'#39's history?';
  res_pushpinnamecaption     = 'Push Pin Rename';
  res_pushpinnameprompt      = 'New push pin name (blank reverts to color):';
  res_pushpinselectcaption   = 'Push Pin';
  res_pushpinselecterror     = 'You must first select a push pin by clicking on it';
  res_XMLMismatchedTag       = 'XML tag mismatch: "%s" found; "%s" expected.';
  res_XMLBadPinName          = '"%s" is not a valid push pin name.';
  res_PushPinPaste           = 'Error Pasting Push Pins';

  res_BulletNotFoundText     = 'HyperlinkBullet.bmp not found in program directory: hyperlinks will not be visible.';
  res_BulletNotFoundCaption  = 'Error Loading Bitmap';

  res_autoname_nopen         = 'Cannot open rule file %s!';

  res_settings_badzoom       = 'Zoom multiplier must be >= 1.0';

  res_bitmapproperties_badbitmap= 'Cannot create bitmap (bad size)';

  // "Undo" set point names for operations to change fractals
  res_fractalstate_normal    = 'Set to normal lines';
  res_fractalstate_fractal   = 'Set to fractal lines';
  res_fractalstate_toggle    = 'Toggle fractal state';

  res_main_line_flip =   'Flip Line Style';

implementation

end.
