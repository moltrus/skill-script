;###########################################################################
;#                                                                         #
;#   Optimize Silkscreen Text Placement                                    #
;#                                                                         #
;#  This routine optimizes the placement of silkscreen text according to   #
;#  the following priorities:                                              #
;#                                                                         #
;#  1. Always avoid copper (pads, vias, holes)                            #
;#  2. First priority: Place text near the component with proper           #
;#     orientation                                                         #
;#  3. Second priority: If insufficient space near components, find        #
;#     empty spaces in the same area of the board                          #
;#  4. Third priority: If no suitable space on the board, place text       #
;#     outside the board outline                                           #
;#                                                                         #
;#  Usage: optimize_silkscreen (from within Allegro)                       #
;###########################################################################

;##########################
;#  Register Program Name #
;##########################

axlCmdRegister("optimize_silkscreen" 'OptimizeSilkscreen)

;#############################
;# Global Variables          #
;#############################

; Layer definitions
silktop_layer = "REF DES/SILKSCREEN_TOP"
silkbot_layer = "REF DES/SILKSCREEN_BOTTOM"

; Copper object layers to avoid
copper_layers = list(
  "PIN/TOP"
  "PIN/BOTTOM"
  "VIA CLASS/TOP"
  "VIA CLASS/BOTTOM"
  "ETCH/TOP"
  "ETCH/BOTTOM"
)

; Default text orientation preferences (like in the reference file)
LRBT = t   ; Left to Right, Bottom to Top (default)
LRTB = nil ; Left to Right, Top to Bottom
RLBT = nil ; Right to Left, Bottom to Top
RLTB = nil ; Right to Left, Top to Bottom

; Safety clearance from copper objects (in mils)
min_clearance = 5

; Form directory
FormDir = "./"

;#############################
;# MAIN Program Starts Here. #
;#############################

(defun OptimizeSilkscreen ()
  ; Initialize variables
  SilkTop = nil
  SilkBot = nil
  opt_clearance = min_clearance
  
  if(axlOKToProceed() then
    ; Create and display the user interface
    createSilkscreenForm()
    fFile = strcat(FormDir "silkscreenForm.form")
    Form = axlFormCreate((gensym) fFile nil 'silkscreenForm_Action t)
    axlFormSetField(Form "SilkTop" SilkTop)
    axlFormSetField(Form "SilkBot" SilkBot)
    axlFormSetField(Form "LRBT" LRBT)
    axlFormSetField(Form "LRTB" LRTB)
    axlFormSetField(Form "RLBT" RLBT)
    axlFormSetField(Form "RLTB" RLTB)
    axlFormSetField(Form "MinClearance" opt_clearance)
    axlFormDisplay(Form)
  else
    printf("E- Please terminate your interactive command.\n")
  ) ;endif OKToProceed
) ;end defun - MAIN Program

;############################################################################
;# doSilkscreenOpt - Process silkscreen text placement optimization         #
;############################################################################

(defun doSilkscreenOpt ()
  axlClearSelSet()
  axlSetFindFilter(?enabled   (list "noall" "text")
                   ?onButtons (list "text")
                  )
  axlMsgPut("Silkscreen optimization started.")
  
  ; Get board outline coordinates
  board_outline = getBoardOutline()
  
  ; Process each selected layer
  Layer_List = nil
  if(SilkTop == t then Layer_List = cons(silktop_layer Layer_List))
  if(SilkBot == t then Layer_List = cons(silkbot_layer Layer_List))
  
  foreach(layer Layer_List 
    axlVisibleDesign(nil)
    axlVisibleLayer(layer t)
    Text_List = axlGetSelSet(axlAddSelectAll())
    
    if(length(Text_List) != 0 then 
      optimizeSilkscreenPlacement(Text_List board_outline)
    )
    
    axlClearSelSet()
    axlFlushDisplay()
    axlMsgPut(strcat(layer " optimization completed."))
  ) ;foreach
  
  ; Turn on the silkscreen layers again
  Display_List = list("paramLayerGroup:BOARD GEOMETRY/paramLayer:OUTLINE")
  if(SilkTop == t then
    Display_List = cons("paramLayerGroup:PACKAGE GEOMETRY/paramLayer:SILKSCREEN_TOP" Display_List)
    Display_List = cons("paramLayerGroup:REF DES/paramLayer:SILKSCREEN_TOP" Display_List)
  )
  if(SilkBot == t then
    Display_List = cons("paramLayerGroup:PACKAGE GEOMETRY/paramLayer:SILKSCREEN_BOTTOM" Display_List)
    Display_List = cons("paramLayerGroup:REF DES/paramLayer:SILKSCREEN_BOTTOM" Display_List)
  )
  
  foreach(Layer Display_List
    val = axlGetParam(Layer)
    val->visible = t
    axlSetParam(val)
  )
) ;end defun - doSilkscreenOpt

;############################################################################
;# getBoardOutline - Get the board outline coordinates                      #
;############################################################################
(defun getBoardOutline ()
  prog((outline llx lly urx ury boardOutline bbox line)
    
    ;; Find board outline
    axlSetFindFilter(?enabled (list "noall" "lines")
                     ?onButtons (list "lines"))
    axlVisibleDesign(nil)
    axlVisibleLayer("BOARD GEOMETRY/OUTLINE" t)
    boardOutline = axlGetSelSet(axlAddSelectAll())

    ;; Get bounding box of board outline
    when(boardOutline
      llx = 999999
      lly = 999999
      urx = -999999
      ury = -999999
      
      foreach(line boardOutline
        bbox = line->bBox
        llx = min(llx car(car(bbox)))
        lly = min(lly cadr(car(bbox)))
        urx = max(urx car(cadr(bbox)))
        ury = max(ury cadr(cadr(bbox)))
      )
      
      outline = list(llx lly urx ury)
    )

    axlClearSelSet()
    return(outline)
  )
)



;############################################################################
;# optimizeSilkscreenPlacement - Main function to optimize silkscreen text  #
;############################################################################

(defun optimizeSilkscreenPlacement (Text_List board_outline)
  ; Make copper layers visible to check for overlap
  foreach(layer copper_layers
    axlVisibleLayer(layer t)
  )
  
  foreach(text_id Text_List
    ; Get text properties
    text = text_id->text
    xy = text_id->xy
    txt_x = xCoord(xy)
    txt_y = yCoord(xy)
    txt_rot = text_id->rotation
    txt_layer = text_id->layer
    parent_id = text_id->parent
    txt_tblk = text_id->textBlock
    txt_jus = text_id->justify
    mirrored = text_id->isMirrored
    
    ; Try to place text according to priorities
    new_position = nil
    
    ; Get component origin and dimensions
    component_pos = getComponentPosition(parent_id)
    
    ; Priority 1: Try to place near component with proper orientation
    new_position = findPositionNearComponent(text_id component_pos board_outline)
    
    ; Priority 2: Try to find empty space in the same area
    if(new_position == nil then
      new_position = findEmptySpaceInArea(text_id component_pos board_outline)
    )
    
    ; Priority 3: Place outside the board outline
    if(new_position == nil then
      new_position = findPositionOutsideBoard(text_id board_outline)
    )
    
    ; Update text position if needed
    if(new_position != nil then
      updateTextPosition(text_id new_position)
    )
  ) ;foreach text_id
) ;end defun - optimizeSilkscreenPlacement

;############################################################################
;# getComponentPosition - Get component position and dimensions              #
;############################################################################

(defun getComponentPosition (parent_id)
  prog((component_data nil))
    
    if(parent_id != nil then
      ; Get component bounding box
      sym_llx = 999999
      sym_lly = 999999
      sym_urx = -999999
      sym_ury = -999999
      
      foreach(childid parent_id->children
        if(childid->layer == "PACKAGE GEOMETRY/PLACE_BOUND_TOP" ||
           childid->layer == "PACKAGE GEOMETRY/PLACE_BOUND_BOTTOM" then
          bbox = childid->bBox
          sym_llx = min(sym_llx xCoord(xCoord(bbox)))
          sym_lly = min(sym_lly yCoord(xCoord(bbox)))
          sym_urx = max(sym_urx xCoord(yCoord(bbox)))
          sym_ury = max(sym_ury yCoord(yCoord(bbox)))
        )
      )
      
      ; Calculate center
      sym_x = (sym_urx + sym_llx) / 2
      sym_y = (sym_ury + sym_lly) / 2
      
      ; Store component data
      component_data = list(
        sym_x       ; Center X
        sym_y       ; Center Y
        sym_llx     ; Lower left X
        sym_lly     ; Lower left Y
        sym_urx     ; Upper right X
        sym_ury     ; Upper right Y
      )
    )
    
    return(component_data)
)

;############################################################################
;# findPositionNearComponent - Find position near component avoiding copper  #
;############################################################################

(defun findPositionNearComponent (text_id component_pos board_outline)
  prog((new_position nil))
    
    if(component_pos != nil then
      ; Extract component data
      sym_x = nth(0 component_pos)
      sym_y = nth(1 component_pos)
      sym_llx = nth(2 component_pos)
      sym_lly = nth(3 component_pos)
      sym_urx = nth(4 component_pos)
      sym_ury = nth(5 component_pos)
      
      ; Text properties
      text = text_id->text
      txt_layer = text_id->layer
      txt_rot = text_id->rotation
      
      ; Get text dimensions
      text_size = getTextDimensions(text_id)
      text_width = nth(0 text_size)
      text_height = nth(1 text_size)
      
      ; Try different positions around the component
      positions = list(
        list(sym_x (sym_lly - text_height - opt_clearance) 0)            ; Bottom
        list((sym_urx + text_height + opt_clearance) sym_y 90)           ; Right
        list(sym_x (sym_ury + text_height + opt_clearance) 0)            ; Top
        list((sym_llx - text_height - opt_clearance) sym_y 270)          ; Left
        list((sym_llx - opt_clearance) (sym_lly - opt_clearance) 0)      ; Bottom left
        list((sym_urx + opt_clearance) (sym_lly - opt_clearance) 0)      ; Bottom right
        list((sym_urx + opt_clearance) (sym_ury + opt_clearance) 0)      ; Top right
        list((sym_llx - opt_clearance) (sym_ury + opt_clearance) 0)      ; Top left
      )
      
      ; Try each position
      foreach(pos positions
        pos_x = nth(0 pos)
        pos_y = nth(1 pos)
        pos_rot = nth(2 pos)
        
        ; Check if position is valid
        if(isValidTextPosition(text_id pos_x pos_y board_outline) then
          new_position = list(pos_x pos_y pos_rot)
          return(new_position)
        )
      )
    )
    
    return(new_position)
)

;############################################################################
;# findEmptySpaceInArea - Find empty space in board area for text placement  #
;############################################################################

(defun findEmptySpaceInArea (text_id component_pos board_outline)
  prog((new_position nil))
    
    if(component_pos != nil && board_outline != nil then
      ; Extract board outline
      board_llx = nth(0 board_outline)
      board_lly = nth(1 board_outline)
      board_urx = nth(2 board_outline)
      board_ury = nth(3 board_outline)
      
      ; Extract component position
      sym_x = nth(0 component_pos)
      sym_y = nth(1 component_pos)
      
      ; Text properties
      text = text_id->text
      
      ; Get text dimensions
      text_size = getTextDimensions(text_id)
      text_width = nth(0 text_size)
      text_height = nth(1 text_size)
      
      ; Create grid of test positions in the board area
      grid_size = 50 ; Grid spacing in mils
      
      ; Search in expanding circles from component position
      max_radius = max((board_urx - board_llx) (board_ury - board_lly))
      
      for(radius 50 max_radius grid_size
        for(angle 0 359 45
          rad_angle = (angle * pi) / 180
          test_x = sym_x + radius * cos(rad_angle)
          test_y = sym_y + radius * sin(rad_angle)
          
          ; Check if position is inside board and valid
          if(test_x > (board_llx + text_width/2 + opt_clearance) &&
             test_x < (board_urx - text_width/2 - opt_clearance) &&
             test_y > (board_lly + text_height/2 + opt_clearance) &&
             test_y < (board_ury - text_height/2 - opt_clearance) then
            
            ; Try horizontal and vertical orientations
            orientations = list(0 90)
            
            foreach(orient orientations
              if(isValidTextPosition(text_id test_x test_y board_outline) then
                new_position = list(test_x test_y orient)
                return(new_position)
              )
            )
          )
        ) ;for angle
        
        ; If found a position, break out of radius loop
        if(new_position != nil then
          return(new_position)
        )
      ) ;for radius
    )
    
    return(new_position)
)

;############################################################################
;# findPositionOutsideBoard - Find position outside board for text placement #
;############################################################################

(defun findPositionOutsideBoard (text_id board_outline)
  prog((new_position nil))
    
    if(board_outline != nil then
      ; Extract board outline
      board_llx = nth(0 board_outline)
      board_lly = nth(1 board_outline)
      board_urx = nth(2 board_outline)
      board_ury = nth(3 board_outline)
      
      ; Text properties
      text_size = getTextDimensions(text_id)
      text_width = nth(0 text_size)
      text_height = nth(1 text_size)
      
      ; Define outside positions to try
      outside_positions = list(
        list(board_llx - text_width - opt_clearance (board_lly + board_ury)/2 90)  ; Left side
        list(board_urx + text_width + opt_clearance (board_lly + board_ury)/2 90)  ; Right side
        list((board_llx + board_urx)/2 board_lly - text_height - opt_clearance 0)  ; Bottom side
        list((board_llx + board_urx)/2 board_ury + text_height + opt_clearance 0)  ; Top side
      )
      
      ; Try each position
      foreach(pos outside_positions
        pos_x = nth(0 pos)
        pos_y = nth(1 pos)
        pos_rot = nth(2 pos)
        
        ; For outside board, we only check copper clearance
        if(hasNoCopperOverlap(text_id pos_x pos_y) then
          new_position = list(pos_x pos_y pos_rot)
          return(new_position)
        )
      )
    )
    
    return(new_position)
)


;############################################################################
;# isValidTextPosition - Check if position is valid for text placement       #
;############################################################################

(defun isValidTextPosition (text_id pos_x pos_y board_outline)
  prog((result t))
    ; Check if position is inside board
    if(board_outline != nil then
      board_llx = nth(0 board_outline)
      board_lly = nth(1 board_outline)
      board_urx = nth(2 board_outline)
      board_ury = nth(3 board_outline)
      
      text_size = getTextDimensions(text_id)
      text_width = nth(0 text_size)
      text_height = nth(1 text_size)
      
      if(pos_x - text_width/2 < board_llx ||
         pos_x + text_width/2 > board_urx ||
         pos_y - text_height/2 < board_lly ||
         pos_y + text_height/2 > board_ury then
        return(nil)
      )
    )
    
    ; Check if position has no copper overlap
    if(!hasNoCopperOverlap(text_id pos_x pos_y) then
      return(nil)
    )
    
    return(t)
)

;############################################################################
;# hasNoCopperOverlap - Check if text at position overlaps with copper       #
;############################################################################

(defun hasNoCopperOverlap (text_id pos_x pos_y)
  prog((result t))
    ; Get text dimensions
    text_size = getTextDimensions(text_id)
    text_width = nth(0 text_size)
    text_height = nth(1 text_size)
    
    ; Create bounding box for text at new position
    test_box = list(
      list(pos_x - text_width/2 - opt_clearance pos_y - text_height/2 - opt_clearance)
      list(pos_x + text_width/2 + opt_clearance pos_y + text_height/2 + opt_clearance)
    )
    
    ; Check for overlap with copper objects
    copper_objects = nil
    
    foreach(layer copper_layers
      objects = axlDBGetDesign()->layer->name->objType->?
      if(objects != nil then
        copper_objects = append(copper_objects objects)
      )
    )
    
    foreach(obj copper_objects
      if(axlDBBoundaryOverlap(test_box obj->bBox) then
        return(nil)
      )
    )
    
    return(t)
)

;############################################################################
;# getTextDimensions - Get width and height of text                         #
;############################################################################

(defun getTextDimensions (text_id)
  prog((result nil))
    ; Get text bounding box
    bbox = text_id->bBox
    
    ; Calculate width and height
    width = abs(xCoord(yCoord(bbox)) - xCoord(xCoord(bbox)))
    height = abs(yCoord(yCoord(bbox)) - yCoord(xCoord(bbox)))
    
    return(list(width height))
)


;############################################################################
;# updateTextPosition - Update text position with new coordinates and rotation #
;############################################################################

(defun updateTextPosition (text_id new_position)
  ; Extract new position data
  new_x = nth(0 new_position)
  new_y = nth(1 new_position)
  new_rot = nth(2 new_position)
  
  ; Get text properties
  text = text_id->text
  txt_layer = text_id->layer
  txt_tblk = text_id->textBlock
  mirrored = text_id->isMirrored
  parent_id = text_id->parent
  
  ; Setup orientation
  textOrientation = make_axlTextOrientation(
    ?textBlock txt_tblk,
    ?rotation new_rot,
    ?mirrored mirrored,
    ?justify "CENTER"
  )
  
  ; Create new text at updated position
  new_xy = list(new_x new_y)
  update = axlDBCreateText(
    text,
    new_xy,
    textOrientation,
    txt_layer,
    parent_id
  )
  
  ; Delete old text if update successful
  if(update != nil then
    axlDeleteObject(list(text_id))
  else
    axlMsgPut(strcat("Update Failed on " text))
  )
) ;end defun - updateTextPosition

;############################################################################
;# Form CallBacks - Determine Allegro command to execute                     #
;############################################################################

(defun silkscreenForm_Action (Form)
  (case Form->curField
    ("SilkTop"
      SilkTop = Form->curValue
    )
    
    ("SilkBot"
      SilkBot = Form->curValue
    )
    
    ("LRBT"
      LRBT = Form->curValue
      orient_0 = t
      orient_90 = t
      orient_180 = nil
      orient_270 = nil
    )
    
    ("LRTB"
      LRTB = Form->curValue
      orient_0 = t
      orient_90 = nil
      orient_180 = nil
      orient_270 = t
    )
    
    ("RLBT"
      RLBT = Form->curValue
      orient_0 = nil
      orient_90 = t
      orient_180 = t
      orient_270 = nil
    )
    
    ("RLTB"
      RLTB = Form->curValue
      orient_0 = nil
      orient_90 = nil
      orient_180 = t
      orient_270 = t
    )
    
    ("MinClearance"
      opt_clearance = Form->curValue
    )
    
    ("done"
      doSilkscreenOpt()
      axlFormClose(Form)
      axlCancelEnterFun()
      deleteFile(fFile)
    )
    
    ("cancel"
      axlFormClose(Form)
      axlCancelEnterFun()
      deleteFile(fFile)
    )
  ) ;end case
) ;end defun - silkscreenForm_Action

;############################################################################
;# createSilkscreenForm - Create the user interface form                     #
;############################################################################

(defun createSilkscreenForm ()
  fFile = strcat(FormDir "silkscreenForm.form")
  Form = outfile(fFile "w")
  fprintf(Form "FILE_TYPE=FORM_DEFN VERSION=2\n")
  fprintf(Form "FORM\n")
  fprintf(Form "FIXED\n")
  fprintf(Form "PORT 77 20\n")
  fprintf(Form "HEADER \"Optimize Silkscreen Text Placement V1.0\"\n")
  fprintf(Form "TILE\n")
  
  fprintf(Form "GROUP \"Select Silkscreen Layers\"\n")
  fprintf(Form "GLOC 2 1\n")
  fprintf(Form "GSIZE 72 4\n")
  fprintf(Form "ENDGROUP\n")
  
  fprintf(Form "FIELD \"SilkTop\"\n")
  fprintf(Form "FLOC 3 3 \n")
  fprintf(Form "CHECKLIST \"Silkscreen-Top\" \n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "FIELD \"SilkBot\"\n")
  fprintf(Form "FLOC 20 3 \n")
  fprintf(Form "CHECKLIST \"Silkscreen-Bottom\" \n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "GROUP \"Text Orientation\"\n")
  fprintf(Form "GLOC 2 5\n")
  fprintf(Form "GSIZE 32 13\n")
  fprintf(Form "ENDGROUP\n")
  
  fprintf(Form "GROUP \"Placement Settings\"\n")
  fprintf(Form "GLOC 36 5\n")
  fprintf(Form "GSIZE 38 13\n")
  fprintf(Form "ENDGROUP\n")
  
  fprintf(Form "TEXT \"Horizontal           Vertical \"\n")
  fprintf(Form "TLOC 6 7 \n")
  fprintf(Form "ENDTEXT\n")
  
  fprintf(Form "FIELD \"LRBT\"\n")
  fprintf(Form "FLOC 3 9 \n")
  fprintf(Form "CHECKLIST \"Left to Right     Bottom to Top\" \"rotate_group\" \n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "FIELD \"LRTB\"\n")
  fprintf(Form "FLOC 3 11 \n")
  fprintf(Form "CHECKLIST \"Left to Right     Top to Bottom\" \"rotate_group\" \n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "FIELD \"RLBT\"\n")
  fprintf(Form "FLOC 3 13 \n")
  fprintf(Form "CHECKLIST \"Right to Left     Bottom to Top\" \"rotate_group\" \n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "FIELD \"RLTB\"\n")
  fprintf(Form "FLOC 3 15 \n")
  fprintf(Form "CHECKLIST \"Right to Left     Top to Bottom\" \"rotate_group\" \n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "TEXT \"Copper Clearance (mils): \"\n")
  fprintf(Form "TLOC 39 9 \n")
  fprintf(Form "ENDTEXT\n")
  
  fprintf(Form "FIELD \"MinClearance\"\n")
  fprintf(Form "FLOC 55 9 \n")
  fprintf(Form "INTSLIDEBAR 5 2\n MIN 1\n MAX 20\n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "TEXT \"Placement Priorities:\"\n")
  fprintf(Form "TLOC 39 11 \n")
  fprintf(Form "ENDTEXT\n")
  
  fprintf(Form "TEXT \"1. Near component\"\n")
  fprintf(Form "TLOC 41 13 \n")
  fprintf(Form "ENDTEXT\n")
  
  fprintf(Form "TEXT \"2. Empty space on board\"\n")
  fprintf(Form "TLOC 41 14 \n")
  fprintf(Form "ENDTEXT\n")
  
  fprintf(Form "TEXT \"3. Outside board outline\"\n")
  fprintf(Form "TLOC 41 15 \n")
  fprintf(Form "ENDTEXT\n")
  
  fprintf(Form "FIELD done\n")
  fprintf(Form "FLOC 6 18\n")
  fprintf(Form "MENUBUTTON \"Done\" 8 3\n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "FIELD cancel\n")
  fprintf(Form "FLOC 16 18\n")
  fprintf(Form "MENUBUTTON \"CANCEL\" 8 3\n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "ENDTILE\n")
  fprintf(Form "ENDFORM\n")
  close(Form)
) ;end defun - createSilkscreenForm