;###########################################################################
;#                                                                         #
;#            Silkscreen Text Placement Rules Management                   #
;#                                                                         #
;#  This routine manages silkscreen text placement according to specific   #
;#  rules prioritized as follows:                                          #
;#                                                                         #
;#  1. Always avoid copper (pads, vias, holes)                             #
;#  2. Primary placement: Look for space near the component with proper    #
;#     orientation                                                         #
;#  3. Secondary placement: If no space near component, look for empty     #
;#     spaces in the board                                                 #
;#  4. Tertiary placement: If no empty spaces available, place outside     #
;#     the board outline                                                   #
;#                                                                         #
;#  Usage:  place_silkscreen (from within Allegro)                         #
;###########################################################################

;##########################
;#  Register Program Name #
;##########################

axlCmdRegister("place_silkscreen" 'SilkscreenPlacementRules)

;#############################
;# Global Variable Declarations #
;#############################

; Board parameters
(defun init_globals ()
  board_outline = nil
  copper_objects = nil
  components = nil
  silkscreen_texts = nil

  ; Text orientation preferences
  orient_horizontal = t
  orient_vertical = nil
  
  ; Layer definitions
  silktop_layer = "REF DES/SILKSCREEN_TOP"
  silkbot_layer = "REF DES/SILKSCREEN_BOTTOM"
  
  ; Clearance values
  copper_clearance = 5.0  ; mils
  component_clearance = 10.0  ; mils
  
  ; Form variables
  fFile = nil
  Form = nil
  FormDir = "./"
)

;#############################
;# MAIN Program Starts Here. #
;#############################

(defun SilkscreenPlacementRules ()
  ; Initialize global variables
  init_globals()

  if( axlOKToProceed() then
    ; Create and display the user interface
    createSilkscreenForm()
    fFile = strcat(FormDir "silkscreenPlacementForm.form")
    Form = axlFormCreate((gensym) fFile nil 'silkscreenForm_Action t)
    
    ; Set default values in form
    axlFormSetField(Form "SilkTop" t)
    axlFormSetField(Form "SilkBot" t)
    axlFormSetField(Form "CopperClearance" copper_clearance)
    axlFormSetField(Form "ComponentClearance" component_clearance)
    
    ; Display the form
    axlFormDisplay(Form)
  else
    printf("E- Please terminate your interactive command.\n")
  ) ;endif OKToProceed
) ;end defun - MAIN Program

;############################################################################
;# performSilkscreenPlacement - Main function to apply silkscreen rules     #
;############################################################################

(defun performSilkscreenPlacement ()
  ; Get board outline
  board_outline = getBoardOutline()
  
  ; Get copper objects (pads, vias, holes)
  copper_objects = getCopperObjects()
  
  ; Get components
  components = getComponents()
  
  ; For each selected silkscreen layer
  silkscreen_layers = nil
  if(SilkTop == t then silkscreen_layers = cons(silktop_layer silkscreen_layers))
  if(SilkBot == t then silkscreen_layers = cons(silkbot_layer silkscreen_layers))
  
  foreach(layer silkscreen_layers
    axlMsgPut(strcat("Processing silkscreen layer: " layer))
    
    ; Get silkscreen texts for this layer
    silkscreen_texts = getSilkscreenTexts(layer)
    
    ; Process each silkscreen text
    foreach(text_id silkscreen_texts
      processSilkscreenText(text_id)
    )
    
    axlMsgPut(strcat("Completed silkscreen placement for layer: " layer))
  )
  
  ; Refresh display
  axlFlushDisplay()
) ;end defun - performSilkscreenPlacement

;############################################################################
;# processSilkscreenText - Apply rules to place a single silkscreen text    #
;############################################################################

(defun processSilkscreenText (text_id)
  ; Get text properties
  text = text_id->text
  xy = text_id->xy
  txt_x = xCoord(xy)
  txt_y = yCoord(xy)
  txt_rot = text_id->rotation
  txt_layer = text_id->layer
  parent_id = text_id->parent
  
  ; Skip if text has no parent component
  if(parent_id == nil then return(nil))
  
  ; Get parent component properties
  component_xy = parent_id->xy
  comp_x = xCoord(component_xy)
  comp_y = yCoord(component_xy)
  
  ; Apply placement rules in order of priority
  new_xy = nil
  
  ; Priority 1: Near component with proper orientation
  new_xy = findSpaceNearComponent(text_id, parent_id)
  
  ; Priority 2: Empty spaces in the board
  if(new_xy == nil then
    new_xy = findEmptySpaceOnBoard(text_id)
  )
  
  ; Priority 3: Outside board outline
  if(new_xy == nil then
    new_xy = findSpaceOutsideBoard(text_id)
  )
  
  ; If a valid placement was found, update the text position
  if(new_xy != nil then
    updateTextPosition(text_id, new_xy)
  )
) ;end defun - processSilkscreenText

;############################################################################
;# findSpaceNearComponent - Find space near component                       #
;############################################################################

(defun findSpaceNearComponent (text_id, component_id)
  ; Get component bounding box
  comp_box = component_id->bBox
  comp_llx = xCoord(xCoord(comp_box))
  comp_lly = yCoord(xCoord(comp_box))
  comp_urx = xCoord(yCoord(comp_box))
  comp_ury = yCoord(yCoord(comp_box))
  
  ; Calculate component center
  comp_center_x = (comp_llx + comp_urx) / 2
  comp_center_y = (comp_lly + comp_ury) / 2
  
  ; Get text dimensions
  text_width = text_id->width
  text_height = text_id->height
  
  ; Try positions in order: top, right, bottom, left
  potential_positions = list(
    list(comp_center_x, comp_ury + text_height + component_clearance),
    list(comp_urx + text_width + component_clearance, comp_center_y),
    list(comp_center_x, comp_lly - text_height - component_clearance),
    list(comp_llx - text_width - component_clearance, comp_center_y)
  )
  
  ; Check each potential position
  foreach(pos potential_positions
    pos_x = xCoord(pos)
    pos_y = yCoord(pos)
    
    ; Check if position is valid (no copper, within board)
    if(isValidPosition(pos_x, pos_y, text_width, text_height) then
      ; Determine proper orientation
      if(pos == nth(0, potential_positions) || pos == nth(2, potential_positions) then
        ; Top or bottom position - use horizontal orientation
        updateTextRotation(text_id, 0)
      else
        ; Left or right position - use vertical orientation
        updateTextRotation(text_id, 90)
      )
      
      return(pos)
    )
  )
  
  ; No valid position found near component
  return(nil)
) ;end defun - findSpaceNearComponent

;############################################################################
;# findEmptySpaceOnBoard - Find empty space on the board                    #
;############################################################################

(defun findEmptySpaceOnBoard (text_id)
  ; Get board dimensions
  board_llx = xCoord(xCoord(board_outline->bBox))
  board_lly = yCoord(xCoord(board_outline->bBox))
  board_urx = xCoord(yCoord(board_outline->bBox))
  board_ury = yCoord(yCoord(board_outline->bBox))
  
  ; Get text dimensions
  text_width = text_id->width
  text_height = text_id->height
  
  ; Create a grid of potential positions to check
  grid_step = 50.0  ; Grid step size
  
  ; Check positions in a grid pattern
  for(x board_llx + grid_step board_urx - grid_step grid_step
    for(y board_lly + grid_step board_ury - grid_step grid_step
      ; Check if position is valid (no copper, no components)
      if(isValidPosition(x, y, text_width, text_height) then
        ; Use horizontal orientation for grid positions
        updateTextRotation(text_id, 0)
        return(list(x, y))
      )
    )
  )
  
  ; No valid position found in board
  return(nil)
) ;end defun - findEmptySpaceOnBoard

;############################################################################
;# findSpaceOutsideBoard - Place text outside the board outline             #
;############################################################################

(defun findSpaceOutsideBoard (text_id)
  ; Get board dimensions
  board_llx = xCoord(xCoord(board_outline->bBox))
  board_lly = yCoord(xCoord(board_outline->bBox))
  board_urx = xCoord(yCoord(board_outline->bBox))
  board_ury = yCoord(yCoord(board_outline->bBox))
  
  ; Get text dimensions
  text_width = text_id->width
  text_height = text_id->height
  
  ; Try positions outside the board: top, right, bottom, left
  potential_positions = list(
    list((board_llx + board_urx) / 2, board_ury + text_height + component_clearance),
    list(board_urx + text_width + component_clearance, (board_lly + board_ury) / 2),
    list((board_llx + board_urx) / 2, board_lly - text_height - component_clearance),
    list(board_llx - text_width - component_clearance, (board_lly + board_ury) / 2)
  )
  
  ; Check each potential position
  foreach(pos potential_positions
    pos_x = xCoord(pos)
    pos_y = yCoord(pos)
    
    ; For outside board positions, we only check copper clearance
    if(hasCopperClearance(pos_x, pos_y, text_width, text_height) then
      ; Determine proper orientation
      if(pos == nth(0, potential_positions) || pos == nth(2, potential_positions) then
        ; Top or bottom position - use horizontal orientation
        updateTextRotation(text_id, 0)
      else
        ; Left or right position - use vertical orientation
        updateTextRotation(text_id, 90)
      )
      
      return(pos)
    )
  )
  
  ; If all else fails, place at top-right corner outside board
  pos = list(board_urx + text_width + component_clearance, board_ury + text_height + component_clearance)
  updateTextRotation(text_id, 0)
  return(pos)
) ;end defun - findSpaceOutsideBoard

;############################################################################
;# Helper Functions                                                         #
;############################################################################

(defun isValidPosition (x, y, width, height)
  ; Check if position is within board outline
  if(!isWithinBoardOutline(x, y, width, height) then
    return(nil)
  )
  
  ; Check if position has copper clearance
  if(!hasCopperClearance(x, y, width, height) then
    return(nil)
  )
  
  ; Check if position has component clearance
  if(!hasComponentClearance(x, y, width, height) then
    return(nil)
  )
  
  ; All checks passed
  return(t)
) ;end defun - isValidPosition

(defun isWithinBoardOutline (x, y, width, height)
  ; Calculate text bounding box
  text_llx = x - width / 2
  text_lly = y - height / 2
  text_urx = x + width / 2
  text_ury = y + height / 2
  
  ; Check if text is within board outline
  board_llx = xCoord(xCoord(board_outline->bBox))
  board_lly = yCoord(xCoord(board_outline->bBox))
  board_urx = xCoord(yCoord(board_outline->bBox))
  board_ury = yCoord(yCoord(board_outline->bBox))
  
  if(text_llx >= board_llx && text_urx <= board_urx && 
     text_lly >= board_lly && text_ury <= board_ury then
    return(t)
  else
    return(nil)
  )
) ;end defun - isWithinBoardOutline

(defun hasCopperClearance (x, y, width, height)
  ; Calculate text bounding box with clearance
  text_llx = x - width / 2 - copper_clearance
  text_lly = y - height / 2 - copper_clearance
  text_urx = x + width / 2 + copper_clearance
  text_ury = y + height / 2 + copper_clearance
  
  ; Check against each copper object
  foreach(copper_obj copper_objects
    copper_box = copper_obj->bBox
    copper_llx = xCoord(xCoord(copper_box))
    copper_lly = yCoord(xCoord(copper_box))
    copper_urx = xCoord(yCoord(copper_box))
    copper_ury = yCoord(yCoord(copper_box))
    
    ; Check for overlap
    if(!(text_urx < copper_llx || text_llx > copper_urx || 
         text_ury < copper_lly || text_lly > copper_ury) then
      return(nil)
    )
  )
  
  ; No overlap with copper objects
  return(t)
) ;end defun - hasCopperClearance

(defun hasComponentClearance (x, y, width, height)
  ; Calculate text bounding box with clearance
  text_llx = x - width / 2 - component_clearance
  text_lly = y - height / 2 - component_clearance
  text_urx = x + width / 2 + component_clearance
  text_ury = y + height / 2 + component_clearance
  
  ; Check against each component
  foreach(comp components
    comp_box = comp->bBox
    comp_llx = xCoord(xCoord(comp_box))
    comp_lly = yCoord(xCoord(comp_box))
    comp_urx = xCoord(yCoord(comp_box))
    comp_ury = yCoord(yCoord(comp_box))
    
    ; Check for overlap
    if(!(text_urx < comp_llx || text_llx > comp_urx || 
         text_ury < comp_lly || text_lly > comp_ury) then
      return(nil)
    )
  )
  
  ; No overlap with components
  return(t)
) ;end defun - hasComponentClearance

;############################################################################
;# Data Collection Functions                                                #
;############################################################################

(defun getBoardOutline ()
  ; Get the board outline
  axlSetFindFilter(?enabled list("noall" "lines") 
                   ?onButtons list("lines"))
  axlVisibleLayer("BOARD GEOMETRY/OUTLINE" t)
  outline = axlGetSelSet(axlAddSelectAll())
  axlClearSelSet()
  return(outline)
) ;end defun - getBoardOutline

(defun getCopperObjects ()
  ; Get copper objects (pads, vias, holes)
  copper_list = nil
  
  ; Get pads
  axlSetFindFilter(?enabled list("noall" "pins") 
                   ?onButtons list("pins"))
  axlVisibleDesign(nil)
  axlVisibleLayer("PIN/TOP" t)
  axlVisibleLayer("PIN/BOTTOM" t)
  pads = axlGetSelSet(axlAddSelectAll())
  copper_list = append(copper_list pads)
  axlClearSelSet()
  
  ; Get vias
  axlSetFindFilter(?enabled list("noall" "vias") 
                   ?onButtons list("vias"))
  axlVisibleLayer("VIA CLASS/TOP" t)
  axlVisibleLayer("VIA CLASS/BOTTOM" t)
  vias = axlGetSelSet(axlAddSelectAll())
  copper_list = append(copper_list vias)
  axlClearSelSet()
  
  ; Get holes
  axlSetFindFilter(?enabled list("noall" "drills") 
                   ?onButtons list("drills"))
  axlVisibleLayer("MANUFACTURING/DRILL_FIGURE" t)
  holes = axlGetSelSet(axlAddSelectAll())
  copper_list = append(copper_list holes)
  axlClearSelSet()
  
  return(copper_list)
) ;end defun - getCopperObjects

(defun getComponents ()
  ; Get components
  axlSetFindFilter(?enabled list("noall" "symbols") 
                   ?onButtons list("symbols"))
  axlVisibleDesign(nil)
  axlVisibleLayer("PACKAGE GEOMETRY/PLACE_BOUND_TOP" t)
  axlVisibleLayer("PACKAGE GEOMETRY/PLACE_BOUND_BOTTOM" t)
  components = axlGetSelSet(axlAddSelectAll())
  axlClearSelSet()
  
  return(components)
) ;end defun - getComponents

(defun getSilkscreenTexts (layer)
  ; Get silkscreen texts for a specific layer
  axlSetFindFilter(?enabled list("noall" "text") 
                   ?onButtons list("text"))
  axlVisibleDesign(nil)
  axlVisibleLayer(layer t)
  texts = axlGetSelSet(axlAddSelectAll())
  axlClearSelSet()
  
  return(texts)
) ;end defun - getSilkscreenTexts

;############################################################################
;# Text Manipulation Functions                                              #
;############################################################################

(defun updateTextPosition (text_id, new_xy)
  ; Update text position
  textBlock = text_id->textBlock
  rotation = text_id->rotation
  parent_id = text_id->parent
  mirrored = text_id->isMirrored
  justify = text_id->justify
  text = text_id->text
  layer = text_id->layer
  
  ; Create text orientation
  textOrientation = make_axlTextOrientation(?textBlock textBlock,
                                           ?rotation rotation,
                                           ?mirrored mirrored,
                                           ?justify justify)
  
  ; Create new text and delete old one
  update = axlDBCreateText(text, new_xy, textOrientation, layer, parent_id)
  
  if(update != nil then
    axlDeleteObject(list(text_id))
    return(t)
  else
    axlMsgPut(strcat("Update Failed on " text))
    return(nil)
  )
) ;end defun - updateTextPosition

(defun updateTextRotation (text_id, new_rotation)
  ; Update text rotation
  textBlock = text_id->textBlock
  parent_id = text_id->parent
  mirrored = text_id->isMirrored
  justify = text_id->justify
  text = text_id->text
  layer = text_id->layer
  xy = text_id->xy
  
  ; Create text orientation
  textOrientation = make_axlTextOrientation(?textBlock textBlock,
                                           ?rotation new_rotation,
                                           ?mirrored mirrored,
                                           ?justify justify)
  
  ; Create new text and delete old one
  update = axlDBCreateText(text, xy, textOrientation, layer, parent_id)
  
  if(update != nil then
    axlDeleteObject(list(text_id))
    return(t)
  else
    axlMsgPut(strcat("Rotation Update Failed on " text))
    return(nil)
  )
) ;end defun - updateTextRotation

;############################################################################
;# Form Functions                                                           #
;############################################################################

(defun silkscreenForm_Action (Form)
  (case Form->curField
    ("SilkTop"
      SilkTop = Form->curValue
    )
    
    ("SilkBot"
      SilkBot = Form->curValue
    )
    
    ("CopperClearance"
      copper_clearance = Form->curValue
    )
    
    ("ComponentClearance"
      component_clearance = Form->curValue
    )
    
    ("done"
      performSilkscreenPlacement()
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

(defun createSilkscreenForm ()
  fFile = strcat(FormDir "silkscreenPlacementForm.form")
  Form = outfile(fFile "w")
  fprintf(Form "FILE_TYPE=FORM_DEFN VERSION=2\n")
  fprintf(Form "FORM\n")
  fprintf(Form "FIXED\n")
  fprintf(Form "PORT 70 16\n")
  fprintf(Form "HEADER \"Silkscreen Placement Rules V1.0\"\n")
  fprintf(Form "TILE\n")
  
  fprintf(Form "GROUP \"Select Silkscreen Layers\"\n")
  fprintf(Form "GLOC 2 1\n")
  fprintf(Form "GSIZE 66 4\n")
  fprintf(Form "ENDGROUP\n")
  
  fprintf(Form "FIELD \"SilkTop\"\n")
  fprintf(Form "FLOC 10 3\n")
  fprintf(Form "CHECKLIST \"Silkscreen-Top\"\n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "FIELD \"SilkBot\"\n")
  fprintf(Form "FLOC 45 3\n")
  fprintf(Form "CHECKLIST \"Silkscreen-Bottom\"\n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "GROUP \"Clearance Settings\"\n")
  fprintf(Form "GLOC 2 6\n")
  fprintf(Form "GSIZE 66 4\n")
  fprintf(Form "ENDGROUP\n")
  
  fprintf(Form "TEXT \"Copper Clearance (mils):\"\n")
  fprintf(Form "TLOC 5 8\n")
  fprintf(Form "ENDTEXT\n")
  
  fprintf(Form "FIELD \"CopperClearance\"\n")
  fprintf(Form "FLOC 28 8\n")
  fprintf(Form "REALFILLIN 6 1\n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "TEXT \"Component Clearance (mils):\"\n")
  fprintf(Form "TLOC 35 8\n")
  fprintf(Form "ENDTEXT\n")
  
  fprintf(Form "FIELD \"ComponentClearance\"\n")
  fprintf(Form "FLOC 60 8\n")
  fprintf(Form "REALFILLIN 6 1\n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "FIELD done\n")
  fprintf(Form "FLOC 18 13\n")
  fprintf(Form "MENUBUTTON \"Run\" 10 3\n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "FIELD cancel\n")
  fprintf(Form "FLOC 40 13\n")
  fprintf(Form "MENUBUTTON \"Cancel\" 10 3\n")
  fprintf(Form "ENDFIELD\n")
  
  fprintf(Form "ENDTILE\n")
  fprintf(Form "ENDFORM\n")
  close(Form)
) ;end defun - createSilkscreenForm