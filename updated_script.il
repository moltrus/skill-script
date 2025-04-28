;##########################
;# Updated Program Logic  #
;##########################

(defun rotateSilkAssy ()
  foreach( text_id Text_List
    changeit = 0
    text = text_id->text
    xy = text_id->xy
    txt_x = xCoord(xy)
    txt_y = yCoord(xy)
    txt_tblk = text_id->textBlock

    ; ==== Change Text to "Center" Justification for ease of use ====
    txt_jus = text_id->justify
    if (txt_jus != "CENTER" then changeit = 1)

    ; ==== Check for valid copper element in the way ====
    if (copperInWay(txt_x, txt_y)) then
      ; Skip text placement if copper (pad, via, hole) is in the way
      continue
    endif

    ; ==== First priority: Look for specifications near the component ====
    if (specsNearComponent(txt_x, txt_y)) then
      changeit = 1
      ; Position based on orientation requirements
      placeNearComponent(txt_x, txt_y)
    else
      ; ==== Second priority: Check for empty space on the board ====
      if (emptySpaceAvailable(txt_x, txt_y)) then
        changeit = 1
        ; Place text in available space
        placeInEmptySpace(txt_x, txt_y)
      else
        ; ==== Third priority: Place outside the board if no space available ====
        if (noSpaceAvailableOnBoard()) then
          changeit = 1
          ; Place text outside the board area
          placeOutsideBoard(txt_x, txt_y)
        endif
      endif
    endif

    ; ==== Apply rotation and update text if necessary ====
    if (changeit == 1) then
      createTextBlock_RD()
    endif
  )
); end defun - rotateSilkAssy

;#######################
;# Helper Functions    #
;#######################

(defun copperInWay (x y)
  ; Check if there is copper (pad, via, hole) at the specified location
  ; Return t if there is copper, nil otherwise
  ; (Example: Use axlFind functions to check the region for copper objects)
  ; This is a placeholder implementation, adapt it to your tool's API
  (if (axlFindObjectAtPoint x y "copper")) t
  nil
)

(defun specsNearComponent (x y)
  ; Check if there are any placement specifications near the component at (x, y)
  ; This can be done by checking design rules or proximity to other components
  ; Placeholder function; you should define specific criteria based on your design requirements
  ; Example: check if there are placement rules or empty spaces near the component
  (if (isComponentWithSpecNearby x y)) t
  nil
)

(defun emptySpaceAvailable (x y)
  ; Check if there is empty space available on the board for silkscreen text
  ; Use design rules or available space check
  (if (isEmptySpace x y)) t
  nil
)

(defun noSpaceAvailableOnBoard ()
  ; Check if no empty space is available on the board
  ; Placeholder logic to determine if the board has space left for silkscreen
  ; Adapt this logic based on your design constraints or area availability
  (if (isBoardFull)) t
  nil
)

;##########################
;# Helper Placement Functions #
;##########################

(defun placeNearComponent (x y)
  ; Place text near a component based on orientation and available space
  ; Placeholder for logic to adjust placement based on component position
  ; You may want to shift the coordinates to find the best spot
  ; For example, move it 20 units to the right
  (setq new_x (+ x 20))
  (setq new_y y)
  (updateTextPosition new_x new_y)
)

(defun placeInEmptySpace (x y)
  ; Place text in empty space
  ; Adjust coordinates based on available area
  (setq new_x x)
  (setq new_y y)
  (updateTextPosition new_x new_y)
)

(defun placeOutsideBoard (x y)
  ; Place the text outside the board boundary
  ; Outside the board might be a fixed location or edge coordinates
  ; Example: Place it 100 units outside the right edge
  (setq new_x (+ x 100))
  (setq new_y y)
  (updateTextPosition new_x new_y)
)

(defun updateTextPosition (new_x new_y)
  ; Update the text position in the design database
  (setq xy (list new_x new_y))
  createTextBlock_RD()
)

;###############################
;# Existing Functions Remain   #
;###############################

(defun createTextBlock_RD ()
  textBlock = txt_tblk
  rotation = txt_rot
  parent_id = text_id->parent
  mirrored = text_id->isMirrored
  justify = "CENTER"

  textOrientation = make_axlTextOrientation( ?textBlock textBlock
                                            ?rotation rotation
                                            ?mirrored mirrored
                                            ?justify justify
                                          )
  update = axlDBCreateText( text, xy, textOrientation, text_id->layer, parent_id )
  if (update != nil then
    axlDeleteObject( list( text_id ))
  else
    axlMsgPut( strcat( "Update Failed on " text ))
  endif
)
