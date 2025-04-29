;##########################
;# Updated Program Logic  #
;##########################

; Define global variables first
(defvar Text_List nil)
(defvar text nil)
(defvar xy nil)
(defvar txt_x nil)
(defvar txt_y nil)
(defvar txt_tblk nil)
(defvar txt_jus nil)
(defvar txt_rot 0.0)
(defvar text_id nil)
(defvar changeit nil)
(defvar textBlock nil)
(defvar rotation nil)
(defvar parent_id nil)
(defvar mirrored nil)
(defvar justify nil)
(defvar textOrientation nil)
(defvar update nil)
(defvar new_x nil)
(defvar new_y nil)

;###############################
;# Helper Functions First      #
;###############################

(defun xCoord (point)
  (nth 0 point)
)

(defun yCoord (point)
  (nth 1 point)
)

(defun copperInWay (x y)
  ; Check if there is copper (pad, via, hole) at the specified location
  ; Return t if there is copper, nil otherwise
  ; Placeholder implementation - replace with actual implementation
  nil
)

(defun isComponentWithSpecNearby (x y)
  ; Placeholder implementation - replace with actual implementation
  nil
)

(defun specsNearComponent (x y)
  ; Check if there are any placement specifications near the component
  ; Placeholder implementation
  (isComponentWithSpecNearby x y)
)

(defun isEmptySpace (x y)
  ; Placeholder implementation - replace with actual implementation
  t
)

(defun emptySpaceAvailable (x y)
  ; Check if there is empty space available
  (isEmptySpace x y)
)

(defun isBoardFull ()
  ; Placeholder implementation - replace with actual implementation
  nil
)

(defun noSpaceAvailableOnBoard ()
  ; Check if no empty space is available on the board
  (isBoardFull)
)

(defun createTextBlock_RD ()
  (setq textBlock txt_tblk)
  (setq rotation txt_rot)
  (setq parent_id (text_id->parent))
  (setq mirrored (text_id->isMirrored))
  (setq justify "CENTER")

  (setq textOrientation (make_axlTextOrientation 
                          ?textBlock textBlock
                          ?rotation rotation
                          ?mirrored mirrored
                          ?justify justify
                        ))
  (setq update (axlDBCreateText text xy textOrientation (text_id->layer) parent_id))
  (if (update
    (axlDeleteObject (list text_id))
    (axlMsgPut (strcat "Update Failed on " text))
  )
)

(defun updateTextPosition (new_x new_y)
  ; Update the text position in the design database
  (setq xy (list new_x new_y))
  (createTextBlock_RD)
)

(defun placeNearComponent (x y)
  ; Place text near a component based on orientation and available space
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
  ; Example: Place it 100 units outside the right edge
  (setq new_x (+ x 100))
  (setq new_y y)
  (updateTextPosition new_x new_y)
)

;##########################
;# Main Function          #
;##########################

(defun rotateSilkAssy ()
  (foreach current_text_id Text_List
    (setq text_id current_text_id) ; Set the global text_id
    (setq changeit 0)
    (setq text (text_id->text))
    (setq xy (text_id->xy))
    (setq txt_x (xCoord xy))
    (setq txt_y (yCoord xy))
    (setq txt_tblk (text_id->textBlock))

    ; ==== Change Text to "Center" Justification for ease of use ====
    (setq txt_jus (text_id->justify))
    (when (neq txt_jus "CENTER") 
      (setq changeit 1)
    )

    ; ==== Check for valid copper element in the way ====
    (when (copperInWay txt_x txt_y)
      ; Skip text placement if copper (pad, via, hole) is in the way
      (continue)
    )

    ; ==== First priority: Look for specifications near the component ====
    (cond
      ((specsNearComponent txt_x txt_y)
        (setq changeit 1)
        ; Position based on orientation requirements
        (placeNearComponent txt_x txt_y)
      )
      ; ==== Second priority: Check for empty space on the board ====
      ((emptySpaceAvailable txt_x txt_y)
        (setq changeit 1)
        ; Place text in available space
        (placeInEmptySpace txt_x txt_y)
      )
      ; ==== Third priority: Place outside the board if no space available ====
      ((noSpaceAvailableOnBoard)
        (setq changeit 1)
        ; Place text outside the board area
        (placeOutsideBoard txt_x txt_y)
      )
    )

    ; ==== Apply rotation and update text if necessary ====
    (when (= changeit 1)
      (createTextBlock_RD)
    )
  )
  t
)

; Return success indicator when file is loaded
t
