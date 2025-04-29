;##########################
;# Updated Program Logic  #
;##########################

(defun rotateSilkAssy ()
  (foreach text_id Text_List
    (setq changeit 0)
    (setq text (text_id->text))
    (setq xy (text_id->xy))
    (setq txt_x (xCoord xy))
    (setq txt_y (yCoord xy))
    (setq txt_tblk (text_id->textBlock))

    ; ==== Change Text to "Center" Justification for ease of use ====
    (setq txt_jus (text_id->justify))
    (if (!= txt_jus "CENTER") (setq changeit 1))

    ; ==== Check for valid copper element in the way ====
    (if (copperInWay txt_x txt_y)
      ; Skip text placement if copper (pad, via, hole) is in the way
      (continue)
    )

    ; ==== First priority: Look for specifications near the component ====
    (if (specsNearComponent txt_x txt_y)
      (progn
        (setq changeit 1)
        ; Position based on orientation requirements
        (placeNearComponent txt_x txt_y)
      )
      ; ==== Second priority: Check for empty space on the board ====
      (if (emptySpaceAvailable txt_x txt_y)
        (progn
          (setq changeit 1)
          ; Place text in available space
          (placeInEmptySpace txt_x txt_y)
        )
        ; ==== Third priority: Place outside the board if no space available ====
        (if (noSpaceAvailableOnBoard)
          (progn
            (setq changeit 1)
            ; Place text outside the board area
            (placeOutsideBoard txt_x txt_y)
          )
        )
      )
    )

    ; ==== Apply rotation and update text if necessary ====
    (if (= changeit 1)
      (createTextBlock_RD)
    )
  )
); end defun - rotateSilkAssy

;#######################
;# Helper Functions    #
;#######################

(defun copperInWay (x y)
  ; Check if there is copper (pad, via, hole) at the specified location
  ; Return t if there is copper, nil otherwise
  (if (axlFindObjectAtPoint x y "copper") 
    t
    nil
  )
)

(defun specsNearComponent (x y)
  ; Check if there are any placement specifications near the component at (x, y)
  ; Placeholder function; you should define specific criteria based on your design requirements
  (if (isComponentWithSpecNearby x y)
    t
    nil
  )
)

(defun emptySpaceAvailable (x y)
  ; Check if there is empty space available on the board for silkscreen text
  (if (isEmptySpace x y)
    t
    nil
  )
)

(defun noSpaceAvailableOnBoard ()
  ; Check if no empty space is available on the board
  (if (isBoardFull)
    t
    nil
  )
)

;##########################
;# Helper Placement Functions #
;##########################

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

(defun updateTextPosition (new_x new_y)
  ; Update the text position in the design database
  (setq xy (list new_x new_y))
  (createTextBlock_RD)
)

;###############################
;# Existing Functions Remain   #
;###############################

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
  (if (!= update nil)
    (axlDeleteObject (list text_id))
    (axlMsgPut (strcat "Update Failed on " text))
  )
)

; Note: The following functions are referenced but not defined in this script:
; - isComponentWithSpecNearby
; - isEmptySpace
; - isBoardFull
; - txt_rot variable is used but not defined
