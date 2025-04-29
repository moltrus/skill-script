;;; =========================================================
;;; Allegro Skill Program - Silkscreen Rules
;;; =========================================================
;;; Description: This program applies silkscreen text placement rules
;;; based on specified priorities while avoiding copper.
;;; 
;;; Rules:
;;; - Global: Always avoid copper (pads, vias, holes)
;;; - 1st priority: Place near component with proper orientation
;;; - 2nd priority: If components in specific area have no space between them,
;;;   look for empty spaces on the board
;;; - 3rd priority: If no empty spaces available, place outside the board
;;; =========================================================

;;; Define main function
(defun apply_silkscreen_rules ()
  (axlClearSelSet)
  (axlVisibleDesign nil)
  
  ;; Get all components
  (setq all_components (axlGetSelSet (list "COMPONENT")))
  
  ;; Process each component
  (foreach component all_components
    (let ((component_name (axlGetComponentName component))
          (ref_des (axlGetComponentRefDes component))
          (component_bbox (axlGetComponentBBox component))
          (copper_objs nil)
          (text_placed nil))
      
      ;; Get copper objects (pads, vias, holes)
      (setq copper_objs (get_copper_objects))
      
      ;; Debug info
      (axlMsgPut (strcat "Processing component: " ref_des))
      
      ;; Try 1st priority: Place near component with proper orientation
      (if (place_near_component component component_bbox copper_objs)
          (setq text_placed t)
          (axlMsgPut (strcat "  1st priority failed for " ref_des))
      )
      
      ;; Try 2nd priority: Look for empty spaces on the board
      (if (and (not text_placed) 
               (place_in_empty_space component component_bbox copper_objs))
          (setq text_placed t)
          (axlMsgPut (strcat "  2nd priority failed for " ref_des))
      )
      
      ;; Try 3rd priority: Place outside the board
      (if (and (not text_placed)
               (place_outside_board component component_bbox))
          (setq text_placed t)
          (axlMsgPut (strcat "  3rd priority failed for " ref_des))
      )
      
      ;; Report final status
      (if text_placed
          (axlMsgPut (strcat "  Silkscreen placed for " ref_des))
          (axlMsgPut (strcat "  Failed to place silkscreen for " ref_des))
      )
    )
  )
  
  (axlMsgPut "Silkscreen rules application completed")
  t
)

;;; Function to get all copper objects (pads, vias, holes)
(defun get_copper_objects ()
  (let ((all_copper nil))
    (setq all_copper (append 
                      (axlGetSelSet (list "PINS"))
                      (axlGetSelSet (list "VIAS"))
                      (axlGetSelSet (list "SHAPES" "CONDUCTOR"))
                     ))
    all_copper
  )
)

;;; Function to check if point is on copper
(defun is_on_copper (point copper_objects)
  (let ((on_copper nil)
        (x (car point)) 
        (y (cadr point)))
    
    (foreach obj copper_objects
      (cond
        ((or (equal "VIA" (axlDBGetObjectType obj))
             (equal "PIN" (axlDBGetObjectType obj)))
         (let ((obj_point (axlDBGetCenter obj))
               (obj_radius (/ (axlDBGetWidth obj) 2.0)))
           (if (<= (distance_between_points point obj_point) obj_radius)
               (setq on_copper t)
           )
         )
        )
        ((equal "SHAPE" (axlDBGetObjectType obj))
         (if (axlDBIsPointInShape point obj)
             (setq on_copper t)
         )
        )
      )
    )
    on_copper
  )
)

;;; Function to calculate distance between two points
(defun distance_between_points (p1 p2)
  (let ((x1 (car p1)) 
        (y1 (cadr p1))
        (x2 (car p2)) 
        (y2 (cadr p2)))
    (sqrt (+ (pow (- x2 x1) 2) (pow (- y2 y1) 2)))
  )
)

;;; Function to place text near component with proper orientation
(defun place_near_component (component bbox copper_objs)
  (let ((ref_des (axlGetComponentRefDes component))
        (text_obj nil)
        (text_width 50.0)  ; Default width if unable to get actual width
        (text_height 25.0) ; Default height if unable to get actual height
        (positions nil)
        (success nil))
    
    ;; Try to get text object and dimensions if available
    (setq text_obj (axlDBGetTextByName ref_des))
    (when text_obj
      (setq text_width (axlDBGetTextWidth text_obj))
      (setq text_height (axlDBGetTextHeight text_obj))
    )
    
    ;; Define potential positions around component
    (setq positions (list
                     (list (+ (car bbox) text_width) (/ (+ (cadr bbox) (cadddr bbox)) 2))      ; Right
                     (list (- (caddr bbox) text_width) (/ (+ (cadr bbox) (cadddr bbox)) 2))     ; Left
                     (list (/ (+ (car bbox) (caddr bbox)) 2) (+ (cadddr bbox) text_height))    ; Top
                     (list (/ (+ (car bbox) (caddr bbox)) 2) (- (cadr bbox) text_height))      ; Bottom
                    ))
    
    (foreach pos positions
      (if (not (is_on_copper pos copper_objs))
          (progn
            (axlDBCreateText 
              ref_des              ; Text string
              pos                  ; Position
              0.0                  ; Rotation
              "SILKSCREEN_TOP"     ; Layer
              3.0                  ; Text height (mils)
              nil                  ; Justify
            )
            (setq success t)
            (return t)
          )
      )
    )
    success
  )
)

;;; Function to find and place text in empty spaces on board
(defun place_in_empty_space (component bbox copper_objs)
  (let ((ref_des (axlGetComponentRefDes component))
        (text_obj nil)
        (text_width 50.0)  ; Default width
        (text_height 25.0) ; Default height
        (board_bbox nil)
        (grid_size 50.0)
        (grid_points nil)
        (success nil))
    
    ;; Try to get text dimensions if available
    (setq text_obj (axlDBGetTextByName ref_des))
    (when text_obj
      (setq text_width (axlDBGetTextWidth text_obj))
      (setq text_height (axlDBGetTextHeight text_obj))
    )
    
    ;; Get board outline
    (setq board_bbox (axlDBGetBoardBoundary))
    
    ;; Create a grid of points to check
    (setq grid_points (create_grid_points board_bbox grid_size text_width text_height))
    
    (foreach point grid_points
      ;; Check if point is away from copper and components
      (if (and (not (is_on_copper point copper_objs))
               (not (is_near_component point)))
          (progn
            (axlDBCreateText 
              ref_des              ; Text string
              point                ; Position
              0.0                  ; Rotation
              "SILKSCREEN_TOP"     ; Layer
              3.0                  ; Text height (mils)
              nil                  ; Justify
            )
            (setq success t)
            (return t)
          )
      )
    )
    success
  )
)

;;; Function to create grid points across the board
(defun create_grid_points (board_bbox grid_size text_width text_height)
  (let ((points nil)
        (x_min (car board_bbox))
        (y_min (cadr board_bbox))
        (x_max (caddr board_bbox))
        (y_max (cadddr board_bbox)))
    
    (for x x_min x_max grid_size
      (for y y_min y_max grid_size
        (if (and (>= x (+ x_min text_width))
                 (<= x (- x_max text_width))
                 (>= y (+ y_min text_height))
                 (<= y (- y_max text_height)))
            (setq points (append points (list (list x y))))
        )
      )
    )
    points
  )
)

;;; Function to check if point is near any component
(defun is_near_component (point)
  (let ((near_component nil)
        (min_distance 10.0)        ; Minimum distance from components in mils
        (all_components (axlGetSelSet (list "COMPONENT"))))
    
    (foreach comp all_components
      (let ((comp_bbox (axlGetComponentBBox comp)))
        ;; Check if point is too close to component bbox
        (if (is_point_near_bbox point comp_bbox min_distance)
            (setq near_component t)
        )
      )
    )
    near_component
  )
)

;;; Function to check if point is near a bounding box
(defun is_point_near_bbox (point bbox min_distance)
  (let ((x (car point))
        (y (cadr point))
        (x_min (- (car bbox) min_distance))
        (y_min (- (cadr bbox) min_distance))
        (x_max (+ (caddr bbox) min_distance))
        (y_max (+ (cadddr bbox) min_distance)))
    
    (and (>= x x_min) (<= x x_max) (>= y y_min) (<= y y_max))
  )
)

;;; Function to place text outside the board
(defun place_outside_board (component bbox)
  (let ((ref_des (axlGetComponentRefDes component))
        (text_obj nil)
        (board_bbox nil)
        (outside_point nil))
    
    ;; Get board outline
    (setq board_bbox (axlDBGetBoardBoundary))
    
    ;; Define a point outside the board (e.g., top-right corner + offset)
    (setq outside_point (list 
                         (+ (caddr board_bbox) 100.0)
                         (+ (cadddr board_bbox) 100.0)
                        ))
    
    ;; Create text outside the board
    (axlDBCreateText 
      ref_des              ; Text string
      outside_point        ; Position
      0.0                  ; Rotation
      "SILKSCREEN_TOP"     ; Layer
      3.0                  ; Text height (mils)
      nil                  ; Justify
    )
    t
  )
)

;;; Command to execute the script
(defun silkscreen_rules ()
  (axlMsgPut "Starting silkscreen rules application...")
  (apply_silkscreen_rules)
)

;;; Register command
(axlCmdRegister "silkscreen_rules" 'silkscreen_rules)
(axlMsgPut "Silkscreen rules command registered. Type 'silkscreen_rules' to execute.")
