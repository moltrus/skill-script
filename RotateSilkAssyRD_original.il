;###########################################################################
;#                                                                         #
;#   Rotate Reference Designator Text on Silkscreen and Assembly Layers    #
;#                                                                         #
;#  This routine rotates Silkscreen and Assembly Reference Designators     #
;#  such that they are all "rightward reading".  This means that all RDs   #
;#  will read left to right if they are horizontal, or bottom to top if    #
;#  they are vertical.                                                     #
;#                                                                         #
;#  Additionally, any Assy RD will automatically be moved to the component #
;#  origin.                                                                #
;#                                                                         #
;#                                                                         #
;#  Usage:  rotate_silkassy (from within Allegro)                          #
;###########################################################################

;load(strcat(eval('mySkillPath), "\\RotateSilkAssyRD.il"))

;##########################
;#  Register Program Name #
;##########################

axlCmdRegister("rotate_silkassy" 'RotateSilkAssyRD)

;#############################
;# MAIN Program Starts Here. #
;#############################

(defun RotateSilkAssyRD ()

  ; ==== Initialize Global Variables ====

  Layer_List = nil
  Text_List  = nil
  Layer_List = nil
  justify    = ""

  LRBT = t   ; Left to Right, Bottom to Top
  LRTB = nil ; Left to Right, Top to Bottom
  RLBT = nil ; Right to Left, Bottom to Top
  RLTB = nil ; Right to Left, Top to Bottom

  CenterAssy = t
  ChangeTextSize  = nil
  TextBlockLarge  = 6
  TextBlockMedium = 3
  TextBlockSmall  = 1
  tblk_large  = "6"
  tblk_medium = "3"
  tblk_small  = "1"

  SilkTop = nil
  SilkBot = nil
  AssyTop = nil
  AssyBot = nil
  silktop_layer = "REF DES/SILKSCREEN_TOP"
  silkbot_layer = "REF DES/SILKSCREEN_BOTTOM"
  assytop_layer = "REF DES/ASSEMBLY_TOP"
  assybot_layer = "REF DES/ASSEMBLY_BOTTOM"
  
  sym_savex  = -1
  sym_savey  = -1
  sym_saveparent = nil

  ;SetupDir(); Commented out DE 26-Jan-11
  FormDir = "./"; Inserted DE 26-Jan-11

  if( axlOKToProceed() then
      
      ; ==== Create and Display the User Interface ====

      createRotateRDForm()
      fFile = strcat( FormDir "rotateRDForm.form" )
      Form = axlFormCreate( (gensym)  fFile  nil 'rotateRDForm_Action t)
      axlFormSetField(Form "LRBT" LRBT)
      axlFormSetField(Form "LRTB" LRTB)
      axlFormSetField(Form "RLBT" RLBT)
      axlFormSetField(Form "RLTB" RLTB)
      axlFormSetField(Form "CenterAssy"      CenterAssy)
      axlFormSetField(Form "TextBlockLarge"  TextBlockLarge )
      axlFormSetField(Form "TextBlockMedium" TextBlockMedium)
      axlFormSetField(Form "TextBlockSmall"  TextBlockSmall ) 
      axlFormDisplay( Form )
  else
      printf("E- Please terminate your interactive command .\n")
  );endif OKToProceed

); end defun - MAIN Program

;############################################################################
;# doRotateSilkAssy - When OK is hit, perform Reference Designator Rotation #
;#                   Do each layer separately to improve performance.       #
;############################################################################

(defun doRotateSilkAssy ()

   axlClearSelSet()
   axlSetFindFilter( ?enabled   (list "noall"  "text")
                     ?onButtons (list "text")
                   )
   axlMsgPut("Silk/Assy RD rotation started.")
   foreach( layer Layer_List 
            axlVisibleDesign(nil)
            axlVisibleLayer( layer t)
            Text_List = axlGetSelSet( axlAddSelectAll() )
            if( length( Text_List ) != 0 then rotateSilkAssy() )
            axlClearSelSet()
            axlFlushDisplay()
            axlMsgPut(strcat( layer " Rotation completed.") )
          ); foreach

; ==== Turn On Top/Bottom Silk & Assembly Layers, if they were changed. ====

   Display_List = list("paramLayerGroup:BOARD GEOMETRY/paramLayer:OUTLINE")
   if( AssyTop == t then
       Display_List = cons("paramLayerGroup:PACKAGE GEOMETRY/paramLayer:ASSEMBLY_TOP" Display_List)
       Display_List = cons("paramLayerGroup:REF DES/paramLayer:ASSEMBLY_TOP"          Display_List)
     )
   if( AssyBot == t then
       Display_List = cons("paramLayerGroup:PACKAGE GEOMETRY/paramLayer:ASSEMBLY_BOTTOM" Display_List)
       Display_List = cons("paramLayerGroup:REF DES/paramLayer:ASSEMBLY_BOTTOM"          Display_List)
     ) 
   if( SilkTop == t then
       Display_List = cons("paramLayerGroup:PACKAGE GEOMETRY/paramLayer:SILKSCREEN_TOP"  Display_List)
       Display_List = cons("paramLayerGroup:REF DES/paramLayer:SILKSCREEN_TOP"           Display_List)
     )
   if( SilkBot == t then
       Display_List = cons("paramLayerGroup:PACKAGE GEOMETRY/paramLayer:SILKSCREEN_BOTTOM" Display_List)
       Display_List = cons("paramLayerGroup:REF DES/paramLayer:SILKSCREEN_BOTTOM"          Display_List)
     )
   foreach( Layer Display_List
            val = axlGetParam( Layer )
            val->visible = t
            axlSetParam( val )
          )

); end defun - doRotateSilkAssy

;######################################################################  
;# rotateSilkAssy - Check layer type, Get RD Coordinates and rotation.#
;#                  Rotate RD in place if wrong orientation, change   #
;#                  text justification to "center".                   #
;######################################################################  

(defun rotateSilkAssy ()
 
   foreach( text_id Text_List
            ;axlMsgPut(strcat("Processing: " text_id->text)) 
            ; ==== retrieve current information on text id              ====
          
        changeit = 0
        text   = text_id->text
        xy     = text_id->xy
            txt_x  = xCoord( xy )
            txt_y  = yCoord( xy )
            txt_tblk = text_id->textBlock

        ; ==== Change Text to "Center" Justification for ease of use ====

            txt_jus= text_id->justify
            if( txt_jus != "CENTER" then changeit = 1 )

        ; ==== Check current rotation against preferred orientation ====
              ; ==== Change if necessary                                  ====

        txt_rot= text_id->rotation
            if( LRBT == t then
                if( txt_rot == 180 then
                    txt_rot  = 0
              changeit = 1
                  )
                if( txt_rot == 270 then 
                    txt_rot  = 90
              changeit = 1
                  )
              );endif LRTB

        if( LRTB == t then
                if( txt_rot == 180 then 
                    txt_rot  = 0
              changeit = 1
                  )
                if( txt_rot == 90 then 
                    txt_rot  = 270
                    changeit = 1   
                  )
              );endif LRBT

        if( RLBT == t then
                if( txt_rot == 180 then 
                    txt_rot  = 0
                    changeit = 1
                  )
                if( txt_rot == 270 then 
                    txt_rot  =  90
                    changeit = 1
                  )
              );endif RLBT

        if( RLTB == t then
                if( txt_rot == 180 then 
                    txt_rot  = 0
                    changeit = 1
                  )
                if( txt_rot == 90 then 
                    txt_rot  = 270
                    changeit = 1
                  )
              );endif RLTB

              ; ==== ON Assembly Layers, if requested by user in form ====
            ; ====    Move Assy Text to Calculated Symbol origin    ====
            ; ====    Change Text Block Sizes for easier reading.   ====
     
            txt_lyr = text_id->layer
            if( txt_lyr == assytop_layer || txt_lyr == assybot_layer || txt_lyr == silktop_layer || txt_lyr == silkbot_layer then
                if( CenterAssy == t then
                    foreach(childid text_id->parent->children
                            if( childid->layer == "PACKAGE GEOMETRY/PLACE_BOUND_TOP" ||
                                childid->layer == "PACKAGE GEOMETRY/PLACE_BOUND_BOTTOM" then                
                                sym_llx = xCoord( xCoord( childid->bBox ))
                                sym_lly = yCoord( xCoord( childid->bBox ))
                                sym_urx = xCoord( yCoord( childid->bBox ))
                                sym_ury = yCoord( yCoord( childid->bBox )) 
                                sym_x   = (sym_urx + sym_llx) / 2
                                sym_y   = (sym_ury + sym_lly) / 2
                              );endif childid
                           );end foreach
                    if( txt_rot ==   0 then sym_y = sym_y - 20 )
                    if( txt_rot ==  90 then sym_x = sym_x + 20 )
                    if( txt_rot == 180 then sym_y = sym_y + 20 ) 
                    if( txt_rot == 270 then sym_x = sym_x - 20 )
                    xy       = list( sym_x sym_y )
                    changeit = 1
                  );endif CenterAssy
              
                if( ChangeTextSize == t then 
                    changeit = 1
                    txt_tblk = tblk_large
                    rdalpha  = substring( text 1 1 )
                    if( rdalpha == "C" || rdalpha == "R" then 
                        txt_tblk = tblk_small
                      )
                    if( rdalpha == "D" then 
                        txt_tblk = tblk_small
                      )
                    rdalpha  = substring( text 1 2 )
                    if( rdalpha == "RN" || rdalpha == "RP" then 
                        txt_tblk = tblk_medium
                      )
                if( rdalpha == "FB" || rdalpha == "KP" then 
                        txt_tblk = tblk_medium
                      )
              if( rdalpha == "TP" then 
                        txt_tblk = tblk_small
                      )
                  );endif ChangeTextBlock 
              );endif txt_lyer

          ; ==== Do text movement, if anything has changed ==== 
          
;VAN HUYNH modified here (add _RD)

            if( changeit == 1 then createTextBlock_RD())
      ); endforeach

); end defun - rotateSilkAssy

;####################################################################  
;# createTextBlock - Create text record and write information to    #
;#                   the database.                                  #
;####################################################################  

;VAN HUYNH modified here (add _RD)

(defun createTextBlock_RD ()

  textBlock = txt_tblk
  rotation  = txt_rot
  parent_id = text_id->parent        ; symbol association
  mirrored  = text_id->isMirrored    ; t/nil
  justify   = "CENTER"               ; "LEFT" "CENTER" "RIGHT"

  ; ==== setup defstruct for text insertion ====

  textOrientation = make_axlTextOrientation( ?textBlock textBlock,
                                             ?rotation  rotation,
                                             ?mirrored  mirrored,
                                             ?justify   justify
                                           )
  ; ==== Add new text and delete the existing text if successful ===
 
  update = axlDBCreateText( text, xy, textOrientation,
                            text_id->layer, parent_id
                          )
  if( update != nil then
    ; axlMsgPut("Update Succeeded.")
      axlDeleteObject( list( text_id ))
  else
      axlMsgPut( strcat( "Update Failed on " text ) )
    ); endif

); end defun - createTextBlock_RD

;VAN HUYNH modified here (add _RD)

 ;###########################################################
 ;# Form CallBacks - Determine Allegro command to execute   #
 ;#                  based on the user's selection          #
 ;###########################################################
 
(defun rotateRDForm_Action (Form)
 
   (case Form->curField
     ("SilkTop"
        SilkTop=Form->curValue
     ); end "SilkTop"

     ("SilkBot"
        SilkBot=Form->curValue
     ); end "SilkBot"

     ("AssyTop"
        AssyTop=Form->curValue
     ); end "AssyTop"

     ("AssyBot"
        AssyBot=Form->curValue
     ); end "AssyBot"

     ("LRBT"
        LRBT=Form->curValue
        orient_0   = t
        orient_90  = t
        orient_180 = nil
        orient_270 = nil
     ); end "LRTB"
 
     ("LRTB"
        LRTB=Form->curValue
        orient_0   = t
        orient_90  = nil
        orient_180 = nil
        orient_270 = t
     ); end "LRTB"

     ("RLBT"
        RLBT=Form->curValue
        orient_0   = nil
        orient_90  = t
        orient_180 = t
        orient_270 = nil
     ); end "LRTB"
 
     ("RLTB"
        RLTB=Form->curValue
        orient_0   = nil
        orient_90  = nil
        orient_180 = t
        orient_270 = t
     ); end "LRTB"

     ("CenterAssy"
        CenterAssy=Form->curValue
     ); end "CenterAssy"

     ("ChangeTextSize"
        ChangeTextSize=Form->curValue
     ); end "ChangeTextSize"

     ("TextBlockLarge"
        TextBlockLarge=Form->curValue
        sprintf( tblk_large "%L" TextBlockLarge )
     ); end "TextBlockLarge"

     ("TextBlockMedium"
        TextBlockMedium=Form->curValue
        sprintf( tblk_medium "%L" TextBlockMedium )
     ); end "TextBlockMedium"

     ("TextBlockSmall"
        TextBlockSmall=Form->curValue
        sprintf( tblk_small "%L" TextBlockSmall )
     ); end "TextBlockSmall"

     ("done"
        ; ==== cons puts the list together in reverse order
        if( AssyBot == t then Layer_List = cons( assybot_layer Layer_List ) )
        if( AssyTop == t then Layer_List = cons( assytop_layer Layer_List ) )
        if( SilkBot == t then Layer_List = cons( silkbot_layer Layer_List ) )
        if( SilkTop == t then Layer_List = cons( silktop_layer Layer_List ) )
        doRotateSilkAssy()
        (axlFormClose Form)
        (axlCancelEnterFun)
        deleteFile( fFile )
     ); end "done"

     ("cancel"
        (axlFormClose Form)
        (axlCancelEnterFun)
        deleteFile( fFile )
     ); end "cancel"

   ); end case
 
 ); end defun - rotateRDForm_Action

  ;#########################
  ;# End Form CallBacks    #
  ;#########################
 
 
  ;########################################################### 
  ;# createTitleBlockForm - Main User Interface Form         #
  ;#                        Brought up by executing command. #
  ;###########################################################

(defun createRotateRDForm ()  

   fFile = strcat( FormDir "rotateRDForm.form" )
   Form  = outfile( fFile "w")
   fprintf(Form "FILE_TYPE=FORM_DEFN VERSION=2\n")
   fprintf(Form "FORM\n")
   fprintf(Form "FIXED\n")
   fprintf(Form "PORT 77 20\n")
   fprintf(Form "HEADER \"Rotate Silkscreen/Assembly Reference Designators V2.0\"\n")
   fprintf(Form "TILE\n")
 
   fprintf(Form "GROUP \"Select Layers to Rotate\"\n")
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
   
   fprintf(Form "FIELD \"AssyTop\"\n")
   fprintf(Form "FLOC 37 3 \n")
   fprintf(Form "CHECKLIST \"Assembly-Top\" \n")
   fprintf(Form "ENDFIELD\n")

   fprintf(Form "FIELD \"AssyBot\"\n")
   fprintf(Form "FLOC 54 3 \n")
   fprintf(Form "CHECKLIST \"Assembly-Bottom\" \n")
   fprintf(Form "ENDFIELD\n")

   fprintf(Form "GROUP \"Select RD Orienation\"\n")
   fprintf(Form "GLOC 2 5\n")
   fprintf(Form "GSIZE 32 13\n")
   fprintf(Form "ENDGROUP\n")

   fprintf(Form "GROUP \"Ref Des Alterations\"\n")
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

   fprintf(Form "FIELD \"CenterAssy\"\n")
   fprintf(Form "FLOC 37 7 \n")
   fprintf(Form "CHECKLIST \"Center Assembly RDs at Part Origin\" \n")
   fprintf(Form "ENDFIELD\n")

   fprintf(Form "FIELD \"ChangeTextSize\"\n")
   fprintf(Form "FLOC 37 9 \n")
   fprintf(Form "CHECKLIST \"Change Text Block Sizes\" \n")
   fprintf(Form "ENDFIELD\n")

   fprintf(Form "TEXT \"Large  Text Block: \"\n")
   fprintf(Form "TLOC 39 11 \n")
   fprintf(Form "ENDTEXT\n")

   fprintf(Form "TEXT \"Medium Text Block: \"\n")
   fprintf(Form "TLOC 39 13 \n")
   fprintf(Form "ENDTEXT\n")

   fprintf(Form "TEXT \"Small Text Block: \"\n")
   fprintf(Form "TLOC 39 15 \n")
   fprintf(Form "ENDTEXT\n")

   fprintf(Form "FIELD \"TextBlockLarge\"\n")
   fprintf(Form "FLOC 55 11 \n")
   fprintf(Form "INTSLIDEBAR 3 3\n MIN 1\n MAX 60\n FGROUP\"TxtBlock\" \n")
   fprintf(Form "ENDFIELD\n")

   fprintf(Form "FIELD \"TextBlockMedium\"\n")
   fprintf(Form "FLOC 55 13 \n")
   fprintf(Form "INTSLIDEBAR 3 3\n MIN 1\n MAX 60\n FGROUP\"TxtBlock\" \n")
   fprintf(Form "ENDFIELD\n")
  
   fprintf(Form "FIELD \"TextBlockSmall\"\n")
   fprintf(Form "FLOC 55 15 \n")
   fprintf(Form "INTSLIDEBAR 3 3\n MIN 1\n MAX 60\n FGROUP\"TxtBlock\" \n")
   fprintf(Form "ENDFIELD\n")

   fprintf(Form "FIELD done\n")
   fprintf(Form "FLOC 6 18\n")
   fprintf(Form "MENUBUTTON \"Done\" 6 3\n")
   fprintf(Form "ENDFIELD\n")

   fprintf(Form "FIELD cancel\n")
   fprintf(Form "FLOC 16 18\n")
   fprintf(Form "MENUBUTTON \"CANCEL\" 6 3\n")
   fprintf(Form "ENDFIELD\n")

   fprintf(Form "ENDTILE\n")
   fprintf(Form "ENDFORM\n")
   close(Form)
); end defun  - createRotateRDForm