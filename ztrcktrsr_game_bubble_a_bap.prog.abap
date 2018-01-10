*&---------------------------------------------------------------------*
*& Report ZTRCKTRSR_GAME_BUBBLE_A_BAP
*&---------------------------------------------------------------------*
REPORT ztrcktrsr_game_bubble_a_bap.

*** Spielfeld ***
SELECTION-SCREEN BEGIN OF BLOCK sfeld WITH FRAME TITLE TEXT-fld.
PARAMETERS:
  x_max(2)    TYPE n DEFAULT 11,
  y_max(2)    TYPE n DEFAULT 11,
  p_farben(1) TYPE n DEFAULT 5.
SELECTION-SCREEN END OF BLOCK sfeld.

*** Spielmodus ***
SELECTION-SCREEN BEGIN OF BLOCK modus WITH FRAME TITLE TEXT-mod.
PARAMETERS:
  p_norm RADIOBUTTON GROUP mod DEFAULT 'X',
  p_fort RADIOBUTTON GROUP mod.
SELECTION-SCREEN END OF BLOCK modus.

AT SELECTION-SCREEN.
  IF p_farben > 7.
    p_farben = 7.
    MESSAGE e000(oo) WITH 'Nur 7 Farben möglich!'.
  ELSEIF p_farben < 3.
    p_farben = 3.
    MESSAGE e000(oo) WITH 'Mindestens 3 Farben!'.
  ENDIF.

  IF x_max > 20.
    x_max = 20.
  ELSEIF x_max < 5.
    x_max = 5.
  ENDIF.

  IF y_max > 20.
    y_max = 20.
  ELSEIF y_max < 5.
    y_max = 5.
  ENDIF.

*----------------------------------------------------------------------*
*       CLASS cl_my_grid DEFINITION
*----------------------------------------------------------------------*
CLASS: cl_my_grid DEFINITION INHERITING FROM cl_gui_alv_grid.
  PUBLIC SECTION.
    METHODS my_resize IMPORTING i_resize TYPE integer.
ENDCLASS.                    "cl_my_grid DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_my_grid IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS: cl_my_grid IMPLEMENTATION.
* normalen Constructor aufrufen
  METHOD my_resize.

* Spaltenbreiten�nderung verbieten
    CALL METHOD me->set_resize_cols
      EXPORTING
        enable = i_resize
      EXCEPTIONS
        error  = 1
        OTHERS = 2.

  ENDMETHOD.                    "constructor
ENDCLASS.                    "cl_my_grid IMPLEMENTATION

TYPE-POOLS wrfac.

TYPES: BEGIN OF ty_colour,
         icon(4),
       END OF ty_colour.
TYPES:
  BEGIN OF ty_field,
    x     TYPE i,
    y     TYPE i,
    farbe TYPE i,
    mark  TYPE c,
  END OF ty_field,
  ty_field_t TYPE STANDARD TABLE OF ty_field WITH DEFAULT KEY.

TYPES:
  BEGIN OF ty_field2,
    01           TYPE icon_d,
    02           TYPE icon_d,
    03           TYPE icon_d,
    04           TYPE icon_d,
    05           TYPE icon_d,
    06           TYPE icon_d,
    07           TYPE icon_d,
    08           TYPE icon_d,
    09           TYPE icon_d,
    10           TYPE icon_d,
    11           TYPE icon_d,
    12           TYPE icon_d,
    13           TYPE icon_d,
    14           TYPE icon_d,
    15           TYPE icon_d,
    16           TYPE icon_d,
    17           TYPE icon_d,
    18           TYPE icon_d,
    19           TYPE icon_d,
    20           TYPE icon_d,
    handle_style TYPE lvc_t_styl,
  END   OF ty_field2.

DATA gref_grid       TYPE REF TO cl_my_grid.
DATA gref_cont       TYPE REF TO cl_gui_custom_container.
DATA gt_fcat         TYPE lvc_t_fcat.
DATA gs_fcat         TYPE lvc_s_fcat.
DATA gs_layout       TYPE lvc_s_layo.
DATA gv_style_fname  TYPE lvc_fname VALUE 'HANDLE_STYLE'.
DATA gt_cell         TYPE lvc_t_styl.
DATA gs_cell         TYPE lvc_s_styl.
DATA x               TYPE i.
DATA y               TYPE i.
DATA colour_max      TYPE i.
DATA gt_colour       TYPE STANDARD TABLE OF ty_colour.
DATA gs_colour       TYPE ty_colour.
DATA gv_dummy_icon   TYPE char4.
DATA gv_continue     TYPE c.

DATA punkte          TYPE i.
DATA punkte1         TYPE i.
DATA punktec(10)     TYPE c.
DATA punktec1(10)    TYPE c.
DATA l_line          TYPE i.
DATA l_feld(20)      TYPE c.
DATA l_tabix         TYPE i.
DATA l_anzahl        TYPE i.
DATA l_farbe         TYPE i.
DATA l_spalte(2)     TYPE n.
DATA t_spalte        TYPE STANDARD TABLE OF i.
DATA field           TYPE STANDARD TABLE OF ty_field.
DATA field2          TYPE STANDARD TABLE OF ty_field.
DATA field_undo      TYPE STANDARD TABLE OF ty_field_t.
DATA punkte_undo     TYPE STANDARD TABLE OF i.
DATA gt_field        TYPE STANDARD TABLE OF ty_field2.

FIELD-SYMBOLS <spiel>    TYPE ty_field.
FIELD-SYMBOLS <field>    TYPE table.
FIELD-SYMBOLS <wa_field> TYPE ty_field2.
FIELD-SYMBOLS <cell>     TYPE any.
FIELD-SYMBOLS <style>    TYPE lvc_t_styl.
FIELD-SYMBOLS <name>     TYPE any.
FIELD-SYMBOLS <fcat>     TYPE lvc_s_fcat.
FIELD-SYMBOLS <fs>       TYPE any.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_hotspot       FOR EVENT hotspot_click OF cl_my_grid
        IMPORTING e_row_id
                    e_column_id
                    es_row_no.


ENDCLASS.                    "lcl_event_receiver DEFINITION



*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_hotspot.
    READ TABLE gt_fcat TRANSPORTING NO FIELDS
          WITH KEY fieldname = e_column_id-fieldname.
    PERFORM click USING sy-tabix e_row_id-index.
  ENDMETHOD.                    "handle_click_cell

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

DATA gv_event_receiver TYPE REF TO lcl_event_receiver.

START-OF-SELECTION.

  PERFORM init.
  CLEAR gv_continue.
  DO 100 TIMES.
    PERFORM new.
    PERFORM neu_von_field.
    PERFORM set_hotspots_field.
    IF gv_continue <> space.
      CLEAR gv_continue.
      EXIT.
    ELSE.
      MESSAGE s000(oo) WITH sy-index ' Versuch'.
    ENDIF.
  ENDDO.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  IF punkte = 0.
    SET PF-STATUS '0100' EXCLUDING 'UNDO'.
  ELSE.
    SET PF-STATUS '0100'.
  ENDIF.

  IF gref_grid IS INITIAL.
*** Create Container
    CREATE OBJECT gref_cont
      EXPORTING
        container_name = 'CC_FIELDS'
      EXCEPTIONS
        OTHERS         = 6.
*** Create Grid
    CREATE OBJECT gref_grid
      EXPORTING
        i_parent      = gref_cont
        i_appl_events = 'X'
      EXCEPTIONS
        OTHERS        = 5.

    CALL METHOD gref_grid->my_resize
      EXPORTING
        i_resize = '0'.

*** Create Event Handler
    CREATE OBJECT gv_event_receiver.
*** Register Events
    SET HANDLER gv_event_receiver->handle_hotspot   FOR gref_grid.

*** Call grid
    CALL METHOD gref_grid->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
      CHANGING
        it_outtab       = gt_field
        it_fieldcatalog = gt_fcat
      EXCEPTIONS
        OTHERS          = 4.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'UNDO'.
      IF punkte > 0.
        READ TABLE field_undo INTO field[] INDEX 1.
        DELETE field_undo INDEX 1.
        READ TABLE punkte_undo INDEX 1 INTO punkte1.
        DELETE punkte_undo INDEX 1.
        SUBTRACT punkte1 FROM punkte.
        punkte1 = 0.
        PERFORM neu_von_field.
        PERFORM set_hotspots_field.
        PERFORM field_malen.
        PERFORM refresh.
      ENDIF.
    WHEN 'NEW'.
      PERFORM new.
      PERFORM neu_von_field.
      PERFORM set_hotspots_field.
      PERFORM refresh.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  refresh
*&---------------------------------------------------------------------*
FORM refresh.

  gref_grid->refresh_table_display( ).

ENDFORM.                    "refresh

*&---------------------------------------------------------------------*
*&      Form  init
*&---------------------------------------------------------------------*
FORM init .

  DATA lv_index(2) TYPE n.

  DATA l_tabix                      TYPE i.


*** "Farben"
  CLEAR gs_colour.
  CLEAR gt_colour.
  IF lines( gt_colour ) < p_farben.
    APPEND icon_system_okay TO gt_colour.
  ENDIF.
  IF lines( gt_colour ) < p_farben.
    APPEND icon_system_cancel TO gt_colour.
  ENDIF.
  IF lines( gt_colour ) < p_farben.
    APPEND icon_system_end TO gt_colour.
  ENDIF.
  IF lines( gt_colour ) < p_farben.
    APPEND icon_oo_class TO gt_colour.
  ENDIF.
  IF lines( gt_colour ) < p_farben.
    APPEND  icon_generate TO gt_colour.
  ENDIF.
  IF lines( gt_colour ) < p_farben.
    APPEND icon_oo_object TO gt_colour.
  ENDIF.
  IF lines( gt_colour ) < p_farben.
    APPEND icon_oo_class TO gt_colour.
  ENDIF.
  IF lines( gt_colour ) < p_farben.
    APPEND icon_led_green TO gt_colour.
  ENDIF.
  IF lines( gt_colour ) < p_farben.
    APPEND  icon_led_yellow TO gt_colour.
  ENDIF.
  IF lines( gt_colour ) < p_farben.
    APPEND icon_led_red TO gt_colour.
  ENDIF.


*** field
  DO x_max TIMES.
    CLEAR gs_fcat.
    lv_index            = sy-index.
    gs_fcat-col_pos     = sy-index.
    gs_fcat-fieldname   = lv_index.
    gs_fcat-tabname     = 'GS_FIELD'.
    gs_fcat-icon        = 'X'.
    gs_fcat-outputlen   = 2.
    gs_fcat-intlen      = 4.
    gs_fcat-hotspot     = ' '.
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_cell.
    gs_cell-fieldname = gs_fcat-fieldname.
    INSERT gs_cell INTO TABLE gt_cell.

  ENDDO.

  REFRESH gt_field.



  DO y_max TIMES.
    APPEND INITIAL LINE TO gt_field.
    y = sy-index.
    DO x_max TIMES.
      APPEND INITIAL LINE TO field ASSIGNING <spiel>.
      <spiel>-x = sy-index.
      <spiel>-y = y.
    ENDDO.
  ENDDO.

  LOOP AT gt_field ASSIGNING <wa_field>.
*    CLEAR lt_tabcolor.
*    DO x_max TIMES.
*      lv_index = sy-index.
*      ls_tabcolor-fname     = lv_index.
*      ls_tabcolor-color-int = 0.
*      ls_tabcolor-color-inv = 0.
*      ls_tabcolor-nokeycol  = 'X'.
*      APPEND ls_tabcolor TO lt_tabcolor.
*    ENDDO.
*    <wa_field>-tabcolor        = lt_tabcolor.
    <wa_field>-handle_style    = gt_cell.
  ENDLOOP.


  gs_layout-no_headers = 'X'.
  gs_layout-no_toolbar = 'X'.
  gs_layout-no_colexpd = 'X'.
  gs_layout-no_rowmove = 'X'.
  gs_layout-grid_title = 'Bubble-La-Bap'.
  gs_layout-stylefname = 'HANDLE_STYLE'.
  gs_layout-no_hgridln = 'X'.
  gs_layout-no_vgridln = 'X'.
  gs_layout-sel_mode   = 'B'.

ENDFORM.                    " init

*&---------------------------------------------------------------------*
*&      Form  set_hotspot
*&---------------------------------------------------------------------*
FORM set_hotspot USING x TYPE i y TYPE i
              CHANGING set_hotspot TYPE c.

  DATA test_x TYPE i.
  DATA test_y TYPE i.
  DATA test_icon TYPE char4.
*  DATA set_hotspot TYPE c.

  CLEAR set_hotspot.

  READ TABLE gt_field ASSIGNING <wa_field> INDEX y.
  ASSIGN COMPONENT x OF STRUCTURE <wa_field> TO <cell>.

  CHECK <cell> IS NOT INITIAL.
*** test LEFT
  IF x > 1.
    test_x = x - 1.
    test_y = y.
    PERFORM chk USING test_x test_y test_icon.
    IF test_icon = <cell>.
      set_hotspot = 'X'.
    ENDIF.
  ENDIF.

  IF set_hotspot = space.
*** test RIGHT
    IF x < x_max.
      test_x = x + 1.
      test_y = y.
      PERFORM chk USING test_x test_y test_icon.
      IF test_icon = <cell>.
        set_hotspot = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.


  IF set_hotspot = space.
*** test UP
    IF y > 1.
      test_x = x.
      test_y = y - 1.
      PERFORM chk USING test_x test_y test_icon.
      IF test_icon = <cell>.
        set_hotspot = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF set_hotspot = space.
*** test DOWN
    IF y < y_max.
      test_x = x.
      test_y = y + 1.
      PERFORM chk USING test_x test_y test_icon.
      IF test_icon = <cell>.
        set_hotspot = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF set_hotspot <> space.

    ASSIGN COMPONENT gv_style_fname OF STRUCTURE <wa_field> TO <style>.

    READ TABLE <style> ASSIGNING <fs> INDEX x.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs> TO <name>.
      IF sy-subrc = 0.
        ADD cl_gui_alv_grid=>mc_style_hotspot TO <name>.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                    "set_hotspot

*&---------------------------------------------------------------------*
*&      Form  field_malen
*&---------------------------------------------------------------------*
FORM field_malen.

  FIELD-SYMBOLS <lspiel> TYPE ty_field.


  LOOP AT field ASSIGNING <lspiel>.
    CASE <lspiel>-mark.
      WHEN 1.
        PERFORM highlight USING 'X' <lspiel>-x <lspiel>-y.
      WHEN 0.
        PERFORM highlight USING space <lspiel>-x <lspiel>-y.
    ENDCASE.
  ENDLOOP.

  punktec  = punkte.
  punktec1 = punkte1.

  CONCATENATE 'Bubble-La-Bap:#' punktec '#(' punktec1 ')' INTO gs_layout-grid_title.
  CONDENSE gs_layout-grid_title NO-GAPS.
  REPLACE ALL OCCURRENCES OF '#' IN gs_layout-grid_title WITH ' '.

  CALL METHOD gref_grid->set_frontend_layout
    EXPORTING
      is_layout = gs_layout.

  PERFORM set_hotspots_field.

  CALL METHOD gref_grid->refresh_table_display.

  IF gv_continue = space.
    MESSAGE  i000(oo) WITH 'Spiel zuende! Du hast' punkte 'Punkte'.
  ENDIF.

ENDFORM.                    "field_malen


*&---------------------------------------------------------------------*
*&      Form  highlight
*&---------------------------------------------------------------------*
FORM highlight USING highlight TYPE c
                     x TYPE i
                     y TYPE i.

  READ TABLE gt_field ASSIGNING <wa_field> INDEX y.
  ASSIGN COMPONENT x OF STRUCTURE <wa_field> TO <cell>.

  ASSIGN COMPONENT gv_style_fname OF STRUCTURE <wa_field> TO <style>.

  READ TABLE <style> ASSIGNING <fs> INDEX x.
  IF sy-subrc = 0.
    ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs> TO <name>.
    IF sy-subrc = 0.
      IF highlight = 'X'.
        <name> = wrfac_style_color_negative.
      ELSE.
        <name> = 0.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                    "highlight

*&---------------------------------------------------------------------*
*&      Form  set
*&---------------------------------------------------------------------*
FORM set  USING VALUE(icon)
                VALUE(x)
                VALUE(y).


  READ TABLE gt_field ASSIGNING <wa_field> INDEX y.
  ASSIGN COMPONENT x OF STRUCTURE <wa_field> TO <cell>.
  <cell> = icon.

  READ TABLE field ASSIGNING <spiel> WITH KEY x = x y = y.
  READ TABLE gt_colour TRANSPORTING NO FIELDS WITH KEY table_line = icon.
  <spiel>-farbe = sy-tabix.


ENDFORM.                    " set

*&---------------------------------------------------------------------*
*&      Form  neu_von_field
*&---------------------------------------------------------------------*
FORM neu_von_field.

  LOOP AT field ASSIGNING <spiel>.

    READ TABLE gt_field ASSIGNING <wa_field> INDEX <spiel>-y.
    ASSIGN COMPONENT <spiel>-x OF STRUCTURE <wa_field> TO <cell>.
    IF <spiel>-farbe = 0.
      <cell> = space.
    ELSE.
      READ TABLE gt_colour INTO <cell> INDEX <spiel>-farbe.
    ENDIF.

    ASSIGN COMPONENT gv_style_fname OF STRUCTURE <wa_field> TO <style>.

    READ TABLE <style> ASSIGNING <fs> INDEX <spiel>-x.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs> TO <name>.
      IF sy-subrc = 0.
        CLEAR <name>.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "neu_von_field
*&---------------------------------------------------------------------*
*&      Form  set_hotspots_field
*&---------------------------------------------------------------------*
FORM set_hotspots_field.

  DATA lv_x           TYPE i.
  DATA lv_y           TYPE i.
  DATA lv_hotspot_set TYPE c.

  CLEAR gv_continue.
  lv_x = 0.
  DO x_max TIMES.
    ADD 1 TO lv_x.
    lv_y = 0.
    DO y_max TIMES.
      ADD 1 TO lv_y.

* Set hotspot?
      PERFORM set_hotspot USING lv_x  lv_y CHANGING lv_hotspot_set.
      IF lv_hotspot_set <> space.
        gv_continue = 'X'.
      ENDIF.
    ENDDO.
  ENDDO.


ENDFORM.                    "set_hotspots_field

*&---------------------------------------------------------------------*
*&      Form  new
*&---------------------------------------------------------------------*
FORM new .

  CLEAR punkte.
  CLEAR punkte1.
  CLEAR field_undo[].
  CLEAR punkte_undo.

  DATA lv_x    TYPE i.
  DATA lv_y    TYPE i.
  DATA lv_i    TYPE i.
  DATA lv_c(4) TYPE c.

  lv_x = 0.
  DO 20 TIMES.
    ADD 1 TO lv_x.
    lv_y = 0.
    DO 20 TIMES.
      ADD 1 TO lv_y.
      PERFORM zufallsfarbe CHANGING lv_i lv_c.
*** Set point
      PERFORM set USING lv_c lv_x  lv_y.

    ENDDO.
  ENDDO.

ENDFORM.                    " new

*&---------------------------------------------------------------------*
*&      Form  zufallsfarbe
*&---------------------------------------------------------------------*
FORM zufallsfarbe CHANGING col TYPE i
                           icn TYPE char4.

  DATA lv_colour_max TYPE i.

  lv_colour_max = lines( gt_colour ).

*** C: Colour
  CALL FUNCTION 'QF05_RANDOM_INTEGER'
    EXPORTING
      ran_int_max = lv_colour_max
      ran_int_min = 1
    IMPORTING
      ran_int     = col
    EXCEPTIONS
      OTHERS      = 2.

  READ TABLE gt_colour INTO icn INDEX col.

ENDFORM.                    "zufallsfarbe

*&---------------------------------------------------------------------*
*&      Form  chk
*&---------------------------------------------------------------------*
FORM chk  USING    p_x TYPE i
                   p_y TYPE i
          CHANGING p_c.

  FIELD-SYMBOLS <wa> TYPE ty_field2.
  FIELD-SYMBOLS <ic>     TYPE any.

  READ TABLE gt_field ASSIGNING <wa> INDEX p_y.
  ASSIGN COMPONENT p_x OF STRUCTURE <wa> TO <ic>.
  p_c = <ic>.

ENDFORM.                    " chk




*&---------------------------------------------------------------------*
*&      Form  click
*&---------------------------------------------------------------------*
FORM click USING VALUE(fi_x)
                 VALUE(fi_y).

  FIELD-SYMBOLS <lspiel> TYPE ty_field.

  READ TABLE field ASSIGNING <spiel>
               WITH KEY x = fi_x
                        y = fi_y.
  IF sy-subrc = 0.
    l_tabix = sy-tabix.
    l_farbe = <spiel>-farbe.
    CLEAR punkte1.

    CASE <spiel>-mark.
      WHEN 0.
        INSERT field[] INTO field_undo INDEX 1.
**-- Alte Auswahl l�schen
*        LOOP AT field ASSIGNING <lspiel> WHERE mark = 1.
*          <lspiel>-mark = 0.
*        ENDLOOP.

*-- Neue Auswahl
        CLEAR punkte1.
        PERFORM suchen USING fi_x fi_y l_farbe.
        l_anzahl = 0.
        LOOP AT field ASSIGNING <lspiel> WHERE mark = 1.
          ADD 1 TO l_anzahl.
        ENDLOOP.
        IF l_anzahl < 2.
          READ TABLE field ASSIGNING <lspiel> INDEX l_tabix.
          IF sy-subrc = 0.
            <lspiel>-mark = 1.
          ENDIF.
        ELSE.
          punkte1 = l_anzahl * ( l_anzahl - 1 ).
          MESSAGE s000(oo) WITH punkte1 'Punkte'.
        ENDIF.

**** Zweiter Klick: L�schen der markierten felder.
        LOOP AT field ASSIGNING <spiel> WHERE mark = 1.
          <spiel>-farbe = 0.
          <spiel>-mark  = 2.
        ENDLOOP.
        ADD punkte1 TO punkte.
        INSERT punkte1 INTO punkte_undo INDEX 1.
        PERFORM rutschen.
        PERFORM rechts_sammeln.
        PERFORM auffuellen.
        PERFORM neu_von_field.
    ENDCASE.
  ENDIF.

  PERFORM field_malen.

ENDFORM.                    "click

*&--------------------------------------------------------------------*
*&      Form  suchen
*&--------------------------------------------------------------------*
FORM suchen USING f_x f_y f_farbe.

  DATA:
    u_x(2) TYPE n,
    u_y(2) TYPE n.

  u_y = f_y.
  u_x = f_x - 1.
  PERFORM test USING u_x u_y f_farbe 'X'.

  u_x = f_x + 1.
  PERFORM test USING u_x u_y f_farbe 'X'.

  u_x = f_x.
  u_y = f_y - 1.
  PERFORM test USING u_x u_y f_farbe 'X'.

  u_y = f_y + 1.
  PERFORM test USING u_x u_y f_farbe 'X'.


ENDFORM.                                                    "suchen

*&--------------------------------------------------------------------*
*&      Form  test
*&--------------------------------------------------------------------*
FORM test USING VALUE(f_x) VALUE(f_y) f_farbe f_rekursiv.

  FIELD-SYMBOLS <lspiel> TYPE ty_field.

  IF f_x = 0 OR f_x > x_max OR f_y = 0 OR f_y > y_max.
    EXIT.
  ELSE.
    READ TABLE field ASSIGNING <lspiel>
         WITH KEY x = f_x  y = f_y.
    IF sy-subrc      = 0 AND
       <lspiel>-farbe = f_farbe AND
       <lspiel>-mark  = 0.
      <lspiel>-mark = 1.
      IF f_rekursiv = 'X'.
        PERFORM suchen USING f_x f_y f_farbe.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                                                    "test



*&--------------------------------------------------------------------*
*&      Form  rutschen
*&--------------------------------------------------------------------*
FORM rutschen.
  DATA:
    l_y2(2)  TYPE n.

  DO x_max TIMES.
    x = sy-index.
    y = y_max.
    CLEAR t_spalte.
    DO y_max TIMES.
      READ TABLE field ASSIGNING <spiel> WITH KEY x = x y = y.

      IF <spiel>-farbe <> 0.
        APPEND <spiel>-farbe TO t_spalte.
      ENDIF.
      SUBTRACT 1 FROM y.
    ENDDO.

    l_y2 = y_max.
    DO y_max TIMES.
      READ TABLE field ASSIGNING <spiel> WITH KEY x = x y = l_y2.
      l_tabix = sy-tabix.
      READ TABLE t_spalte INTO <spiel>-farbe INDEX sy-index.
      IF sy-subrc = 0.
        <spiel>-mark = 0.
      ELSE.
        <spiel>-farbe = 0.
        <spiel>-mark  = 0.
      ENDIF.
      SUBTRACT 1 FROM l_y2.
    ENDDO.

  ENDDO.

ENDFORM.                                                    "rutschen

*&--------------------------------------------------------------------*
*&      Form  rechts_sammeln
*&--------------------------------------------------------------------*
FORM rechts_sammeln.

  FIELD-SYMBOLS <s> TYPE ty_field.

  DATA:
    l_y2(2)  TYPE n.

*** Rechts beginnen
  x = x_max.
  DO x_max TIMES.
    DO x_max TIMES.
*** Pr�fen, ob die Spalte leer ist
      LOOP AT field ASSIGNING <s> WHERE x = x
                                        AND farbe <> 0.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
*** spalte nicht leer; schleife verlassen
        EXIT.                                               "from do
      ELSE.
** spalte ist leer
        l_spalte = x - 1.
        DO x TIMES.
          PERFORM verschiebe_spalte USING l_spalte.
          SUBTRACT 1 FROM l_spalte.
        ENDDO.
      ENDIF.
    ENDDO.
    SUBTRACT 1 FROM x.
  ENDDO.

ENDFORM.                    "rechts_sammeln


*&--------------------------------------------------------------------*
*&      Form  verschiebe_spalte
*&--------------------------------------------------------------------*
FORM verschiebe_spalte USING f_spalte.

  FIELD-SYMBOLS <s> TYPE ty_field.

  DATA: l_spalte2(2) TYPE n.
  CLEAR t_spalte.

  LOOP AT field ASSIGNING <s> WHERE x = f_spalte.
    APPEND <s>-farbe TO t_spalte.
    <s>-farbe = 0.
    <s>-mark  = 0.
  ENDLOOP.

  l_spalte2 = f_spalte + 1.
  l_tabix = 0.
  LOOP AT field ASSIGNING <s> WHERE x = l_spalte2.
    ADD 1 TO l_tabix.
    READ TABLE t_spalte INTO <s>-farbe INDEX l_tabix.
  ENDLOOP.

ENDFORM.                    "verschiebe_spalte



*---------------------------------------------------------------------*
*       FORM spalte_auffuellen                                        *
*---------------------------------------------------------------------*
FORM spalte_auffuellen USING VALUE(fx).

  DATA:
    l_frb TYPE i,
    l_anz TYPE i,
    l_min TYPE i,
    l_max TYPE i,
    l_rnd TYPE i,
    l_row TYPE i.

  FIELD-SYMBOLS <s> TYPE ty_field.

  l_max = 20.
  l_min = 1.

  CALL FUNCTION 'QF05_RANDOM_INTEGER'
    EXPORTING
      ran_int_max   = l_max
      ran_int_min   = l_min
    IMPORTING
      ran_int       = l_rnd
    EXCEPTIONS
      invalid_input = 1
      OTHERS        = 2.

  l_row = y_max.
  DO l_rnd TIMES.
    READ TABLE field ASSIGNING <s> WITH KEY x = fx y = l_row.
    PERFORM zufallsfarbe CHANGING <s>-farbe gv_dummy_icon.
    SUBTRACT 1 FROM l_row.
  ENDDO.

ENDFORM.                    "spalte_auffuellen

*---------------------------------------------------------------------*
*       FORM auffuellen                                               *
*---------------------------------------------------------------------*
FORM auffuellen.


  FIELD-SYMBOLS <s> TYPE ty_field.

  DATA l_spalte(2) TYPE n.

  IF p_fort = 'X'.

    l_spalte = y_max.
    DO x_max TIMES.
      LOOP AT field ASSIGNING <s> WHERE x = l_spalte AND farbe <> 0.
      ENDLOOP.
      IF sy-subrc > 0.
        PERFORM spalte_auffuellen USING l_spalte.
      ENDIF.
      SUBTRACT 1 FROM l_spalte.
    ENDDO.
  ENDIF.

ENDFORM.                    "auffuellen