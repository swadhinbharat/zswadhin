CLASS zcl_cq_product_via_rfc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS is_key_filter
      IMPORTING it_filter_cond          TYPE if_rap_query_filter=>tt_name_range_pairs
      RETURNING VALUE(rv_is_key_filter) TYPE abap_bool.

    METHODS get_orderby_clause
      IMPORTING it_sort_elements         TYPE if_rap_query_request=>tt_sort_elements
      RETURNING VALUE(rv_orderby_string) TYPE string.

ENDCLASS.



CLASS ZCL_CQ_PRODUCT_VIA_RFC IMPLEMENTATION.


  METHOD if_rap_query_provider~select.



    "variables needed to call BAPI's
    DATA lt_product TYPE STANDARD TABLE OF  zce_product_via_rfc.
    DATA lt_result TYPE STANDARD TABLE OF  zce_product_via_rfc.
    DATA ls_product TYPE zce_product_via_rfc.

    "key for BAPI_GET_DETAIL
    TYPES : BEGIN OF product_rfc_key_type,
              productid TYPE zce_product_via_rfc-productid,
            END OF product_rfc_key_type.
    DATA ls_product_rfc_key TYPE product_rfc_key_type.

    "select options
    DATA lt_filter_ranges_productid TYPE RANGE OF zce_product_via_rfc-productid.
    DATA ls_filter_ranges_productid LIKE LINE OF lt_filter_ranges_productid.
    DATA lt_filter_ranges_supplier  TYPE RANGE OF zce_product_via_rfc-suppliername.
    DATA ls_filter_ranges_supplier  LIKE LINE OF lt_filter_ranges_supplier.
    DATA lt_filter_ranges_category  TYPE RANGE OF zce_product_via_rfc-category.
    DATA ls_filter_ranges_category  LIKE LINE OF lt_filter_ranges_category.

    "######################### ABAP source code ################################
    " ABAP source code for type definition for BAPIRET2
    " generated on: 20190301 at: 165321 in: UIA
    TYPES : BEGIN OF ty_bapiret2,
              type      TYPE c LENGTH 1,
              id        TYPE c LENGTH 20,
              number    TYPE n LENGTH 3,
              message   TYPE c LENGTH 220,
              logno     TYPE c LENGTH 20,
              logmsgno  TYPE n LENGTH 6,
              messagev1 TYPE c LENGTH 50,
              messagev2 TYPE c LENGTH 50,
              messagev3 TYPE c LENGTH 50,
              messagev4 TYPE c LENGTH 50,
              parameter TYPE c LENGTH 32,
              row       TYPE i,
              field     TYPE c LENGTH 30,
              system    TYPE c LENGTH 10,
            END OF ty_bapiret2.

    "DATA lt_return   TYPE STANDARD TABLE OF bapiret2.
    DATA lt_return   TYPE STANDARD TABLE OF ty_bapiret2.
    "variables generic for implementation of custom entity
    DATA lv_details_read TYPE abap_bool.
    "DATA ls_sel_opt TYPE /iwbep/s_cod_select_option.

*   ensure: in case of a single record is requested (e.g. data for a detail page),
*           only one record is returned and SET_TOTAL_NUMBER_OF_RECORDS = 1
    DATA lv_orderby_string TYPE string.
    DATA lv_select_string TYPE string.
    "In the trial version we cannot call RFC function module in backend systems
    DATA(lv_abap_trial) = abap_true.

    IF lv_abap_trial = abap_false.

      TRY.
          DATA(lo_rfc_dest) = cl_rfc_destination_provider=>create_by_cloud_destination(
                                   i_name = |S4H_ON_PREM_RFC|
                                   i_service_instance_name = |OutboundCommunication| ).
          DATA(lv_rfc_dest_name) = lo_rfc_dest->get_destination_name( ).

        CATCH cx_rfc_dest_provider_error INTO DATA(lx_dest).

      ENDTRY.

    ENDIF.

    TRY.

        IF io_request->is_data_requested( ).

          TRY.
              "get and add filter
              DATA(lt_filter_cond) = io_request->get_filter( )->get_as_ranges( ). "  get_filter_conditions( ).

            CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_sel_option).

              "@todo :
              " raise an exception that the filter that has been provided
              " cannot be converted into select options
              " here we just continue

          ENDTRY.

          DATA(lv_top)     = io_request->get_paging( )->get_page_size( ).
          DATA(lv_skip)    = io_request->get_paging( )->get_offset( ).
          DATA(lt_fields)  = io_request->get_requested_elements( ).
          DATA(lt_sort)    = io_request->get_sort_elements( ).



          " $orderby was called
          IF lt_sort IS NOT INITIAL.
            CLEAR lv_orderby_string.
            LOOP AT lt_sort INTO DATA(ls_sort).
              IF ls_sort-descending = abap_true.
                CONCATENATE lv_orderby_string ls_sort-element_name 'DESCENDING' INTO lv_orderby_string SEPARATED BY space.
              ELSE.
                CONCATENATE lv_orderby_string ls_sort-element_name 'ASCENDING' INTO lv_orderby_string SEPARATED BY space.
              ENDIF.
            ENDLOOP.
          ELSE.
            " lv_orderby_string must not be empty.
            lv_orderby_string = 'PRODUCTID'.
          ENDIF.

          " $select handling
          IF lt_fields IS NOT INITIAL.
            CONCATENATE LINES OF lt_fields INTO lv_select_string  SEPARATED BY ','.
          ELSE.
            "check coding. If no columns are specified via $select retrieve all columns from the model instead?
            lv_select_string = '*'.
          ENDIF.

          "check if filter condition is for a single read
          lv_details_read = is_key_filter( lt_filter_cond ).

          "single read
          IF lv_details_read = abap_true.

            READ TABLE lt_filter_cond WITH KEY name = 'PRODUCTID' INTO DATA(ls_productid_filter_key).
            IF sy-subrc = 0 AND lines( ls_productid_filter_key-range ) = 1.
              READ TABLE ls_productid_filter_key-range INTO DATA(ls_id_option) INDEX 1.
              IF sy-subrc = 0 AND ls_id_option-sign = 'I' AND ls_id_option-option = 'EQ' AND ls_id_option-low IS NOT INITIAL.
                "read details for single record in list
                ls_product_rfc_key-productid = ls_id_option-low.

                IF lv_abap_trial = abap_true.

                  "fill structure with test data
                  ls_product = VALUE #( productid = ls_product_rfc_key-productid name = 'Notebook' ).

                ELSE.

                  CALL FUNCTION 'BAPI_EPM_PRODUCT_GET_DETAIL'
                    DESTINATION lv_rfc_dest_name
                    EXPORTING
                      product_id = ls_product_rfc_key
                    IMPORTING
                      headerdata = ls_product
                    TABLES
                      return     = lt_return.


                ENDIF.

                APPEND ls_product TO lt_product.

              ENDIF.
            ENDIF.

            "the request is a GET_LIST request and a filter has been provided
          ELSE .

            "-get filter for ProductID
            READ TABLE lt_filter_cond WITH KEY name = 'PRODUCTID' INTO DATA(ls_productid_cond).
            IF sy-subrc EQ 0.
              LOOP AT ls_productid_cond-range INTO DATA(ls_sel_opt_productid).
                MOVE-CORRESPONDING ls_sel_opt_productid TO ls_filter_ranges_productid.
                INSERT ls_filter_ranges_productid INTO TABLE lt_filter_ranges_productid.
              ENDLOOP.
            ENDIF.

            "-get filter for SUPPLIERNAME
            READ TABLE  lt_filter_cond WITH  KEY name = 'SUPPLIERNAME' INTO DATA(ls_suppliername_cond).
            IF sy-subrc EQ 0.
              LOOP AT ls_suppliername_cond-range INTO DATA(ls_sel_opt_suppliername).
                MOVE-CORRESPONDING ls_sel_opt_suppliername TO ls_filter_ranges_supplier.
                INSERT ls_filter_ranges_supplier INTO TABLE lt_filter_ranges_supplier.
              ENDLOOP.
            ENDIF.

            "-get filter for CATEGORY
            READ TABLE  lt_filter_cond WITH  KEY name = 'CATEGORY' INTO DATA(ls_category_cond).
            IF sy-subrc EQ 0.
              LOOP AT ls_category_cond-range INTO DATA(ls_sel_opt_category).
                MOVE-CORRESPONDING ls_sel_opt_category TO ls_filter_ranges_category.
                INSERT ls_filter_ranges_category INTO TABLE lt_filter_ranges_category.
              ENDLOOP.
            ENDIF.

            IF lv_abap_trial = abap_true.

              "fill table with demo data
              lt_product = VALUE #( ( productid = 'HT-1000' name = 'Notebook' )
                                    ( productid = 'HT-1001' name = 'Aotebook' )
                                    ( productid = 'HT-1002' name = 'Notebook' )
                                    ( productid = 'HT-1003' name = 'Notebook' )
                                    ( productid = 'HT-1004' name = 'Notebook' )
                                    ( productid = 'HT-1005' name = 'Notebook' )
                              ).
            ELSE.

              CALL FUNCTION 'BAPI_EPM_PRODUCT_GET_LIST'
                DESTINATION lv_rfc_dest_name
*          EXPORTING
*            max_rows              =
                TABLES
                  headerdata            = lt_product
                  selparamproductid     = lt_filter_ranges_productid
                  selparamsuppliernames = lt_filter_ranges_supplier
                  selparamcategories    = lt_filter_ranges_category
                  return                = lt_return.

            ENDIF.



          ENDIF.

          "Apply all query options to filter so that also filter options are supported that
          "are not available as filter parameters for the RFC function modules being used
          "Also ensure that not more elements are returned than have been
          "requested by the framework

          IF lv_details_read = abap_false.

            DATA(dyn_clause) =  io_request->get_filter( )->get_as_sql_string( ).

            SELECT (lv_select_string) FROM @lt_product AS products
            WHERE (dyn_clause)
            ORDER BY (lv_orderby_string)
            INTO CORRESPONDING FIELDS OF TABLE @lt_result
            UP TO @lv_top ROWS
            OFFSET @lv_skip .

            io_response->set_total_number_of_records( lines( lt_result ) ).
            io_response->set_data( lt_result ).

          ELSE.

            io_response->set_total_number_of_records( lines( lt_product ) ).
            io_response->set_data( lt_product ).

          ENDIF.




        ELSE.
          "no data has been requested
        ENDIF.

        "error handling
      CATCH cx_rap_query_provider INTO DATA(lx_exc).


    ENDTRY.

  ENDMETHOD.


  METHOD is_key_filter.

    "check if the request is a single read
    READ TABLE it_filter_cond WITH KEY name = 'PRODUCTID' INTO DATA(ls_productid_filter_key).
    IF sy-subrc = 0 AND lines( ls_productid_filter_key-range ) = 1.
      READ TABLE ls_productid_filter_key-range INTO DATA(ls_id_option) INDEX 1.
      IF sy-subrc = 0 AND ls_id_option-sign = 'I' AND ls_id_option-option = 'EQ' AND ls_id_option-low IS NOT INITIAL.
        "read details for single record in list
        rv_is_key_filter = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_orderby_clause.

  ENDMETHOD.
ENDCLASS.
