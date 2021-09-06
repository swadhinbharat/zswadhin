CLASS zsw_cl_rap_eml_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zsw_cl_rap_eml_test IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    MODIFY ENTITIES OF zi_fe_travel_000077
      ENTITY Travel
        CREATE
           FIELDS ( agencyid customerid begindate enddate description  ) WITH
                       VALUE #( ( %cid       = 'CID_100'    " Preliminary ID for new travel instance
                                  %data-agencyid   = '70012'
                                  %data-customerid = '12'
                                  %data-begindate  = cl_abap_context_info=>get_system_date( )
                                  %data-enddate    = cl_abap_context_info=>get_system_date( )
                                  %data-description = 'Testing from EML'
                              ) )

          " Update data of travel instance
*           UPDATE FIELDS ( agencyid description  ) WITH
*                       VALUE #( ( %cid_ref    = 'CID_100'    " Refers to travel instance
*                                  %data-agencyid    = '70012'
*                                  %data-description = 'Changed Agency and Status!' ) )

*            SET FIELDS WITH VALUE #( ( %is_draft          = lv_is_draft
*                                       %cid               = mycid_travel
**                                       %data-TravelUUID   = _get_uuid( )
**                                       %data-travelid     = '2939'
*                                       %data-agencyid     = '70012'
*                                       %data-customerid   = '12'
*                                       %data-begindate    = cl_abap_context_info=>get_system_date( )
*                                       %data-enddate      = cl_abap_context_info=>get_system_date( )
*                                       %data-bookingfee   = '1024'
*                                       %data-currencycode = 'EUR'
*                                       %data-Description  = 'Generated by Action Draft Template'
*                                   ) )

      MAPPED   DATA(ls_mapped)
      FAILED   DATA(failed)
      REPORTED DATA(reported).

    out->write( ls_mapped-travel[ 1 ]-TravelUUID ).


    READ ENTITIES OF zi_fe_travel_000077
          ENTITY Travel
          ALL FIELDS
        WITH CORRESPONDING #( ls_mapped-travel )
        RESULT DATA(lt_awarding_scenario_copied).

out->write(
    EXPORTING
      data = lt_awarding_scenario_copied
      name = 'Output Title'
  ).

*    CL_DEMO_OUTPUT=>display( lt_awarding_scenario_copied ).

*    out->write('Hello world!').

  ENDMETHOD.

ENDCLASS.