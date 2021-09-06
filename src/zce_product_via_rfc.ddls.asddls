@EndUserText.label: 'product demo data read via rfc from on prem'
@ObjectModel: {
    query: {
        implementedBy: 'ABAP:ZCL_CQ_PRODUCT_VIA_RFC'
    }
}

define root custom entity ZCE_Product_via_RFC
{
      @UI.facet     : [
           {
             id     :       'Product',
             purpose:  #STANDARD,
             type   :     #IDENTIFICATION_REFERENCE,
             label  :    'Product',
             position  : 10 }
         ]
         // DDL source code for custom entity for BAPI_EPM_PRODUCT_HEADER
         // generated on: 20190214 at:142338
      @UI           : {
      lineItem      : [{position: 10, importance: #HIGH}],
      identification: [{position: 10}],
      selectionField: [{position: 10}]
      }
  key ProductId     : abap.char( 10 );
      TypeCode      : abap.char( 2 );
      @UI           : {
      lineItem      : [{position: 20, importance: #HIGH}],
      identification: [{position: 20}],
      selectionField: [{position: 20}]
      }
      Category      : abap.char( 40 );
      @UI           : {
      lineItem      : [{position: 30, importance: #HIGH}],
      identification: [{position: 30}]
      }
      Name          : abap.char( 255 );
      @UI           : {
      identification: [{position: 40}]
      }
      Description   : abap.char( 255 );
      SupplierId    : abap.char( 10 );
      SupplierName  : abap.char( 80 );
      TaxTarifCode  : abap.int1;
      @Semantics.unitOfMeasure: true
      MeasureUnit   : abap.unit( 3 );
      @Semantics.quantity.unitOfMeasure: 'WeightUnit'
      WeightMeasure : abap.quan( 13, 3 );
      @Semantics.unitOfMeasure: true
      WeightUnit    : abap.unit( 3 );
      @UI           : {
      lineItem      : [{position: 50, importance: #HIGH}],
      identification: [{position: 50}]
      }
      Price         : abap.dec( 23, 4 );
      @Semantics.currencyCode: true
      CurrencyCode  : abap.cuky( 5 );
      @Semantics.quantity.unitOfMeasure: 'DimUnit'
      Width         : abap.quan( 13, 3 );
      @Semantics.quantity.unitOfMeasure: 'DimUnit'
      Depth         : abap.quan( 13, 3 );
      @Semantics.quantity.unitOfMeasure: 'DimUnit'
      Height        : abap.quan( 13, 3 );
      @Semantics.unitOfMeasure: true
      DimUnit       : abap.unit( 3 );
      ProductPicUrl : abap.char( 255 );
}
