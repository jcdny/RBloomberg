package org.findata.blpwrapper;

import com.bloomberglp.blpapi.*;

import java.util.logging.Logger;

public class ReferenceDataResult extends DataResult {
  private String[] fields;
  private String[] securities;
  private String[] data_types;
  private String[][] result_data;

  public ReferenceDataResult(String[] argSecurities, String[] argFields) {
    securities = argSecurities;
    fields = argFields;

    data_types = new String[fields.length];
    // Because we may get data type info out of order, need to
    // initialize array at start with a default value.
    for (int i = 0; i < fields.length; i++) {
      // Call this "NOT_APPLICABLE" since "NA" causes problems in R.
      data_types[i] = "NOT_APPLICABLE";
    }
    result_data = new String[securities.length][fields.length];
  }

  public String[][] getData() {
    return(result_data);
  }

  public String[] getColumnNames() {
    return(fields);
  }

  public String[] getRowNames() {
    return(securities);
  }

  public String[] getDataTypes() {
    return(data_types);
  }

  public void processResponse(Element response, Logger logger, boolean throwInvalidTickerError) throws WrapperException {
    Element securityDataArray = response.getElement("securityData");
    int numItems = securityDataArray.numValues();

    for (int i = 0; i < numItems; i++) {
      Element securityData = securityDataArray.getValueAsElement(i);
      Element fieldData = securityData.getElement("fieldData");
      int seq = securityData.getElementAsInt32("sequenceNumber");

      processSecurityError(securityData, logger, throwInvalidTickerError);
      processFieldExceptions(securityData, logger);

      int field_data_counter = 0;
      for (int j = 0; j < fields.length; j++) { 
        String field_name = fields[j];

        if (field_data_counter < fieldData.numElements()) {
          logger.finest("i = " + i + "\n" + "seq = " + seq + "\n" + "j = " + j + "\n" + "field_data_counter = " + field_data_counter);
          Element field = fieldData.getElement(field_data_counter);
          if (field.name().toString().equals(field_name)) {
            // Raise an error if we're trying to read SEQUENCE data.
            // Store the data type for later (if it hasn't already been stored).
            if (data_types[j].equals("NOT_APPLICABLE")) {
              if (field.datatype().intValue() == Schema.Datatype.Constants.SEQUENCE) {
                throw new WrapperException("reference data request cannot handle SEQUENCE data in field " + field.name().toString());
              }
              String data_type = field.datatype().toString();
              if (!data_type.equals("NA")) {
                logger.finest("Setting field data type to " + data_type);
                data_types[j] = data_type;
              }
            } else {
              logger.finest("Field data type is " + data_types[j]);
            }

            String value = field.getValueAsString();

            logger.finest("Setting field value to " + value);
            field_data_counter++;

            if (value.equals("-2.4245362661989844E-14")) {
              logger.info("Numeric of -2.4245362661989844E-14 encountered. Not a real value. Will be left NULL.");
            } else {
              result_data[seq][j] = value;
            }
          } else {
            logger.finest("Skipping field.");
          }
        }

      } 
    }
  }
}
