package com.bl.tax.populators;

import com.bl.logging.BlLogger;
import com.bl.tax.ResponseData;
import com.bl.tax.TaxResponse;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.http.ResponseEntity;

public class BlResponseDetailsAndLoggingPopulator<SERVICERESPONSE>  implements
    Populator<ResponseEntity<SERVICERESPONSE>, ResponseData> {

  private static final Logger LOG = Logger.getLogger(BlResponseDetailsAndLoggingPopulator.class);


  @Override
  public void populate(ResponseEntity<SERVICERESPONSE> source, ResponseData target) throws ConversionException
  {
    target.setStatusCode(String.valueOf(source.getStatusCodeValue()));
    target.setStatusMessage(source.getStatusCode().name());
    target.setResults((TaxResponse) source.getBody());
    logResonseStatus(source);
  }

  private void logResonseStatus(ResponseEntity<SERVICERESPONSE> source) {
    BlLogger.logMessage(LOG , Level.INFO , source.getStatusCode().getReasonPhrase());
  }
}
