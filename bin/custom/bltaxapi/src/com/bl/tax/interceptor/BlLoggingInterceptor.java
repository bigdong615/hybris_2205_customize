package com.bl.tax.interceptor;

import com.bl.logging.BlLogger;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.http.HttpRequest;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;

/**
 * This interceptor class created for logging request and response for avalara tax
 * @author Manikandan
 */
public class BlLoggingInterceptor implements ClientHttpRequestInterceptor {

  private static final Logger LOG = Logger.getLogger(BlLoggingInterceptor.class);

  /**
   * {@inheritDoc}
   * this method created for logging logging request and response
   * @param request  request
   * @param body  request body
   * @param execution ClientHttpRequestExecution
   * @return ClientHttpResponse
   * @throws IOException exception for I/O
   */
  @Override
  public ClientHttpResponse intercept(final HttpRequest request, final byte[] body, final ClientHttpRequestExecution execution)
      throws IOException
  {
    traceRequest(request, body);
    final ClientHttpResponse response = execution.execute(request, body);
    traceResponse(response);
    return response;
  }

  /**
   * this method logs the avalara request
   */
  private void traceRequest(final HttpRequest request, final byte[] body) throws IOException
  {
    BlLogger.logMessage(LOG, Level.INFO,"===========================Tax Request Begin================================================");
    BlLogger.logMessage(LOG,Level.INFO,"URI : {}" + request.getURI());
    BlLogger.logMessage(LOG,Level.INFO,"Method : {}" + request.getMethod());
    BlLogger.logMessage(LOG,Level.INFO,"URI : {}"+request.getURI());
    BlLogger.logMessage(LOG,Level.INFO,"Request Body : {}",new String(body, "UTF-8"));
    BlLogger.logMessage(LOG, Level.INFO,"==========================Tax Request End================================================");
  }


  /**
   * this method logs the avalara response
   */
  private void traceResponse(final ClientHttpResponse response) throws IOException
  {
    final StringBuilder inputStringBuilder = new StringBuilder();
    final BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(response.getBody(), "UTF-8"));

    try {
      String line = bufferedReader.readLine();
      while (line != null)
      {
        inputStringBuilder.append(line);
        inputStringBuilder.append('\n');
        line = bufferedReader.readLine();
      }

      BlLogger.logMessage(LOG, Level.INFO,"===========================Tax Response Begin==========================================");
      BlLogger.logMessage(LOG, Level.INFO,"Status code: {}" + response.getStatusCode());
      BlLogger.logMessage(LOG, Level.INFO,"Status text: {}", response.getStatusText());
      BlLogger.logMessage(LOG, Level.INFO,"Response Body : {}", inputStringBuilder.toString());
      BlLogger.logMessage(LOG, Level.INFO,"===========================Tax Response End==========================================");

    } catch (Exception e) {
      BlLogger.logMessage(LOG,Level.ERROR,"Error in traceResponse method" ,e);
    } finally {
      bufferedReader.close();
    }

  }

}
