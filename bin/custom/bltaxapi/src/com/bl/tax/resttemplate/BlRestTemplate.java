package com.bl.tax.resttemplate;

import com.bl.tax.constants.BltaxapiConstants;
import com.bl.tax.interceptor.BlLoggingInterceptor;
import de.hybris.platform.util.Config;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.BufferingClientHttpRequestFactory;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

/**
 * This class created for custom restTemplate add some custom code
 * @author Manikandan
 */
public class BlRestTemplate {
  public <SERVICERESPONSE> ResponseEntity<SERVICERESPONSE> executeRequest(final String urlPath,
      final HttpEntity<?> requestEntity, final Class<SERVICERESPONSE> responseEntity)
      throws RestClientException, URISyntaxException {
    return getRestTemplate().exchange(new URI(urlPath), HttpMethod.POST, requestEntity, responseEntity);
  }

  /**
   * This method created to adding  for adding ClientHttpRequestInterceptor , SimpleClientHttpRequestFactory and Logging interceptors
   * @return
   */
  protected RestTemplate getRestTemplate()
  {
    final RestTemplate restTemplate = new RestTemplate(
        new BufferingClientHttpRequestFactory(new SimpleClientHttpRequestFactory()));
    if (Config.getBoolean(BltaxapiConstants.BL_TAX_REST_CLIENT_LOGGING_ENABLED, true))
    {
      final List<ClientHttpRequestInterceptor> interceptors = new ArrayList<ClientHttpRequestInterceptor>();
      interceptors.add(new BlLoggingInterceptor());
      List<HttpMessageConverter<?>> converters = new ArrayList<HttpMessageConverter<?>>();
      converters.add(new MappingJackson2HttpMessageConverter());
      restTemplate.setMessageConverters(converters);
      restTemplate.getInterceptors().addAll(interceptors);
    }
    return restTemplate;
  }

}
