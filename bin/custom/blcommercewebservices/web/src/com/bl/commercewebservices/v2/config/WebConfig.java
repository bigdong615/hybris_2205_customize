/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.commercewebservices.v2.config;


import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.webservicescommons.swagger.services.ApiVendorExtensionService;
import com.bl.commercewebservices.constants.YcommercewebservicesConstants;
import com.bl.commercewebservices.request.mapping.handler.CommerceHandlerMapping;

import javax.annotation.Resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.format.support.FormattingConversionService;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.web.accept.ContentNegotiationManager;
import org.springframework.web.servlet.HandlerExceptionResolver;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurationSupport;
import org.springframework.web.servlet.mvc.annotation.ResponseStatusExceptionResolver;
import org.springframework.web.servlet.mvc.method.annotation.ExceptionHandlerExceptionResolver;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;
import org.springframework.web.servlet.mvc.support.DefaultHandlerExceptionResolver;
import org.springframework.web.servlet.resource.ResourceUrlProvider;

import com.google.common.collect.ImmutableSet;

import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.AuthorizationScope;
import springfox.documentation.service.ClientCredentialsGrant;
import springfox.documentation.service.OAuth;
import springfox.documentation.service.ResourceOwnerPasswordCredentialsGrant;
import springfox.documentation.service.SecurityReference;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spi.service.contexts.SecurityContext;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;


/**
 * Spring configuration which replace <mvc:annotation-driven> tag. It allows override default
 * RequestMappingHandlerMapping with our own mapping handler
 */
@EnableSwagger2
@Configuration
@ImportResource({ "WEB-INF/config/v2/springmvc-v2-servlet.xml" })
public class WebConfig extends WebMvcConfigurationSupport
{
	private static final String PASSWORD_AUTHORIZATION_SCOPE = "blcommercewebservices.oauth2.password.scope";
	private static final String CLIENT_CREDENTIAL_AUTHORIZATION_SCOPE = "blcommercewebservices.oauth2.clientCredentials.scope";
	private static final String AUTHORIZATION_URL = "blcommercewebservices.oauth2.tokenUrl";

	private static final String DESC = "blcommercewebservices.v2.description";
	private static final String TITLE = "blcommercewebservices.v2.title";
	private static final String VERSION = "blcommercewebservices.v2.version";
	private static final String LICENSE = "blcommercewebservices.v2.license";
	private static final String LICENSE_URL = "blcommercewebservices.v2.license.url";

	private static final String PASSWORD_AUTHORIZATION_NAME = "oauth2_Password";
	private static final String CLIENT_CREDENTIAL_AUTHORIZATION_NAME = "oauth2_client_credentials";

	@Resource(name = "configurationService")
	private ConfigurationService configurationService;

	@Resource(name = "apiVendorExtensionService")
	private ApiVendorExtensionService apiVendorExtensionService;

	@Resource(name = "messageConvertersV2")
	private List<HttpMessageConverter<?>> messageConvertersV2;

	@Resource
	private List<HandlerExceptionResolver> exceptionResolversV2;

	private ApplicationContext applicationContext;

	@SuppressWarnings({ "deprecation", "squid:CallToDeprecatedMethod" })
	@Override
	@Bean
	public RequestMappingHandlerMapping requestMappingHandlerMapping(final ContentNegotiationManager mvcContentNegotiationManager,
			final FormattingConversionService mvcConversionService, final ResourceUrlProvider mvcResourceUrlProvider)
	{
		final CommerceHandlerMapping handlerMapping = new CommerceHandlerMapping("v2");
		handlerMapping.setOrder(0);
		handlerMapping.setDetectHandlerMethodsInAncestorContexts(true);
		handlerMapping.setInterceptors(getInterceptors(mvcConversionService, mvcResourceUrlProvider));
		handlerMapping.setContentNegotiationManager(mvcContentNegotiationManager);
		/*
		 * For more details about deprecation see: https://github.com/spring-projects/spring-framework/issues/24179
		 */
		handlerMapping.setUseRegisteredSuffixPatternMatch(true);
		return handlerMapping;
	}

	@Override
	protected void configureMessageConverters(final List<HttpMessageConverter<?>> converters)
	{
		converters.addAll(messageConvertersV2);
	}

	@Override
	protected void configureHandlerExceptionResolvers(final List<HandlerExceptionResolver> exceptionResolvers)
	{
		final ExceptionHandlerExceptionResolver exceptionHandlerExceptionResolver = new ExceptionHandlerExceptionResolver();
		exceptionHandlerExceptionResolver.setApplicationContext(applicationContext);
		exceptionHandlerExceptionResolver.setContentNegotiationManager(mvcContentNegotiationManager());
		exceptionHandlerExceptionResolver.setMessageConverters(getMessageConverters());
		exceptionHandlerExceptionResolver.afterPropertiesSet();

		exceptionResolvers.add(exceptionHandlerExceptionResolver);
		exceptionResolvers.addAll(exceptionResolversV2);
		exceptionResolvers.add(new ResponseStatusExceptionResolver());
		exceptionResolvers.add(new DefaultHandlerExceptionResolver());
	}

	@Override
	public void setApplicationContext(final ApplicationContext applicationContext) throws BeansException //NOSONAR
	{
		super.setApplicationContext(applicationContext);
		this.applicationContext = applicationContext;
	}

	@Override
	public void configureContentNegotiation(final ContentNegotiationConfigurer configurer)
	{
		configurer.favorPathExtension(false).favorParameter(true);
	}

	@Bean
	public Docket apiDocumentation()
	{
		return new Docket(DocumentationType.SWAGGER_2).apiInfo(apiInfo()).select().paths(PathSelectors.any()).build()
				.produces(ImmutableSet.of("application/json", "application/xml"))
				.securitySchemes(Arrays.asList(clientCredentialFlow(), passwordFlow()))
				.securityContexts(Arrays.asList(securityContext())) //
				.extensions(apiVendorExtensionService.getAllVendorExtensions(YcommercewebservicesConstants.EXTENSIONNAME));
	}

	private ApiInfo apiInfo()
	{
		return new ApiInfoBuilder().title(getTitle()).description(getDescription()).license(getLicense())
				.licenseUrl(getLicenseUrl()).version(getVersion()).build();
	}

	protected OAuth passwordFlow()
	{
		final ResourceOwnerPasswordCredentialsGrant resourceOwnerPasswordCredentialsGrant = new ResourceOwnerPasswordCredentialsGrant(
				getAuthorizationUrl());
		return new OAuth(PASSWORD_AUTHORIZATION_NAME, getAuthorizationScopes(PASSWORD_AUTHORIZATION_SCOPE),
				Arrays.asList(resourceOwnerPasswordCredentialsGrant));
	}

	protected OAuth clientCredentialFlow()
	{
		final ClientCredentialsGrant clientCredentialsGrant = new ClientCredentialsGrant(getAuthorizationUrl());
		return new OAuth(CLIENT_CREDENTIAL_AUTHORIZATION_NAME, getAuthorizationScopes(CLIENT_CREDENTIAL_AUTHORIZATION_SCOPE),
				Arrays.asList(clientCredentialsGrant));
	}

	private SecurityContext securityContext()
	{
		return SecurityContext.builder().securityReferences(defaultAuth()).forPaths(PathSelectors.any()).build();
	}

	private List<SecurityReference> defaultAuth()
	{
		final AuthorizationScope[] authorizationScopes = {};
		return Arrays.asList(new SecurityReference(PASSWORD_AUTHORIZATION_NAME, authorizationScopes),
				new SecurityReference(CLIENT_CREDENTIAL_AUTHORIZATION_NAME, authorizationScopes));
	}

	private List<AuthorizationScope> getAuthorizationScopes(final String propertyName)
	{
		final List<AuthorizationScope> authorizationScopes = new ArrayList<>();

		final String strScopes = configurationService.getConfiguration().getString(propertyName);
		if (StringUtils.isNotEmpty(strScopes))
		{
			final String[] scopes = strScopes.split(",");
			for (final String scope : scopes)
			{
				authorizationScopes.add(new AuthorizationScope(scope, StringUtils.EMPTY));
			}

		}
		return authorizationScopes;
	}

	private String getAuthorizationUrl()
	{
		return configurationService.getConfiguration().getString(AUTHORIZATION_URL);
	}

	private String getVersion()
	{
		return configurationService.getConfiguration().getString(VERSION);
	}

	private String getTitle()
	{
		return configurationService.getConfiguration().getString(TITLE);
	}

	private String getDescription()
	{
		return configurationService.getConfiguration().getString(DESC);
	}

	private String getLicense()
	{
		return configurationService.getConfiguration().getString(LICENSE);
	}

	private String getLicenseUrl()
	{
		return configurationService.getConfiguration().getString(LICENSE_URL);
	}

}
