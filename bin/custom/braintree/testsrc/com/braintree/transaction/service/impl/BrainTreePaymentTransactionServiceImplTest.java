/**
 *
 */
package com.braintree.transaction.service.impl;

import static com.braintree.constants.BraintreeConstants.BRAINTREE_PROVIDER_NAME;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.model.ModelService;

import javax.annotation.Resource;

import org.apache.log4j.Logger;
import org.junit.Test;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;

import com.braintree.transaction.service.BrainTreePaymentTransactionService;


@IntegrationTest
public class BrainTreePaymentTransactionServiceImplTest extends ServicelayerTest
{
	public static final Logger LOG = Logger.getLogger(BrainTreePaymentTransactionServiceImplTest.class);

	private static final String REQUEST_ID = "REQUEST_ID";

	@Resource
	private BrainTreePaymentTransactionService brainTreePaymentTransactionService;

	@Resource
	private ModelService modelService;

	@Test
	@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)
	public void shouldReturnPaymentTransactionForRequestIdAndPaymentProvider()
	{
		final PaymentTransactionModel paymentTransactionModelExample = modelService.create(PaymentTransactionModel.class);
		paymentTransactionModelExample.setRequestId(REQUEST_ID);
		paymentTransactionModelExample.setPaymentProvider(BRAINTREE_PROVIDER_NAME);
		modelService.save(paymentTransactionModelExample);

		final PaymentTransactionModel paymentTransactionModelExpected = brainTreePaymentTransactionService
				.getTransactionByRequestIdAndPaymentProvider(REQUEST_ID, BRAINTREE_PROVIDER_NAME);

		assertNotNull(paymentTransactionModelExpected);
		assertEquals(paymentTransactionModelExpected.getRequestId(), REQUEST_ID);
		assertEquals(paymentTransactionModelExpected.getPaymentProvider(), BRAINTREE_PROVIDER_NAME);
	}
}
