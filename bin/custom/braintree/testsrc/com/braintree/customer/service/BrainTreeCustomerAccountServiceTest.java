/**
 *
 */
package com.braintree.customer.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.customer.dao.BrainTreeCustomerAccountDao;
import com.braintree.customer.service.BrainTreeCustomerAccountService;
import com.braintree.model.BrainTreePaymentInfoModel;


@UnitTest
public class BrainTreeCustomerAccountServiceTest
{
	private static final String MERCHANT_ACCOUNT_ID = "ACCOUNT";

	private static final String CODE = "123456";

	@Mock
	private BrainTreeConfigService brainTreeConfigService;

	@Mock
	private ModelService modelService;

	@Mock
	private BrainTreeCustomerAccountDao brainTreeCustomerAccountDao;

	@InjectMocks
	private final BrainTreeCustomerAccountService brainTreeCustomerAccountService = new BrainTreeCustomerAccountService();

	@Before
	public void init()
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldGetBrainTreePaymentInfoForCode()
	{
		final CustomerModel customerModel = mock(CustomerModel.class);
		final BrainTreePaymentInfoModel brainTreePaymentInfoModelExpected = mock(BrainTreePaymentInfoModel.class);

		when(brainTreeCustomerAccountDao.findBrainTreePaymentInfoByCustomer(customerModel, CODE))
				.thenReturn(brainTreePaymentInfoModelExpected);

		final BrainTreePaymentInfoModel brainTreePaymentInfoModelActual = brainTreeCustomerAccountService
				.getBrainTreePaymentInfoForCode(customerModel, CODE);

		assertEquals(brainTreePaymentInfoModelExpected, brainTreePaymentInfoModelActual);
	}

	@Test
	public void shouldGetBrainTreePaymentInfos()
	{
		final CustomerModel customerModel = mock(CustomerModel.class);
		final List<BrainTreePaymentInfoModel> brainTreePaymentInfoModelListExpected = new ArrayList<BrainTreePaymentInfoModel>();
		final boolean saved = true;

		when(brainTreeCustomerAccountDao.findBrainTreePaymentInfosByCustomer(customerModel, saved, MERCHANT_ACCOUNT_ID))
				.thenReturn(brainTreePaymentInfoModelListExpected);
		when(brainTreeConfigService.getMerchantAccountIdForCurrentSiteAndCurrency()).thenReturn(MERCHANT_ACCOUNT_ID);

		final List<BrainTreePaymentInfoModel> brainTreePaymentInfoModelListActual = brainTreeCustomerAccountService
				.getBrainTreePaymentInfos(customerModel, saved);

		assertEquals(brainTreePaymentInfoModelListExpected, brainTreePaymentInfoModelListActual);
	}

	@Test
	public void shouldUnlinkCCPaymentInfo()
	{
		final CustomerModel customerModel = mock(CustomerModel.class);
		final BrainTreePaymentInfoModel brainTreePaymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		final Collection<PaymentInfoModel> paymentInfoList = new ArrayList<>();
		paymentInfoList.add(brainTreePaymentInfoModel);
		final Collection<PaymentInfoModel> paymentInfoListWithoutBTPaymentModel = new ArrayList<>(paymentInfoList);
		paymentInfoListWithoutBTPaymentModel.remove(brainTreePaymentInfoModel);

		when(customerModel.getPaymentInfos()).thenReturn(paymentInfoList);

		brainTreeCustomerAccountService.unlinkCCPaymentInfo(customerModel, brainTreePaymentInfoModel);

		final InOrder inOrder = inOrder(customerModel, modelService);
		inOrder.verify(customerModel).setPaymentInfos(paymentInfoListWithoutBTPaymentModel);
		inOrder.verify(modelService).save(customerModel);
		inOrder.verify(modelService).refresh(customerModel);
	}

	@Test(expected = IllegalArgumentException.class)
	public void shouldThrowExceptionWhenNoPaymentInfo()
	{
		final CustomerModel customerModel = mock(CustomerModel.class);
		final BrainTreePaymentInfoModel brainTreePaymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		final Collection<PaymentInfoModel> paymentInfoList = new ArrayList<>();

		when(customerModel.getPaymentInfos()).thenReturn(paymentInfoList);

		brainTreeCustomerAccountService.unlinkCCPaymentInfo(customerModel, brainTreePaymentInfoModel);
	}
}
