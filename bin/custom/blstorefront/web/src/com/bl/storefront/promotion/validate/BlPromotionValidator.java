package com.bl.storefront.promotion.validate;

import org.springframework.ui.Model;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;


/**
 * This class validates the promotions.
 *
 * @author Ravikumar
 *
 */
public interface BlPromotionValidator
{

	/**
	 * Check invalid promotions.
	 *
	 * @param voucherCode
	 *           the voucher code
	 * @param errorKey
	 *           the error key
	 * @param redirectModel
	 *           the redirect model
	 * @return true, if successful
	 */
	public boolean checkInvalidPromotions(final String voucherCode, final String errorKey, final Model model,
			final RedirectAttributes redirectModel);

	/**
	 * Check invalid promotions for extend order.
	 *
	 * @param voucherCode
	 *           the voucher code
	 * @param errorKey
	 *           the error key
	 * @param model
	 *           the model
	 * @param redirectModel
	 *           the redirect model
	 * @param referer
	 *           the referer
	 * @return true, if successful
	 */
	public boolean checkInvalidPromotionsForExtendOrder(final String voucherCode, final String errorKey, final Model model,
			final RedirectAttributes redirectModel, final String referer);
}
