/**
 *
 */
package com.bl.storefront.file.validate;

import org.springframework.ui.Model;
import org.springframework.validation.Errors;


/**
 * This class is used for validating files uploaded
 * 
 * @author Avani Patel
 *
 */
public interface BlValidator
{
	public void validate(final Object object, final Errors errors, final Model model);
}
