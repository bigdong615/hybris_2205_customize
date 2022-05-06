/**
 *
 */
package com.bl.storefront.file.validate;

import org.springframework.ui.Model;
import org.springframework.validation.Errors;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;


/**
 * This class is used for validating files uploaded
 * 
 * @author Avani Patel
 *
 */
public interface BlValidator
{
	/**
	 * Validate document format and size
	 * @param Object the Object
	 * @param Errors the errors
	 * @param Model the model
	 */
	public void validate(final Object object, final Errors errors, final Model model,final RedirectAttributes redirectModel);
}
