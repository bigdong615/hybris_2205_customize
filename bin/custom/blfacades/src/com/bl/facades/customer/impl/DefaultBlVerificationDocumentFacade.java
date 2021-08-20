package com.bl.facades.customer.impl;

import com.bl.core.model.VerificationDocumentMediaModel;
import com.bl.core.services.documentupload.BlVerificationDocumentService;
import com.bl.facades.customer.BlVerificationDocumentFacade;
import com.bl.facades.product.data.VerificationDocumentData;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;

/**
 * @author Neeraj Singh
 */
public class DefaultBlVerificationDocumentFacade implements BlVerificationDocumentFacade {

  private BlVerificationDocumentService blVerificationDocumentService;
  private Converter<VerificationDocumentData,VerificationDocumentMediaModel> verificationDocumentConverter;

  private ModelService modelService;
  private UserService userService;

  @Override
  public void uploadDocument(final VerificationDocumentData verificationDocumentData) {

    final CustomerModel customerModel = (CustomerModel) getUserService().getCurrentUser();
    final VerificationDocumentMediaModel verificationDocumentMediaModel = getModelService().create(VerificationDocumentMediaModel.class);
    getVerificationDocumentConverter().convert(verificationDocumentData,verificationDocumentMediaModel);
    getBlVerificationDocumentService().uploadVerificationDocument(customerModel,verificationDocumentMediaModel,verificationDocumentData.getDocument());
  }

  public BlVerificationDocumentService getBlVerificationDocumentService() {
    return blVerificationDocumentService;
  }

  public void setBlVerificationDocumentService(BlVerificationDocumentService blVerificationDocumentService) {
    this.blVerificationDocumentService = blVerificationDocumentService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

  public Converter<VerificationDocumentData, VerificationDocumentMediaModel> getVerificationDocumentConverter() {
    return verificationDocumentConverter;
  }

  public void setVerificationDocumentConverter(Converter<VerificationDocumentData, VerificationDocumentMediaModel> verificationDocumentConverter) {
    this.verificationDocumentConverter = verificationDocumentConverter;
  }
}
