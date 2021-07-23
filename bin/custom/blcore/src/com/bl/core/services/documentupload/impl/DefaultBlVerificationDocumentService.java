package com.bl.core.services.documentupload.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.VerificationDocumentMediaModel;
import com.bl.core.services.documentupload.BlVerificationDocumentService;
import de.hybris.platform.core.model.user.CustomerModel;
import org.springframework.web.multipart.MultipartFile;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import javax.annotation.Resource;

/**
 * Service implementation class is used to save uploaded document.
 * @author Neeraj Singh
 */
public class DefaultBlVerificationDocumentService implements BlVerificationDocumentService {

  private UserService userService;
  private ModelService modelService;
  @Resource
  private MediaService mediaService;

  @Override
  public void uploadVerificationDocument(final CustomerModel customerModel, final VerificationDocumentMediaModel verificationDocumentMediaModel, final MultipartFile document) {

    try {
      verificationDocumentMediaModel.setCode(customerModel.getName() + BlCoreConstants.HYPHEN + document.getOriginalFilename());
      getModelService().save(verificationDocumentMediaModel);
      getMediaService().setStreamForMedia(verificationDocumentMediaModel, document.getInputStream(), document.getOriginalFilename(), document.getContentType());
      getModelService().refresh(verificationDocumentMediaModel);
      persistVerificationDocument(customerModel, verificationDocumentMediaModel);
      getModelService().save(customerModel);
      getModelService().refresh(customerModel);
    }catch (Exception exception){
      //need to add logger here.
    }
  }

  /**
   *
   * @param customerModel
   * @param verificationDocumentMediaModel
   */
  private void persistVerificationDocument(final CustomerModel customerModel,final VerificationDocumentMediaModel verificationDocumentMediaModel) {
    switch(verificationDocumentMediaModel.getDocumentType())
    {
      case DRIVING_LICENSE:
        customerModel.setDrivingLicense(verificationDocumentMediaModel);
        break;
      case UTILITY_BILL:
        customerModel.setUtilityBill(verificationDocumentMediaModel);
        break;
      case INSURANCE_CERTIFICATE:
        customerModel.setInsuranceCertificate(verificationDocumentMediaModel);
        break;
      case EXTRA_DOCUMENT1:
        customerModel.setExtraDocument1(verificationDocumentMediaModel);
        break;
      case EXTRA_DOCUMENT2:
        customerModel.setExtraDocument2(verificationDocumentMediaModel);
        break;
    }
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public MediaService getMediaService() {
    return mediaService;
  }

  public void setMediaService(MediaService mediaService) {
    this.mediaService = mediaService;
  }
}
