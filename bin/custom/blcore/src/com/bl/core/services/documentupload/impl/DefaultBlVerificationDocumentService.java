package com.bl.core.services.documentupload.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.VerificationDocumentMediaModel;
import com.bl.core.services.document.dao.BlVerificationDocumentDao;
import com.bl.core.services.documentupload.BlVerificationDocumentService;
import de.hybris.platform.core.model.user.CustomerModel;
import java.util.ArrayList;
import java.util.List;
import org.springframework.web.multipart.MultipartFile;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import javax.annotation.Resource;
import com.bl.logging.BlLogger;
import org.apache.log4j.Logger;
import org.apache.log4j.Level;
import java.util.Date;
 /**
 * Service implementation class is used to save uploaded document.
 * @author Neeraj Singh
 */
public class DefaultBlVerificationDocumentService implements BlVerificationDocumentService {
   private static final Logger LOGGER = Logger.getLogger(DefaultBlVerificationDocumentService.class);
  private UserService userService;
  private ModelService modelService;
  private BlVerificationDocumentDao blVerificationDocumentDao;

@Resource
  private MediaService mediaService;

  @Override
  public void uploadVerificationDocument(final CustomerModel customerModel, final VerificationDocumentMediaModel verificationDocumentMediaModel, final MultipartFile document) {

    try {
      verificationDocumentMediaModel.setCode(document.getOriginalFilename() + BlCoreConstants.UNDERSCORE + new Date());
      getModelService().save(verificationDocumentMediaModel);
      verificationDocumentMediaModel.setFolder(getMediaService().getFolder("verificationDocuments"));
      getMediaService().setStreamForMedia(verificationDocumentMediaModel, document.getInputStream(), document.getOriginalFilename(), document.getContentType());
      getModelService().refresh(verificationDocumentMediaModel);
      persistVerificationDocument(customerModel, verificationDocumentMediaModel);
      getModelService().save(customerModel);
      getModelService().refresh(customerModel);
    }catch (Exception exception){
      BlLogger.logMessage(LOGGER, Level.ERROR,
          "Unable upload Verification Document", exception);
    }
  }
   /**
    * Remove Verification Document
    * @param code
    * @return VerificationDocumentMedia
    */
   @Override
   public VerificationDocumentMediaModel removeVerificationDocument(final String code){

     return getBlVerificationDocumentDao().removeVerificationDocument(code);
   }

  /**
   *
   * @param customerModel
   * @param verificationDocumentMediaModel
   */
  private void persistVerificationDocument(final CustomerModel customerModel,final VerificationDocumentMediaModel verificationDocumentMediaModel) {
    List<VerificationDocumentMediaModel> documents = new ArrayList<>(customerModel.getVerificationDocuments());
    documents.add(verificationDocumentMediaModel);
    customerModel.setVerificationDocuments(documents);
//    switch(verificationDocumentMediaModel.getDocumentType())
//    {
//      case DRIVING_LICENSE:
//        customerModel.setDrivingLicense(verificationDocumentMediaModel);
//        break;
//      case UTILITY_BILL:
//        customerModel.setUtilityBill(verificationDocumentMediaModel);
//        break;
//      case INSURANCE_CERTIFICATE:
//        customerModel.setInsuranceCertificate(verificationDocumentMediaModel);
//        break;
//      case EXTRA_DOCUMENT1:
//        customerModel.setExtraDocument1(verificationDocumentMediaModel);
//        break;
//      case EXTRA_DOCUMENT2:
//        customerModel.setExtraDocument2(verificationDocumentMediaModel);
//        break;
//    }
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

   /**
    * @return the blVerificationDocumentDao
    */
   public BlVerificationDocumentDao getBlVerificationDocumentDao()
   {
     return blVerificationDocumentDao;
   }
   /**
    * @param blVerificationDocumentDao the blVerificationDocumentDao to set
    */
   public void setBlVerificationDocumentDao(BlVerificationDocumentDao blVerificationDocumentDao)
   {
     this.blVerificationDocumentDao = blVerificationDocumentDao;
   }
}
