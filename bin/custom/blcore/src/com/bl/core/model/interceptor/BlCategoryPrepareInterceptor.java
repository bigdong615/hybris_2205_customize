package com.bl.core.model.interceptor;

import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;
import org.apache.commons.lang.StringUtils;

public class BlCategoryPrepareInterceptor implements PrepareInterceptor<CategoryModel> {

  private KeyGenerator keyGenerator;

  @Override
  public void onPrepare(CategoryModel categoryModel, InterceptorContext interceptorContext)
      throws InterceptorException {
    if(StringUtils.isBlank(categoryModel.getCategoryId())) {
      categoryModel.setCategoryId(getKeyGenerator().generate().toString());
    }

  }

  public KeyGenerator getKeyGenerator() {
    return keyGenerator;
  }

  public void setKeyGenerator(KeyGenerator keyGenerator) {
    this.keyGenerator = keyGenerator;
  }
}
