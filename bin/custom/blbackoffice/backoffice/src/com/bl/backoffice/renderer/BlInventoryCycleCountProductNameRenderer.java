package com.bl.backoffice.renderer;

import com.bl.core.model.BlInventoryCycleCountProductDetailsModel;
import com.hybris.cockpitng.core.config.impl.jaxb.listview.ListColumn;
import com.hybris.cockpitng.dataaccess.facades.type.DataType;
import com.hybris.cockpitng.engine.WidgetInstanceManager;
import com.hybris.cockpitng.widgets.common.WidgetComponentRenderer;
import java.util.Objects;
import org.apache.commons.lang.StringUtils;
import org.zkoss.zhtml.Text;
import org.zkoss.zul.Listcell;

/**
 * This classs is created to render product name from BlProduct
 * @author Ritika
 */
public class BlInventoryCycleCountProductNameRenderer implements WidgetComponentRenderer<Listcell, ListColumn, Object> {


    /**
     * Renders the localized name Value
     */
    @Override
    public void render(final Listcell listcell, final ListColumn lictColumn, final Object data, final DataType dataType,
        final WidgetInstanceManager widgetInstanceManager){

      String localizedName = StringUtils.EMPTY;
      if (data instanceof BlInventoryCycleCountProductDetailsModel) {
        final BlInventoryCycleCountProductDetailsModel productDetailsModel = (BlInventoryCycleCountProductDetailsModel) data;
        if (Objects.nonNull(productDetailsModel.getInventoryCycleCountProduct())) {
          localizedName = productDetailsModel.getInventoryCycleCountProduct().getName();
          if (StringUtils.isBlank(localizedName)) {
            localizedName = productDetailsModel.getInventoryCycleCountProduct().getDisplayName();
          }
        }
      }
      final Text nameText = new Text();
      nameText.setValue(localizedName);
      listcell.appendChild(nameText);
    }

  }



