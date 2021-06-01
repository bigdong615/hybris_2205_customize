package com.bl.backoffice.wizards.renderer;

import com.bl.logging.BlLogger;
import com.hybris.cockpitng.components.Editor;
import com.hybris.cockpitng.core.config.impl.jaxb.editorarea.AbstractPanel;
import com.hybris.cockpitng.core.config.impl.jaxb.editorarea.Attribute;
import com.hybris.cockpitng.core.config.impl.jaxb.editorarea.CustomPanel;
import com.hybris.cockpitng.dataaccess.facades.type.DataAttribute;
import com.hybris.cockpitng.dataaccess.facades.type.DataType;
import com.hybris.cockpitng.dataaccess.facades.type.TypeFacade;
import com.hybris.cockpitng.dataaccess.facades.type.exceptions.TypeNotFoundException;
import com.hybris.cockpitng.engine.WidgetInstanceManager;
import com.hybris.cockpitng.labels.LabelService;
import com.hybris.cockpitng.util.YTestTools;
import com.hybris.cockpitng.widgets.editorarea.renderer.impl.DefaultEditorAreaPanelRenderer;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zul.Div;

/**
 * This class is created for rendering order on cscokcpit
 * @author Manikandan
 */
public class BlTotalPriceWithTaxRenderer extends DefaultEditorAreaPanelRenderer {

  private static final Logger LOG = Logger.getLogger(BlTotalPriceWithTaxRenderer.class);
  protected static final String ORDER = "Order";
  protected static final String QUALIFIER = "totalPrice";
  protected static final String LABEL = "customersupportbackoffice.order.details.total";

  private TypeFacade typeFacade;
  private LabelService labelService;
  private Double totalPriceWithTax;

  /**
   * This method renders the order details
   */
  @Override
  public void render(Component component, AbstractPanel abstractPanelConfiguration, Object object, DataType dataType, WidgetInstanceManager widgetInstanceManager) {
    if (abstractPanelConfiguration instanceof CustomPanel && object instanceof AbstractOrderModel) {
      this.totalPriceWithTax = this.getOrderTotalWithTax((AbstractOrderModel)object);

      try {
        Attribute attribute = new Attribute();
        attribute.setLabel(LABEL);
        attribute.setQualifier(QUALIFIER);
        attribute.setReadonly(Boolean.TRUE);
        DataType order = this.getTypeFacade().load(ORDER);
        boolean canReadObject = this.getPermissionFacade().canReadInstanceProperty(order.getClazz(), QUALIFIER);
        if (canReadObject) {
          this.createAttributeRenderer().render(component, attribute, order.getClazz(), order, widgetInstanceManager);
        } else {
          Div attributeContainer = new Div();
          attributeContainer.setSclass("yw-editorarea-tabbox-tabpanels-tabpanel-groupbox-ed");
          this.renderNotReadableLabel(attributeContainer, attribute, dataType, this.getLabelService().getAccessDeniedLabel(attribute));
          attributeContainer.setParent(component);
        }
      } catch (TypeNotFoundException e) {
        BlLogger.logMessage(LOG , Level.ERROR , "Error while BlTotalPriceWithTaxRenderer" ,e);
      }
    }

  }

  /**
   * This method override for create editor
   */
  @Override
  protected Editor createEditor(DataType genericType, WidgetInstanceManager widgetInstanceManager, Attribute attribute, Object object) {
    DataAttribute genericAttribute = genericType.getAttribute(attribute.getQualifier());
    if (genericAttribute == null) {
      return null;
    } else {
      String qualifier = genericAttribute.getQualifier();
      String referencedModelProperty = "Order." + attribute.getQualifier();
      Editor editor = new Editor();
      editor.setReadOnly(Boolean.TRUE);
      editor.setLocalized(Boolean.FALSE); //NOSONAR
      editor.setWidgetInstanceManager(widgetInstanceManager);
      editor.setType(this.resolveEditorType(genericAttribute));
      editor.setOptional(!genericAttribute.isMandatory());
      YTestTools.modifyYTestId(editor, "editor_Order." + qualifier);
      editor.setAttribute("parentObject", object);
      editor.setWritableLocales(this.getPermissionFacade().getWritableLocalesForInstance(object));
      editor.setReadableLocales(this.getPermissionFacade().getReadableLocalesForInstance(object));
      if (genericAttribute.isLocalized()) {
        editor.addParameter("localizedEditor.attributeDescription", this.getAttributeDescription(genericType, attribute));
      }

      editor.setProperty(referencedModelProperty);
      if (StringUtils.isNotBlank(attribute.getEditor())) {
        editor.setDefaultEditor(attribute.getEditor());
      }

      editor.setPartOf(genericAttribute.isPartOf());
      editor.setOrdered(genericAttribute.isOrdered());
      editor.afterCompose();
      editor.setSclass("ye-default-editor-readonly");
      editor.setInitialValue(this.totalPriceWithTax);
      return editor;
    }
  }

  protected Double getOrderTotalWithTax(AbstractOrderModel abstractOrderModel) {
    return abstractOrderModel.getTotalPrice();
  }

  public TypeFacade getTypeFacade() {
    return typeFacade;
  }

  public void setTypeFacade(TypeFacade typeFacade) {
    this.typeFacade = typeFacade;
  }

  @Override
  public LabelService getLabelService() {
    return labelService;
  }

  @Override
  public void setLabelService(LabelService labelService) {
    this.labelService = labelService;
  }


}
