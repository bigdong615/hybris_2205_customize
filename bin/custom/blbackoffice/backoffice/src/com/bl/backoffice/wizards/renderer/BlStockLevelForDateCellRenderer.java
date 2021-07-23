package com.bl.backoffice.wizards.renderer;

import com.bl.core.utils.BlDateTimeUtils;
import com.hybris.cockpitng.core.config.impl.jaxb.listview.ListColumn;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import org.zkoss.zk.ui.HtmlBasedComponent;
import org.zkoss.zul.Label;

import com.hybris.cockpitng.dataaccess.facades.type.DataType;
import com.hybris.cockpitng.engine.WidgetInstanceManager;
import com.hybris.cockpitng.util.UITools;
import com.hybris.cockpitng.widgets.common.AbstractWidgetComponentRenderer;
import org.zkoss.zul.Listcell;

/**
 * It is used to format the date in list view of Stock level
 *
 * @author Moumita
 */
public class BlStockLevelForDateCellRenderer<R extends StockLevelModel>
        extends AbstractWidgetComponentRenderer<Listcell, ListColumn, R> {
    public static final String RENTAL_DATE_FORMAT = "MMM dd, YYYY";

    @Override
    public void render(final Listcell parent, final ListColumn columnConfiguration, final R stockLevelModel,
                       final DataType dataType, final WidgetInstanceManager widgetInstanceManager) {
        if (null != stockLevelModel.getDate()) {
            final HtmlBasedComponent label = new Label(getLabelText(stockLevelModel));

            UITools.modifySClass(parent, "yw-listview-cell-restricted", true);
            UITools.modifySClass(label, "yw-listview-cell-label", true);

            parent.appendChild(label);

            fireComponentRendered(label, parent, columnConfiguration, stockLevelModel);
            fireComponentRendered(parent, parent, columnConfiguration, stockLevelModel);
        }
    }

    /**
     * It sets the date without time
     *
     * @param stockLevelModel
     * @return String date
     */
    protected String getLabelText(final StockLevelModel stockLevelModel) {
        return BlDateTimeUtils.convertDateToStringDate(stockLevelModel.getDate(),
                RENTAL_DATE_FORMAT);
    }
}
