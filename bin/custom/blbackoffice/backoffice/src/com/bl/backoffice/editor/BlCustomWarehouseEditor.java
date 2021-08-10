package com.bl.backoffice.editor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.editor.defaultreferenceeditor.DefaultReferenceEditor;
import com.hybris.cockpitng.editors.EditorContext;
import com.hybris.cockpitng.editors.EditorListener;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;

/**
 * ################ BL-747 #######################
 * The type Bl custom warehouse editor.
 *
 * @param <T> the type parameter
 *
 * @author Krishan Vashishth
 */
public class BlCustomWarehouseEditor<T> extends DefaultReferenceEditor<T> {

  private static final Logger LOG = Logger.getLogger(BlCustomWarehouseEditor.class);

  @Resource
  private BaseStoreService baseStoreService;

  @Resource
  private UserService userService;

  /**
   * Render.
   *
   * @param parent   the parent
   * @param context  the context
   * @param listener the listener
   */
  @Override
  public void render(final Component parent, final EditorContext<T> context,
      final EditorListener<T> listener) {
    super.render(parent, context, listener);
    final BaseStoreModel currentBaseStore = baseStoreService
        .getBaseStoreForUid(BlCoreConstants.BASE_STORE_UID);
    WarehouseModel warehouse = null;
    if (userService.getCurrentUser() instanceof EmployeeModel) {
      final EmployeeModel employeeModel = (EmployeeModel) userService.getCurrentUser();
      warehouse = employeeModel.getWarehouse();
    } else if (currentBaseStore != null && CollectionUtils.isNotEmpty(currentBaseStore.getWarehouses())) {
      warehouse = currentBaseStore.getWarehouses().get(0);
    }
    if (warehouse == null) {
      throw new IllegalArgumentException("Found warehouse as null either at user or base store");
    }
    BlLogger.logFormattedMessage(LOG, Level.DEBUG, StringUtils.EMPTY,
        "Found warehouse with code : {}", warehouse.getCode());
    addSelectedObject((T) warehouse);
  }
}
