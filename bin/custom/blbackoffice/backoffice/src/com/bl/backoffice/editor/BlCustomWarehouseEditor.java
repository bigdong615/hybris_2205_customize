package com.bl.backoffice.editor;

import com.bl.logging.BlLogger;
import com.hybris.cockpitng.editor.defaultreferenceeditor.DefaultReferenceEditor;
import com.hybris.cockpitng.editors.EditorContext;
import com.hybris.cockpitng.editors.EditorListener;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.core.model.user.UserGroupModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.user.UserService;
import java.util.Optional;
import java.util.Set;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;

/**
 * ################ BL-747 ####################### The type Bl custom warehouse editor.
 *
 * @param <T> the type parameter
 * @author Krishan Vashishth
 */
public class BlCustomWarehouseEditor<T> extends DefaultReferenceEditor<T> {

  private static final Logger LOG = Logger.getLogger(BlCustomWarehouseEditor.class);

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
    WarehouseModel warehouse = null;
    if (userService.getCurrentUser() instanceof EmployeeModel) {
      final Set<PrincipalGroupModel> userGroups = userService.getCurrentUser().getGroups();
      if (CollectionUtils.isNotEmpty(userGroups)) {
        final Optional<PrincipalGroupModel> optionalGroup = userGroups.stream()
            .filter(userGroup -> userGroup instanceof UserGroupModel
                && ((UserGroupModel) userGroup).getWarehouse() != null).findFirst();
        if (optionalGroup.isPresent()) {
          warehouse = ((UserGroupModel) optionalGroup.get()).getWarehouse();
          BlLogger.logFormattedMessage(LOG, Level.DEBUG, StringUtils.EMPTY,
              "Warehouse Code : {} Found For User Group : {}", warehouse.getCode(),
              optionalGroup.get().getUid());
        }
      }
    }
    if (warehouse == null) {
      throw new IllegalArgumentException("Found warehouse as null either at user or base store");
    }
    addSelectedObject((T) warehouse);
  }
}
