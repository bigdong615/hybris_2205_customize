/**
 *
 */
package com.bl.backoffice.editor;

import com.hybris.cockpitng.core.util.Validate;
import com.hybris.cockpitng.editor.defaultreferenceeditor.DefaultReferenceEditor;
import com.hybris.cockpitng.editors.EditorContext;
import com.hybris.cockpitng.editors.EditorListener;
import java.util.Calendar;
import org.zkoss.zk.ui.Component;


/**
 * @author Krishan Vashishth
 *
 */
public class BlCustomDateEditor<T> extends DefaultReferenceEditor<T> {

  @Override
  public void render(final Component parent, final EditorContext<T> context,
      final EditorListener<T> listener) {
    Validate.notNull("All parameters are mandatory", new Object[]{parent, context, listener});
    this.setEditorParameters(context);
    this.setEditorLayout(this.createReferenceLayout(context));
    this.setParentEditor(this.findAncestorEditor(parent));
    final Object parentObject = parent.getAttribute(PARENT_OBJECT);
    this.setParentObject(parentObject);
    this.setEditorListener(listener);
    this.setEditorContext(context);
    this.getEditorLayout().createLayout(parent);
    this.getEditorLayout().addListeners();
    this.setInitialValue(context);
    this.getEditorLayout().setEditableState(context.isEditable());
    this.addSocketInputEventListener(SOCKET_IN_REFERENCE_EDITOR, this.createInputSocketEventListener());
    this.setSuccessNotificationId(context.getSuccessNotificationId());
    addSelectedObject((T) Calendar.getInstance().getTime());
  }
}