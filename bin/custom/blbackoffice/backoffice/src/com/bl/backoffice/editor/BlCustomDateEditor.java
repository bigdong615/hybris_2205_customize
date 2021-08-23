/**
 *
 */
package com.bl.backoffice.editor;

import java.util.Calendar;
import java.util.Date;

import org.zkoss.zul.Datebox;
import org.zkoss.zul.impl.InputElement;

import com.hybris.cockpitng.editor.defaultdate.DefaultDateEditor;
import com.hybris.cockpitng.editors.EditorContext;
import com.hybris.cockpitng.editors.EditorListener;


/**
 * BL-769 : This class is being used to render the current date in Advanced search.
 *
 * @author Krishan Vashishth
 *
 */
public class BlCustomDateEditor<T> extends DefaultDateEditor
{

	@Override
	protected void initViewComponent(final InputElement editorView, final EditorContext<Date> context,
			final EditorListener<Date> listener)
	{
		super.initViewComponent(editorView, context, listener);
		if (this.shouldInputBeDisabled(context))
		{
			editorView.setReadonly(true);
			editorView.setClass("ye-inline-editing-disabled");
		}

		final Datebox box = (Datebox) editorView;
		box.setValue(Calendar.getInstance().getTime());
		box.setWidth("100%");
		box.setStyle("display: table;\r\n"
				+ "    position: relative;\r\n"
				+ "    width: 100%;");
		this.initTimeZone(box, context);

		if (context.getParameterAsBoolean("calendarOnFocus", false))
		{
			box.setButtonVisible(false);
			box.addEventListener("onFocus", (event) -> {
				box.open();
			});
		}

	}
}
