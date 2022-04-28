package com.bl.batch.imports.translator;

import de.hybris.platform.catalog.enums.ArticleApprovalStatus;
import de.hybris.platform.util.CSVCellDecorator;

import java.util.Map;


public class BlApprovalStatusCellDecorator implements CSVCellDecorator
{

	@Override
	public String decorate(final int position, final Map<Integer, String> impexLine)
	{
		final String approvalStatusCode = impexLine.get(Integer.valueOf(position));

		if (approvalStatusCode != null)
		{
			return ArticleApprovalStatus.valueOf(approvalStatusCode).getCode();
		}
		return "";
	}
}
