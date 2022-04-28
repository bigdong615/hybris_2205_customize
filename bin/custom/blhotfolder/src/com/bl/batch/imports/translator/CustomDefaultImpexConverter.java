/*
 * [y] hybris Platform
 *
 * Copyright (c) 2000-2015 hybris AG
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of hybris
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with hybris.
 *
 *
 */
package com.bl.batch.imports.translator;

import de.hybris.platform.acceleratorservices.dataimport.batch.converter.ImpexConverter;
import de.hybris.platform.acceleratorservices.dataimport.batch.converter.ImpexRowFilter;
import de.hybris.platform.acceleratorservices.dataimport.batch.converter.impl.NullImpexRowFilter;
import de.hybris.platform.servicelayer.exceptions.SystemException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.CharUtils;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.text.StrTokenizer;
import org.springframework.util.Assert;


/**
 * Default implementation of {@link ImpexConverter}.
 */
public class CustomDefaultImpexConverter implements ImpexConverter
{
	private static final char PLUS_CHAR = '+';
	private static final char SEQUENCE_CHAR = 'S';
	private static final String EMPTY_STRING = "";
	private static final char BRACKET_END = '}';
	private static final char BRACKET_START = '{';

	private String header;
	private String impexRow;
	private String type;
	private ImpexRowFilter rowFilter = new NullImpexRowFilter();

	@Override
	public String convert(final Map<Integer, String> row, final Long sequenceId)
	{
		String result = EMPTY_STRING;
		if (!MapUtils.isEmpty(row))
		{
			final StringBuilder builder = new StringBuilder();
			int copyIdx = 0;
			int idx = impexRow.indexOf(BRACKET_START);
			while (idx > -1)
			{
				final int endIdx = impexRow.indexOf(BRACKET_END, idx);
				if (endIdx < 0)
				{
					throw new SystemException("Invalid row syntax [brackets not closed]: " + impexRow);
				}
				builder.append(impexRow.substring(copyIdx, idx));
				if (impexRow.charAt(idx + 1) == SEQUENCE_CHAR)
				{
					builder.append(sequenceId);
				}
				else
				{
					final boolean mandatory = impexRow.charAt(idx + 1) == PLUS_CHAR;
					Integer mapIdx = null;
					try
					{
						mapIdx = Integer.valueOf(impexRow.substring(mandatory ? idx + 2 : idx + 1, endIdx));
					}
					catch (final NumberFormatException e)
					{
						throw new SystemException("Invalid row syntax [invalid column number]: " + impexRow, e);
					}
					String colValue = row.get(mapIdx);


					if (mandatory && StringUtils.isBlank(colValue))
					{
						throw new IllegalArgumentException("Missing value for " + mapIdx);
					}
					if (colValue != null)
					{
						//						if (mapIdx.intValue() == 5 | mapIdx.intValue() == 25 | mapIdx.intValue() == 27 
						//								| mapIdx.intValue() == 28 | mapIdx.intValue() == 29 | mapIdx.intValue() == 39 |
						//								mapIdx.intValue() == 40)
						if (colValue.contains(";"))
						{
							colValue = "\"" + colValue + "\"";
							builder.append(colValue);
						}
						else
						{
							builder.append(colValue);
						}
						/*
						 * else { builder.append(CSVUtils.escapeString(colValue, new char[] { '"' }, true)); }
						 */

					}
				}
				copyIdx = endIdx + 1;
				idx = impexRow.indexOf(BRACKET_START, endIdx);
			}
			if (copyIdx < impexRow.length())
			{
				builder.append(impexRow.substring(copyIdx));
			}
			result = builder.toString();
		}
		return escapeQuotes(result);
	}


	protected String escapeQuotes(final String input)
	{

		//final String[] splitedInput =	 StringUtils.splitPreserveAllTokens(input, SEMICOLON_CHAR);
		final StrTokenizer splitedInput = new StrTokenizer(input, ';', '"');
		splitedInput.setIgnoreEmptyTokens(false);
		final String[] inputTokens = splitedInput.getTokenArray();
		final List<String> tmp = new ArrayList<String>();
		for (final String string : inputTokens)
		{
			//			if (tmp.size() == 6 | tmp.size() == 26 | tmp.size() == 28 | tmp.size() == 29 |
			//					tmp.size() == 30 | tmp.size() == 40 | tmp.size() == 41)
			if (string.contains(";"))
			{
				tmp.add("\"" + StringEscapeUtils.escapeCsv(string) + "\"");
			}
			else if (doesNotContainNewLine(string))
			{
				tmp.add(StringEscapeUtils.escapeCsv(string));
			}
			else
			{
				tmp.add(string);
			}

		}

		return StringUtils.join(tmp, ";");
	}

	protected boolean doesNotContainNewLine(final String string)
	{
		return !StringUtils.contains(string, CharUtils.LF);
	}

	/**
	 * @see de.hybris.platform.acceleratorservices.dataimport.batch.converter.ImpexConverter#filter(java.util.Map)
	 */
	public boolean filter(final Map<Integer, String> row)
	{
		return rowFilter.filter(row);
	}

	/**
	 * @see de.hybris.platform.acceleratorservices.dataimport.batch.converter.ImpexConverter#getHeader()
	 */
	public String getHeader()
	{
		return header;
	}

	/**
	 * @param header
	 *           the header to set
	 */
	public void setHeader(final String header)
	{
		Assert.hasText(header);
		this.header = header;
	}

	/**
	 * @param impexRow
	 *           the impexRow to set
	 */
	public void setImpexRow(final String impexRow)
	{
		Assert.hasText(impexRow);
		this.impexRow = impexRow;
	}

	/**
	 * @param type
	 *           the type to set
	 */
	public void setType(final String type)
	{
		this.type = type;
	}

	/**
	 * @return the type
	 */
	public String getType()
	{
		return type;
	}

	/**
	 * @param rowFilter
	 *           the rowFilter to set
	 */
	public void setRowFilter(final ImpexRowFilter rowFilter)
	{
		Assert.notNull(rowFilter);
		this.rowFilter = rowFilter;
	}

}
