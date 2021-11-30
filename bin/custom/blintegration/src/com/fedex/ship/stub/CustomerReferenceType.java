/**
 * CustomerReferenceType.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class CustomerReferenceType implements java.io.Serializable
{
	private static final long serialVersionUID = 1L;
	private final java.lang.String _value_;
	private static java.util.HashMap _table_ = new java.util.HashMap();

	// Constructor
	public CustomerReferenceType(final java.lang.String value)
	{
		_value_ = value;
		_table_.put(_value_, this);
	}

	public CustomerReferenceType()
	{
		this._value_ = new java.lang.String();

	}

	public static final java.lang.String _CUSTOMER_REFERENCE = "CUSTOMER_REFERENCE";
	public static final java.lang.String _DEPARTMENT_NUMBER = "DEPARTMENT_NUMBER";
	public static final java.lang.String _INTRACOUNTRY_REGULATORY_REFERENCE = "INTRACOUNTRY_REGULATORY_REFERENCE";
	public static final java.lang.String _INVOICE_NUMBER = "INVOICE_NUMBER";
	public static final java.lang.String _P_O_NUMBER = "P_O_NUMBER";
	public static final java.lang.String _RMA_ASSOCIATION = "RMA_ASSOCIATION";
	public static final java.lang.String _SHIPMENT_INTEGRITY = "SHIPMENT_INTEGRITY";
	public static final CustomerReferenceType CUSTOMER_REFERENCE = new CustomerReferenceType(_CUSTOMER_REFERENCE);
	public static final CustomerReferenceType DEPARTMENT_NUMBER = new CustomerReferenceType(_DEPARTMENT_NUMBER);
	public static final CustomerReferenceType INTRACOUNTRY_REGULATORY_REFERENCE = new CustomerReferenceType(
			_INTRACOUNTRY_REGULATORY_REFERENCE);
	public static final CustomerReferenceType INVOICE_NUMBER = new CustomerReferenceType(_INVOICE_NUMBER);
	public static final CustomerReferenceType P_O_NUMBER = new CustomerReferenceType(_P_O_NUMBER);
	public static final CustomerReferenceType RMA_ASSOCIATION = new CustomerReferenceType(_RMA_ASSOCIATION);
	public static final CustomerReferenceType SHIPMENT_INTEGRITY = new CustomerReferenceType(_SHIPMENT_INTEGRITY);

	public java.lang.String getValue()
	{
		return _value_;
	}

	public static CustomerReferenceType fromValue(final java.lang.String value) throws java.lang.IllegalArgumentException
	{
		final CustomerReferenceType enumeration = (CustomerReferenceType) _table_.get(value);
		if (enumeration == null)
		{
			throw new java.lang.IllegalArgumentException();
		}
		return enumeration;
	}

	public static CustomerReferenceType fromString(final java.lang.String value) throws java.lang.IllegalArgumentException
	{
		return fromValue(value);
	}

	@Override
	public boolean equals(final java.lang.Object obj)
	{
		return (obj == this);
	}

	@Override
	public int hashCode()
	{
		return toString().hashCode();
	}

	@Override
	public java.lang.String toString()
	{
		return _value_;
	}

	public java.lang.Object readResolve() throws java.io.ObjectStreamException
	{
		return fromValue(_value_);
	}

	public static org.apache.axis.encoding.Serializer getSerializer(final java.lang.String mechType,
			final java.lang.Class _javaType, final javax.xml.namespace.QName _xmlType)
	{
		return new org.apache.axis.encoding.ser.EnumSerializer(_javaType, _xmlType);
	}

	public static org.apache.axis.encoding.Deserializer getDeserializer(final java.lang.String mechType,
			final java.lang.Class _javaType, final javax.xml.namespace.QName _xmlType)
	{
		return new org.apache.axis.encoding.ser.EnumDeserializer(_javaType, _xmlType);
	}

	// Type metadata
	private static org.apache.axis.description.TypeDesc typeDesc = new org.apache.axis.description.TypeDesc(
			CustomerReferenceType.class);

	static
	{
		typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomerReferenceType"));
	}

	/**
	 * Return type metadata object
	 */
	public static org.apache.axis.description.TypeDesc getTypeDesc()
	{
		return typeDesc;
	}

}
