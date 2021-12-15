package com.fedex.soapsamples.util;

import org.apache.axis.encoding.ser.*;

public class CustomCalendarSerializerFactory extends BaseSerializerFactory {
    public CustomCalendarSerializerFactory(Class javaType, javax.xml.namespace.QName xmlType) {
        super(CustomCalendarSerializer.class, xmlType, javaType);
    }
}
