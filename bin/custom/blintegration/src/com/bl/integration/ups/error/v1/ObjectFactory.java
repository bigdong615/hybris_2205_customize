
package com.bl.integration.ups.error.v1;

import com.ups.xmlschema.xoltws.error.v1.AdditionalCodeDescType;
import com.ups.xmlschema.xoltws.error.v1.AdditionalInfoType;
import com.ups.xmlschema.xoltws.error.v1.CodeType;
import com.ups.xmlschema.xoltws.error.v1.ElementIdentifierType;
import com.ups.xmlschema.xoltws.error.v1.ElementLevelInformationType;
import com.ups.xmlschema.xoltws.error.v1.ErrorDetailType;
import com.ups.xmlschema.xoltws.error.v1.Errors;
import com.ups.xmlschema.xoltws.error.v1.LocationType;
import javax.xml.bind.annotation.XmlRegistry;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.ups.xmlschema.xoltws.error.v1 package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.ups.xmlschema.xoltws.error.v1
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Errors }
     *
     */
    public Errors createErrors() {
        return new Errors();
    }

    /**
     * Create an instance of {@link ErrorDetailType }
     *
     */
    public ErrorDetailType createErrorDetailType() {
        return new ErrorDetailType();
    }

    /**
     * Create an instance of {@link AdditionalInfoType }
     *
     */
    public AdditionalInfoType createAdditionalInfoType() {
        return new AdditionalInfoType();
    }

    /**
     * Create an instance of {@link CodeType }
     *
     */
    public CodeType createCodeType() {
        return new CodeType();
    }

    /**
     * Create an instance of {@link ElementIdentifierType }
     *
     */
    public ElementIdentifierType createElementIdentifierType() {
        return new ElementIdentifierType();
    }

    /**
     * Create an instance of {@link ElementLevelInformationType }
     *
     */
    public ElementLevelInformationType createElementLevelInformationType() {
        return new ElementLevelInformationType();
    }

    /**
     * Create an instance of {@link LocationType }
     *
     */
    public LocationType createLocationType() {
        return new LocationType();
    }

    /**
     * Create an instance of {@link AdditionalCodeDescType }
     *
     */
    public AdditionalCodeDescType createAdditionalCodeDescType() {
        return new AdditionalCodeDescType();
    }

}
