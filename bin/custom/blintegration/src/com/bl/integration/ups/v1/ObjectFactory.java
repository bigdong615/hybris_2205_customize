
package com.bl.integration.ups.v1;

import com.ups.xmlschema.xoltws.upss.v1.UPSSecurity;
import javax.xml.bind.annotation.XmlRegistry;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.ups.xmlschema.xoltws.upss.v1 package. 
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
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.ups.xmlschema.xoltws.upss.v1
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link UPSSecurity }
     * 
     */
    public UPSSecurity createUPSSecurity() {
        return new UPSSecurity();
    }

    /**
     * Create an instance of {@link UPSSecurity.UsernameToken }
     * 
     */
    public UPSSecurity.UsernameToken createUPSSecurityUsernameToken() {
        return new UPSSecurity.UsernameToken();
    }

    /**
     * Create an instance of {@link UPSSecurity.ServiceAccessToken }
     * 
     */
    public UPSSecurity.ServiceAccessToken createUPSSecurityServiceAccessToken() {
        return new UPSSecurity.ServiceAccessToken();
    }

}
