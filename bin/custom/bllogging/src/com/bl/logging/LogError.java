package com.bl.logging;

/**
 *
 * Marker interface for logging error codes
 *
 * @author Kushal Kumar
 */
public interface LogError {

    /**
     * @return returns the error code
     */
    String getCode();

    /**
     * @return the description
     */
    String getDescription();

}
