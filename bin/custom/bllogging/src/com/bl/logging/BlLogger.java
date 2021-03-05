package com.bl.logging;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 *
 * Final class for logging all types of messages in BL project. Need to call its static API's to log different type of messages as per requirement
 *
 * @author Kushal Kumar
 */
public final class BlLogger
{
    private static final String CODE_DELIMITER = "-";
    private static final String CODE_PREFIX = "HYB";
    private static final String INVALID_CHAR = "%";
    private static final String ARGS_REGEX = "\\{\\d*\\}";
    private static final String ARGS_PLACE_HOLDER = "%s";

    private BlLogger()
    {
        /* Private Constructor will prevent
         * the instantiation of this class directly */
    }

    /**
     * Log generic message with message code without exception.
     *
     * ex: BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.PRODUCT_IMPORT_ERROR.getCode(), "product name required");
     *
     * @param logger      Logger to use for logging
     * @param level       Level to use for logging
     * @param messageCode message Code
     * @param message     Logging message
     */
    public static void logMessage(final Logger logger, final Level level,
                                  final String messageCode, final String message)
    {
        logMessage(logger, level, messageCode, message, null);
    }

    /**
     * Log generic message with messageCode and arguments without exception.
     *
     * ex: BlLogger.logFormattedMessage(LOG, Level.DEBUG, BlCoreConstants.EMPTY_STRING,
     *                                 "Using delivery mode {} and zone {} for a delivery cost of {}", deliveryModeModel.getCode(),
     *                                 zoneDeliveryModeModel.getCode(), zoneDeliveryModeModel.getValue());
     *
     * @param logger      Logger to use for logging
     * @param level       Level to use for logging
     * @param messageCode message Code
     * @param message     Logging message
     * @param args        Arguments
     */
    public static void logFormattedMessage(Logger logger,
                                           Level level,
                                           String messageCode,
                                           String message,
                                           Object ...args)
    {
        logMessage(logger, level, messageCode, replaceCurlies(message, args), null);
    }

    /**
     * Log generic message with messageCode, arguments and exception.
     *
     * ex: BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.ADDRESS_FALL_BACK_ERROR.getCode(), e,
     *                     "No dummy address found for locale: {}", i18NService.getCurrentLocale());
     *
     * @param logger      Logger to use for logging
     * @param level       Level to use for logging
     * @param messageCode message Code
     * @param e           Exception to be logged
     * @param message     Logging message
     * @param args        Arguments
     */
    public static void logFormattedMessage(Logger logger,
                                           Level level,
                                           String messageCode,
                                           Throwable e,
                                           String message,
                                           Object ...args)
    {
        logMessage(logger, level, messageCode, replaceCurlies(message, args), e);
    }

    /**
     * Replace the {} or {0} with %s
     * @param message string that {} in it
     * @param args Values to replace {} place holders
     * @return parsed string
     */
    protected static String replaceCurlies(String message, Object ...args) {
        // Replace null message with Empty string
        String parsedMessage = StringUtils.defaultString(message);
        // Replace % Characters with Empty string. Because it's causing the String.format() to throw an exception
        parsedMessage = parsedMessage.replaceAll(INVALID_CHAR, StringUtils.EMPTY);
        // Convert all valid {} into %s
        parsedMessage = parsedMessage.replaceAll(ARGS_REGEX, ARGS_PLACE_HOLDER);
        // Fill %s place holder(s) with value(s) from the <code>args</code>  parameter.
        return String.format(parsedMessage, args);
    }

    /**
     * Log generic message with message code and exception.
     *
     * ex: BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.SALES_ORDER_EXPORT_ERROR.getCode(),
     *                         "something went wrong", ex);
     *
     * @param logger      Logger to use for logging
     * @param level       Level to use for logging
     * @param messageCode message Code
     * @param message     Logging message
     * @param e           Exception to be logged
     */
    public static void logMessage(final Logger logger, final Level level,
                                  final String messageCode, final String message,
                                  final Throwable e)
    {

        if (level.equals(Level.TRACE))
        {
            logger.trace(message, e);
        }
        if (level.equals(Level.DEBUG))
        {
            logger.debug(message, e);
        }
        if (level.equals(Level.INFO))
        {
            logger.info(message, e);
        }
        if (level.equals(Level.WARN))
        {
            logger.warn(message, e);
        }
        if (level.equals(Level.ERROR))
        {
            logError(logger, messageCode, message, e);
        }
    }


    /**
     * Log generic message with exception.
     *
     * ex: BlLogger.logMessage(LOG, Level.ERROR, "URL decoding failed.", e);
     *
     * @param logger      Logger to use for logging
     * @param level       Level to use for logging
     * @param message     Logging message
     * @param e           Exception to be logged
     */
    public static void logMessage(final Logger logger, final Level level,
                                  final String message,
                                  final Throwable e)
    {
        logMessage(logger, level, null, message, e);
    }

    /**
     * Log generic message without exception.
     *
     * ex: BlLogger.logMessage(LOG, Level.DEBUG, "Failed to export order: " + orderModel.getCode());
     *
     * @param logger      Logger to use for logging
     * @param level       Level to use for logging
     * @param message     Logging message
     */
    public static void logMessage(final Logger logger, final Level level,
                                  final String message)
    {
        logMessage(logger, level, null, message, null);
    }

    /**
     * Log generic message with arguments and without exception.
     *
     * ex: BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Process {} completed",process.getCode());
     *
     * @param logger      Logger to use for logging
     * @param level       Level to use for logging
     * @param message     Logging message
     * @param args        Arguments
     */
    public static void logFormatMessageInfo(Logger logger,
                                            Level level,
                                            String message,
                                            Object ...args)
    {
        logMessage(logger, level, null, replaceCurlies(message, args), null);
    }

    /**
     * Log generic message with message code and exception.
     *
     * @param logger      Logger to use for logging
     * @param code 		  message Code
     * @param message     Logging message
     * @param e           Exception to be logged
     */
    protected static void logError(final Logger logger, final String code, final String message,
                                   final Throwable e)
    {
        StringBuilder messageStr = new StringBuilder();
        if (StringUtils.isNotEmpty(code))
        {
            messageStr.append(CODE_PREFIX).append(CODE_DELIMITER).append(code).append(" ");
        }
        messageStr.append(message);
        logger.error(messageStr.toString(), e);
    }

}
