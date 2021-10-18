package com.bl.integration.services;

/**
 * This interface created to call UPS Scrape
 * @author Manikandan
 */
public interface UPSScrapeService {

  /**
   * This method created for UPS scrape order
   */
   void performUPSScrapeForOrders();

  /**
   * This method created for UPS scrape Late order
   */
   void performUPSScrapeForLateOrder();

}
