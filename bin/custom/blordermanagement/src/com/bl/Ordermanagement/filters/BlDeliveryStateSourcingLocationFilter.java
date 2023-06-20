package com.bl.Ordermanagement.filters;

import com.bl.core.dao.warehouse.BlStateWarehouseMappingDao;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlStateWarehouseMappingModel;
import com.bl.core.model.ShippingOptimizationModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.WarehouseService;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.TreeMap;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Filter to find out exact warehouse matching with delivery address.
 *
 * @author Sunil
 */
public class BlDeliveryStateSourcingLocationFilter {

  private static final String Separator = "-";
  private static final Integer ONE = 1;
  private static final Integer TWO = 2;
  private static final String UPS = "UPS";
  private static final String MA = "warehouse_ma";
  private static final String CA = "warehouse_ca";
  private static final Logger LOG = Logger.getLogger(BlDeliveryStateSourcingLocationFilter.class);
  private BlStateWarehouseMappingDao blStateWarehouseMappingDao;
   @Resource
   private WarehouseService warehouseService;

  /**
   * {@inheritDoc}
   */
  public WarehouseModel applyFilter(final AbstractOrderModel order) {
	  
	  if (order.getDeliveryMode() instanceof BlPickUpZoneDeliveryModeModel
      		&& null != ((ZoneDeliveryModeModel) order.getDeliveryMode()).getWarehouse()) {
      	
      	return ((ZoneDeliveryModeModel) order.getDeliveryMode()).getWarehouse();
      	
      }else if (null != order.getDeliveryAddress() && null != order.getDeliveryAddress().getPostalcode()) {
      	String postalCode = order.getDeliveryAddress().getPostalcode();

      	final String carrierID = ((ZoneDeliveryModeModel) order.getDeliveryMode()).getCarrier().getCode();
      	
      	Integer carrierIDVal = carrierID.equalsIgnoreCase(UPS) ? TWO : ONE;

      	if(postalCode.contains(Separator)) {
      		String[] postalCodeDigits = postalCode.split(Separator);
      		postalCode = postalCodeDigits[0];
      	}
      	
      	List<ShippingOptimizationModel> blShippingOptimizationModels = blStateWarehouseMappingDao.getWarehouseForPostalCode(postalCode, carrierIDVal);

      	// Business logic to filter warehouseModel from list of warehouse model.
      	if(CollectionUtils.isNotEmpty(blShippingOptimizationModels) && blShippingOptimizationModels.size() > 1) {
      		blShippingOptimizationModels = blShippingOptimizationModels.stream().collect(minList(Comparator.comparing(ShippingOptimizationModel::getServiceDays)));      		
      	}
      
         WarehouseModel foundLocation = null;
   
         if (CollectionUtils.isNotEmpty(blShippingOptimizationModels) && blShippingOptimizationModels.size() == 1 && null != blShippingOptimizationModels.get(0)) {
         	
         	String warehouseCode = ONE.equals(blShippingOptimizationModels.get(0).getHomeBaseID()) ? CA : MA ;
         	foundLocation = warehouseService.getWarehouseForCode(warehouseCode);
         	
         	BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                "Location found for Order {} with postal code {} and carrier ID {} is warehouse {}", order, postalCode, carrierID,
                foundLocation.getCode());
         	
         	return foundLocation;
         	
         }else if(CollectionUtils.isNotEmpty(blShippingOptimizationModels) && blShippingOptimizationModels.size() > 1) {
         	List<ShippingOptimizationModel> list1 = blShippingOptimizationModels.stream().filter(p -> ONE.equals(p.getHomeBaseID())).collect(Collectors.toList());
         	List<ShippingOptimizationModel> list2 = blShippingOptimizationModels.stream().filter(p -> TWO.equals(p.getHomeBaseID())).collect(Collectors.toList());
         			
         	final Integer homeBaseID = (blShippingOptimizationModels.size() == list1.size() || blShippingOptimizationModels.size() == list2.size()) ? blShippingOptimizationModels.iterator().next().getHomeBaseID() : ONE;
         	
         	String warehouseCode = ONE.equals(homeBaseID) ? CA : MA ;
         	foundLocation = warehouseService.getWarehouseForCode(warehouseCode);
         	   
         	BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                   "Location found for Order {} with postal code {} and carrier ID {} is warehouse {}", order, postalCode, carrierID,
                   foundLocation.getCode());
   
            return foundLocation;
         }      
         
         BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Warehouse not found for Order {} with postal code : {}", order, postalCode);
         return null;
      }	  	
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Postal code and delivery address is null");
      return null;
  }

  public BlStateWarehouseMappingDao getBlStateWarehouseMappingDao() {
    return blStateWarehouseMappingDao;
  }

  public void setBlStateWarehouseMappingDao(final BlStateWarehouseMappingDao blStateWarehouseMappingDao) {
    this.blStateWarehouseMappingDao = blStateWarehouseMappingDao;
  }
  
  static <T> Collector<T, ?, List<T>> minList(Comparator<? super T> comp) {
	    return Collector.of(ArrayList::new, (list, t) -> {
	        int c;
	        if (list.isEmpty() || (c = comp.compare(t, list.get(0))) == 0)
	            list.add(t);
	        else if (c < 0) {
	            /*
	             * We have found a smaller element than what we already have. Clear the list and
	             * add this smallest element to it.
	             */
	            list.clear();
	            list.add(t);
	        }
	    }, (list1, list2) -> {
	        if (comp.compare(list1.get(0), list2.get(0)) < 0)
	            return list1;
	        else if (comp.compare(list1.get(0), list2.get(0)) > 0)
	            return list2;
	        else {
	            list1.addAll(list2);
	            return list1;
	        }
	    });
	}
}

