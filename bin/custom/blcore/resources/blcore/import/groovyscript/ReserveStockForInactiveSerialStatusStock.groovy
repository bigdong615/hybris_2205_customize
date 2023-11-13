import de.hybris.platform.ordersplitting.model.StockLevelModel
import de.hybris.platform.servicelayer.model.ModelService
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery
import de.hybris.platform.servicelayer.search.FlexibleSearchService
import de.hybris.platform.servicelayer.search.SearchResult
import org.apache.commons.collections.CollectionUtils


final String queryString ="Select {stock.pk} from {StockLevel as stock}, {SerialStatusEnum as sse} where {stock.serialStatus}={sse.pk} and {stock.reservedStatus}=0  and {sse.code} in ('REPAIR','REPAIR_IN_HOUSE','REPAIR_SEND_TO_VENDOR','REPAIR_PARTS_NEEDED','REPAIR_AWAITING_QUOTES','LOST','LOST_IN_TRANSIT','LOST_IN_HOUSE','LOST_UNDER_INVESTIGATION','STOLEN','STOLEN_PAID_IN_FULL','STOLEN_PAID_SOME','STOLEN_NOT_PAID','STOLEN_PAID_12_PERCENT','SCRAPPED','ARCHIVED','REPAIR_NEEDED','PARTS_NEEDED','SOLD','ADDED_TO_CART','LATE')";

final FlexibleSearchService flexibleSearchService = spring.getBean("flexibleSearchService");
final ModelService modelService = spring.getBean("modelService");
final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(queryString);
final SearchResult<StockLevelModel> stockResult = flexibleSearchService.search(fQuery);

final List<StockLevelModel> stockList = stockResult.getResult();

System.out.println("Number of stock :"+stockList.size());

if (CollectionUtils.isNotEmpty(stockList)) {
    stockList.forEach(stock -> stock.setReservedStatus(Boolean.TRUE));
    modelService.saveAll(stockList);
    System.out.println("All The stock reserved");
}else{
    System.out.println("No Stock found");
}
