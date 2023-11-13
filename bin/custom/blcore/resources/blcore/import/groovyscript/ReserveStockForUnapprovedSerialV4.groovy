import com.bl.core.model.BlSerialProductModel
import de.hybris.platform.ordersplitting.model.StockLevelModel
import de.hybris.platform.servicelayer.model.ModelService
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery
import de.hybris.platform.servicelayer.search.FlexibleSearchService
import de.hybris.platform.servicelayer.search.SearchResult
import org.apache.commons.collections.CollectionUtils
import com.bl.core.constants.BlCoreConstants
import com.bl.core.utils.BlDateTimeUtils

final String selectedFromDate = "09-11-2023"; //(mm-dd-yyyy)
final String selectedToDate = "10-11-2023";

String queryString ="Select {stock.pk} from {StockLevel as stock},{BlSerialProduct as serial},{ArticleApprovalStatus as aas}, {CatalogVersion as c }" +
        "where {stock.serialProductCode}={serial.code} and {serial.approvalStatus}={aas.pk} and  {serial.catalogVersion} ={c.pk}  and {aas.code}='unapproved' and {c.version} = 'Online' and {stock.date} BETWEEN ?startDate and ?endDate and {stock.reservedStatus}=0";

FlexibleSearchService flexibleSearchService = spring.getBean("flexibleSearchService");
ModelService modelService = spring.getBean("modelService");
Date startDate = BlDateTimeUtils.convertStringDateToDate(selectedFromDate, BlCoreConstants.SQL_DATE_FORMAT);
Date endDate = BlDateTimeUtils.convertStringDateToDate(selectedToDate, BlCoreConstants.SQL_DATE_FORMAT);
final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(queryString);

fQuery.addQueryParameter(BlCoreConstants.START_DATE, BlDateTimeUtils.getFormattedStartDay(startDate).getTime());
fQuery.addQueryParameter(BlCoreConstants.END_DATE, BlDateTimeUtils.getFormattedEndDay(endDate).getTime());
final SearchResult<StockLevelModel> stockResult = flexibleSearchService.search(fQuery);

final List<StockLevelModel> stockList = stockResult.getResult();

System.out.println("Number of stock :"+stockList.size());

if (CollectionUtils.isNotEmpty(stockList)) {
    stockList.forEach(stock -> stock.setReservedStatus(Boolean.TRUE));
    modelService.saveAll(stockList);
}