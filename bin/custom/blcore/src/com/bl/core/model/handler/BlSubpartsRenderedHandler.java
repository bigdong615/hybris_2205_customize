package com.bl.core.model.handler;

import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Logger;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSubpartsModel;

/**
 * @author Jyoti Swamy
 *
 */
public class BlSubpartsRenderedHandler implements DynamicAttributeHandler<List<BlSubpartsModel>, ConsignmentModel> {
    private static final Logger LOG = Logger.getLogger(BlSubpartsRenderedHandler.class);

    @Override
	 public List<BlSubpartsModel> get(final ConsignmentModel consignmentModel)
	 {
        final List<BlSubpartsModel> blSubpartsList = new ArrayList<>();
		  if (Objects.nonNull(consignmentModel) && CollectionUtils.isNotEmpty(consignmentModel.getConsignmentEntries()))
		  {
			  for (final ConsignmentEntryModel consignmentEntryModel : consignmentModel.getConsignmentEntries())
			  {
				  getSubPartsFromOrderEntry(consignmentEntryModel, blSubpartsList);
            }

		  }
        return blSubpartsList;
	  }

	  private void getSubPartsFromOrderEntry(final ConsignmentEntryModel consignmentEntryModel,
			  final List<BlSubpartsModel> blSubpartsList)
	  {
		  if (Objects.nonNull(consignmentEntryModel.getOrderEntry())
				  && Objects.nonNull(consignmentEntryModel.getOrderEntry().getProduct()))
		  {
			  final BlProductModel product = (BlProductModel) consignmentEntryModel.getOrderEntry().getProduct();
			  if (CollectionUtils.isNotEmpty(product.getSubpartProducts()))
			  {
				  for (final BlSubpartsModel subparts : product.getSubpartProducts())
				  {
					  blSubpartsList.add(subparts);
				  }
			  }
		  }
    }


	 @Override
	 public void set(final ConsignmentModel consignmentModel, final List<BlSubpartsModel> blSubparts)
	 {
		 throw new UnsupportedOperationException();

	 }






}
