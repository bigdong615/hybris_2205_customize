package com.bl.core.ghostorderremovaljob;
import de.hybris.platform.core.model.order.OrderModel;

import java.util.List;

public interface GhostOrderRemovalJobService {

    public List<OrderModel> removeGhostOrders();

}
