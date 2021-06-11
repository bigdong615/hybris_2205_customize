package com.braintree.customersupportbackoffice.widgets;

import com.braintree.method.BrainTreePaymentService;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.site.BaseSiteService;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.event.EventListener;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.event.SelectEvent;
import org.zkoss.zul.Listbox;
import org.zkoss.zul.Listitem;

import javax.annotation.Resource;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;


public class PrepareTransactionWidgetController extends DefaultWidgetController {
    private static final Logger LOG = Logger.getLogger(PrepareTransactionWidgetController.class);

    private Listbox siteList;

    private Listbox currencyList;

    private CurrencyModel currentCurrency;

	@Resource(name = "commonI18NService")
    private CommonI18NService commonI18NService;
	@Resource(name = "baseSiteService")
    private BaseSiteService baseSiteService;
	@Resource(name = "brainTreePaymentService")
	private BrainTreePaymentService brainTreePaymentService;


    @SuppressWarnings("Duplicates")
	@Override
	public void initialize(Component comp) {
		super.initialize(comp);

		siteList.setMultiple(false);
		currencyList.setMultiple(false);

		final Collection<BaseSiteModel> allBaseSites = baseSiteService.getAllBaseSites();
		for (BaseSiteModel baseSiteModel : allBaseSites) {
            siteList.appendItem(baseSiteModel.getName(), baseSiteModel.getUid());
			if (siteList.getSelectedCount() == 0) {
				siteList.setSelectedIndex(0);
				populateCurrencyList(currencyList, getCurrenciesBySite(baseSiteModel));
			}
		}

		siteList.addEventListener(Events.ON_SELECT, addSiteListEventListener(allBaseSites, currencyList));
        currencyList.addEventListener(Events.ON_SELECT, addCurrencyListEventListener(currencyList));
	}

	@ViewEvent(componentID = "nextBtn", eventName = Events.ON_CLICK)
	public void generateToken() {
        final String token = brainTreePaymentService.generateClientToken(siteList.getSelectedItem().getValue().toString(), currentCurrency.getIsocode());
        sendOutput("outgoingMsg", token + "|" + siteList.getSelectedItem().getValue().toString() + "|" + currentCurrency.getIsocode());
	}

	private void populateCurrencyList(Listbox currencyList, final Map<CurrencyModel, String> currencies) {
		currencies.forEach((currency, description) -> {

            Listitem listitem = new Listitem(description, currency);
            currencyList.appendChild(listitem);

            if (currency.getIsocode().equals(
                    baseSiteService.getBaseSiteForUID(siteList.getSelectedItem().getValue()).getStores().get(0).getDefaultCurrency().getIsocode())) {
                currencyList.setSelectedItem(listitem);
            }
        });

        if (currencyList.getSelectedCount() == 0 && currencyList.getItemCount() > 0) {
            currencyList.setSelectedIndex(0);
        }

        currentCurrency = currencyList.getSelectedItem().getValue();
	}

    @SuppressWarnings("Duplicates")
	private EventListener<SelectEvent> addCurrencyListEventListener(final Listbox currencyList) {
        return (event -> {
            Object selectedItem = event.getSelectedItems().iterator().next();
            currentCurrency = ((Listitem) selectedItem).getValue();
        });
    }

    private Map<CurrencyModel, String> getCurrenciesBySite(final BaseSiteModel baseSiteModel) {
		Set<CurrencyModel> currencies = baseSiteModel.getStores().get(0).getCurrencies();
		Map<CurrencyModel, String> currenciesMap = new HashMap<>();
		for (CurrencyModel currency : currencies) {
			String description = currency.getName() + " [" + currency.getSymbol() + "]";
			currenciesMap.put(currency, description);
		}
		return currenciesMap;
	}

	@SuppressWarnings("Duplicates")
	private EventListener<SelectEvent> addSiteListEventListener(Collection<BaseSiteModel> allBaseSites, Listbox currencyList) {
		return (event -> {
			Object selectedItem = event.getSelectedItems().iterator().next();
			String selectedSite = ((Listitem) selectedItem).getValue().toString();
			for (BaseSiteModel baseSiteModel : allBaseSites) {
				if (baseSiteModel.getUid().equals(selectedSite)) {
					clearListbox(currencyList);
					populateCurrencyList(currencyList, getCurrenciesBySite(baseSiteModel));
					break;
				}
			}
		});
	}

	private void clearListbox(Listbox listbox) {
		while (!listbox.getChildren().isEmpty()) {
			listbox.removeChild(listbox.getChildren().get(0));
		}
	}

}
