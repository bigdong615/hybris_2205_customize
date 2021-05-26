package com.bl.storefront.controllers.wishlist;

import com.bl.core.constants.BlCoreConstants;
import com.bl.facades.populators.BlWishListPopulator;
import com.bl.facades.wishlist.impl.DefaultBlWishListFacade;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.selectivecartfacades.data.Wishlist2Data;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import java.util.List;
import javax.annotation.Resource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
public class BlWishlistController {

  private static final Logger LOG = LoggerFactory.getLogger(BlWishlistController.class);
  @Resource(name = "wishlistFacade")
  private DefaultBlWishListFacade wishlistFacade;

  @Resource(name = "wishlistPopulator")
  private BlWishListPopulator wishListPopulator;

  @Resource(name = "wishlistConverter")
  private Converter<Wishlist2Model, Wishlist2Data> wishlistConverter;

  @RequireHardLogIn
  @RequestMapping(value = "/wishlist/add", method = RequestMethod.POST)
  @ResponseBody
  public String addToWishlist(@RequestParam("productwishlistCode") final String code) {
    try {
      wishlistFacade.addToWishlist(code);
      return BlCoreConstants.SUCCESS;
    } catch (Exception e) {
      LOG.error("in default wish list " + code + "already Exists", e);
      return BlCoreConstants.ERROR;
    }
  }

//  @RequireHardLogIn
//  @RequestMapping(value = "/bookmarks", method = RequestMethod.GET)
//  public List<ProductData> getWishlist(final Model model) {
//
//    //ProductData productData = new ProductData();
//    List<ProductData> wishlistData = null;
//    List<Wishlist2Model> wishlist = wishlistFacade.getWishlist();
//
//    for (Wishlist2Model wishlistModel : wishlist) {
//      ProductData productData = new ProductData();
//      wishListPopulator.populate(wishlistModel, productData);
//      wishlistData.add(productData);
//    }
//
//    model.addAttribute("wishlistData", wishlistData);
//    return wishlistData;
//  }

  @RequireHardLogIn
  @RequestMapping(value = "/removewishlist", method = RequestMethod.POST)
  @ResponseBody
  public String removeWishlist(@RequestParam("removeproductCode") final String code) {
    try {
      wishlistFacade.removeWishlist(code);
      return BlCoreConstants.SUCCESS;
    } catch (Exception e) {
      LOG.error("in default wishlist " + code + "not found OR" + code
          + "found more than one entry in default wish list entry");
      return BlCoreConstants.ERROR;
    }
  }
}
