package com.bl.storefront.controllers.wishlist;

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
  @ResponseBody
  @RequestMapping(value = "/wishlist/add", method = RequestMethod.POST)
  public String addToWishlist(@RequestParam("productwishlistCode") final String code) {
    try {
      wishlistFacade.addToWishlist(code);
      return "Success";
    } catch (Exception e) {
      LOG.error("Wishlist not found", e);
      return "error";
    }
  }

  @RequireHardLogIn
  @RequestMapping(value = "/getwishlist", method = RequestMethod.GET)
  public List<ProductData> getWishlist(final Model model) {

    //ProductData productData = new ProductData();
    List<ProductData> productData1 = null;
    List<Wishlist2Model> wishlist = wishlistFacade.getWishlist();

    for (Wishlist2Model wishlistModel : wishlist) {
      ProductData productData = new ProductData();
      wishListPopulator.populate(wishlistModel, productData);
      productData1.add(productData);
    }

    model.addAttribute("wishlist2Data", productData1);
    return productData1;
  }

  @RequireHardLogIn
  @ResponseBody
  @RequestMapping(value = "/removewishlist", method = RequestMethod.POST)
  public void removeWishlist(@RequestParam("removeproductCode") final String code) {
    wishlistFacade.removeWishlist(code);

  }
}
