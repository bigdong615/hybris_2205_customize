package com.bl.facades.populators;

import com.bl.facades.wishlist.data.Wishlist2Data;
import com.bl.facades.wishlist.data.Wishlist2EntryData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.wishlist2.model.Wishlist2EntryModel;
import de.hybris.platform.wishlist2.model.Wishlist2Model;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;

/*
 * This class is to Populate the Wishlist2Model to Wishlist2Data
 * @author Sahana SB
 */
public class BlWishListPopulator implements Populator<Wishlist2Model, Wishlist2Data> {

  private UserService userService;
  private Converter<Wishlist2EntryModel, Wishlist2EntryData> blWishList2EntryConverter;


  @Override
  public void populate(Wishlist2Model source, Wishlist2Data target) {
    if (!userService.isAnonymousUser(userService.getCurrentUser())) {

      target.setDecription(source.getDescription());
      target.setName(source.getName());
      if (CollectionUtils.isNotEmpty(source.getEntries())) {
        List<Wishlist2EntryData> entryDataList = new ArrayList<>();
        for (Wishlist2EntryModel entry : source.getEntries()) {
          entryDataList.add(getBlWishList2EntryConverter().convert(entry));
        }
        target.setEntries(entryDataList);
      }

    }
  }


  public Converter<Wishlist2EntryModel, Wishlist2EntryData> getBlWishList2EntryConverter() {
    return blWishList2EntryConverter;
  }

  public void setBlWishList2EntryConverter(
      Converter<Wishlist2EntryModel, Wishlist2EntryData> blWishList2EntryConverter) {
    this.blWishList2EntryConverter = blWishList2EntryConverter;
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

}

