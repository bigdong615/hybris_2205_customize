package braintreehac.services;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.braintreehac.data.PayPalButtonStyleData;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

@Component
public class PayPalButtonConfigurationService {

    private static final Logger LOG = Logger.getLogger(PayPalButtonConfigurationService.class);

    private static final String CART_BUTTON_CONFIG_FILE =
            System.getProperty("HYBRIS_CONFIG_DIR") + "/paypalbuttonconfig.properties";

    private static final String CART_BUTTON_CONFIG = "cart.button.config";
    private static final String MINI_CART_BUTTON_CONFIG = "mini.cart.button.config";
    private static final String MARK_BUTTON_CONFIG = "mark.button.config";
    private Properties prop;
    private File propFile;
    private ObjectMapper mapper;
    private List<Integer> smartButtonHeight;
    private List<String> smartButtonColor = new ArrayList<>();
    private List<String> smartButtonShape = new ArrayList<>();
    private List<String> smartButtonLabel = new ArrayList<>();
    private List<String> smartButtonLayout = new ArrayList<>();

    @PostConstruct
    public void init()
    {
        prop = new Properties();
        propFile = new File(CART_BUTTON_CONFIG_FILE);
        mapper = new ObjectMapper();
        mapper.configure(JsonGenerator.Feature.QUOTE_FIELD_NAMES, false);
        mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        if (!propFile.isFile())
        {
            LOG.info("Created new " + CART_BUTTON_CONFIG_FILE);
            setDefaultButtonConfigs(MINI_CART_BUTTON_CONFIG);
            setDefaultButtonConfigs(MARK_BUTTON_CONFIG);
            setDefaultButtonConfigs(CART_BUTTON_CONFIG);

            storeInPropertiesFile();
        }
        LOG.info("Loaded " + CART_BUTTON_CONFIG_FILE);
        loadPropertiesFromFile();
        smartButtonHeight = initHeightDefaultValues();
        Collections.addAll(smartButtonColor, "gold", "blue", "silver", "black");
        Collections.addAll(smartButtonShape, "pill", "rect");
        Collections.addAll(smartButtonLabel, "checkout", "pay", "buynow", "paypal");
        Collections.addAll(smartButtonLayout, "horizontal", "vertical");
    }

    public void setProperty(String key, String value)
    {
        prop.setProperty(key, value);
        storeInPropertiesFile();
    }

    public void setDefaultButtonConfigs(final String key){
        PayPalButtonStyleData miniCartButtonStyleData = createButtonStyleData("gold", "rect",
                "paypal", "vertical", Boolean.FALSE, 35);
        PayPalButtonStyleData cartButtonStyleData = createButtonStyleData("gold", "rect", "paypal",
                "vertical", Boolean.FALSE, 35);
        PayPalButtonStyleData markButtonStyleData = createButtonStyleData("gold", "rect", "paypal",
                "horizontal", Boolean.FALSE, 55);

        String miniCartStyle = parsePayPalButtonConfigDataIntoJsonString(miniCartButtonStyleData);
        String cartButtonStyle = parsePayPalButtonConfigDataIntoJsonString(cartButtonStyleData);
        String markButtonStyle = parsePayPalButtonConfigDataIntoJsonString(markButtonStyleData);

        switch (key) {
            case MINI_CART_BUTTON_CONFIG:
                setProperty(key, miniCartStyle);
                break;
            case MARK_BUTTON_CONFIG:
                setProperty(key, markButtonStyle);
                break;
            case CART_BUTTON_CONFIG:
                setProperty(key, cartButtonStyle);
        }
    }

    private PayPalButtonStyleData createButtonStyleData(String color, String shape, String label, String layout,
                                                        boolean tagline, int height){
        PayPalButtonStyleData buttonStyleData = new PayPalButtonStyleData();
        buttonStyleData.setColor(color);
        buttonStyleData.setLayout(layout);
        buttonStyleData.setTagline(tagline);
        buttonStyleData.setHeight(height);
        buttonStyleData.setLabel(label);
        buttonStyleData.setShape(shape);
        return buttonStyleData;
    }

    private List<Integer> initHeightDefaultValues()
    {
        List<Integer> heightValues = new ArrayList<>();
        for (int i = 25; i<= 55; i++)
        {
            heightValues.add(i);
        }
        return heightValues;
    }

    public void handleStyleDataUpdate(HttpServletRequest request, final String buttonConfig){
        PayPalButtonStyleData buttonStyleData = createButtonStyleData(request.getParameter("color"),
                request.getParameter("shape"), request.getParameter("label"), request.getParameter("layout"),
                Boolean.parseBoolean(request.getParameter("tagline")), Integer.parseInt(request.getParameter("height")));
        String styleDataJson = parsePayPalButtonConfigDataIntoJsonString(buttonStyleData);
        setProperty(buttonConfig, styleDataJson);
    }

    public String parsePayPalButtonConfigDataIntoJsonString(PayPalButtonStyleData buttonConfig){
        String buttonStyleConfig = "";

        try {
            buttonStyleConfig = getMapper().writeValueAsString(buttonConfig);
        } catch (JsonProcessingException e) {
            LOG.error("Error during parsing PayPalButtonConfigData into JSON");
            e.printStackTrace();
        }

        return buttonStyleConfig;
    }

    public PayPalButtonStyleData parsingJsonStringIntoPayPalButtonConfigData(String buttonStyleConfig){
        PayPalButtonStyleData buttonConfig = null;
        try {
            buttonConfig = getMapper().readValue(buttonStyleConfig, PayPalButtonStyleData.class);

        } catch (JsonProcessingException e) {
            buttonConfig = new PayPalButtonStyleData();
            LOG.error("Error during parsing JSON date into PayPalButtonConfigData");
            e.printStackTrace();
        }

        return buttonConfig;
    }

    public String getProperty(String key)
    {
        loadPropertiesFromFile();
        String result = prop.getProperty(key);
        return result == null ? "" : result;
    }

    public void storeInPropertiesFile()
    {
        try (OutputStream out = new FileOutputStream(propFile))
        {
            prop.store(out, "Save properties in file");
        }
        catch (Exception e)
        {
            LOG.error(CART_BUTTON_CONFIG_FILE + " is not saved,  error: " + e.getMessage(), e);
        }
    }

    private void loadPropertiesFromFile()
    {
        try (InputStream input = new FileInputStream(propFile))
        {
            prop.load(input);
        }
        catch (Exception e)
        {
            LOG.error("Error during reading " + CART_BUTTON_CONFIG_FILE + ": " + e.getMessage(), e);
        }
    }

    public String getFormattedProperty(String key) {
        return getProperty(key)
                .trim()
                .replace("\"","\\\"")
                .replaceAll("^\\{|}$", "");
    }

    public ObjectMapper getMapper() {
        return mapper;
    }

    public void setMapper(ObjectMapper mapper) {
        this.mapper = mapper;
    }

    public List<String> getSmartButtonColor() {
        return smartButtonColor;
    }

    public List<String> getSmartButtonShape() {
        return smartButtonShape;
    }

    public List<Integer> getSmartButtonHeight() {
        return smartButtonHeight;
    }

    public List<String> getSmartButtonLabel() {
        return smartButtonLabel;
    }

    public List<String> getSmartButtonLayout() {
        return smartButtonLayout;
    }

}
