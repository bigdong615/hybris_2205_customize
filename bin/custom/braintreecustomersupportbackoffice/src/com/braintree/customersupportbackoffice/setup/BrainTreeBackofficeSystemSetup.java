package com.braintree.customersupportbackoffice.setup;

import com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants;
import de.hybris.platform.commerceservices.setup.AbstractSystemSetup;
import de.hybris.platform.core.initialization.SystemSetup;
import de.hybris.platform.core.initialization.SystemSetupContext;
import de.hybris.platform.core.initialization.SystemSetupParameter;
import de.hybris.platform.core.initialization.SystemSetupParameterMethod;

import java.util.Collections;
import java.util.List;

@SystemSetup(extension = BraintreecustomersupportbackofficeConstants.EXTENSIONNAME)
public class BrainTreeBackofficeSystemSetup extends AbstractSystemSetup {
    @Override
    @SystemSetupParameterMethod
    public List<SystemSetupParameter> getInitializationOptions() {
        return Collections.emptyList();
    }

    @SystemSetup(type = SystemSetup.Type.PROJECT, process = SystemSetup.Process.ALL)
    public void createProjectData(final SystemSetupContext context) {
        importImpexFile(context, "/impex/projectdata_braintreecustomersupport-access-rights.impex", true);
    }
}
