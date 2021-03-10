
Steps to add custom coredata stores impexes :

1. Add custom impex file in - /blinitialdata/import/coredata/stores/bl/custom
2. After adding impex file in custom folder we need to add the files entry in sequence impex - /blinitialdata/resources/blinitialdata/import/coredata/stores/core_custom_stores_data_sequence.impex
3. To add custom impex entry please use the below example to add entry in core_custom_stores_data_sequence.impex file :
	#% impex.includeExternalData(InitialDataSystemSetup.class.getResourceAsStream("$storesCustomPath/demo.impex"), "utf-8", 0, 0);

