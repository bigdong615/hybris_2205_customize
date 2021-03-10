
Steps to add custom coredata common impexes :

1. Add custom impex file in - /blinitialdata/import/coredata/common/custom
2. After adding impex file in custom folder we need to add the files entry in sequence impex - /blinitialdata/resources/blinitialdata/import/coredata/common/core_custom_common_data_sequence.impex
3. To add custom impex entry please use the below example to add entry in core_custom_common_data_sequence.impex file :
	#% impex.includeExternalData(InitialDataSystemSetup.class.getResourceAsStream("$commonCustomPath/demo.impex"), "utf-8", 0, 0);

