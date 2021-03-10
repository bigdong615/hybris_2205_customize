
Steps to add custom coredata product catalog impexes :

1. Add custom impex file in - /blinitialdata/import/coredata/productCatalogs/blProductCatalog/custom
2. After adding impex file in custom folder we need to add the files entry in sequence impex - /blinitialdata/resources/blinitialdata/import/coredata/productCatalogs/core_custom_product_data_sequence.impex
3. To add custom impex entry please use the below example to add entry in core_custom_product_data_sequence.impex file :
	#% impex.includeExternalData(InitialDataSystemSetup.class.getResourceAsStream("$productCatalogsCustomPath/demo.impex"), "utf-8", 0, 0);

